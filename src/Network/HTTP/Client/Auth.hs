------------------------------------------------------------------------
-- |
-- Module      : HLint
-- Description : HLint Static Analysis
-- Copyright   : (c) Miguel Mitrofanov <miguelimo38@yandex.ru>
-- License     : BSD3
-- Maintainer  : Christopher Reichert <creichert07@gmail.com>
-- Stability   : unstable
-- Portability : POSIX
--
-- @
-- > let url = "http://host.com"
-- > let handler = withManager . httpLbs
-- > parseUrl url >>= runMaybeT . requestWithAuth "user" "pass" handler
-- @


module Network.HTTP.Client.Auth (

      -- * High-level functions
      requestWithAuth
    , realm
    , getChallenge
    , makeRequestHeader

      -- * Types
    , Challenge

      -- * Low-level functions
    , extractAuthHeader
    , parseChallenge

      -- * Utils
    , makeRequestUri
    , makeRequestBodyHash
    ) where

import           Blaze.ByteString.Builder     (toLazyByteString)
import           Codec.Binary.Base64.String   as B64 (encode)
import           Control.Applicative          ((<$>), (<*>))
import           Control.Monad                (guard, join, mplus, mzero)
import           Control.Monad.Trans          (lift, liftIO)
import           Control.Monad.Trans.Maybe    (MaybeT (MaybeT, runMaybeT),
                                               mapMaybeT)
import           Control.Monad.Trans.Resource (ResourceT, runResourceT)
import           Control.Monad.Trans.State    (State, evalState, get, put)
import           Crypto.Conduit               (sinkHash)
import qualified Data.ByteString.Lazy         as L (toChunks)
import qualified Data.ByteString.Lazy.UTF8    as LU (fromString)
import qualified Data.ByteString.UTF8         as BU (fromString, toString)
import           Data.CaseInsensitive         (mk)
import           Data.Char                    (isAlphaNum, isAscii, isSpace)
import           Data.Conduit                 (yield, ($$), (=$))
import qualified Data.Conduit.List            as CL (concatMap, sourceList)
import           Data.Digest.Pure.MD5         (MD5Digest, md5)
import           Data.List                    (intersperse, isPrefixOf)
import           Data.Maybe                   (catMaybes)
import           Data.Monoid                  (Monoid (mappend, mconcat, mempty))
import           Network.HTTP.Conduit



data Once a = NotEncountered
            | Once a
            | Multiple
            deriving Show



instance Monoid (Once a) where
    mempty = NotEncountered
    NotEncountered `mappend` o = o
    Once a `mappend` NotEncountered = Once a
    Once _ `mappend` Once _ = Multiple
    Once _ `mappend` Multiple = Multiple
    Multiple `mappend` _ = Multiple


onceToMaybe :: Once a -> Maybe (Maybe a)
onceToMaybe NotEncountered = Just Nothing
onceToMaybe (Once a) = Just (Just a)
onceToMaybe Multiple = Nothing



-- | challenge sent by the server.
data Challenge = None
               | Basic BasicChallenge
               | Digest DigestChallenge
               deriving Show


-- | Realm is the only thing users are supposed to know about the
--   challenge.
realm :: Challenge -> Maybe String
realm None = Nothing
realm (Basic bc) = Just $ basicRealm bc
realm (Digest dc) = Just $ digestRealm dc


newtype BasicChallenge = BasicChallenge { basicRealm :: String }
                       deriving Show


data DigestChallenge = DigestChallenge
    { digestRealm :: String
    , domain      :: Maybe String
    , nonce       :: String
    , opaque      :: Maybe String
    , stale       :: Maybe Bool
    , algorithm   :: Maybe DigestAlgorithm
    , qop         :: Maybe QopValue
    } deriving Show


data MDigestChallenge = MDigestChallenge
    { mDigestRealm :: Once String
    , mDomain      :: Once String
    , mNonce       :: Once String
    , mOpaque      :: Once String
    , mStale       :: Once Bool
    , mAlgorithm   :: Once DigestAlgorithm
    , mQop         :: Maybe QopValue
    } deriving Show


instance Monoid MDigestChallenge where
    mempty = MDigestChallenge
           { mDigestRealm = mempty
           , mDomain = mempty
           , mNonce = mempty
           , mOpaque = mempty
           , mStale = mempty
           , mAlgorithm = mempty
           , mQop = mempty
           }

    mappend md1 md2 =
        let mapp f = mappend (f md1) (f md2)
        in MDigestChallenge
               { mDigestRealm = mapp mDigestRealm
               , mDomain = mapp mDomain
               , mNonce = mapp mNonce
               , mOpaque = mapp mOpaque
               , mStale = mapp mStale
               , mAlgorithm = mapp mAlgorithm
               , mQop = mapp mQop
               }


finDigestChallenge :: MDigestChallenge -> Maybe DigestChallenge
finDigestChallenge md =
    DigestChallenge
    <$> (join $ onceToMaybe $ mDigestRealm md)
    <*> (onceToMaybe $ mDomain md)
    <*> (join $ onceToMaybe $ mNonce md)
    <*> (onceToMaybe $ mOpaque md)
    <*> (onceToMaybe $ mStale md)
    <*> (onceToMaybe $ mAlgorithm md)
    <*> Just (mQop md)


data DigestAlgorithm = MD5
                     | MD5Sess
                     deriving Show


data QopValue = Auth
              | AuthInt
              deriving Show


instance Monoid QopValue where
    mempty = AuthInt
    Auth    `mappend` _ = Auth
    AuthInt `mappend` a = a


-- | This function extracts a WWW-Authenticate header from the
--   response.
extractAuthHeader :: Response body -> Maybe String
extractAuthHeader resp =
    BU.toString
    <$> lookup authheader headers
  where
    authheader = mk $ BU.fromString "WWW-Authenticate"
    headers    = responseHeaders resp


isWordChar :: Char -> Bool
isWordChar c = isAscii c && (c `elem` "_.-:" || isAlphaNum c)


orElse :: MaybeT (State String) a
       -> MaybeT (State String) a
       -> MaybeT (State String) a
orElse p1 p2 = do
    str <- lift get
    p1 `mplus` (lift (put str) >> p2)


token :: MaybeT (State String) String
token = do
    str <- lift get
    let ~(tok, rst) = span isWordChar str
    guard $ not $ null tok
    lift $ put $ dropWhile isSpace rst
    return tok


equal :: MaybeT (State String) ()
equal = do '=' : rst <- lift get
           lift $ put $ dropWhile isSpace rst


singleQuote :: MaybeT (State String) ()
singleQuote = do '"' : rst <- lift get
                 lift $ put $ dropWhile isSpace rst

quotedStr :: MaybeT (State String) String
quotedStr =
    let getStr str =
            do (f, rst) <- return $ span (`notElem` "\"\\") str
               let quote =
                       do '"' : tl <- return rst
                          lift $ put $ dropWhile isSpace tl
                          return f
               let escape =
                       do '\\' : c : tl <- return rst
                          guard $ isAscii c
                          s <- getStr tl
                          return $ f ++ c : s
               quote `orElse` escape
    in do '"' : str <- lift get
          getStr str

commaSep :: MaybeT (State String) a -> MaybeT (State String) [a]
commaSep g =
    let commaSepG =
            do a <- g
               str <- lift get
               as <-
                   case str of
                     ',' : rst ->
                         do lift $ put $ dropWhile isSpace rst
                            commaSepG
                     _ -> return []
               return $ a : as
    in commaSepG

eol :: MaybeT (State String) ()
eol = do str <- lift get
         guard $ null str

parseDigestChallenge :: MaybeT (State String) DigestChallenge
parseDigestChallenge = do
    digest <- token
    guard $ digest == "Digest"
    fields <- commaSep parseDigestFields
    eol
    MaybeT $ return $ finDigestChallenge $ mconcat fields

parseDigestFields :: MaybeT (State String) MDigestChallenge
parseDigestFields = do
      param <- token
      case param of
         "realm" ->
             do equal
                str <- quotedStr
                return $ mempty {mDigestRealm = Once str}
         "domain" ->
             do equal
                str <- quotedStr
                return $ mempty {mDomain = Once str}
         "nonce" ->
             do equal
                str <- quotedStr
                return $ mempty {mNonce = Once str}
         "opaque" ->
             do equal
                str <- quotedStr
                return $ mempty {mOpaque = Once str}
         "stale" ->
             do equal
                str <- token
                return $ mempty {mStale = Once $ str == "true"}
         "algorithm" ->
             do equal
                str <- token
                case str of
                  "MD5" -> return $ mempty {mAlgorithm = Once MD5}
                  "MD5-sess" -> return $ mempty {mAlgorithm = Once MD5Sess}
                  _ -> mzero
         "qop" ->
             do equal
                singleQuote
                qops <- commaSep token
                singleQuote
                let qopsData =
                        flip fmap qops $ \t ->
                            case t of
                              "auth" -> Just Auth
                              "auth-int" -> Just AuthInt
                              _ -> mempty
                return $ mempty {mQop = mconcat qopsData}
         _ -> do
             equal
             _ <- token `orElse` quotedStr
             return mempty



parseBasicChallenge :: MaybeT (State String) BasicChallenge
parseBasicChallenge = do
       basic <- token
       guard $ basic == "Basic"
       parseRealm
    where
      parseRealm = do
          param <- token
          case param of
              "realm" -> do
                  equal
                  str <- quotedStr
                  return $ BasicChallenge {basicRealm = str}
              _ -> do
                  equal
                  _ <- token `orElse` quotedStr
                  parseRealm



-- | This function parses the WWW-Authenticate header line to get a challenge.
--
-- If it fails, it's probably because the header is malformed
parseChallenge :: String -> Maybe Challenge
parseChallenge header =
    flip evalState header $ runMaybeT $
    fmap Basic parseBasicChallenge `orElse` fmap Digest parseDigestChallenge



-- | This function parses the response headers to get the challenge.
--
-- It failes if there is some challenge, but it can't be parsed.
-- If there is no challenge at all, function doesn't fail.
getChallenge :: Response body -> Maybe Challenge
getChallenge req =
    case extractAuthHeader req of
      Nothing -> return None
      Just header -> parseChallenge header


-- | This function creates a string that should be sent in the
--   Authorization header.
makeRequestHeader
    :: Monad m
    => String -- ^ login
    -> String -- ^ password
    -> String -- ^ string to use as cnonce, not very important yet
    -> Request -- ^ first request, already sent to the server
    -> Challenge -- ^ challenge generated by server in responce to that request
    -> MaybeT m String
makeRequestHeader _ _ _ _ None = mzero
makeRequestHeader login password _ _ (Basic _) =
    return $ "Basic " ++ concat (lines $ B64.encode $ login ++ ':' : password)
makeRequestHeader login password cnonce req (Digest dc) = do
       entityBodyHash <- lift $ makeRequestBodyHash req
       let fields =
               [
                return $ "username=\"" ++ login ++ "\"",
                return $ "realm=\"" ++ digestRealm dc ++ "\"",
                return $ "nonce=\"" ++ nonce dc ++ "\"",
                return $ "uri=\"" ++ uri ++ "\"",
                return $ "response=\"" ++ requestDigest ++ "\"",
                case algorithm dc of
                  Nothing -> mzero
                  Just MD5 -> return "algorithm=MD5"
                  Just MD5Sess -> return "algorithm=MD5-sess",
                case qop dc of
                  Nothing -> mzero
                  Just _ -> return $ "cnonce=\"" ++ cnonce ++ "\"",
                case opaque dc of
                  Nothing -> mzero
                  Just o -> return $ "opaque=\"" ++ o ++ "\"",
                case qop dc of
                  Nothing -> mzero
                  Just Auth -> return "qop=\"auth\""
                  Just AuthInt -> return "qop=\"auth-int\"",
                case qop dc of
                  Nothing -> mzero
                  Just _ -> return "nc=00000001"
               ]
           requestDigest =
               case qop dc of
                 Nothing -> h $ h a1 ++ ':' : nonce dc ++ ':' : h a2
                 Just Auth ->
                     h $
                     h a1 ++ ":" ++ nonce dc ++ ":00000001:" ++
                     cnonce ++ ":auth:" ++ h a2
                 Just AuthInt ->
                     h $
                     h a1 ++ ':' : nonce dc ++ ":00000001:" ++
                     cnonce ++ ":auth-int:" ++ h a2
           a1 =
               case algorithm dc of
                 Just MD5Sess ->
                     h (login ++ ':' : digestRealm dc ++ ':' : password) ++
                           ':' : nonce dc ++ ':' : cnonce

                 _ -> login ++ ':' : digestRealm dc ++ ':' : password
           a2 =
               case qop dc of
                 Just AuthInt ->
                      mtd ++ ':' : uri ++ ':' : entityBodyHash
                 _ -> mtd ++ ":" ++ uri
           uri = makeRequestUri req
           mtd = BU.toString $ method req
           h = show . md5 . LU.fromString
       return $ concat $ "Digest " : intersperse ", " (catMaybes fields)

-- | This function extracts URI part from the request.
--
-- It wouldn't include the host name.
makeRequestUri :: Request -> String
makeRequestUri req =
    let p = BU.toString $ path req
        pp = if "/" `isPrefixOf` p then p else '/' : p
    in pp

-- | This function makes an MD5 hash of the request body
makeRequestBodyHash :: Monad m => Request -> m String
makeRequestBodyHash req =
    case requestBody req of
        RequestBodyLBS lbs -> CL.sourceList (L.toChunks lbs) $$ hashSink
        RequestBodyBS bs -> yield bs $$ hashSink
        RequestBodyBuilder _ bldr -> yield bldr $$ bldrSink
        -- RequestBodyStream _ bldr -> bldr $$ bldrSink
        -- RequestBodyStreamChunked bldr -> bldr $$ bldrSink
    where
      bldrSink = CL.concatMap (L.toChunks . toLazyByteString) =$ hashSink
      hashSink = fmap (show :: MD5Digest -> String) sinkHash

-- | This is the main function. It sends a request, gets the response, and,
--
-- if this response requires authorization, it sends the same request again,
-- now including authorization data (user-supplied login and password).
requestWithAuth
  :: String -- ^ login
  -> String -- ^ password
  -> (Request -> IO (Response body))
  -- ^ function like @withManager . httpLbs@, to actually send a request
  -> Request -- ^ request to send (without authorization)
  -> MaybeT IO (Response body)
requestWithAuth login password query req = do
    let safeReq = req {checkStatus = \_ _ _ -> Nothing}
    resp <- lift $ query safeReq
    Just challenge <- return $ getChallenge resp
    let repeatReq = do
            let makeHeader = makeRequestHeader
                                           login
                                           password
                                           "cnonce" -- TODO set correctly
                                           req
                                           challenge

            header <- mapMaybeT runResourceT makeHeader
            let reqHeader =
                    (mk (BU.fromString "Authorization"),
                     BU.fromString header)
                authReq = req { requestHeaders = reqHeader : requestHeaders req
                              , cookieJar = Just $ responseCookieJar resp
                              }
            liftIO $ query authReq
    repeatReq `mplus` return resp
