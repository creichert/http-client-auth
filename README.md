# http-client-auth

[![Build Status](https://secure.travis-ci.org/creichert/http-client-auth.png?branch=master)](http://travis-ci.org/creichert/http-client-auth)

HTTP Basic and Digest Authentication for `http-client` and
`http-conduit`.

# Example

```haskell
> let url = "http://foo.com"
> let handler = withManager . httpLbs
> parseUrl url >>= runMaybeT . requestWithAuth "user" "pass" handler
```

# Build

```
$ cabal sandbox init
$ cabal install --only-dep
```
***It may be necessary to use `--allow-newer` to build the package.***

This repository is a fork of
[http-client-auth-0.1.0.0](http://hackage.haskell.org/package/http-client-auth)
which has been updated to work with latest GHC and supporting
libraries.

I have modified the `qop` message in the digest auth request which has
fixed a bug in my own use cases. See commit #a8d542cf.
