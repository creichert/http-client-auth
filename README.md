# http-client-auth

[![Build Status](https://secure.travis-ci.org/creichert/http-client-auth.png?branch=master)](http://travis-ci.org/creichert/http-client-auth)

HTTP Basic and Digest Authentication for `http-client` and `http-conduit`.

***This repository is a fork of http-client-auth-0.1.0.0 which has
   been modified to work with latest GHC and supporting libraries.***

# Example

> let url = "http://foo.com"
> let handler = withManager . httpLbs
> parseUrl url >>= runMaybeT . requestWithAuth "user" "pass" handler

# Build

```
$ cabal sandbox init
$ cabal install --only-dep
```

### Warning

It may be necessary to use `--allow-newer` to build the package.
