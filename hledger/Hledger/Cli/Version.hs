{-# LANGUAGE CPP #-}
{-
Version-related utilities. See the Makefile for details of our version
numbering policy.
-}

module Hledger.Cli.Version (
                            version
                           ,progversionstr
                           ,binaryfilename
)
where
import System.Info (os, arch)

import Hledger.Data.Utils


-- version and PATCHLEVEL are set by the make process

version       = "0.13.0"

#ifdef PATCHLEVEL
patchlevel = "." ++ show PATCHLEVEL -- must be numeric !
#else
patchlevel = ""
#endif

buildversion  = version ++ patchlevel :: String

-- | Given a program name, return a human-readable version string.  For
-- development builds, at least non-cabal builds, the patch level (ie the
-- number of patches applied since last release tag) will also be
-- included.
progversionstr :: String -> String
progversionstr progname = progname ++ "-" ++ versionstr ++ configmsg
    where
      versionstr = prettify $ splitAtElement '.' buildversion
          where
            prettify (major:minor:bugfix:patches:[]) =
                printf "%s.%s%s%s" major minor bugfix' patches'
                    where
                      bugfix'
                          | bugfix `elem` ["0"{-,"98","99"-}] = ""
                          | otherwise = '.' : bugfix
                      patches'
                          | patches/="0" = "+"++patches
                          | otherwise = ""
            prettify s = intercalate "." s

      configmsg | null buildflags = ""
                | otherwise       = " with " ++ intercalate ", " buildflags

      buildflags = []

-- | Given a program name, return a precise platform-specific executable
-- name suitable for naming downloadable binaries.  Can raise an error if
-- the version and patch level was not defined correctly at build time.
binaryfilename :: String -> String
binaryfilename progname = prettify $ splitAtElement '.' buildversion
                where
                  prettify (major:minor:bugfix:patches:[]) =
                      printf "%s-%s.%s%s%s-%s-%s%s" progname major minor bugfix' patches' os' arch suffix
                          where
                            bugfix'
                                | bugfix `elem` ["0"{-,"98","99"-}] = ""
                                | otherwise = '.' : bugfix
                            patches'
                                | patches/="0" = '+' : patches
                                | otherwise = ""
                            (os',suffix)
                                | os == "darwin"  = ("mac","")
                                | os == "mingw32" = ("windows",".exe")
                                | otherwise       = (os,"")
                  prettify (major:minor:bugfix:[]) = prettify [major,minor,bugfix,"0"]
                  prettify (major:minor:[])        = prettify [major,minor,"0","0"]
                  prettify (major:[])              = prettify [major,"0","0","0"]
                  prettify []                      = error' "VERSION is empty, please fix"
                  prettify _                       = error' "VERSION has too many components, please fix"
