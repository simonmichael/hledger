{-# LANGUAGE CPP #-}
{-
Version-related utilities. See the Makefile for details of our version
numbering policy.
-}

module Hledger.Cli.Version
where
import System.Info (os, arch)
import Hledger.Data.Utils

-- version and PATCHLEVEL are set by the makefile
version       = "0.11.99"

#ifdef PATCHLEVEL
patchlevel = "." ++ show PATCHLEVEL -- must be numeric !
#else
patchlevel = ""
#endif

progname      = "hledger"
timeprogname  = "hours"

buildversion  = version ++ patchlevel :: String

binaryfilename = prettify $ splitAtElement '.' buildversion :: String
                where
                  prettify (major:minor:bugfix:patches:[]) =
                      printf "hledger-%s.%s%s%s-%s-%s%s" major minor bugfix' patches' os' arch suffix
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
                  prettify []                      = error "VERSION is empty, please fix"
                  prettify _                       = error "VERSION has too many components, please fix"

versionstr    = prettify $ splitAtElement '.' buildversion :: String
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

versionmsg    = progname ++ "-" ++ versionstr ++ configmsg :: String
    where configmsg
              | null configflags = " with no extras"
              | otherwise = " with " ++ intercalate ", " configflags

configflags   = tail [""
#ifdef CHART
  ,"chart"
#endif
#ifdef VTY
  ,"vty"
#endif
#if defined(WEB)
  ,"web (using yesod/hamlet/simpleserver)"
#elif defined(WEB610)
  ,"web (using loli/hsp/simpleserver)"
#endif
 ]
