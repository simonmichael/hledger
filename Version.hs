{-# OPTIONS_GHC -cpp #-}
{-
Version-related utilities.

We should follow http://haskell.org/haskellwiki/Package_versioning_policy .
But currently hledger's version is MAJOR[.MINOR[.BUGFIX]][+PATCHLEVEL].
See also the Makefile.

-}

module Version
where
import System.Info (os, arch)
import Ledger.Utils
import Options (progname)

-- version and PATCHLEVEL are set by the makefile
version       = "0.6.0"

#ifdef PATCHLEVEL
patchlevel = "." ++ show PATCHLEVEL -- must be numeric !
#else
patchlevel = ""
#endif

buildversion  = version ++ patchlevel

binaryfilename = prettify $ splitAtElement '.' buildversion
                where
                  prettify (major:minor:bugfix:patches:[]) =
                      printf "hledger-%s.%s%s%s-%s-%s" major minor bugfix' patches' os arch
                          where
                            bugfix'
                                | bugfix `elem` ["0"{-,"98","99"-}] = ""
                                | otherwise = "."++bugfix
                            patches'
                                | patches/="0" = "+"++patches
                                | otherwise = ""
                  prettify s = intercalate "." s

versionstr    = prettify $ splitAtElement '.' buildversion
                where
                  prettify (major:minor:bugfix:patches:[]) =
                      printf "%s.%s%s%s%s" major minor bugfix' patches' desc
                          where
                            bugfix'
                                | bugfix `elem` ["0"{-,"98","99"-}] = ""
                                | otherwise = "."++bugfix
                            patches'
                                | patches/="0" = "+"++patches++" patches"
                                | otherwise = ""
                            desc
--                                 | bugfix=="98" = " (alpha)"
--                                 | bugfix=="99" = " (beta)"
                                | otherwise = ""
                  prettify s = intercalate "." s

versionmsg    = progname ++ "-" ++ versionstr ++ configmsg
    where configmsg
              | null configflags = " with no extras"
              | otherwise = " with " ++ intercalate ", " configflags

configflags   = tail [""
#ifdef VTY
  ,"vty"
#endif
#ifdef HAPPS
  ,"happs"
#endif
 ]
