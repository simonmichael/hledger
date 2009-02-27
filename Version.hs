{-# OPTIONS_GHC -cpp #-}
module Version
where
import Ledger.Utils
import Options (progname)

#ifndef PATCHES
#define PATCHES "0"
#endif

-- updated by build process from VERSION
version       = "0.3.99"
-- PATCHES defined by build process from repo state
buildversion  = version ++ "." ++ PATCHES

versionstr    = prettify $ splitAtElement '.' buildversion
                where
                  prettify (major:minor:bugfix:patches:[]) =
                      printf "%s.%s%s%s%s" major minor bugfix' patches' desc
                          where
                            bugfix'
                                | bugfix `elem` ["0"{-,"98","99"-}] = ""
                                | otherwise = "."++bugfix
                            patches'
                                | patches/="0" = " + "++patches++" patches"
                                | otherwise = ""
                            desc
--                                 | bugfix=="98" = " (alpha)"
--                                 | bugfix=="99" = " (beta)"
                                | otherwise = ""
                  prettify s = intercalate "." s

versionmsg    = progname ++ " " ++ versionstr ++ configmsg ++ "\n"
    where configmsg
              | null configflags = ""
              | otherwise = ", built with " ++ intercalate ", " configflags

configflags   = tail [""
#ifdef VTY
  ,"vty"
#endif
#ifdef ANSI
  ,"ansi"
#endif
#ifdef HAPPS
  ,"happs"
#endif
 ]
