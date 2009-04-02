{-# OPTIONS_GHC -cpp #-}
module Version
where
import Ledger.Utils
import Options (progname)

-- updated by build process from VERSION
version       = "0.3.99"
#ifdef PATCHES
-- a "make" development build defines PATCHES from the repo state
patchlevel = "." ++ show PATCHES -- must be numeric !
#else
patchlevel = ""
#endif
buildversion  = version ++ patchlevel

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
#elif ANSI
  ,"ansi"
#endif
#ifdef HAPPS
  ,"happs"
#endif
 ]
