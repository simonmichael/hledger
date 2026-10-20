{-# LANGUAGE TemplateHaskell #-}
{-|
The git info (commit hash, date..) of the source being built, if available,
for the version strings of hledger, hledger-ui and hledger-web.

This is the only git lookup at compile time. Since a git change (commit, amend,
switch, or even staging) forces it to be recompiled, it is kept in this small module.
And NOINLINE keeps the value out of this module's interface, so that the modules
using it are not recompiled too; only the executables need to be relinked.
-}
module Hledger.Cli.GitInfo (gitInfo) where

import GitHash (GitInfo, tGitInfoCwdTry)

gitInfo :: Either String GitInfo
gitInfo = $$tGitInfoCwdTry
{-# NOINLINE gitInfo #-}
