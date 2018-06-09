{-# LANGUAGE CPP #-}
module Settings.Development where

development :: Bool
development =
#if DEVELOPMENT
  True
#else
  False
#endif

production :: Bool
production = not development
