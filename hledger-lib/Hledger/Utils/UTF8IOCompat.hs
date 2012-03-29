{-# LANGUAGE CPP #-}
{- |

UTF-8 aware string IO functions that will work across multiple platforms
and GHC versions. Includes code from Text.Pandoc.UTF8 ((C) 2010 John
MacFarlane).

Example usage:

 import Prelude hiding (readFile,writeFile,appendFile,getContents,putStr,putStrLn)
 import UTF8IOCompat   (readFile,writeFile,appendFile,getContents,putStr,putStrLn)
 import UTF8IOCompat   (SystemString,fromSystemString,toSystemString,error',userError')

-}

module Hledger.Utils.UTF8IOCompat (
  readFile,
  writeFile,
  appendFile,
  getContents,
  hGetContents,
  putStr,
  putStrLn,
  hPutStr,
  hPutStrLn,
  --
  SystemString,
  fromSystemString,
  toSystemString,
  error',
  userError',
)
where

import Control.Monad (liftM)
import qualified Data.ByteString.Lazy as B
import qualified Data.ByteString.Lazy.Char8 as B8
import qualified Data.ByteString.Lazy.UTF8 as U8 (toString, fromString)
import Prelude hiding (readFile, writeFile, appendFile, getContents, putStr, putStrLn)
import System.IO (Handle)
#if __GLASGOW_HASKELL__ < 702
import Codec.Binary.UTF8.String as UTF8 (decodeString, encodeString, isUTF8Encoded)
import System.Info (os)
#endif

bom :: B.ByteString
bom = B.pack [0xEF, 0xBB, 0xBF]

stripBOM :: B.ByteString -> B.ByteString
stripBOM s | bom `B.isPrefixOf` s = B.drop 3 s
stripBOM s = s

readFile :: FilePath -> IO String
readFile = liftM (U8.toString . stripBOM) . B.readFile

writeFile :: FilePath -> String -> IO ()
writeFile f = B.writeFile f . U8.fromString

appendFile :: FilePath -> String -> IO ()
appendFile f = B.appendFile f . U8.fromString

getContents :: IO String
getContents = liftM (U8.toString . stripBOM) B.getContents

hGetContents :: Handle -> IO String
hGetContents h = liftM (U8.toString . stripBOM) (B.hGetContents h)

putStr :: String -> IO ()
putStr = bs_putStr . U8.fromString

putStrLn :: String -> IO ()
putStrLn = bs_putStrLn . U8.fromString

hPutStr :: Handle -> String -> IO ()
hPutStr h = bs_hPutStr h . U8.fromString

hPutStrLn :: Handle -> String -> IO ()
hPutStrLn h = bs_hPutStrLn h . U8.fromString

-- span GHC versions including 6.12.3 - 7.4.1:
bs_putStr         = B8.putStr
bs_putStrLn       = B8.putStrLn
bs_hPutStr        = B8.hPut
bs_hPutStrLn h bs = B8.hPut h bs >> B8.hPut h (B.singleton 0x0a)


-- | A string received from or being passed to the operating system, such
-- as a file path, command-line argument, or environment variable name or
-- value. With GHC versions before 7.2 on some platforms (posix) these are
-- typically encoded. When converting, we assume the encoding is UTF-8 (cf
-- <http://www.dwheeler.com/essays/fixing-unix-linux-filenames.html#UTF8>).
type SystemString = String

-- | Convert a system string to an ordinary string, decoding from UTF-8 if
-- it appears to be UTF8-encoded and GHC version is less than 7.2.
fromSystemString :: SystemString -> String
#if __GLASGOW_HASKELL__ < 702
fromSystemString s = if UTF8.isUTF8Encoded s then UTF8.decodeString s else s
#else
fromSystemString = id
#endif

-- | Convert a unicode string to a system string, encoding with UTF-8 if
-- we are on a posix platform with GHC < 7.2.
toSystemString :: String -> SystemString
#if __GLASGOW_HASKELL__ < 702
toSystemString = case os of
                     "unix" -> UTF8.encodeString
                     "linux" -> UTF8.encodeString
                     "darwin" -> UTF8.encodeString
                     _ -> id
#else
toSystemString = id
#endif

-- | A SystemString-aware version of error.
error' :: String -> a
error' = error . toSystemString

-- | A SystemString-aware version of userError.
userError' :: String -> IOError
userError' = userError . toSystemString
