module Main (main) where

import           System.Environment    (getArgs, getProgName)
import           System.IO

import qualified Data.ByteString.Char8 as BC

import           Network.Rtail


main :: IO ()
main = do
    hSetBuffering stdin LineBuffering
    parseArgs >>= uncurry runPipe

  where
    parseArgs = getArgs >>= opts >>=
                  \ (ipc, topic) -> return (aggregationUri ipc, topic)

    opts (x:y:_) = return (x, BC.pack y)
    opts _       = getProgName >>= \p -> ioError $ userError $
                   "Usage: " ++ p ++ " /path/to/rtaild.sock <topic>"
