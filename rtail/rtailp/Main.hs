module Main (main) where

import           System.Environment    (getArgs, getProgName)

import qualified Data.ByteString.Char8 as BC

import           Network.Rtail


main :: IO ()
main = parseArgs >>= \ (x,y,z) -> runPipe x y z

  where
    parseArgs = getArgs >>= opts >>= \ (ipc, topic, bufsize) ->
                    return (aggregationUri ipc, topic, bufsize)

    opts (x:y:z:_) = return (x, BC.pack y, read z)
    opts (x:y:_)   = return (x, BC.pack y, 50000)
    opts _         = getProgName >>= \p -> ioError $ userError $
                       "Usage: " ++ p ++ " /path/to/rtaild.sock <topic> <buffer size>"
