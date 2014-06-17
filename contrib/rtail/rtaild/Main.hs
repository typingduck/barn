module Main (main) where

import System.Environment (getArgs, getProgName)

import Network.Rtail


main :: IO ()
main = parseArgs >>= uncurry runDaemon

  where
    parseArgs = getArgs >>= opts >>= \ (ipc, port) ->
                  return (aggregationUri ipc, publicUriFromPort . read $ port)

    opts (x:y:_) = return (x, y)
    opts _       = getProgName >>= \p -> ioError $ userError $
                     "Usage: " ++ p ++ " /path/to/rtaild.sock <port>"
