module Main (main) where

import Data.List          (partition)
import System.Environment (getArgs, getProgName)

import Network.Rtail


main :: IO ()
main = parseArgs >>= uncurry runClient

  where
    parseArgs = getArgs >>= opts >>=
                  \ (addrs, topics) -> return (map publicUri addrs, topics)

    opts xs@(_:_) = do
        let (as,ts) = partition (elem ':') xs
        if (null as || null ts) then opts [] else return (as,ts)

    opts _ = getProgName >>= \p -> ioError $ userError $
             "Usage: " ++ p ++ " <host:port>... <topic>..."
