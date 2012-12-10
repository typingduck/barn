module Main (main) where

import Control.Monad      (forever)
import System.Environment (getArgs, getProgName)

import qualified System.ZMQ3 as Z


main :: IO ()
main = do
    (local,port) <- getArgs >>= opts

    Z.withContext 1 $ \c ->
        Z.withSocket c Z.Sub $ \sub ->
        Z.withSocket c Z.Pub $ \pub -> do
            Z.bind sub $ "ipc://" ++ local
            Z.bind pub $ "tcp://*:" ++ port

            Z.subscribe sub ""

            forever $ Z.receiveMulti sub >>= Z.sendMulti pub

    where
        opts (x:y:_) = return (x, y)
        opts _       = getProgName >>= \p -> ioError $ userError $
                       "Usage: " ++ p ++ " /path/to/rtaild.sock <port>"
