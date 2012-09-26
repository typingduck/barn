module Main (main) where

import Control.Monad      (forever)
import System.Environment (getArgs, getProgName)
import System.IO

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import qualified System.ZMQ3           as Z


main :: IO ()
main = do
    port <- getArgs >>= opts

    Z.withContext 1 $ \c ->
        Z.withSocket c Z.Sub $ \sub ->
        Z.withSocket c Z.Pub $ \pub -> do
            Z.bind sub "ipc:///tmp/rtaild"
            Z.bind pub $ "tcp://*:" ++ port

            Z.subscribe sub ""

            forever $ Z.receiveMulti sub >>= Z.sendMulti pub

    where
        opts (x:_) = return x
        opts _     = getProgName >>= \p -> ioError $ userError $
                     "Usage: " ++ p ++ " <port>"
