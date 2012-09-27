module Main (main) where

import Control.Monad      (forever)
import System.Environment (getArgs, getProgName)
import System.IO

import qualified Data.ByteString       as BS
import qualified Data.ByteString.Char8 as BC
import qualified System.ZMQ3           as Z


main :: IO ()
main = do
    topic <- getArgs >>= opts

    hSetBuffering stdin LineBuffering

    Z.withContext 1 $ \c ->
        Z.withSocket c Z.Pub $ \s -> do
            Z.connect s "ipc:///tmp/rtaild"

            forever $ do
                l <- BS.getLine
                Z.sendMulti s [topic, l]
                BS.hPut stdout l >> BS.hPut stdout nl

    where
        opts (x:_) = return . BC.pack $ x
        opts _     = getProgName >>= \p -> ioError $ userError $
                     "Usage: " ++ p ++ " <topic>"

        nl = BS.pack [0x0A]
