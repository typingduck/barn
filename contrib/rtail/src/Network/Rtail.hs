{-# LANGUAGE OverloadedStrings #-}

module Network.Rtail
    ( runClient
    , runDaemon
    , runPipe

    , AggregationUri
    , PublicUri
    , aggregationUri
    , publicUri
    , publicUriFromPort
    ) where


import           Control.Exception (finally)
import           Control.Monad     (forever, unless)
import           Data.ByteString   (ByteString)
import           Data.List         (intersperse)
import           System.IO
import           System.ZMQ3

import qualified Data.ByteString   as BS


newtype AggregationUri = A { unA :: String } deriving (Show)
newtype PublicUri      = P { unP :: String } deriving (Show)

aggregationUri :: FilePath -> AggregationUri
aggregationUri path = A $ "ipc://" ++ path

publicUri :: String -> PublicUri
publicUri spec = P $ "tcp://" ++ spec

publicUriFromPort :: Int -> PublicUri
publicUriFromPort port = P $ "tcp://*:" ++ show port


runClient :: [PublicUri] -> [String] -> IO ()
runClient daemons topics =
    withContext $ \ c -> do
        setIoThreads 1 c

        withSocket c Sub $ \ s -> do
            mapM_ (connect s . unP) daemons
            mapM_ (subscribe s) topics

            forever $ receiveMulti s >>=
                mapM_ (BS.hPut stdout) . drop 1 . intersperse nl
            `finally` mapM_ (unsubscribe s) topics

runDaemon :: AggregationUri -> PublicUri -> IO ()
runDaemon (A ipc) (P pub) =
    withContext $ \ c -> do
        setIoThreads 1 c

        withSocket c Sub $ \ subS -> do
        withSocket c Pub $ \ pubS -> do
            bind subS ipc
            bind pubS pub

            subscribe subS ""

            forever $ receiveMulti subS >>= sendMulti pubS

runPipe :: AggregationUri -> ByteString -> Int -> IO ()
runPipe (A ipc) topic bufsize = do
    hSetBuffering stdin (BlockBuffering (Just bufsize))

    withContext $ \ c -> do
        setIoThreads 1 c

        withSocket c Pub $ \ s -> do
            connect s ipc
            loop s

  where
    loop sock = do
        chunk <- BS.hGetSome stdin bufsize

        unless (BS.null chunk) $ do
            let lines' = filter (not . BS.null) . BS.split 0x0A $ chunk
            sendMulti sock $ topic : lines'
            mapM_ (BS.hPut stdout) . intersperse nl $ lines'
            BS.hPut stdout nl

            loop sock

nl :: ByteString
nl = BS.pack [0x0A]
