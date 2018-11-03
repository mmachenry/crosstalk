module Main where

import Data.Char (isPunctuation, isSpace)
import Data.Monoid (mappend)
import Data.Text (Text)
import Control.Exception (finally)
import Control.Monad (forM_, forever)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Time.LocalTime (ZonedTime(..), getZonedTime)

import qualified Network.WebSockets as WS

main :: IO ()
main = WS.runServer "127.0.0.1" 9160 application

application :: WS.ServerApp
application pending = do
    conn <- WS.acceptRequest pending
    WS.forkPingThread conn 30
    --msg <- WS.receiveData conn
    forever $ do
      ZonedTime localTime _timeZone <- getZonedTime
      --print localTime
      WS.sendTextData conn (T.pack (show localTime))
      threadDelay 1000000

