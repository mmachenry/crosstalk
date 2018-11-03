#!/usr/bin/env stack
-- stack --resolver lts-12.5 runghc --package websockets server.hs 

module Main where

import Control.Monad (forever)
import Control.Concurrent (threadDelay)
import qualified Data.Text as T
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

