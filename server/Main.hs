{-# LANGUAGE OverloadedStrings #-}
module Main where

import Control.Monad.Trans(liftIO)
import Data.Text(pack)
import Web.Spock(SpockM, var, text, get, root, (<//>), spock, runSpock)
import Web.Spock.Config(PoolOrConn(PCNoDatabase), defaultSpockCfg)


data MySession = EmptySession
data MyAppState = EmptyAppState


main :: IO ()
main = do
    spockCfg <- defaultSpockCfg EmptySession PCNoDatabase EmptyAppState
    runSpock 8080 (spock spockCfg app)


app :: SpockM () MySession MyAppState ()
app = do
    get root $ text "Hello World!"
    get ("hello" <//> var) $ \name ->
        text ("Hello " <> name <> ", you are visitor " <> pack (show 1234))
