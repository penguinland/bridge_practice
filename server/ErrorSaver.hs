module ErrorSaver(ErrorSaver, newErrorSaver, saveError) where

import Control.Concurrent(forkIO)
import Control.Concurrent.Classy.BoundedChan(
    BoundedChan, newBoundedChan, writeBoundedChan, readBoundedChan)
import Control.Exception(SomeException)
import Control.Monad(forever)
import Data.Time(getCurrentTime)


type ErrorSaver = BoundedChan IO SomeException


newErrorSaver :: IO ErrorSaver
newErrorSaver = do
    -- Appending to a file is not thread safe: if multiple threads append
    -- concurrently, your data can get all interleaved. Instead, spin up a new
    -- thread to append to the file, and send things to it via a thread safe
    -- channel.
    channel <- newBoundedChan 1
    _ <- forkIO $ runWorker channel  -- Throw away the threadID: we don't care
    return channel
  where
    runWorker :: ErrorSaver -> IO ()
    runWorker chan = forever $ do
        err <- readBoundedChan chan
        now <- getCurrentTime
        let prefix = show now ++ " New Error:\n"
            suffix = "\n" ++ replicate 80 '-' ++ "\n"
        -- Sneaky trick alert: put `show err` after the `$` instead of just
        -- `err`, or else the error gets re-thrown as it is evaluated.
        appendFile "errors.log" .  (++ suffix) . (prefix ++) $ show err


saveError :: ErrorSaver -> SomeException -> IO ()
saveError = writeBoundedChan
