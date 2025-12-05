module ErrorSaver(ErrorSaver, newErrorSaver, saveError) where

import Control.Concurrent(forkIO)
import Control.Concurrent.Classy.BoundedChan(
    BoundedChan, newBoundedChan, writeBoundedChan, readBoundedChan)
import Control.Exception(SomeException)


type ErrorSaver = BoundedChan IO SomeException


newErrorSaver :: IO ErrorSaver
newErrorSaver = do
    -- Appending to a file is not thread safe: if multiple threads append
    -- concurrently, your data can get all interleaved. Instead, spin up a new
    -- thread to append to the file, and send things to it via a thread safe
    -- channel.
    channel <- newBoundedChan 3
    _ <- forkIO $ runWorker channel  -- Throw away the threadID: we don't care
    return channel
  where
    runWorker :: ErrorSaver -> IO ()
    runWorker chan = do
        e <- readBoundedChan chan
        appendFile "errors.log" ('\n':'\n':show e)
        runWorker chan


saveError :: ErrorSaver -> SomeException -> IO ()
saveError = writeBoundedChan
