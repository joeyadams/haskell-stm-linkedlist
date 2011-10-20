import Data.STM.LinkedList (LinkedList)
import qualified Data.STM.LinkedList as LinkedList

import Control.Concurrent
import Control.Concurrent.STM
import Control.Exception
import Control.Monad (forM_, forever)
import Foreign.Marshal.Error (void)

type Event = String
type EventHandler = (Event -> IO ())

withEventHandler :: LinkedList EventHandler
                 -> EventHandler
                 -> IO a
                 -> IO a
withEventHandler list handler action =
    bracket (atomically $ LinkedList.append handler list)
            (atomically . LinkedList.delete)
            (\_ -> action)

dispatchEvent :: LinkedList EventHandler
              -> Event
              -> IO ()
dispatchEvent list event = do
    handlers <- atomically $ LinkedList.toList list
    forM_ handlers $ \handler -> handler event

main :: IO ()
main = do
    list <- LinkedList.emptyIO

    let testThread listeningFor = void $ forkIO $ do
            eventReceived <- atomically $ newTVar False
            let handler ev = do
                    putStrLn $ listeningFor ++ ": Received " ++ ev
                    if ev == listeningFor
                        then atomically $ writeTVar eventReceived True
                        else return ()
            withEventHandler list handler $ do
                atomically $ do
                    r <- readTVar eventReceived
                    if r
                        then return ()
                        else retry
                putStrLn $ listeningFor ++ ": Caught my event; leaving now"

    testThread "brown"
    testThread "chicken"
    testThread "brown"
    testThread "cow"

    forever $ do
        line <- getLine
        dispatchEvent list line
