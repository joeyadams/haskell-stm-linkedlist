-- | Doubly linked lists for use with STM (Software Transactional Memory).
--
-- Doubly linked lists provide efficient insertion and removal of
-- non-comparable items.

module Data.STM.LinkedList.Internal where

import Control.Concurrent.STM
import System.IO (fixIO)

-- | Circular doubly linked list.
newtype LinkedList a = LinkedList (Node a)

data Node a
    = Node
        { nodePrev  :: NodePtr a
        , nodeNext  :: NodePtr a
        , nodeValue :: Maybe a
            -- ^ 'Nothing' if this is the list head.
        }

type NodePtr a = TVar (Node a)

empty :: STM (LinkedList a)
empty = do
    prev_ptr <- newTVar undefined
    next_ptr <- newTVar undefined
    let node = Node prev_ptr next_ptr Nothing
    writeTVar prev_ptr node
    writeTVar next_ptr node
    return $ LinkedList node

-- | IO version of 'empty'.
emptyIO :: IO (LinkedList a)
emptyIO = do
    node <- fixIO $ \node -> do
        prev_ptr <- newTVarIO node
        next_ptr <- newTVarIO node
        return (Node prev_ptr next_ptr Nothing)
    return $ LinkedList node

prepend :: a -> LinkedList a -> STM (Node a)
prepend = undefined

append :: a -> LinkedList a -> STM (Node a)
append = undefined

remove :: Node a -> STM ()
remove = undefined

toList :: LinkedList a -> STM [a]
toList = undefined
