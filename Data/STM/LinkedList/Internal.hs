-- | Doubly linked lists for use with STM (Software Transactional Memory).
--
-- Doubly linked lists provide efficient insertion and removal of
-- non-comparable items.

module Data.STM.LinkedList.Internal where

import Control.Concurrent.STM
import System.IO (fixIO)

-- | Circular doubly linked list.
newtype LinkedList a = LinkedList (NodePtr a)

data Node a
    = Node
        { nodePrev  :: NodePtr a
        , nodeNext  :: NodePtr a
        , nodeValue :: a
            -- ^ Warning: the value of a 'LinkedList' anchor is undefined.
        }

type NodePtr a = TVar (Node a)

emptyAnchor :: NodePtr a -> Node a
emptyAnchor ptr =
    Node { nodePrev  = ptr
         , nodeNext  = ptr
         , nodeValue = error "nodeValue: undefined (list anchor)"
         }

empty :: STM (LinkedList a)
empty = do
    anchor <- newTVar undefined
    writeTVar anchor $ emptyAnchor anchor
    return $ LinkedList anchor

-- | IO version of 'empty'.
emptyIO :: IO (LinkedList a)
emptyIO = do
    anchor <- fixIO (newTVarIO . emptyAnchor)
    return $ LinkedList anchor
