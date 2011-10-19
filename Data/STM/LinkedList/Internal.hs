-- | Doubly linked lists for use with STM (Software Transactional Memory).
--
-- Doubly linked lists provide efficient insertion and removal of
-- non-comparable items.

module Data.STM.LinkedList.Internal where

import Control.Concurrent.STM
import System.IO (fixIO)

-- | Circular doubly linked list.
--
-- Warning: the value of the list anchor is undefined.
newtype LinkedList a = LinkedList (Node a)

data Node a
    = Node
        { nodePrev  :: TVar (Node a)
        , nodeNext  :: TVar (Node a)
        , nodeValue :: a
        }

emptyAnchor :: TVar (Node a) -> Node a
emptyAnchor ptr =
    Node { nodePrev  = ptr
         , nodeNext  = ptr
         , nodeValue = error "nodeValue: undefined (list anchor)"
         }

empty :: STM (LinkedList a)
empty = do
    anchor <- newTVar Node undefined
    writeTVar anchor $ emptyAnchor anchor
    return $ LinkedList anchor

-- | IO version of 'empty'.
emptyIO :: IO (LinkedList a)
emptyIO = fixIO $ \(LinkedList anchor) -> newTVar Node $ emptyAnchor anchor
