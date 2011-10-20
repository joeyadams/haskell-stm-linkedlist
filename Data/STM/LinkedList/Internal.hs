{-# LANGUAGE BangPatterns #-}
module Data.STM.LinkedList.Internal where

import Control.Concurrent.STM
import Data.Maybe (isJust, isNothing)
import System.IO (fixIO)

-- | Mutable, doubly linked list, supporting efficient insertion and removal.
--
-- This is implemented internally as a circular list with a designated \"list
-- head\" node.
newtype LinkedList a = LinkedList (Node a)

-- | Unwrap the list head, a special 'Node' with the following properties:
--
-- * @'next' . 'listHead' == 'start'@
--
-- * @'prev' . 'listHead' == 'end'@
--
-- * @'insertBefore' v . 'listHead' == 'append' v@
--
-- * @'insertAfter' v . 'listHead' == 'prepend' v@
--
-- * @'value' . 'listHead' ==> /error/@
--
-- * @'delete' . 'listHead' ==> /error/@
listHead :: LinkedList a -> Node a
listHead (LinkedList h) = h

-- | Used for traversal and removal.  A 'Node' contains an immutable value of
-- type @a@, and 'TVar's that point to the previous and next nodes.
data Node a
    = Node
        { nodePrev  :: NodePtr a
        , nodeNext  :: NodePtr a
        , nodeValue :: Maybe a
            -- ^ 'Nothing' if this is the list head.
        }

type NodePtr a = TVar (Node a)

instance Eq (Node a) where
    a == b = nodeNext a == nodeNext b

-- | Extract the value of a node.
value :: Node a -> a
value node = case nodeValue node of
                 Just v  -> v
                 Nothing -> error "LinkedList.value: list head"

-- | /O(1)/. Is the list empty?
null :: LinkedList a -> STM Bool
null (LinkedList list_head) = do
    first <- readTVar $ nodeNext list_head
    return $ isNothing $ nodeValue first

-- | /O(n)/. Count the number of items in the list.
length :: LinkedList a -> STM Int
length (LinkedList list_head) = foldlHelper (\a _ -> a + 1) 0 nodeNext list_head

-- | /O(1)/. Create an empty linked list.
empty :: STM (LinkedList a)
empty = do
    prev_ptr <- newTVar undefined
    next_ptr <- newTVar undefined
    let node = Node prev_ptr next_ptr Nothing
    writeTVar prev_ptr node
    writeTVar next_ptr node
    return $ LinkedList node

-- | /O(1)/. Version of 'empty' that can be used in the 'IO' monad.
emptyIO :: IO (LinkedList a)
emptyIO = do
    node <- fixIO $ \node -> do
        prev_ptr <- newTVarIO node
        next_ptr <- newTVarIO node
        return (Node prev_ptr next_ptr Nothing)
    return $ LinkedList node

-- | Insert a node between two adjacent nodes.
insertBetween :: a -> Node a -> Node a -> STM (Node a)
insertBetween v left right = do
    prev_ptr <- newTVar left
    next_ptr <- newTVar right
    let node = Node prev_ptr next_ptr (Just v)
    writeTVar (nodeNext left) node
    writeTVar (nodePrev right) node
    return node

-- | /O(1)/. Add a node to the beginning of a linked list.
prepend :: a -> LinkedList a -> STM (Node a)
prepend v (LinkedList list_head) = do
    right <- readTVar $ nodeNext list_head
    insertBetween v list_head right

-- | /O(1)/. Add a node to the end of a linked list.
append :: a -> LinkedList a -> STM (Node a)
append v (LinkedList list_head) = do
    left <- readTVar $ nodePrev list_head
    insertBetween v left list_head

-- | /O(1)/. Insert an item before the given node.
insertBefore :: a -> Node a -> STM (Node a)
insertBefore v node = do
    left <- readTVar $ nodePrev node
    if left == node && isJust (nodeValue node)
        then error "LinkedList.insertBefore: node removed from list"
        else insertBetween v left node

-- | /O(1)/. Insert an item after the given node.
insertAfter :: a -> Node a -> STM (Node a)
insertAfter v node = do
    right <- readTVar $ nodeNext node
    if right == node && isJust (nodeValue node)
        then error "LinkedList.insertAfter: node removed from list"
        else insertBetween v node right

-- | /O(1)/. Remove a node from whatever 'LinkedList' it is in.  If the node
-- has already been removed, this is a no-op.
delete :: Node a -> STM ()
delete node
    | isNothing (nodeValue node) =
        error "LinkedList.delete: list head"
    | otherwise = do
        left <- readTVar $ nodePrev node
        right <- readTVar $ nodeNext node
        writeTVar (nodeNext left) right
        writeTVar (nodePrev right) left

        -- Link list node to itself so subsequent 'delete' calls will be harmless.
        writeTVar (nodePrev node) node
        writeTVar (nodeNext node) node

stepHelper :: (Node a -> NodePtr a) -> Node a -> STM (Maybe (Node a))
stepHelper step node = do
    node' <- readTVar $ step node
    if node' == node
        then return Nothing
        else case nodeValue node' of
            Just _  -> return $ Just node'
            Nothing -> return Nothing

-- | /O(1)/. Get the previous node.  Return 'Nothing' if this is the first item,
-- or if this node has been 'delete'd from its list.
prev :: Node a -> STM (Maybe (Node a))
prev = stepHelper nodePrev

-- | /O(1)/. Get the next node.  Return 'Nothing' if this is the last item,
-- or if this node has been 'delete'd from its list.
next :: Node a -> STM (Maybe (Node a))
next = stepHelper nodeNext

-- | /O(1)/. Get the node corresponding to the first item of the list.  Return
-- 'Nothing' if the list is empty.
start :: LinkedList a -> STM (Maybe (Node a))
start = next . listHead

-- | /O(1)/. Get the node corresponding to the last item of the list.  Return
-- 'Nothing' if the list is empty.
end :: LinkedList a -> STM (Maybe (Node a))
end = prev . listHead

-- | Traverse list nodes with a fold function.  The traversal terminates when
-- the list head is reached.
--
-- This is strict in the accumulator.
foldlHelper :: (a -> b -> a)            -- ^ Fold function
            -> a                        -- ^ Initial value
            -> (Node b -> NodePtr b)    -- ^ Step function ('nodePrev' or 'nodeNext')
            -> Node b                   -- ^ Starting node.  This node's value is not used!
            -> STM a
foldlHelper f z nodeStep start_node =
        loop z start_node
    where
        loop !accum node = do
            node' <- readTVar $ nodeStep node
            case nodeValue node' of
                Nothing -> return accum
                Just v  -> loop (f accum v) node'

-- | /O(n)/. Return all of the items in a 'LinkedList'.
toList :: LinkedList a -> STM [a]
toList (LinkedList list_head) = foldlHelper (flip (:)) [] nodePrev list_head

-- | /O(n)/. Return all of the items in a 'LinkedList', in reverse order.
toListRev :: LinkedList a -> STM [a]
toListRev (LinkedList list_head) = foldlHelper (flip (:)) [] nodeNext list_head
