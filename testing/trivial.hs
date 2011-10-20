import Control.Concurrent.STM
import Control.Exception
import Data.STM.LinkedList (LinkedList)
import qualified Data.STM.LinkedList as LinkedList

ok :: Bool -> String -> IO ()
ok False message = putStrLn $ "not ok: " ++ message
ok True  message = putStrLn $ "ok: " ++ message

okList :: (Eq a, Show a) => String -> LinkedList a -> [a] -> IO ()
okList context list expected = do
    actual     <- atomically $ LinkedList.toList list
    actual_rev <- atomically $ LinkedList.toListRev list
    actual_len <- atomically $ LinkedList.length list
    ok (expected == actual && expected == reverse actual_rev && length expected == actual_len)
        $ context ++ ": " ++ show expected

okError :: String -> IO a -> IO ()
okError context action = do
    e <- try action
    case e of
        Left (ErrorCall errmsg) -> putStrLn $ "ok: " ++ context ++ ": error: " ++ errmsg
        Right _                 -> putStrLn $ "not ok: " ++ context ++ ": expected error"

testEmpty :: IO ()
testEmpty = do
    list <- atomically LinkedList.empty :: IO (LinkedList Int)
    n <- atomically $ LinkedList.null list
    ok n          "testEmpty: List created with empty is null"

    xs <- atomically $ LinkedList.toList list
    ok (xs == []) "testEmpty: toList yields an empty list"

    xs_rev <- atomically $ LinkedList.toListRev list
    ok (xs_rev == []) "testEmpty: toListRev yields an empty list"

testEmptyIO :: IO ()
testEmptyIO = do
    list <- LinkedList.emptyIO :: IO (LinkedList Int)
    n <- atomically $ LinkedList.null list
    ok n          "testEmptyIO: List created with empty is null"

    xs <- atomically $ LinkedList.toList list
    ok (xs == []) "testEmptyIO: toList yields an empty list"

    xs_rev <- atomically $ LinkedList.toListRev list
    ok (xs_rev == []) "testEmptyIO: toListRev yields an empty list"

testPrepend :: IO ()
testPrepend = do
    list <- LinkedList.emptyIO :: IO (LinkedList Int)
    okList "testPrepend" list []
    _ <- atomically $ LinkedList.prepend 3 list
    okList "testPrepend" list [3]
    _ <- atomically $ LinkedList.prepend 2 list
    okList "testPrepend" list [2,3]
    _ <- atomically $ LinkedList.prepend 1 list
    okList "testPrepend" list [1,2,3]

testAppend :: IO ()
testAppend = do
    list <- LinkedList.emptyIO :: IO (LinkedList Int)
    okList "testAppend" list []
    _ <- atomically $ LinkedList.append 1 list
    okList "testAppend" list [1]
    _ <- atomically $ LinkedList.append 2 list
    okList "testAppend" list [1,2]
    _ <- atomically $ LinkedList.append 3 list
    okList "testAppend" list [1,2,3]

testDelete :: IO ()
testDelete = do
    list <- LinkedList.emptyIO :: IO (LinkedList Int)
    okList "testDelete" list []
    node4 <- atomically $ LinkedList.append  4 list
    node5 <- atomically $ LinkedList.append  5 list
    node6 <- atomically $ LinkedList.append  6 list
    node3 <- atomically $ LinkedList.prepend 3 list
    node2 <- atomically $ LinkedList.prepend 2 list
    node1 <- atomically $ LinkedList.prepend 1 list
    okList "testDelete" list [1,2,3,4,5,6]
    atomically $ LinkedList.delete node1
    atomically $ LinkedList.delete node1
    okList "testDelete" list [2,3,4,5,6]
    atomically $ LinkedList.delete node6
    atomically $ LinkedList.delete node6
    okList "testDelete" list [2,3,4,5]
    atomically $ LinkedList.delete node4
    atomically $ LinkedList.delete node4
    okList "testDelete" list [2,3,5]
    atomically $ LinkedList.delete node3
    atomically $ LinkedList.delete node3
    okList "testDelete" list [2,5]
    atomically $ LinkedList.delete node2
    atomically $ LinkedList.delete node2
    okList "testDelete" list [5]
    node2' <- atomically $ LinkedList.prepend 2 list
    okList "testDelete" list [2,5]
    atomically $ LinkedList.delete node5
    atomically $ LinkedList.delete node5
    okList "testDelete" list [2]
    atomically $ LinkedList.delete node2
    atomically $ LinkedList.delete node2
    okList "testDelete" list [2]
    atomically $ LinkedList.delete node2'
    atomically $ LinkedList.delete node2'
    okList "testDelete" list []
    atomically $ mapM_ LinkedList.delete [node1, node2, node2', node3, node4, node5, node6]

    list2 <- LinkedList.emptyIO :: IO (LinkedList Int)
    atomically $ do
        node <- LinkedList.append 5 list2
        LinkedList.delete node
    okList "testDelete" list2 []

    list3 <- atomically LinkedList.empty :: IO (LinkedList Int)
    atomically $ do
        node <- LinkedList.append 5 list3
        LinkedList.delete node
    okList "testDelete" list3 []

testInsertBefore :: IO ()
testInsertBefore = do
    list <- LinkedList.emptyIO :: IO (LinkedList Int)
    atomically $ do
        node1 <- LinkedList.append 1 list
        node3 <- LinkedList.insertBefore 3 node1
        _     <- LinkedList.insertBefore 4 node3
        _     <- LinkedList.insertBefore 2 node1
        return ()
    okList "testInsertBefore" list [4,3,2,1]

testInsertAfter :: IO ()
testInsertAfter = do
    list <- LinkedList.emptyIO :: IO (LinkedList Int)
    atomically $ do
        node1 <- LinkedList.append 1 list
        node3 <- LinkedList.insertAfter 3 node1
        _     <- LinkedList.insertAfter 4 node3
        _     <- LinkedList.insertAfter 2 node1
        return ()
    okList "testInsertAfter" list [1,2,3,4]

testInsertValuePrevNext :: IO ()
testInsertValuePrevNext = do
    list <- LinkedList.emptyIO :: IO (LinkedList Int)
    node1 <- atomically $ LinkedList.append 1 list
    ok (LinkedList.value node1 == 1) "value 1"
    atomically $ LinkedList.delete node1
    ok (LinkedList.value node1 == 1) "value 2"

    okError "Insert before deleted node" $ atomically $ LinkedList.insertBefore 0 node1
    okError "Insert after deleted node" $ atomically $ LinkedList.insertAfter 2 node1

    node2 <- atomically $ LinkedList.append 2 list
    okList "testInsertValuePrevNext" list [2]
    node2p <- atomically $ LinkedList.prev node2
    node2n <- atomically $ LinkedList.next node2
    ok (node2p == Nothing && node2n == Nothing) "Singleton node has no neighbors"

    node3 <- atomically $ LinkedList.append 3 list
    node3p <- atomically $ LinkedList.prev node3
    node3n <- atomically $ LinkedList.next node3
    ok (node3p == Just node2 && node3n == Nothing) "node2 is before node3"

    node1' <- atomically $ LinkedList.prepend 1 list
    node1'p <- atomically $ LinkedList.prev node1'
    node1'n <- atomically $ LinkedList.next node1'
    ok (node1'p == Nothing && node1'n == Just node2) "node2 is after node1'"

    node2p' <- atomically $ LinkedList.prev node2
    node2n' <- atomically $ LinkedList.next node2
    ok (node2p' == Just node1' && node2n' == Just node3) "node2 is between node1' and node3"

    remove_correct <- atomically $ do
        LinkedList.delete node2
        n1p <- LinkedList.prev node1'
        n1n <- LinkedList.next node1'
        n2p <- LinkedList.prev node2
        n2n <- LinkedList.next node2
        n3p <- LinkedList.prev node3
        n3n <- LinkedList.next node3
        return $ n1p == Nothing
              && n1n == Just node3
              && n2p == Nothing
              && n2n == Nothing
              && n3p == Just node1'
              && n3n == Nothing

    okList "testInsertValuePrevNext" list [1,3]
    ok remove_correct "Removing node2 made node1' and node3 neighbors, and made node2 have no neighbors"

main :: IO ()
main = do
    testEmpty
    testEmptyIO
    testPrepend
    testAppend
    testDelete
    testInsertBefore
    testInsertAfter
    testInsertValuePrevNext
