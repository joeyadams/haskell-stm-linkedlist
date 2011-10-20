-- |
-- Module:      Data.STM.LinkedList
-- Copyright:   (c) Joseph Adams 2011
-- License:     BSD3
-- Maintainer:  joeyadams3.14159@gmail.com
--
-- Mutable, doubly linked lists for use with STM (software transactional
-- memory).  Provides efficient random insertion and removal.
--
-- This module is usually imported qualified:
--
-- >import Data.STM.LinkedList (LinkedList)
-- >import qualified Data.STM.LinkedList as LinkedList
module Data.STM.LinkedList (
    -- * The LinkedList type
    LinkedList,
    listHead,

    -- * The Node type
    Node,
    value,

    -- * Query
    null,
    length,

    -- * Construction
    empty,
    emptyIO,

    -- ** Insertion
    prepend,
    append,
    insertBefore,
    insertAfter,

    -- ** Deletion
    delete,

    -- * Traversal
    prev,
    next,
    start,
    end,

    -- * Conversion
    toList,
    toListRev
) where

import Prelude ()
import Data.STM.LinkedList.Internal
