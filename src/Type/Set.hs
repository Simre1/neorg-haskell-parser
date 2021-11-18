{-# language UndecidableInstances #-}

-- Used from https://github.com/isovector/type-sets. 
-- Changes:
-- - Specialised type sets to work only on symbols 
-- - Replaced CmpType with CmpSymbol
-- - Introduced FromList

-- Copyright Sandy Maguire (c) 2019
-- 
-- All rights reserved.
-- 
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- 
--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.
-- 
--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.
-- 
--     * Neither the name of Sandy Maguire nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.
-- 
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

module Type.Set
  ( -- * Core type
    TypeSet (..),

    FromList

    -- * Set operations
  , Member
  , Insert
  , Remove
  , Merge

    -- * Tree operations
  , Locate
  , Follow
  , Side (..)
  ) where

import Type.Reflection
import GHC.TypeLits


------------------------------------------------------------------------------
-- | A binary search tree. When @-XDataKinds@ is turned on, this becomes the
-- backbone of the type-level set.
--
-- >>> type MySet = Insert Bool (Insert String (Insert (Maybe Int) 'Empty))
data TypeSet
  = Empty
  | Branch Symbol TypeSet TypeSet


------------------------------------------------------------------------------
-- | Either left or right down a path.
data Side = L | R
  deriving (Eq, Ord, Show, Typeable)

------------------------------------------------------------------------------
-- | /O(log n)/. Insert an element into the 'TypeSet'.
type family Insert (t :: Symbol) (bst :: TypeSet) :: TypeSet where
  Insert t 'Empty = 'Branch t 'Empty 'Empty
  Insert t ('Branch a lbst rbst) =
    InsertImpl (CmpSymbol t a) t a lbst rbst

type family InsertImpl (ord :: Ordering)
                       (t :: Symbol)
                       (a :: Symbol)
                       (lbst :: TypeSet)
                       (rbst :: TypeSet) :: TypeSet where
  InsertImpl 'EQ t a lbst rbst = 'Branch a lbst rbst
  InsertImpl 'LT t a lbst rbst = 'Branch a (Insert t lbst) rbst
  InsertImpl 'GT t a lbst rbst = 'Branch a lbst (Insert t rbst)


------------------------------------------------------------------------------
-- | /O(log n)/. Determine membership in the 'TypeSet.'
type family Member (t :: Symbol) (bst :: TypeSet)  :: Bool where
  Member t 'Empty = 'False
  Member t ('Branch a lbst rbst) = MemberImpl (CmpSymbol t a) t lbst rbst

type family MemberImpl (ord :: Ordering)
                       (t :: Symbol)
                       (lbst :: TypeSet)
                       (rbst :: TypeSet) :: Bool where
  MemberImpl 'EQ t lbst rbst = 'True
  MemberImpl 'LT t lbst rbst = Member t lbst
  MemberImpl 'GT t lbst rbst = Member t rbst


------------------------------------------------------------------------------
-- | /O(m log n)/ for @Merge m n@; put your smaller set on the left side. Merge
-- two 'TypeSet's together.
type family Merge (small :: TypeSet) (big :: TypeSet) :: TypeSet where
  Merge Empty big   = big
  Merge small Empty = small
  Merge ('Branch a lbst rbst) big = Merge rbst (Merge lbst (Insert a big))


------------------------------------------------------------------------------
-- | /O(log n)/. Remove an element from the 'TypeSet'.
type family Remove (t :: Symbol) (bst :: TypeSet) :: TypeSet where
  Remove t Empty = Empty
  Remove t ('Branch a lbst rbst) = RemoveImpl (CmpSymbol t a) t a lbst rbst

type family RemoveImpl (ord :: Ordering)
                       (t :: Symbol)
                       (a :: Symbol)
                       (lbst :: TypeSet)
                       (rbst :: TypeSet) :: TypeSet where
  RemoveImpl 'LT t a lbst rbst = 'Branch a (Remove t lbst) rbst
  RemoveImpl 'EQ t a Empty rbst = rbst
  RemoveImpl 'EQ t a lbst Empty = lbst
  RemoveImpl 'EQ t a lbst rbst =
    'Branch (RightMost lbst) (Remove (RightMost lbst) lbst) rbst
  RemoveImpl 'GT t a lbst rbst = 'Branch a lbst (Remove t rbst)


------------------------------------------------------------------------------
-- | /O(log n)/. Get the right-most element in a tree. This function is stuck
-- if the tree is empty.
type family RightMost (bst :: TypeSet) :: Symbol where
  RightMost ('Branch a lbst 'Empty) = a
  RightMost ('Branch a lbst rbst) = RightMost rbst


------------------------------------------------------------------------------
-- | /O(log n)/. Compute a @['Side']@ which finds the desired element in the
-- tree. The result of this can be passed to 'Follow' in order to look up the
-- same element again later.
type family Locate (t :: Symbol) (bst :: TypeSet) :: [Side] where
  Locate t ('Branch a lbst rbst) = LocateImpl (CmpSymbol t a) t lbst rbst
  Locate t 'Empty = TypeError ('Text "Unable to locate: " ':<>: 'ShowType t)

type family LocateImpl (ord :: Ordering)
                       (t :: Symbol)
                       (lbst :: TypeSet)
                       (rbst :: TypeSet) :: [Side] where
  LocateImpl 'EQ t lbst rbst = '[]
  LocateImpl 'LT t lbst rbst = 'L ': Locate t lbst
  LocateImpl 'GT t lbst rbst = 'R ': Locate t rbst


------------------------------------------------------------------------------
-- | /O(log n)/. Follow the result of a 'Locate' to get a particular element in
-- the tree.
type family Follow (ss :: [Side]) (bst :: TypeSet) :: Symbol where
  Follow '[] ('Branch t _ _) = t
  Follow ('L ': ss) ('Branch _ l _) = Follow ss l
  Follow ('R ': ss) ('Branch _ _ r) = Follow ss r
  Follow ss 'Empty = TypeError ('Text "Unable to follow: " ':<>: 'ShowType ss)

type family FromList (as :: [Symbol]) where
  FromList '[] = Empty
  FromList (a : as) = Insert a (FromList as)

