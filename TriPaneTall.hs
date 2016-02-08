{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE DeriveDataTypeable #-}

-- --------------------------------------------------------------------------
-- |
-- Module      :  TriPaneTall
-- Copyright   :  (c) Bruce Li 2016
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  leilmyxwz@gmail.com
-- Stability   :  unstable
-- Portability :  not portable, Typeable deriving, mtl, posix
--
-- 1 : 2 tiling for ultrawide screens
--
-----------------------------------------------------------------------------

module TriPaneTall (
                     -- * Usage
                     -- $usage
                     TriPaneTall (..)
                   , IncMaster1N (..)
                   , IncMaster2N (..)
                   ) where

import           Graphics.X11 (Rectangle(..))
import           XMonad.Core
import qualified XMonad.StackSet as W
import           XMonad.Layout ( splitVertically
                               , splitHorizontally
                               , splitHorizontallyBy )
import           Control.Monad

-----------------------------------------------------------------------------

-- | Increse the number of windows in the master 1 pane
data IncMaster1N = IncMaster1N !Int deriving Typeable

-- | Increse the number of windows in the master 2 pane
data IncMaster2N = IncMaster2N !Int deriving Typeable

instance Message IncMaster1N
instance Message IncMaster2N

-- | The 1:2 tiling mode for ultrawide screens. Supports 'IncMaster1N' and
-- | 'IncMaster2N'
data TriPaneTall a = TriPaneTall
    { tallNMaster1 :: !Int         -- ^ The default number of windows in the
                                   -- first master pane (default: 1)
    , tallRatio1 :: !Rational      -- ^ The default ratio of the first master
                                   -- pane (default: 2/3)
    , tallNMaster2 :: !Int         -- ^ The default number of windows in the
                                   -- second master pane (default: 1)
    , tallRatio2 :: !Rational      -- ^ The default ratio of the second master
                                   --  pane (default: 1/2)
    }
                     deriving (Show, Read)

instance LayoutClass TriPaneTall a where
    pureLayout (TriPaneTall nm1 mr1 nm2 mr2) r s = zip ws rs
      where ws = W.integrate s
            rs = triTile nm1 mr1 nm2 mr2 r (length ws)
    pureMessage (TriPaneTall nm1 mr1 nm2 mr2) m =
        msum [ fmap incMaster1n (fromMessage m)
             , fmap incMaster2n (fromMessage m) ]
        where incMaster1n (IncMaster1N d) =
                  TriPaneTall (max 1 (nm1 + d)) mr1 nm2 mr2
              incMaster2n (IncMaster2N d) =
                  TriPaneTall nm1 mr1 (max 1 (nm2 + d)) mr2
    description _ = "TriPaneTall"

-- | Actually computes the tiling:
-- | 1. If the number of windows <= maximum number of windows in pane 1, do
-- |    vertical split
-- | 2. If the number of windows > maximum number of windows in pane 1 and <=
-- |    maximum number of windows in pane 2, do horizontal 1:2 split. Then split
-- |    windows in each pane
-- | 3. If the number of windows > sum of maximum windows in both panes, do
-- |    a 1:1:1 horizontal split. Then vertical split in each rectangles
triTile :: Int -> Rational -> Int -> Rational -> Rectangle -> Int -> [Rectangle]
triTile nm1 f1 nm2 f2 r n
    | n <= nm1 || nm1 == 0 = splitVertically n r
    | n > nm1 && n <= (nm1 + nm2) = splitVertically nm1 r1 ++
                                    splitVertically (n - nm1) r2
    | otherwise = splitVertically nm1 r1 ++ splitVertically nm2 r2' ++
                  splitVertically (n - nm1 - nm2) r3
    where (r1, r2) = splitHorizontallyBy f1 r
          (r2', r3) = splitHorizontallyBy f2 r2
