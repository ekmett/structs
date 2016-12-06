{-# LANGUAGE CPP #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_HADDOCK not-home #-}
-----------------------------------------------------------------------------
-- |
-- Copyright   :  (C) 2015 Edward Kmett
-- License     :  BSD-style (see the file LICENSE)
-- Maintainer  :  Edward Kmett <ekmett@gmail.com>
-- Stability   :  experimental
-- Portability :  non-portable
--
-----------------------------------------------------------------------------

module Data.Struct.Internal.LinkCut where

import Control.Exception
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Struct.Internal
import Data.Struct.TH

#ifdef HLINT
{-# ANN module "HLint: ignore Reduce duplication" #-}
{-# ANN module "HLint: ignore Redundant do" #-}
#endif

-- | Amortized Link-Cut trees via splay trees based on Tarjan's little book.
--
-- These support O(log n) operations for a lot of stuff.
--
-- The parameter `a` is an arbitrary user-supplied monoid that will be summarized
-- along the path to the root of the tree.
--
-- In this example the choice of 'Monoid' is 'String', so we can get a textual description of the path to the root.
--
-- >>> x <- new "x"
-- >>> y <- new "y"
-- >>> link x y -- now x is a child of y
-- >>> x == y
-- False
-- >>> connected x y
-- True
-- >>> z <- new "z"
-- >>> link z x -- now z is a child of y
-- >>> (y ==) <$> root z
-- True
-- >>> cost z
-- "yxz"
-- >>> w <- new "w"
-- >>> u <- new "u"
-- >>> v <- new "v"
-- >>> link u w
-- >>> link v z
-- >>> link w z
-- >>> cost u
-- "yxzwu"
-- >>> (y ==) <$> root v
-- True
-- >>> connected x v
-- True
-- >>> cut z
--
-- @
--      y
--     x          z    y
--    z    ==>   w v  x
--   w v        u
--  u
-- @
--
-- >>> connected x v
-- False
-- >>> cost u
-- "zwu"
-- >>> (z ==) <$> root v
-- True
makeStruct [d|
  data LinkCut a s = LinkCut
    { path, parent, left, right :: !(LinkCut a s)
    , value, summary :: a
    }
   |]

-- | O(1). Allocate a new link-cut tree with a given monoidal summary.
new :: PrimMonad m => a -> m (LinkCut a (PrimState m))
new a = st (newLinkCut Nil Nil Nil Nil a a)
{-# INLINE new #-}

-- | O(log n). @'cut' v@ removes the linkage between @v@ upwards to whatever tree it was in, making @v@ a root node.
--
-- Repeated calls on the same value without intermediate accesses are O(1).
cut :: (PrimMonad m, Monoid a) => LinkCut a (PrimState m) -> m ()
cut this = st $ do
  access this
  l <- get left this
  unless (isNil l) $ do
    set left this Nil
    set parent l Nil
    v <- getField value this
    setField summary this v
{-# INLINE cut #-}

-- | O(log n). @'link' v w@ inserts @v@ which must be the root of a tree in as a child of @w@. @v@ and @w@ must not be 'connected'.
link :: (PrimMonad m, Monoid a) => LinkCut a (PrimState m) -> LinkCut a (PrimState m) -> m ()
link v w = st $ do
  --   w          w<~v
  --  a  , v  => a
  --
  --
  access v
  access w
  set path v w
{-# INLINE link #-}

-- | O(log n). @'connected' v w@ determines if @v@ and @w@ inhabit the same tree.
connected :: (PrimMonad m, Monoid a) => LinkCut a (PrimState m) -> LinkCut a (PrimState m) -> m Bool
connected v w = st $ (==) <$> root v <*> root w
{-# INLINE connected #-}

-- | O(log n). @'cost' v@ computes the root-to-leaf path cost of @v@ under whatever 'Monoid' was built into the tree.
--
-- Repeated calls on the same value without intermediate accesses are O(1).
cost :: (PrimMonad m, Monoid a) => LinkCut a (PrimState m) -> m a
cost v = st $ do
  access v
  getField summary v
{-# INLINE cost #-}

-- | O(log n). Find the root of a tree.
--
-- Repeated calls on the same value without intermediate accesses are O(1).
root :: (PrimMonad m, Monoid a) => LinkCut a (PrimState m) -> m (LinkCut a (PrimState m))
root this = st $ do
    access this
    r <- leftmost this
    splay r -- r is already in the root aux tree
    return r
  where
    leftmost v = do
      l <- get left v
      if isNil l then return v
      else leftmost l
{-# INLINE root #-}

-- | O(log n). Move upward one level.
--
-- This will return 'Nil' if the parent is not available.
--
-- Note: Repeated calls on the same value without intermediate accesses are O(1).
up :: (PrimMonad m, Monoid a) => LinkCut a (PrimState m) -> m (LinkCut a (PrimState m))
up this = st $ do
    access this
    a <- get left this
    if isNil a then return Nil
    else do
      p <- rightmost a
      splay p -- p is already in the root aux tree
      return p
  where
    rightmost v = do
      p <- get right v
      if isNil p then return v
      else rightmost p
{-# INLINE up #-}

-- | O(1)
summarize :: Monoid a => LinkCut a s -> ST s a
summarize this
  | isNil this = return mempty
  | otherwise  = getField summary this
{-# INLINE summarize #-}

-- | O(log n)
access :: Monoid a => LinkCut a s -> ST s ()
access this = do
  when (isNil this) $ throw NullPointerException
  splay this
  -- the right hand child is no longer preferred
  r <- get right this
  unless (isNil r) $ do
    set right this Nil
    set parent r Nil
    set path r this
    -- resummarize
    l <- get left this
    sl <- summarize l
    v <- getField value this
    setField summary this (sl `mappend` v)
  go this
  splay this
 where
  go v = do
    w <- get path v
    unless (isNil w) $ do
      splay w
      --      w    v          w
      --     a b  c d   ==>  a  v, b.path = w
      --                       c d
      b <- get right w
      unless (isNil b) $ do -- b is no longer on the preferred path
        set path b w
        set parent b Nil
      a <- get left w
      sa <- summarize a
      vw <- getField value w
      sv <- getField summary v
      set parent v w
      set right w v
      setField summary w (sa `mappend` vw `mappend` sv)
      go w

-- | O(log n). Splay within an auxiliary tree
splay :: Monoid a => LinkCut a s -> ST s ()
splay x = do
  p <- get parent x
  unless (isNil p) $ do
    g <- get parent p
    pl <- get left p
    if isNil g then do -- zig step
      set parent p x
      set parent x Nil
      pp <- get path p
      set path x pp
      set path p Nil
      sp <- getField summary p
      setField summary x sp
      if pl == x then do
        --      p           x
        --    x   d  ==>  b   p
        --   b c             c d
        c <- get right x
        d <- get right p
        unless (isNil c) $ set parent c p
        set right x p
        set left p c
        sc <- summarize c
        sd <- summarize d
        vp <- getField value p
        setField summary p (sc `mappend` vp `mappend` sd)
      else do
        --      p            x
        --    a   x   ==>  p   c
        --       b c      a b
        b <- get left x
        unless (isNil b) $ set parent b p
        let a = pl
        set left x p
        set right p b
        sa <- summarize a
        sb <- summarize b
        vp <- getField value p
        setField summary p (sa `mappend` vp `mappend` sb)
    else do -- zig-zig or zig-zag
      gg <- get parent g
      gl <- get left g
      sg <- getField summary g
      setField summary x sg
      set parent x gg
      gp <- get path g
      set path x gp
      set path g Nil
      if gl == p then do
        if pl == x then do -- zig-zig
          --      g       x
          --    p  d     a  p
          --  x  c   ==>   b  g
          -- a b             c d
          b <- get right x
          c <- get right p
          d <- get right g
          set parent p x
          set parent g p
          unless (isNil b) $ set parent b p
          unless (isNil c) $ set parent c g
          set right x p
          set right p g
          set left p b
          set left g c
          sb <- summarize b
          vp <- getField value p
          sc <- summarize c
          vg <- getField value g
          sd <- summarize d
          let sg' = sc `mappend` vg `mappend` sd
          setField summary g sg'
          setField summary p (sb `mappend` vp `mappend` sg')
        else do -- zig-zag
          --       g           x
          --   p    d  ==>   p   g
          --  a  x          a b c d
          --    b c
          let a = pl
          b <- get left x
          c <- get right x
          d <- get right g
          set parent p x
          set parent g x
          unless (isNil b) $ set parent b p
          unless (isNil c) $ set parent c g
          set left x p
          set right x g
          set right p b
          set left g c
          sa <- summarize a
          vp <- getField value p
          sb <- summarize b
          setField summary p (sa `mappend` vp `mappend` sb)
          sc <- summarize c
          vg <- getField value g
          sd <- summarize d
          setField summary g (sc `mappend` vg `mappend` sd)
      else if pl == x then do -- zig-zag
        --   g               x
        --  a    p         g   p
        --     x  d  ==>  a b c d
        --    b c
        let a = gl
        b <- get left x
        c <- get right x
        d <- get right p
        set parent g x
        set parent p x
        unless (isNil b) $ set parent b g
        unless (isNil c) $ set parent c p
        set left x g
        set right x p
        set right g b
        set left p c
        sa <- summarize a
        vg <- getField value g
        sb <- summarize b
        setField summary g (sa `mappend` vg `mappend` sb)
        sc <- summarize c
        vp <- getField value p
        sd <- summarize d
        setField summary p (sc `mappend` vp `mappend` sd)
      else do -- zig-zig
        --  g               x
        -- a  p           p  d
        --   b  x  ==>  g  c
        --     c d     a b
        let a = gl
        let b = pl
        c <- get left x
        unless (isNil b) $ set parent b g
        unless (isNil c) $ set parent c p
        set parent p x
        set parent g p
        set left x p
        set left p g
        set right g b
        set right p c
        sa <- summarize a
        vg <- getField value g
        sb <- summarize b
        vp <- getField value p
        sc <- summarize c
        let sg' = sa `mappend` vg `mappend` sb
        setField summary g sg'
        setField summary p (sg' `mappend` vp `mappend` sc)
      unless (isNil gg) $ do
        ggl <- get left gg
        -- NB: this replacement leaves the summary intact
        if ggl == g then set left gg x
        else set right gg x
        splay x
