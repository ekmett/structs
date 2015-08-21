--------------------------------------------------------------------------------
-- * List Labeling: Maintain n keys each labeled with n^2 bits.
--------------------------------------------------------------------------------
module Data.Struct.Label
  ( Label
  , newLabel
  , insertAfterLabel
  , firstLabel
  , lastLabel
  , cutAfterLabel
  , cutBeforeLabel
  , keys
  , OrdM(..)
  , Key
  ) where

import Data.Struct.Label.Internal
