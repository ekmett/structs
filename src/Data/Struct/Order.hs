module Data.Struct.Order
  ( Order(..)
  , newOrder
  , IsLabel ( insertAfter, delete )
  , value
  ) where

import Data.Struct.Internal.Label
import Data.Struct.Internal.Order
