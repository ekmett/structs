module Data.Struct.Label
  ( Label
  , newLabel
  , IsLabel
    ( insertAfter
    , least, greatest
    , cutAfter, cutBefore
    , compareM
    , next
    , prev
    , key
    )
  , keys
  , Key
  ) where

import Data.Struct.Internal.Label
