-- | Resizable sidebar via drag handle.
module FFI.Resize
  ( initResize
  ) where

import Prelude

import Effect (Effect)

-- | Initialize the resize handle between graph
-- | and sidebar.
foreign import initResize :: Effect Unit
