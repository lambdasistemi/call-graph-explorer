-- | Trigger highlight.js on the source code element.
module FFI.Highlight
  ( highlightCode
  ) where

import Prelude

import Effect (Effect)

-- | Highlight the #source-code code element.
foreign import highlightCode :: Effect Unit
