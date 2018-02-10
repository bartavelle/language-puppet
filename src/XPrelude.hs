{-|
General specific prelude for language-puppet.
Customization of the Protolude with extra specific utilities.
-}
module XPrelude
  ( module XPrelude.Extra
  , module XPrelude.PP
  ) where

import XPrelude.Extra
import XPrelude.PP hiding (width, (</>))
