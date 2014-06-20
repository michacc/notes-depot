module Internationalization where

import Prelude
import qualified Data.Text as T (Text, append)
import Yesod.Form (FormMessage(..), defaultFormMessage)

-- Class for messages that are available in Polish
class PolishMessage a where
    renderMessagePl :: a -> T.Text

instance PolishMessage FormMessage where
    renderMessagePl MsgValueRequired = "To pole jest wymagane."
    renderMessagePl msg = (`T.append`"$") . ("$"`T.append`) . defaultFormMessage $ msg
