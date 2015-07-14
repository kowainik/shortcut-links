module ShortcutLinks
(
  Result(..),
  allShortcuts,
)
where


-- Text
import Data.Text (Text)
import qualified Data.Text as T
-- shortcut-links
import ShortcutLinks.All (allShortcuts, Result(..))


-- | Use a shortcut from 'allShortcuts'.
--
-- This is the main function you should use. It does the lookup and so on.
useShortcut
  :: Text          -- ^ Shortcut name
  -> Maybe Text    -- ^ Option
  -> Text          -- ^ Link text
  -> Result Text   -- ^ Resulting URL
useShortcut name option link =
  let givenShortcut (names,_) = name `elem` names
      quotedName = "'" ++ T.unpack name ++ "'"
  in  case filter givenShortcut allShortcuts of
        []   -> fail ("there's no shortcut named " ++ quotedName)
        [sh] -> (snd sh) option link
        _    -> fail ("there's more than one shortcut named " ++ quotedName)
