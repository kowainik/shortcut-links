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
import ShortcutLinks.Utils (format)


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
  in  case filter givenShortcut allShortcuts of
        []   -> fail (format "there's no shortcut named '{}'" name)
        [sh] -> (snd sh) option link
        _    -> fail (format "there's more than one shortcut named '{}'" name)
