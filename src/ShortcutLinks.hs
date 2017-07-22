{-# LANGUAGE OverloadedStrings #-}

module ShortcutLinks
(
  Result(..),
  Shortcut,
  allShortcuts,
  useShortcut,
  useShortcutFrom,
)
where


-- Text
import Data.Text (Text)
import qualified Data.Text as T
-- shortcut-links
import ShortcutLinks.All
import ShortcutLinks.Utils (format)


-- | Use a shortcut from 'allShortcuts'.
--
-- This is the main function you should use.
useShortcut
  :: Text                   -- ^ Shortcut name
  -> Maybe Text             -- ^ Option
  -> Text                   -- ^ Link text
  -> Result Text            -- ^ Resulting URL
useShortcut = useShortcutFrom allShortcuts

-- | Use a shortcut from a list.
--
-- For instance, if you want to add @hk@ as a synonym for @hackage@, you'd
-- write:
--
-- >>> useShortcutFrom ((["hk"], hackage) : allShortcuts)
useShortcutFrom
  :: [([Text], Shortcut)]
  -> Text                   -- ^ Shortcut name
  -> Maybe Text             -- ^ Option
  -> Text                   -- ^ Link text
  -> Result Text            -- ^ Resulting URL
useShortcutFrom shortcuts name option link =
  let givenShortcut (names,_) = name `elem` names
  in  case filter givenShortcut shortcuts of
        []   -> fail (format "there's no shortcut named '{}'" name)
        [sh] -> (snd sh) option link
        _    -> fail (format "there's more than one shortcut named '{}'" name)
