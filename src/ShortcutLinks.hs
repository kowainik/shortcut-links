{- |
Copyright:  (c) 2015-2019 Aelve
            (c) 2019-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>
-}

module ShortcutLinks
    ( Result(..)
    , Shortcut
    , allShortcuts
    , useShortcut
    , useShortcutFrom
    ) where

import Data.Text (Text)

import ShortcutLinks.All (Result (..), Shortcut, allShortcuts)
import ShortcutLinks.Utils (format)


{- | Use a shortcut from 'allShortcuts'.

This is the main function you should use.
-}
useShortcut
  :: Text                   -- ^ Shortcut name
  -> Maybe Text             -- ^ Option
  -> Text                   -- ^ Link text
  -> Result Text            -- ^ Resulting URL
useShortcut = useShortcutFrom allShortcuts

{- | Use a shortcut from a list.

For instance, if you want to add @hk@ as a synonym for @hackage@, you'd
write:

>>> useShortcutFrom ((["hk"], hackage) : allShortcuts)
-}
useShortcutFrom
  :: [([Text], Shortcut)]
  -> Text                   -- ^ Shortcut name
  -> Maybe Text             -- ^ Option
  -> Text                   -- ^ Link text
  -> Result Text            -- ^ Resulting URL
useShortcutFrom shortcuts name option link =
    case filter givenShortcut shortcuts of
        []   -> fail (format "there's no shortcut named '{}'" name)
        [sh] -> snd sh option link
        _    -> fail (format "there's more than one shortcut named '{}'" name)
  where
    givenShortcut :: ([Text], Shortcut) -> Bool
    givenShortcut (names, _) = name `elem` names
