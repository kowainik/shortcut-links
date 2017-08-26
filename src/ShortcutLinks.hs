{-# LANGUAGE OverloadedStrings #-}

module ShortcutLinks
(
  Result(..),
  Shortcut,
  allShortcuts,
  useShortcut,
  useShortcutFrom,
  parseShortcut
)
where


-- Text
import Data.Text (Text)
import qualified Data.Text as T
-- megaparsec
import Text.Megaparsec (alphaNumChar, anyChar, char, noneOf,
                        optional, parse, some, (<|>))
import Text.Megaparsec.Text (Parser)
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

-- | Parse a shortcut link. Allowed formats:
--
-- @
-- \@name
-- \@name:text
-- \@name(option)
-- \@name(option):text
-- @
parseShortcut :: Text -> Either String (Text, Maybe Text, Maybe Text)
parseShortcut = either (Left . show) Right . parse p ""
  where
    shortcut = some (alphaNumChar <|> char '-')
    opt      = char '(' *> some (noneOf [')']) <* char ')'
    text     = char ':' *> some anyChar
    p :: Parser (Text, Maybe Text, Maybe Text)
    p = do
      char '@'
      (,,) <$> T.pack <$> shortcut
           <*> optional (T.pack <$> opt)
           <*> optional (T.pack <$> text)
