{-# LANGUAGE
OverloadedStrings
  #-}

{- |
Useful functions when writing your own link rules.
-}
module ShortcutLinks.Utils
(
  replaceSpaces,
  titleFirst,
)
where


-- Text
import Data.Char
import qualified Data.Text as T
import Data.Text (Text)


-- | Replace spaces in text with chosen character (useful when processing queries containing spaces – they are often turned into “+” or “_”).
--
-- >>> replaceSpaces '_' "hi   there"
-- "hi___there"
replaceSpaces :: Char -> Text -> Text
replaceSpaces r = T.map (\c -> if isSpace c then r else c)

-- | Convert the 1st character of a string to upper case.
--
-- This function is dumber than it could've been; when the 1st character
-- doesn't have a single-character uppercase form (like “ß”), it is left
-- intact instead of being converted (“Ss” in the case of “ß”). This is good,
-- however; for instance, if the “proper” capitalisation rule was applied to
-- e.g. Wikipedia links, a link to the article on “ß” would've been rendered
-- as “Ss”, which is a redirect to “Schutzstaffel”.
titleFirst :: Text -> Text
titleFirst s = case T.uncons s of
  Nothing        -> ""
  Just (c, rest) -> toUpper c `T.cons` rest
