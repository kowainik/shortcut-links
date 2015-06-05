{- |
Useful functions when writing your own link rules.
-}
module ShortcutLinks.Utils
(
  replaceSpaces,
)
where


-- Text
import Data.Char (isSpace)
import qualified Data.Text as T
import Data.Text (Text)


-- | Replace spaces in text with chosen character (useful when processing queries containing spaces – they are often turned into “+” or “_”).
--
-- >>> replaceSpaces '_' "hi   there"
-- "hi___there"
replaceSpaces :: Char -> Text -> Text
replaceSpaces r = T.map (\c -> if isSpace c then r else c)
