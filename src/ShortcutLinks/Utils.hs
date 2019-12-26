{-# LANGUAGE FlexibleInstances #-}

{- |
Copyright:  (c) 2015-2019 Aelve
            (c) 2019-2020 Kowainik
SPDX-License-Identifier: MPL-2.0
Maintainer: Kowainik <xrom.xkov@gmail.com>

Useful functions when writing your own link rules.
-}

module ShortcutLinks.Utils
    ( replaceSpaces
    , titleFirst
    , tryStripPrefixCI
    , stripPrefixCI
    , orElse
    , format
    ) where

import Data.Char (isSpace, toUpper)
import Data.Semigroup ((<>))
import Data.Text (Text)

import qualified Data.Text as T


{- | Replace spaces in text with chosen character (useful when processing queries containing spaces – they are often turned into “+” or “_”).

>>> replaceSpaces '_' "hi   there"
"hi___there"
-}
replaceSpaces :: Char -> Text -> Text
replaceSpaces r = T.map (\c -> if isSpace c then r else c)

{- | Convert the 1st character of a string to upper case.

This function is dumber than it could've been; when the 1st character
doesn't have a single-character uppercase form (like “ß”), it is left
intact instead of being converted (“Ss” in the case of “ß”). This is good,
however; for instance, if the “proper” capitalisation rule was applied to
e.g. Wikipedia links, a link to the article on “ß” would've been rendered
as “Ss”, which is a redirect to “Schutzstaffel”.
-}
titleFirst :: Text -> Text
titleFirst s = case T.uncons s of
  Nothing        -> ""
  Just (c, rest) -> toUpper c `T.cons` rest

{- | Strip given prefix from a string, or do nothing if the string doesn't
have given prefix.

This function is case-insensitive.

>>> tryStripPrefixCI "FOO" "FooBAR"
"BAR"

>>> tryStripPrefixCI "foo" "quux"
"quux"
-}
tryStripPrefixCI :: Text -> Text -> Text
tryStripPrefixCI pref str =
  let pref' = T.toCaseFold pref
      (str_pref, rest) = T.splitAt (T.length pref') str
  in  if T.toCaseFold str_pref == pref' then rest else str

{- | Strip given prefix from a string.

This function is case-insensitive.

>>> stripPrefixCI "FOO" "FooBAR"
Just "BAR"

>>> stripPrefixCI "foo" "quux"
Nothing
-}
stripPrefixCI :: Text -> Text -> Maybe Text
stripPrefixCI pref str =
  let pref' = T.toCaseFold pref
      (str_pref, rest) = T.splitAt (T.length pref') str
  in  if T.toCaseFold str_pref == pref' then Just rest else Nothing

-- | Choose the 2nd value if the 1st is empty (equal to 'mempty').
orElse :: (Eq a, Monoid a) => a -> a -> a
orElse a b = if a == mempty then b else a

------------------------------------------------------------------------------
-- A micro formatting library which supports Text better than printf.
------------------------------------------------------------------------------

class FormatArg a where
  formatArg :: a -> Text

instance FormatArg Text    where formatArg = id
instance FormatArg String  where formatArg = T.pack
instance FormatArg Int     where formatArg = T.pack . show
instance FormatArg Integer where formatArg = T.pack . show

class FormatType r where
  format' :: Text -> [Text] -> r

instance FormatType String where
  format' str params = T.unpack $ format' str params

instance FormatType Text where
  format' str params = go fragments (reverse params)
    where
      fragments = T.splitOn "{}" str
      go (f:fs) (y:ys) = f <> y <> go fs ys
      go [f] []        = f
      go _ _ = error $ format
        "ShortcutLinks.Utils.format: {} placeholders, but {} parameters"
        (length fragments - 1)
        (length params)

instance (FormatArg a, FormatType r) => FormatType (a -> r) where
  format' :: Text -> [Text] -> (a -> r)
  format' str params a = format' str (formatArg a : params)

{- | A 'printf'-like function which fully supports 'Text' as an input and
output format and which uses @{}@ instead of @%@ to indicate
placeholders. If you use it, don't forget to enable @OverloadedStrings@.

This is a lightweight alternative to something like the text-format
package, and it's closer to 'printf' and simpler to use.
-}
format :: FormatType r => Text -> r
format str = format' str []
