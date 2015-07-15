{-# LANGUAGE
OverloadedStrings,
DeriveFunctor,
ViewPatterns
  #-}


module ShortcutLinks.All
(
  Result(..),
  Shortcut,
  allShortcuts,

  -- * Encyclopedias
  wikipedia, tvtropes,

  -- * Social networks
  facebook, vk, googleplus,

  -- * Microblogs
  twitter, juick,

  -- * Major search engines
  google, duckduckgo, yandex, baidu,

  -- * Programming language libraries
  npm, jam, rubygems, pypi, metacpanPod, metacpanRelease, hackage, cargo,
  pub, hex, cran, swiprolog, dub, bpkg, pear,

  -- * Code hosting
  github, gitlab, bitbucket,

  -- * OS packages
  -- ** Mobile
  googleplay,
  -- ** Windows
  chocolatey,
  -- ** OS X
  braumeister,
  -- ** Linux
  debian, aur, mint, fedora, gentoo, opensuse,

  -- * Addons
  -- ** Text editors
  marmalade, melpa, elpa, packagecontrol, atomPackage, atomTheme, jedit, vim,
  -- ** Browsers
  operaExt, operaTheme, firefox, chrome,

  -- * Manuals
  ghcExt,

  -- * Standards and databases
  rfc, ecma, cve,
)
where


-- General
import Data.Monoid
import Control.Applicative
import Control.Monad
import Data.Maybe
-- Text
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char
-- shortcut-links
import ShortcutLinks.Utils


data Result a = Failure String | Warning [String] a | Success a
  deriving (Show, Functor)

instance Applicative Result where
  pure = return
  (<*>) = ap

instance Monad Result where
  fail = Failure
  return = Success
  Failure x    >>= _ = Failure x
  Warning wa a >>= f = case f a of
    Success    b -> Warning wa b
    Warning wb b -> Warning (wa ++ wb) b
    Failure x    -> Failure x
  Success    a >>= f = f a

warn :: String -> Result ()
warn s = Warning [s] ()

type Shortcut = Maybe Text -> Text -> Result Text

{- |
A list of all functions included in this module, together with suggested names for them.
-}
allShortcuts :: [([Text], Shortcut)]
allShortcuts =
  let (.=) names func = (T.words names, func)
  in [
  -- encyclopedias
  "w wikipedia"             .= wikipedia,
  "tvtropes"                .= tvtropes,
  -- social networks
  "fb facebook"             .= facebook,
  "vk vkontakte"            .= vk,
  "gp gplus googleplus"     .= googleplus,
  -- microblogs
  "t twitter"               .= twitter,
  "juick"                   .= juick,
  -- search engines
  "google"                  .= google,
  "ddg duckduckgo"          .= duckduckgo,
  "yandex"                  .= yandex,
  "baidu"                   .= baidu,
  -- programming language libraries
  "npm"                     .= npm,
  "jam"                     .= jam,
  "gem"                     .= rubygems,
  "pypi"                    .= pypi,
  "cpan"                    .= metacpanPod,
  "cpan-r"                  .= metacpanRelease,
  "hackage"                 .= hackage,
  "cargo"                   .= cargo,
  "pub"                     .= pub,
  "hex"                     .= hex,
  "cran"                    .= cran,
  "swiprolog"               .= swiprolog,
  "dub"                     .= dub,
  "bpkg"                    .= bpkg,
  "pear"                    .= pear,
  -- code hosting
  "gh github"               .= github,
  "gitlab"                  .= gitlab,
  "bitbucket"               .= bitbucket,
  -- OS
  "gplay googleplay"        .= googleplay,
  "chocolatey"              .= chocolatey,
  "brew"                    .= braumeister,
  -- OS – Linux
  "debian"                  .= debian,
  "aur"                     .= aur,
  "mint"                    .= mint,
  "fedora"                  .= fedora,
  "gentoo"                  .= gentoo,
  "opensuse"                .= opensuse,
  -- text editors
  "marmalade"               .= marmalade,
  "melpa"                   .= melpa,
  "elpa"                    .= elpa,
  "sublimepc"               .= packagecontrol,
  "atom"                    .= atomPackage,
  "atom-theme"              .= atomTheme,
  "jedit"                   .= jedit,
  "vim"                     .= vim,
  -- browsers
  "opera"                   .= operaExt,
  "opera-theme"             .= operaTheme,
  "firefox"                 .= firefox,
  "chrome"                  .= chrome,
  -- manuals
  "ghc-ext"                 .= ghcExt,
  -- standards and databases
  "rfc"                     .= rfc,
  "ecma"                    .= ecma,
  "cve"                     .= cve ]

{- | <https://facebook.com Facebook>

Link by username:

@
\[green\](\@fb)
<https://facebook.com/green>
@

Or by profile ID (are there still people without usernames, actually?):

@
\[someone something\](\@fb:164680686880529)
<https://www.facebook.com/profile.php?id=164680686880529>
@
-}
facebook :: Shortcut
facebook _ q = return $ "https://facebook.com/" <> q

{- | <https://vk.com Vkontakte> (Вконтакте)

Link by username:

@
\[green\](\@vk)
<https://vk.com/green>
@

Or by ID:

@
\[Durov\](\@vk:1)
<https://vk.com/id1>
@
-}
vk :: Shortcut
vk _ q = return $ "https://vk.com/" <> q'
  where q' = if not (T.null q) && isDigit (T.head q) then "id" <> q else q

{- | <https://plus.google.com Google+>

Link by username:

@
\[SergeyBrin\](\@gp)
<https://plus.google.com/+SergeyBrin>
@

It's alright if the username already starts with a “+”:

@
\[+SergeyBrin\](\@gp)
<https://plus.google.com/+SergeyBrin>
@

Since many usernames are just “your full name without spaces”, in many cases you can give a name and it's easy to make a username from it:

@
\[Sergey Brin\](\@gp)
<https://plus.google.com/+SergeyBrin>
@

You can also link by ID:

@
\[Sergey Brin\](\@gp:109813896768294978296)
<https://plus.google.com/109813896768294978296>
@

Finally, there are different links for hashtags:

@
\[#Australia\](\@gp)
<https://plus.google.com/explore/Australia>
@
-}
googleplus :: Shortcut
googleplus _ q
  | T.null q        = return $ url
  | T.head q == '#' = return $ format "{}/explore/{}" url (T.tail q)
  | T.head q == '+' = return $ format "{}/{}" url q
  | T.all isDigit q = return $ format "{}/{}" url q
  | otherwise       = return $ format "{}/+{}" url (T.concat (T.words q))
  where url = "https://plus.google.com"

{- | <https://twitter.com Twitter>

Link by username:

@
\[Edward Kmett\](\@t:kmett)
<https://twitter.com/kmett>
@

It's alright if the username already starts with a “\@”:

@
\[\@kmett\](\@t)
<https://twitter.com/kmett>
@

There are different links for hashtags:

@
\[#haskell\](\@t)
<https://twitter.com/hashtag/haskell>
@
-}
twitter :: Shortcut
twitter _ q
  | T.null q        = return $ url
  | T.head q == '#' = return $ format "{}/hashtag/{}" url (T.tail q)
  | T.head q == '@' = return $ format "{}/{}" url (T.tail q)
  | otherwise       = return $ format "{}/{}" url q
  where url = "https://twitter.com"

{- | <https://juick.com Juick>

Link by username:

@
\[thefish\](\@juick)
<https://juick.com/thefish>
@

It's alright if the username already starts with a “\@”:

@
\[\@thefish\](\@juick)
<https://juick.com/thefish>
@

There are different links for tags (which start with “\*” and not with “#”, by the way):

@
\[*Haskell\](\@juick)
<https://juick.com/tag/Haskell>
@
-}
juick :: Shortcut
juick _ q
  | T.null q        = return $ url
  | T.head q == '*' = return $ format "{}/tag/{}" url (T.tail q)
  | T.head q == '@' = return $ format "{}/{}" url (T.tail q)
  | otherwise       = return $ format "{}/{}" url q
  where url = "https://juick.com"

{- | <https://google.com Google>

Search results:

@
\[random query\](\@google)
<https://www.google.com/search?nfpr=1&q=random+query>
@
-}
google :: Shortcut
google _ q = return $
  "https://google.com/search?nfpr=1&q=" <> replaceSpaces '+' q

{- | <https://duckduckgo.com Duckduckgo>

Search results:

@
\[random query\](\@ddg)
<https://duckduckgo.com/?q=random+query>
@
-}
duckduckgo :: Shortcut
duckduckgo _ q = return $ "https://duckduckgo.com/?q=" <> replaceSpaces '+' q

{- | <http://yandex.ru Yandex> (Russian search engine)

Search results:

@
\[random query\](\@yandex)
<http://yandex.ru/search/?noreask=1&text=random+query>
@
-}
yandex :: Shortcut
yandex _ q = return $
  "http://yandex.ru/search/?noreask=1&text=" <> replaceSpaces '+' q

{- | <http://baidu.com Baidu> (Chinese search engine)

Search results:

@
\[random query\](\@baidu)
<http://baidu.com/s?nojc=1&wd=random+query>
@
-}
baidu :: Shortcut
baidu _ q = return $ "http://baidu.com/s?nojc=1&wd=" <> replaceSpaces '+' q

{- | __Node.js__ – <https://npmjs.com NPM>

Link to a package:

@
\[markdown\](\@npm)
<https://www.npmjs.com/package/markdown>
@
-}
npm :: Shortcut
npm _ q = return $ "https://npmjs.com/package/" <> q

{- | __Javascript__ – <http://jamjs.org/packages/#/ Jam>

Link to a package:

@
\[pagedown\](\@jam)
<http://jamjs.org/packages/#/details/pagedown>
@
-}
jam :: Shortcut
jam _ q = return $ "http://jamjs.org/packages/#/details/" <> q

{- | __Ruby__ – <https://rubygems.org RubyGems.org>

Link to a package:

@
\[github-markdown\](\@gem)
<https://rubygems.org/gems/github-markdown>
@
-}
rubygems :: Shortcut
rubygems _ q = return $ "https://rubygems.org/gems/" <> q

{- | __Python__ – <https://pypi.python.org/pypi PyPI>

Link to a package:

@
\[Markdown\](\@pypi)
<https://pypi.python.org/pypi/Markdown>
@
-}
pypi :: Shortcut
pypi _ q = return $ "https://pypi.python.org/pypi/" <> q

{- | __Perl__ – <https://metacpan.org MetaCPAN> (by module)

Link to a module:

@
\[Text::Markdown\](\@cpan)
<https://metacpan.org/pod/Text::Markdown>
@

To link to a release, look at 'metacpanRelease'.
-}
metacpanPod :: Shortcut
metacpanPod _ q = return $ "https://metacpan.org/pod/" <> q

{- | __Perl__ – <https://metacpan.org MetaCPAN> (by release)

Link to a release:

@
\[Text-Markdown\](\@cpan-r)
<https://metacpan.org/release/Text-Markdown>
@
-}
metacpanRelease :: Shortcut
metacpanRelease _ q = return $ "https://metacpan.org/release/" <> q

{- | __Haskell__ – <https://hackage.haskell.org Hackage>

Link to a package:

@
\[cmark\](\@hackage)
<https://hackage.haskell.org/package/cmark>
@
-}
hackage :: Shortcut
hackage _ q = return $ "https://hackage.haskell.org/package/" <> q

{- | __Rust__ – <https://crates.io Cargo>

Link to a package:

@
\[hoedown\](\@cargo)
<https://crates.io/crates/hoedown>
@
-}
cargo :: Shortcut
cargo _ q = return $ "https://crates.io/crates/" <> q

{- | __PHP__ – <http://pear.php.net PEAR>

Link to a package:

@
\[Text_Wiki_Doku\](\@pear)
<http://pear.php.net/package/Text_Wiki_Doku>
@
-}
pear :: Shortcut
pear _ q = return $ "http://pear.php.net/package/" <> q

{- | __Dart__ – <https://pub.dartlang.org pub>

Link to a package:

@
\[md_proc\](\@pub)
<https://pub.dartlang.org/packages/md_proc>
@
-}
pub :: Shortcut
pub _ q = return $ "https://pub.dartlang.org/packages/" <> q

{- | __R__ – <http://cran.r-project.org/web/packages/ CRAN>

Link to a package:

@
\[markdown\](\@cran)
<http://cran.r-project.org/web/packages/markdown>
@
-}
cran :: Shortcut
cran _ q = return $ "http://cran.r-project.org/web/packages/" <> q

{- | __Erlang__ – <https://hex.pm Hex>

Link to a package:

@
\[earmark\](\@hex)
<https://hex.pm/packages/earmark>
@
-}
hex :: Shortcut
hex _ q = return $ "https://hex.pm/packages/" <> q

{- | __SWI-Prolog__ – <http://www.swi-prolog.org/pack/list packages>

Link to a package:

@
\[markdown\](\@swiprolog)
<http://www.swi-prolog.org/pack/list?p=markdown>
@
-}
swiprolog :: Shortcut
swiprolog _ q = return $ "http://www.swi-prolog.org/pack/list?p=" <> q

{- | __D__ – <http://code.dlang.org DUB>

Link to a package:

@
\[dmarkdown\](\@dub)
<http://code.dlang.org/packages/dmarkdown>
@
-}
dub :: Shortcut
dub _ q = return $ "http://code.dlang.org/packages/" <> q

{- | __Bash__ – <http://bpkg.io bpkg>

Link to a package:

@
\[markdown\](\@bpkg)
<http://www.bpkg.io/pkg/markdown>
@
-}
bpkg :: Shortcut
bpkg _ q = return $ "http://bpkg.io/pkg/" <> q

{- | <https://github.com Github>

Link to a user:

@
\[Aelve\](\@gh:aelve)
<https://github.com/aelve>
@

Link to a repository:

@
\[aelve/shortcut-links\](\@gh)
<https://github.com/aelve/shortcut-links>
@

The repository owner can also be given as an option (to avoid mentioning nem in the link text):

@
\[shortcut-links\](\@gh(aelve))
<https://github.com/aelve/shortcut-links>
@
-}
github :: Shortcut
github mbOwner q = case mbOwner of
  Nothing    -> return $ format "https://github.com/{}" q
  Just owner -> return $ format "https://github.com/{}/{}" owner q

{- | <https://bitbucket.org Bitbucket>

Link to a user:

@
\[Bryan\](\@bitbucket:bos)
<https://bitbucket.org/bos>
@

Link to a repository:

@
\[bos/text\](\@bitbucket)
<https://bitbucket.org/bos/text>
@

The repository owner can also be given as an option (to avoid mentioning nem in the link text):

@
\[text\](\@bitbucket(bos))
<https://bitbucket.org/bos/text>
@
-}
bitbucket :: Shortcut
bitbucket mbOwner q = case mbOwner of
  Nothing    -> return $ format "https://bitbucket.org/{}" q
  Just owner -> return $ format "https://bitbucket.org/{}/{}" owner q

{- | <https://gitlab.com Gitlab>

Link to a user or a team (note that links like <https://gitlab.com/owner> work but are going to be automatically redirected to either <https://gitlab.com/u/owner> or <https://gitlab.com/groups/owner>, depending on whether it's a user or a team – so, it's a case when the “links have to look as authentic as possible” principle is violated, but nothing can be done with that):

@
\[CyanogenMod\](\@bitbucket)
<https://gitlab.com/CyanogenMod>
@

Link to a repository:

@
\[learnyou/lysa\](\@gitlab)
<https://gitlab.com/learnyou/lysa>
@

The repository owner can also be given as an option (to avoid mentioning nem in the link text):

@
\[lysa\](\@gitlab(learnyou))
<https://gitlab.com/learnyou/lysa>
@
-}
gitlab :: Shortcut
gitlab mbOwner q = case mbOwner of
  Nothing    -> return $ format "https://gitlab.com/{}" q
  Just owner -> return $ format "https://gitlab.com/{}/{}" owner q

{- | __Android__ – <https://play.google.com Google Play> (formerly Play Market)

Link to an app:

@
\[Opera Mini\](\@gplay:com.opera.mini.native)
<https://play.google.com/store/apps/details?id=com.opera.mini.native>
@
-}
googleplay :: Shortcut
googleplay _ q = return $ "https://play.google.com/store/apps/details?id=" <> q

{- | <http://braumeister.org Braumeister> (Homebrew formulas)

Link to a formula:

@
\[multimarkdown\](\@brew)
<http://braumeister.org/formula/multimarkdown>
@
-}
braumeister :: Shortcut
braumeister _ q = return $ "http://braumeister.org/formula/" <> q

{- | <https://chocolatey.org Chocolatey>

Link to a package:

@
\[Opera\](\@chocolatey)
<https://chocolatey.org/packages/Opera>
@
-}
chocolatey :: Shortcut
chocolatey _ q = return $ "https://chocolatey.org/packages/" <> q

{- | __Debian__ – <https://debian.org/distrib/packages packages>

Link to a package in stable distribution:

@
\[ghc\](\@debian)
<https://packages.debian.org/stable/ghc>
@

Distribution can be given as an option:

@
\[ghc\](\@debian(experimental))
<https://packages.debian.org/experimental/ghc>
@
-}
debian :: Shortcut
debian mbDist q = return $ format "https://packages.debian.org/{}/{}" dist q
  where
    dist = fromMaybe "stable" mbDist

{- | __Arch Linux__ – <https://aur.archlinux.org AUR> (“user repository”)

Link to a package:

@
\[ghc-git\](\@aur)
<https://aur.archlinux.org/packages/ghc-git>
@
-}
aur :: Shortcut
aur _ q = return $ "https://aur.archlinux.org/packages/" <> q

{- | __Gentoo__ – <https://packages.gentoo.org packages>

Link to a package:

@
\[dev-lang/ghc\](\@gentoo)
<https://packages.gentoo.org/package/dev-lang/ghc>
@

Category can be given as an option, to avoid cluttering link text:

@
\[ghc\](\@gentoo(dev-lang))
<https://packages.gentoo.org/package/dev-lang/ghc>
@

Note that if you don't specify any category, the link would still work – but there are a lot of packages with overlapping names (like “ace”, “csv”, “http”), and such links would lead to search pages listing several packages. So, it's better to include categories.
-}
gentoo :: Shortcut
gentoo mbCat q = return $ "https://packages.gentoo.org/package/" <> pkg
  where
    pkg = case mbCat of
      Nothing  -> q
      Just cat -> cat <> "/" <> q

{- | __openSUSE__ – <http://software.opensuse.org packages>

Link to a package:

@
\[ghc\](\@opensuse)
<http://software.opensuse.org/package/ghc>
@
-}
opensuse :: Shortcut
opensuse _ q = return $ "http://software.opensuse.org/package/" <> q

{- | __Linux Mint__ – <http://community.linuxmint.com/software/browse packages>

Link to a package:

@
\[ghc\](\@mint)
<http://community.linuxmint.com/software/view/ghc>
@
-}
mint :: Shortcut
mint _ q = return $ "http://community.linuxmint.com/software/view/" <> q

{- | __Fedora__ – <https://admin.fedoraproject.org/pkgdb packages>

Link to a package:

@
\[ghc\](\@fedora)
<https://admin.fedoraproject.org/pkgdb/package/ghc>
@
-}
fedora :: Shortcut
fedora _ q = return $ "https://admin.fedoraproject.org/pkgdb/package/" <> q

{- | __Emacs__ – <https://marmalade-repo.org Marmalade>

Link to a package:

@
\[markdown-mode\](\@marmalade)
<https://marmalade-repo.org/packages/markdown-mode>
@
-}
marmalade :: Shortcut
marmalade _ q = return $ "https://marmalade-repo.org/packages/" <> q

{- | __Emacs__ – <http://melpa.org MELPA>

Link to a package:

@
\[markdown-mode\](\@melpa)
<http://melpa.org/#/markdown-mode>
@
-}
melpa :: Shortcut
melpa _ q = return $ "http://melpa.org/#/" <> q

{- | __Emacs__ – <https://elpa.gnu.org ELPA>

Link to a package:

@
\[undo-tree\](\@elpa)
<https://elpa.gnu.org/packages/undo-tree.html>
@
-}
elpa :: Shortcut
elpa _ q = return $ format "https://elpa.gnu.org/packages/{}.html" q

{- | __Sublime Text__ – <https://packagecontrol.io Package Control>

Link to a package:

@
\[MarkdownEditing\](\@sublimepc)
<https://packagecontrol.io/packages/MarkdownEditing>
@
-}
packagecontrol :: Shortcut
packagecontrol _ q = return $ "https://packagecontrol.io/packages/" <> q

{- | __Atom__ – <https://atom.io/packages packages>

Link to a package:

@
\[tidy-markdown\](\@atom)
<https://atom.io/packages/tidy-markdown>
@
-}
atomPackage :: Shortcut
atomPackage _ q = return $ "https://atom.io/packages/" <> q

{- | __Atom__ – <https://atom.io/themes themes>

Link to a theme:

@
\[atom-material-ui\](\@atom-theme)
<https://atom.io/themes/atom-material-ui>
@
-}
atomTheme :: Shortcut
atomTheme _ q = return $ "https://atom.io/themes/" <> q

{- | __jEdit__ – <http://plugins.jedit.org plugins>

Link to a plugin:

@
\[MarkdownPlugin\](\@jedit)
<http://plugins.jedit.org/plugins/?MarkdownPlugin>
@
-}
jedit :: Shortcut
jedit _ q = return $ "http://plugins.jedit.org/plugins/?" <> q

{- | __Vim__ – <http://www.vim.org/scripts/ scripts>

Link to a script (by ID):

@
\[haskell.vim\](\@vim:2062)
<http://www.vim.org/scripts/script.php?script_id=2062>
@
-}
vim :: Shortcut
vim _ q = return $ "http://www.vim.org/scripts/script.php?script_id=" <> q

{- | __Opera__ – <https://addons.opera.com/extensions/ extensions>

Link to an extension:

@
\[Amazon\](\@opera:amazon-for-opera)
<https://addons.opera.com/extensions/details/amazon-for-opera>
@
-}
operaExt :: Shortcut
operaExt _ q = return $ "https://addons.opera.com/extensions/details/" <> q

{- | __Opera__ – <https://addons.opera.com/themes/ themes>

Link to a theme:

@
\[Space theme\](\@opera-theme:space-15)
<https://addons.opera.com/en/themes/details/space-15>
@
-}
operaTheme :: Shortcut
operaTheme _ q = return $ "https://addons.opera.com/themes/details/" <> q

{- | __Firefox__ – <https://addons.mozilla.org/firefox add-ons>

Link to an extension (or a theme):

@
\[tree-style-tab](\@firefox)
<https://addons.mozilla.org/firefox/addon/tree-style-tab>
@
-}
firefox :: Shortcut
firefox _ q = return $ "https://addons.mozilla.org/firefox/addon/" <> q

{- | __Chrome__ – <https://chrome.google.com/webstore Chrome Web Store>

Link to an extension, app, or theme (using that weird random-looking ID):

@
\[hdokiejnpimakedhajhdlcegeplioahd](\@chrome)
<https://chrome.google.com/webstore/detail/hdokiejnpimakedhajhdlcegeplioahd>
@
-}
chrome :: Shortcut
chrome _ q = return $ "https://chrome.google.com/webstore/detail/" <> q

{- | <https://www.haskell.org/ghc/ GHC> (Glasgow Haskell Compiler) extensions

Link to an extension's description in the user manual:

@
\[ViewPatterns\](\@ghc-ext)
<https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/syntax-extns.html#view-patterns>
@
-}
ghcExt :: Shortcut
ghcExt _ ext = case lookup ext ghcExtsList of
  Nothing   -> fail (format "unknown GHC extension '{}'" ext)
  Just link -> return link

{- | <https://www.ietf.org/rfc.html RFCs>

Link to an RFC:

@
\[RFC 2026\](\@rfc)
<https://tools.ietf.org/html/rfc2026>
@

Precise format of recognised text: optional “rfc” (case-insensitive), then arbitrary amount of spaces and punctuation (or nothing), then the number. Examples: “RFC 2026”, “RFC-2026”, “rfc2026”, “rfc #2026”, “2026”, “#2026”.
-}
rfc :: Shortcut
rfc _ x = do
  let n = T.dropWhile (not . isAlphaNum) (tryStripPrefixCI "rfc" x)
  -- We don't use 'readMaybe' here because 'readMaybe' isn't available in GHC
  -- 7.4, which Pandoc has to be compatible with.
  unless (T.all isDigit n) $
    warn "non-digits in RFC number"
  when (T.null n) $
    warn "no RFC number"
  let n' = T.dropWhile (== '0') n `orElse` "0"
  return ("https://tools.ietf.org/html/rfc" <> n')

{- | <http://ecma-international.org/publications/index.html Ecma standards and technical reports>

Link to a standard:

@
\[ECMA-262\](\@ecma)
<http://www.ecma-international.org/publications/standards/Ecma-262.htm>
@

Link to a technical report:

@
\[TR/71\](\@ecma)
<http://ecma-international.org/publications/techreports/E-TR-071.htm>
@

Precise format of recognised text for standards: optional “ECMA” (case-insensitive), then arbitrary amount of spaces and punctuation (or nothing), then the number. Examples: “ECMA-262”, “ECMA 262”, “ecma262”, “ECMA #262”, “262”, “#262”.

Format for technical reports is the same, except that “TR” (instead of “ECMA”) is not optional (so, if there's only a number given, it's considered a standard and not a technical report).
-}
ecma :: Shortcut
ecma _ q = do
  -- TODO: move dropSeparators to Utils and use it in 'rfc' and 'cve'
  let dropSeparators = T.dropWhile (not . isAlphaNum)
  let (dropSeparators -> mbNum, isTR) = case stripPrefixCI "tr" q of
        Nothing -> (tryStripPrefixCI "ecma" q, False)
        Just q' -> (q', True)
  -- We don't use 'readMaybe' here because 'readMaybe' isn't available in GHC
  -- 7.4, which Pandoc has to be compatible with.
  unless (T.all isDigit mbNum) $
    warn "non-digits in ECMA standard number"
  when (T.null mbNum) $
    warn "no ECMA standard number"
  -- The number has to have at least 3 digits.
  let num = T.justifyRight 3 '0' mbNum
      url = "http://ecma-international.org/publications" :: Text
  return $ if isTR
    then format "{}/techreports/E-TR-{}.htm" url num
    else format "{}/standards/Ecma-{}.htm" url num

{- | <http://cve.mitre.org CVEs> (Common Vulnerabilities and Exposures)

Link to a CVE:

@
\[CVE-2014-10001\](\@cve)
<http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2014-10001>
@

Precise format of recognised text: optional “cve” (case-insensitive), then arbitrary amount of spaces and punctuation (or nothing), then the year, “-”, and a number. Examples: “CVE-2014-10001”, “cve 2014-10001”, “2014-10001”.
-}
cve :: Shortcut
cve _ x = do
  let n = T.dropWhile (not . isAlphaNum) (tryStripPrefixCI "cve" x)
  unless (T.length n >= 9) $
    warn "CVE-ID is too short"
  let isValid = and [
        T.length n >= 9,
        T.all isDigit (T.take 4 n),
        T.index n 4 == '-',
        T.all isDigit (T.drop 5 n) ]
  unless isValid $
    warn "CVE-ID doesn't follow the <year>-<digits> format"
  return ("http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-" <> n)

{- | <https://wikipedia.org/ Wikipedia>

Link to an article in English Wikipedia:

@
\[grey-headed flying fox\](\@w)
<https://en.wikipedia.org/wiki/Grey-headed_flying_fox>
@

You can link to Wikipedia-in-another-language if you give language code as an option:

@
\[Haskell\](\@w(ru))
<https://ru.wikipedia.org/wiki/Haskell>
@
-}
wikipedia :: Shortcut
wikipedia mbLang q = return $
  format "https://{}.wikipedia.org/wiki/{}" lang q'
  where
    lang = fromMaybe "en" mbLang
    q'   = titleFirst (replaceSpaces '_' q)

{- | <http://tvtropes.org TV Tropes>

Link to a trope:

@
\[so bad, it's good\](\@tvtropes)
<http://tvtropes.org/pmwiki/pmwiki.php/Main/SoBadItsGood>
@

Link to anything else (a series, for example):

@
\[Elementary\](\@tvtropes(series))
<http://tvtropes.org/pmwiki/pmwiki.php/Series/Elementary>
@

Or something on Sugar Wiki:

@
\[awesome music\](\@tvtropes(sugar wiki))
<http://tvtropes.org/pmwiki/pmwiki.php/SugarWiki/AwesomeMusic>
@
-}
tvtropes :: Shortcut
tvtropes mbCat q = return $
  format "http://tvtropes.org/pmwiki/pmwiki.php/{}/{}" cat (camel q)
  where
    isSep c = (isSpace c || isPunctuation c) && c /= '\''
    -- Break into words, transform each word like “it's” → “Its”, and concat.
    -- Note that e.g. “man-made” is considered 2 separate words.
    camel = T.concat . map (titleFirst . T.filter isAlphaNum) . T.split isSep
    cat = maybe "Main" camel mbCat

ghcExtsList :: [(Text, Text)]
ghcExtsList = do
  let (.=) = (,)
      base = "https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/"
      prefix x = map (\(a, b) -> (a, x <> b))
  prefix base $ concat [
    prefix "syntax-extns.html" [
      "DisambiguateRecordFields" .= "#disambiguate-fields",
      "EmptyCase" .= "#empty-case",
      "NoImplicitPrelude" .= "#rebindable-syntax",
      "NegativeLiterals" .= "#negative-literals",
      "RebindableSyntax" .= "#rebindable-syntax",
      "PatternGuards" .= "#pattern-guards",
      "ViewPatterns" .= "#view-patterns",
      "UnicodeSyntax" .= "#unicode-syntax",
      "MagicHash" .= "#magic-hash",
      "ParallelListComp" .= "#parallel-list-comprehensions",
      "TransformListComp" .= "#generalised-list-comprehensions",
      "MonadComprehensions" .= "#monad-comprehensions",
      "ExplicitNamespaces" .= "#explicit-namespaces",
      "RecursiveDo" .= "#recursive-do-notation",
      "RecordWildCards" .= "#record-wildcards",
      "NamedFieldPuns" .= "#record-puns",
      "PackageImports" .= "#package-imports",
      "LambdaCase" .= "#lambda-case",
      "MultiWayIf" .= "#multi-way-if",
      "NumDecimals" .= "#num-decimals",
      "BinaryLiterals" .= "#binary-literals",
      "PostfixOperators" .= "#postfix-operators",
      "TupleSections" .= "#tuple-sections",
      "PatternSynonyms" .= "#pattern-synonyms" ],

    prefix "data-type-extensions.html" [
      "GADTs" .= "#gadt",
      "GADTSyntax" .= "#gadt-style",
      "ExistentialQuantification" .= "#existential-quantification",
      "LiberalTypeSynonyms" .= "#type-synonyms",
      "EmptyDataDecls" .= "#nullary-types",
      "DatatypeContexts" .= "#datatype-contexts",
      "TypeOperators" .= "#type-operators" ],

    prefix "other-type-extensions.html" [
      "AllowAmbiguousTypes" .= "#ambiguity",
      "ImplicitParams" .= "#implicit-parameters",
      "NoMonomorphismRestriction" .= "#monomorphism",
      "RelaxedPolyRec" .= "#typing-binds",
      "MonoLocalBinds" .= "#mono-local-binds",
      "ScopedTypeVariables" .= "#scoped-type-variables",
      "ExplicitForAll" .= "#explicit-foralls",
      "PolymorphicComponents" .= "#universal-quantification",
      "Rank2Types" .= "#universal-quantification",
      "RankNTypes" .= "#universal-quantification",
      "ImpredicativeTypes" .= "#impredicative-polymorphism",
      "KindSignatures" .= "#kinding",
      "FlexibleContexts" .= "#flexible-contexts" ],

    prefix "type-class-extensions.html" [
      "IncoherentInstances" .= "#instance-overlap",
      "OverlappingInstances" .= "#instance-overlap",
      "OverloadedLists" .= "#overloaded-lists",
      "OverloadedStrings" .= "#overloaded-strings",
      "UndecidableInstances" .= "#undecidable-instances",
      "TypeSynonymInstances" .= "#flexible-instance-head",
      "FlexibleInstances" .= "#instance-rules",
      "ConstrainedClassMethods" .= "#class-method-types",
      "DefaultSignatures" .= "#class-default-signatures",
      "MultiParamTypeClasses" .= "#multi-param-type-classes",
      "NullaryTypeClasses" .= "#nullary-type-classes",
      "InstanceSigs" .= "#instance-sigs",
      "FunctionalDependencies" .= "#functional-dependencies" ],

    prefix "deriving.html" [
      "AutoDeriveTypeable" .= "#auto-derive-typeable",
      "DeriveDataTypeable" .= "#deriving-typeable",
      "DeriveGeneric" .= "#deriving-typeable",
      "DeriveFunctor" .= "#deriving-extra",
      "DeriveTraversable" .= "#deriving-extra",
      "DeriveFoldable" .= "#deriving-extra",
      "GeneralizedNewtypeDeriving" .= "#newtype-deriving",
      "DeriveAnyClass" .= "#derive-any-class",
      "StandaloneDeriving" .= "#stand-alone-deriving" ],

    prefix "template-haskell.html" [
      "TemplateHaskell" .= "",
      "QuasiQuotes" .= "#th-quasiquotation" ],

    prefix "ffi.html" [
      "ForeignFunctionInterface" .= "",
      "InterruptibleFFI" .= "#ffi-interruptible",
      "CApiFFI" .= "#ffi-capi" ],

    prefix "partial-type-signatures.html" [
      "PartialTypeSignatures" .= "",
      "NamedWildCards" .= "#named-wildcards" ],

    map (.= "safe-haskell.html") [
      "Safe",
      "Trustworthy",
      "Unsafe" ],

    [ "Arrows" .= "arrow-notation.html",
      "ConstraintKinds" .= "constraint-kind.html",
      "DataKinds" .= "promotion.html",
      "ExtendedDefaultRules" .= "interactive-evaluation.html#extended-default-rules",
      "TypeFamilies" .= "type-families.html",
      "PolyKinds" .= "kind-polymorphism.html",
      "BangPatterns" .= "bang-patterns.html",
      "CPP" .= "options-phases.html#c-pre-processor",
      "RoleAnnotations" .= "roles.html#role-annotations",
      "UnboxedTuples" .= "primitives.html#unboxed-tuples" ] ]
