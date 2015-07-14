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

-- | A list of all functions included in this module, together with suggested
-- names for them.
allShortcuts :: [([Text], Shortcut)]
allShortcuts =
  let (.=) names func = (T.words names, func)
  in [
  -- encyclopedias
  "w wikipedia"             .= wikipedia,
  "tvt tvtropes"            .= tvtropes,
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
  "sublime"                 .= packagecontrol,
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

-- | <https://facebook.com Facebook>
--
-- Link example:
-- @[green](\@fb)@ →
-- <https://facebook.com/green>
facebook :: Shortcut
facebook _ q = return $ "https://facebook.com/" <> q

-- | <https://vk.com Vkontakte> (Вконтакте)
--
-- Link example (username):
-- @[green](\@vk)@ →
-- <https://vk.com/green>
--
-- Link example (ID):
--
-- @[1337](\@vk)@ →
-- <https://vk.com/id1337>
vk :: Shortcut
vk _ q = return $ "https://vk.com/" <> q'
  where q' = if not (T.null q) && isDigit (T.head q) then "id" <> q else q

-- | <https://plus.google.com Google+>
--
-- Link example (username):
-- @[SergeyBrin](\@gp)@ →
-- <https://plus.google.com/+SergeyBrin SergeyBrin>
-- 
-- Link example (username with @+@):
-- @[+SergeyBrin](\@gp)@ →
-- <https://plus.google.com/+SergeyBrin +SergeyBrin>
--
-- Link example (full name – will just be concatenated):
-- @[Sergey Brin](\@gp)@ →
-- <https://plus.google.com/+SergeyBrin Sergey Brin>
--
-- Link example (ID):
-- @[Sergey Brin](\@gp:109813896768294978296)@ →
-- <https://plus.google.com/109813896768294978296 Sergey Brin>
--
-- Link example (hashtag):
-- @[#Australia](\@gp)@ →
-- <https://plus.google.com/explore/Australia #Australia>
googleplus :: Shortcut
googleplus _ q
  | T.null q        = return $ url
  | T.head q == '#' = return $ format "{}/explore/{}" url (T.tail q)
  | T.head q == '+' = return $ format "{}/{}" url q
  | T.all isDigit q = return $ format "{}/{}" url q
  | otherwise       = return $ format "{}/+{}" url (T.concat (T.words q))
  where url = "https://plus.google.com"

-- | <https://twitter.com Twitter>
--
-- Link example (username):
-- @[kmett](\@t)@ →
-- <https://twitter.com/kmett kmett>
-- 
-- Link example (username with @\@@):
-- @[\@kmett](\@t)@ →
-- <https://twitter.com/kmett \@kmett>
--
-- Link example (hashtag):
-- @[#haskell](\@t)@ →
-- <https://twitter.com/hashtag/haskell #haskell>
twitter :: Shortcut
twitter _ q
  | T.null q        = return $ url
  | T.head q == '#' = return $ format "{}/hashtag/{}" url (T.tail q)
  | T.head q == '@' = return $ format "{}/{}" url (T.tail q)
  | otherwise       = return $ format "{}/{}" url q
  where url = "https://twitter.com"

-- | <https://juick.com Juick>
--
-- Link example (username):
-- @[thefish](\@juick)@ →
-- <https://juick.com/thefish/ thefish>
-- 
-- Link example (username with @\@@):
-- @[\@thefish](\@juick)@ →
-- <https://juick.com/thefish/ \@thefish>
--
-- Link example (tag):
-- @[*Haskell](\@juick)@ →
-- <https://juick.com/tag/Haskell *Haskell>
juick :: Shortcut
juick _ q
  | T.null q        = return $ url
  | T.head q == '*' = return $ format "{}/tag/{}" url (T.tail q)
  | T.head q == '@' = return $ format "{}/{}" url (T.tail q)
  | otherwise       = return $ format "{}/{}" url q
  where url = "https://juick.com"

-- | <https://google.com Google>
--
-- Link example:
-- @[random query](\@google)@ →
-- <https://www.google.com/search?nfpr=1&q=random+query random query>
google :: Shortcut
google _ q = return $
  "https://google.com/search?nfpr=1&q=" <> replaceSpaces '+' q

-- | <https://duckduckgo.com Duckduckgo>
--
-- Link example:
-- @[random query](\@ddg)@ →
-- <https://duckduckgo.com/?q=random+query random query>
duckduckgo :: Shortcut
duckduckgo _ q = return $ "https://duckduckgo.com/?q=" <> replaceSpaces '+' q

-- | <http://yandex.ru Yandex> (Russian search engine)
--
-- Link example:
-- @[random query](\@yandex)@ →
-- <http://yandex.ru/search/?noreask=1&text=random+query random query>
yandex :: Shortcut
yandex _ q = return $
  "http://yandex.ru/search/?noreask=1&text=" <> replaceSpaces '+' q

-- | <http://baidu.com Baidu> (Chinese search engine)
--
-- Link example:
-- @[random query](\@baidu)@ →
-- <http://baidu.com/s?nojc=1&wd=random+query random query>
baidu :: Shortcut
baidu _ q = return $ "http://baidu.com/s?nojc=1&wd=" <> replaceSpaces '+' q

-- | __Node.js__ – <https://npmjs.com NPM>
--
-- Link example:
-- @[markdown](\@npm)@ →
-- <https://www.npmjs.com/package/markdown markdown>
npm :: Shortcut
npm _ q = return $ "https://npmjs.com/package/" <> q

-- | __Javascript__ – <http://jamjs.org/packages/#/ Jam>
--
-- Link example:
-- @[pagedown](\@jam)@ →
-- <http://jamjs.org/packages/#/details/pagedown pagedown>
jam :: Shortcut
jam _ q = return $ "http://jamjs.org/packages/#/details/" <> q

-- | __Ruby__ – <https://rubygems.org RubyGems.org>
--
-- Link example:
-- @[github-markdown](\@gem)@ →
-- <https://rubygems.org/gems/github-markdown github-markdown>
rubygems :: Shortcut
rubygems _ q = return $ "https://rubygems.org/gems/" <> q

-- | __Python__ – <https://pypi.python.org/pypi PyPI>
--
-- Link example:
-- @[Markdown](\@pypi)@ →
-- <https://pypi.python.org/pypi/Markdown Markdown>
pypi :: Shortcut
pypi _ q = return $ "https://pypi.python.org/pypi/" <> q

-- | __Perl__ – <https://metacpan.org MetaCPAN> (by module)
--
-- Link example:
-- @[Text::Markdown](\@cpan)@ →
-- <https://metacpan.org/pod/Text::Markdown Text::Markdown>
metacpanPod :: Shortcut
metacpanPod _ q = return $ "https://metacpan.org/pod/" <> q

-- | __Perl__ – <https://metacpan.org MetaCPAN> (by release)
--
-- Link example:
-- @[Text-Markdown](\@cpan-r)@ →
-- <https://metacpan.org/release/Text-Markdown Text-Markdown>
metacpanRelease :: Shortcut
metacpanRelease _ q = return $ "https://metacpan.org/release/" <> q

-- | __Haskell__ – <https://hackage.haskell.org Hackage>
--
-- Link example:
-- @[cmark](\@hackage)@ →
-- <https://hackage.haskell.org/package/cmark cmark>
hackage :: Shortcut
hackage _ q = return $ "https://hackage.haskell.org/package/" <> q

-- | __Rust__ – <https://crates.io Cargo>
--
-- Link example:
-- @[hoedown](\@cargo)@ →
-- <https://crates.io/crates/hoedown hoedown>
cargo :: Shortcut
cargo _ q = return $ "https://crates.io/crates/" <> q

-- | __PHP__ – <http://pear.php.net PEAR>
--
-- Link example:
-- @[Text_Wiki_Doku](\@pear)@ →
-- <http://pear.php.net/package/Text_Wiki_Doku Text_Wiki_Doku>
pear :: Shortcut
pear _ q = return $ "http://pear.php.net/package/" <> q

-- | __Dart__ – <https://pub.dartlang.org pub>
--
-- Link example:
-- @[md_proc](\@pub)@ →
-- <https://pub.dartlang.org/packages/md_proc md_proc>
pub :: Shortcut
pub _ q = return $ "https://pub.dartlang.org/packages/" <> q

-- | __R__ – <http://cran.r-project.org/web/packages/ CRAN>
--
-- Link example:
-- @[markdown](\@cran)@ →
-- <http://cran.r-project.org/web/packages/markdown markdown>
cran :: Shortcut
cran _ q = return $ "http://cran.r-project.org/web/packages/" <> q

-- | __Erlang__ – <https://hex.pm Hex>
--
-- Link example:
-- @[earmark](\@hex)@ →
-- <https://hex.pm/packages/earmark earmark>
hex :: Shortcut
hex _ q = return $ "https://hex.pm/packages/" <> q

-- | __SWI-Prolog__ – <http://www.swi-prolog.org/pack/list packages>
--
-- Link example:
-- @[markdown](\@swiprolog)@ →
-- <http://www.swi-prolog.org/pack/list?p=markdown markdown>
swiprolog :: Shortcut
swiprolog _ q = return $ "http://www.swi-prolog.org/pack/list?p=" <> q

-- | __D__ – <http://code.dlang.org DUB>
--
-- Link example:
-- @[dmarkdown](\@dub)@ →
-- <http://code.dlang.org/packages/dmarkdown dmarkdown>
dub :: Shortcut
dub _ q = return $ "http://code.dlang.org/packages/" <> q

-- | __Bash__ – <http://bpkg.io bpkg>
--
-- Link example:
-- @[markdown](\@bpkg)@ →
-- <http://www.bpkg.io/pkg/markdown markdown>
bpkg :: Shortcut
bpkg _ q = return $ "http://bpkg.io/pkg/" <> q

-- | <https://github.com Github>
--
-- Link example:
-- @[aelve/shortcut-links](\@gh)@ →
-- <https://github.com/aelve/shortcut-links aelve/shortcut-links>
--
-- The repository owner can also be given as an option:
--
-- @[shortcut-links](\@gh(aelve))@ →
-- <https://github.com/aelve/shortcut-links shortcut-links>
github :: Shortcut
github mbOwner q = case mbOwner of
  Nothing    -> return $ format "https://github.com/{}" q
  Just owner -> return $ format "https://github.com/{}/{}" owner q

-- | <https://bitbucket.org Bitbucket>
--
-- Link example:
-- @[bos/text](\@bitbucket)@ →
-- <https://bitbucket.org/bos/text bos/text>
--
-- The repository owner can also be given as an option:
--
-- @[text](\@bitbucket(bos))@ →
-- <https://bitbucket.org/bos/text text>
bitbucket :: Shortcut
bitbucket mbOwner q = case mbOwner of
  Nothing    -> return $ format "https://bitbucket.org/{}" q
  Just owner -> return $ format "https://bitbucket.org/{}/{}" owner q

-- | <https://gitlab.com Gitlab>
--
-- Link example:
-- @[learnyou/lysa](\@gitlab)@ →
-- <https://gitlab.com/learnyou/lysa learnyou/lysa>
--
-- The repository owner can also be given as an option:
--
-- @[lysa](\@gitlab(learnyou))@ →
-- <https://gitlab.com/learnyou/lysa lysa>
--
-- Note that links like <https://gitlab.com/owner> work but are going to be
-- automatically redirected to either <https://gitlab.com/u/owner> or
-- <https://gitlab.com/groups/owner>, depending on whether it's a user or a
-- team. So, it's a case when the “links have to look as authentic as
-- possible” principle is violated (but c'mon, this “u” thing looks ugly
-- anyway).
gitlab :: Shortcut
gitlab mbOwner q = case mbOwner of
  Nothing    -> return $ format "https://gitlab.com/{}" q
  Just owner -> return $ format "https://gitlab.com/{}/{}" owner q

-- | __Android__ – <https://play.google.com Google Play> (formerly Play Market)
--
-- Link example:
-- @[Opera Mini](\@gplay:com.opera.mini.native)@ →
-- <https://play.google.com/store/apps/details?id=com.opera.mini.native Opera Mini>
googleplay :: Shortcut
googleplay _ q = return $ "https://play.google.com/store/apps/details?id=" <> q

-- | <http://braumeister.org Braumeister> (Homebrew formulas)
--
-- Link example:
-- @[multimarkdown](\@brew)@ →
-- <http://braumeister.org/formula/multimarkdown multimarkdown>
braumeister :: Shortcut
braumeister _ q = return $ "http://braumeister.org/formula/" <> q

-- | <https://chocolatey.org Chocolatey>
--
-- Link example:
-- @[Opera](\@chocolatey)@ →
-- <https://chocolatey.org/packages/Opera Opera>
chocolatey :: Shortcut
chocolatey _ q = return $ "https://chocolatey.org/packages/" <> q

-- | __Debian__ – <https://debian.org/distrib/packages packages>
--
-- Link example (stable distribution):
-- @[ghc](\@debian)@ →
-- <https://packages.debian.org/stable/ghc ghc>
-- 
-- Link example (experimental distribution, but could be any other):
-- @[ghc](\@debian(experimental))@ →
-- <https://packages.debian.org/experimental/ghc ghc>
debian :: Shortcut
debian mbDist q = return $ format "https://packages.debian.org/{}/{}" dist q
  where
    dist = fromMaybe "stable" mbDist

-- | __Arch Linux__ – <https://aur.archlinux.org AUR> (“user repository”)
--
-- Link example:
-- @[ghc-git](\@aur)@ →
-- <https://aur.archlinux.org/packages/ghc-git ghc-git>
aur :: Shortcut
aur _ q = return $ "https://aur.archlinux.org/packages/" <> q

-- | __Gentoo__ – <https://packages.gentoo.org packages>
--
-- Link example:
-- @[dev-lang/ghc](\@gentoo)@ →
-- <https://packages.gentoo.org/package/dev-lang/ghc dev-lang/ghc>
-- 
-- Link example (category as an option):
-- @[ghc](\@gentoo(dev-lang))@ →
-- <https://packages.gentoo.org/package/dev-lang/ghc ghc>
--
-- Note that if you don't specify any category, the link would still work –
-- but there are a lot of packages with overlapping names (like “ace”, “csv”,
-- “http”), and such links would lead to search pages listing several
-- packages. So, it's better to include categories.
gentoo :: Shortcut
gentoo mbCat q = return $ "https://packages.gentoo.org/package/" <> pkg
  where
    pkg = case mbCat of
      Nothing  -> q
      Just cat -> cat <> "/" <> q

-- | __openSUSE__ – <http://software.opensuse.org packages>
--
-- Link example:
-- @[ghc](\@opensuse)@ →
-- <http://software.opensuse.org/package/ghc ghc>
opensuse :: Shortcut
opensuse _ q = return $ "http://software.opensuse.org/package/" <> q

-- | __Linux Mint__ – <http://community.linuxmint.com/software/browse packages>
--
-- Link example:
-- @[ghc](\@mint)@ →
-- <http://community.linuxmint.com/software/view/ghc ghc>
mint :: Shortcut
mint _ q = return $ "http://community.linuxmint.com/software/view/" <> q

-- | __Fedora__ – <https://admin.fedoraproject.org/pkgdb packages>
--
-- Link example:
-- @[ghc](\@fedora)@ →
-- <https://admin.fedoraproject.org/pkgdb/package/ghc ghc>
fedora :: Shortcut
fedora _ q = return $ "https://admin.fedoraproject.org/pkgdb/package/" <> q

-- | __Emacs__ – <https://marmalade-repo.org Marmalade>
--
-- Link example:
-- @[markdown-mode](\@marmalade)@ →
-- <https://marmalade-repo.org/packages/markdown-mode markdown-mode>
marmalade :: Shortcut
marmalade _ q = return $ "https://marmalade-repo.org/packages/" <> q

-- | __Emacs__ – <http://melpa.org MELPA>
--
-- Link example:
-- @[markdown-mode](\@melpa)@ →
-- <http://melpa.org/#/markdown-mode markdown-mode>
melpa :: Shortcut
melpa _ q = return $ "http://melpa.org/#/" <> q

-- | __Emacs__ – <https://elpa.gnu.org ELPA>
--
-- Link example:
-- @[undo-tree](\@elpa)@ →
-- <https://elpa.gnu.org/packages/undo-tree.html undo-tree>
elpa :: Shortcut
elpa _ q = return $ format "https://elpa.gnu.org/packages/{}.html" q

-- | __Sublime Text__ – <https://packagecontrol.io Package Control>
--
-- Link example:
-- @[MarkdownEditing](\@sublime)@ →
-- <https://packagecontrol.io/packages/MarkdownEditing MarkdownEditing>
packagecontrol :: Shortcut
packagecontrol _ q = return $ "https://packagecontrol.io/packages/" <> q

-- | __Atom__ – <https://atom.io/packages packages>
--
-- Link example:
-- @[tidy-markdown](\@atom)@ →
-- <https://atom.io/packages/tidy-markdown tidy-markdown>
atomPackage :: Shortcut
atomPackage _ q = return $ "https://atom.io/packages/" <> q

-- | __Atom__ – <https://atom.io/themes themes>
--
-- Link example:
-- @[atom-material-ui](\@atom-theme)@ →
-- <https://atom.io/themes/atom-material-ui atom-material-ui>
atomTheme :: Shortcut
atomTheme _ q = return $ "https://atom.io/themes/" <> q

-- | __jEdit__ – <http://plugins.jedit.org packages>
--
-- Link example:
-- @[MarkdownPlugin](\@jedit)@ →
-- <http://plugins.jedit.org/plugins/?MarkdownPlugin MarkdownPlugin>
jedit :: Shortcut
jedit _ q = return $ "http://plugins.jedit.org/plugins/?" <> q

-- | __Vim__ – <http://www.vim.org/scripts/ scripts>
--
-- Link example (ID):
-- @[haskell.vim](\@vim:2062)@ →
-- <http://www.vim.org/scripts/script.php?script_id=2062 haskell.vim>
vim :: Shortcut
vim _ q = return $ "http://www.vim.org/scripts/script.php?script_id=" <> q

-- | __Opera__ – <https://addons.opera.com/extensions/ extensions>
--
-- Link example:
-- @[Amazon](\@opera:amazon-for-opera)@ →
-- <https://addons.opera.com/extensions/details/amazon-for-opera Amazon>
operaExt :: Shortcut
operaExt _ q = return $ "https://addons.opera.com/extensions/details/" <> q

-- | __Opera__ – <https://addons.opera.com/themes/ themes>
--
-- Link example:
-- @[Space theme](\@opera-theme:space-15)@ →
-- <https://addons.opera.com/en/themes/details/space-15 Space theme>
operaTheme :: Shortcut
operaTheme _ q = return $ "https://addons.opera.com/themes/details/" <> q

-- | __Firefox__ – <https://addons.mozilla.org/firefox Add-ons> (extensions, themes)
--
-- Link example:
-- @[tree-style-tab]@ →
-- <https://addons.mozilla.org/firefox/addon/tree-style-tab Tree Style Tab>
firefox :: Shortcut
firefox _ q = return $ "https://addons.mozilla.org/firefox/addon/" <> q

-- | __Chrome__ – <https://chrome.google.com/webstore Chrome Web Store> (extensions, apps, themes)
--
-- Link example:
-- @[hdokiejnpimakedhajhdlcegeplioahd]@ →
-- <https://chrome.google.com/webstore/detail/hdokiejnpimakedhajhdlcegeplioahd LastPass>
chrome :: Shortcut
chrome _ q = return $ "https://chrome.google.com/webstore/detail/" <> q

-- | <https://www.haskell.org/ghc/ GHC> (Glasgow Haskell Compiler) extensions
--
-- Link example:
-- @[ViewPatterns](\@ghc-ext)@ →
-- <https://downloads.haskell.org/~ghc/latest/docs/html/users_guide/syntax-extns.html#view-patterns ViewPatterns>
ghcExt :: Shortcut
ghcExt _ ext = case lookup ext ghcExtsList of
  Nothing   -> fail (format "unknown GHC extension '{}'" ext)
  Just link -> return link

-- | <https://www.ietf.org/rfc.html RFCs>
--
-- Link example:
-- @[RFC 2026](\@rfc)@ →
-- <https://tools.ietf.org/html/rfc2026 RFC 2026>
--
-- Precise format of recognised text: optional “rfc” (case-insensitive), then
-- arbitrary amount of spaces and punctuation (or nothing), then the
-- number. Examples: “RFC 2026”, “RFC-2026”, “rfc2026”, “rfc #2026”, “2026”,
-- “#2026”.
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

-- | <http://ecma-international.org/publications/index.html Ecma standards and technical reports>
--
-- Link example (standard):
-- @[ECMA-262](\@ecma)@ →
-- <http://www.ecma-international.org/publications/standards/Ecma-262.htm ECMA-262>
--
-- Link example (technical report):
-- @[TR/71](\@ecma)@ →
-- <http://ecma-international.org/publications/techreports/E-TR-071.htm TR/71>
--
-- Precise format of recognised text for standards: optional “ECMA”
-- (case-insensitive), then arbitrary amount of spaces and punctuation (or
-- nothing), then the number. Examples: “ECMA-262”, “ECMA 262”, “ecma262”,
-- “ECMA #262”, “262”, “#262”.
--
-- Format for technical reports is the same, except that “TR” (instead of
-- “ECMA”) is not optional (so, if there's only a number given, it's
-- considered a standard and not a technical report).
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

-- | <http://cve.mitre.org CVEs> (Common Vulnerabilities and Exposures)
--
-- Link example:
-- @[CVE-2014-10001](\@cve)@ →
-- <http://cve.mitre.org/cgi-bin/cvename.cgi?name=CVE-2014-10001>
--
-- Precise format of recognised text: optional “cve” (case-insensitive), then
-- arbitrary amount of spaces and punctuation (or nothing), then the year,
-- ‘-’, and a number. Examples: “CVE-2014-10001”, “cve 2014-10001”,
-- “2014-10001”.
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

-- | <https://wikipedia.org/ Wikipedia>
--
-- Link example (English Wikipedia):
-- @[grey-headed flying fox](\@w)@ →
-- <https://en.wikipedia.org/wiki/Grey-headed_flying_fox>
-- 
-- Link example (Russian Wikipedia, but any language code can be used):
-- @[Haskell](\@w(ru))@ →
-- <https://ru.wikipedia.org/wiki/Haskell>
wikipedia :: Shortcut
wikipedia mbLang q = return $
  format "https://{}.wikipedia.org/wiki/{}" lang q'
  where
    lang = fromMaybe "en" mbLang
    q'   = titleFirst (replaceSpaces '_' q)

-- | <http://tvtropes.org TV Tropes>
--
-- Link example (trope):
-- @[so bad, it's good](\@tvtropes)@ →
-- <http://tvtropes.org/pmwiki/pmwiki.php/Main/SoBadItsGood>
--
-- Link example (series):
-- @[Elementary](\@tvtropes(series))@ →
-- <http://tvtropes.org/pmwiki/pmwiki.php/Series/Elementary>
--
-- You can give anything as a category instead of “series”, it'll be
-- capitalised but nothing else.
tvtropes :: Shortcut
tvtropes mbCat q = return $
  format "http://tvtropes.org/pmwiki/pmwiki.php/{}/{}" cat q'
  where
    cat = maybe "Main" titleFirst mbCat
    isSep c = (isSpace c || isPunctuation c) && c /= '\''
    -- Break into words, transform each word like “it's” → “Its”, and concat.
    -- Note that e.g. “man-made” is considered 2 separate words.
    q' = T.concat $ map (titleFirst . T.filter isAlphaNum) (T.split isSep q)

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
