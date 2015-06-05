{-# LANGUAGE
OverloadedStrings
  #-}


module ShortcutLinks.All
(
  allSites,

  -- * Major search engines
  google, duckduckgo, yandex, baidu,

  -- * Programming language libraries
  npm, jam, rubygems, pypi, metacpanPod, metacpanRelease, hackage, cargo,
  pub, hex, cran, swiprolog, dub, bpkg, pear,

  -- * OS packages
  -- ** Mobile
  googleplay,
  -- ** Windows
  chocolatey,
  -- ** OS X
  braumeister, brewformulas,
  -- ** Linux
  debian, aur, mint, fedora, gentoo, opensuse, mageia,

  -- * Addons
  -- ** Text editors
  marmalade, melpa, elpa, packagecontrol, atom, jedit,
  -- ** Browsers
  opera, firefox, chrome,
)
where


-- General
import Data.Monoid
-- Text
import qualified Data.Text as T
import Data.Text (Text)
import Data.Char
-- shortcut-links
import ShortcutLinks.Utils


-- | A list of all functions included in this module.
allSites :: [Text -> Text]
allSites = [
  -- search engines
  google, duckduckgo, yandex, baidu,
  -- programming language libraries
  npm, jam, rubygems, pypi, metacpanPod, metacpanRelease, hackage, cargo,
  pub, hex, cran, swiprolog, dub, bpkg, pear,
  -- OS
  googleplay, chocolatey, braumeister, brewformulas,
  -- OS – Linux
  debian, aur, mint, fedora, gentoo, opensuse, mageia,
  -- text editors
  marmalade, melpa, elpa, packagecontrol, atom, jedit,
  -- browsers
  opera, firefox, chrome ]

-- | <https://google.com Google>
--
-- Link example:
-- @[random query]@ →
-- <https://www.google.com/search?nfpr=1&q=random+query random query>
google :: Text -> Text
google q = "https://google.com/search?nfpr=1&q=" <> replaceSpaces '+' q

-- | <https://duckduckgo.com Duckduckgo>
--
-- Link example:
-- @[random query]@ →
-- <https://duckduckgo.com/?q=random+query random query>
duckduckgo :: Text -> Text
duckduckgo q = "https://duckduckgo.com/?q=" <> replaceSpaces '+' q

-- | <http://yandex.ru Yandex> (Russian search engine)
--
-- Link example:
-- @[random query]@ →
-- <http://yandex.ru/search/?noreask=1&text=random+query random query>
yandex :: Text -> Text
yandex q = "http://yandex.ru/search/?noreask=1&text=" <> replaceSpaces '+' q

-- | <http://baidu.com Baidu> (Chinese search engine)
--
-- Link example:
-- @[random query]@ →
-- <http://baidu.com/s?nojc=1&wd=random+query random query>
baidu :: Text -> Text
baidu q = "http://baidu.com/s?nojc=1&wd=" <> replaceSpaces '+' q

-- | __Node.js__ – <https://npmjs.com NPM>
--
-- Link example:
-- @[markdown]@ →
-- <https://www.npmjs.com/package/markdown markdown>
npm :: Text -> Text
npm q = "https://npmjs.com/package/" <> q

-- | __Javascript__ – <http://jamjs.org/packages/#/ Jam>
--
-- Link example:
-- @[pagedown]@ →
-- <http://jamjs.org/packages/#/details/pagedown pagedown>
jam :: Text -> Text
jam q = "http://jamjs.org/packages/#/details/" <> q

-- | __Ruby__ – <https://rubygems.org RubyGems.org>
--
-- Link example:
-- @[github-markdown]@ →
-- <https://rubygems.org/gems/github-markdown github-markdown>
rubygems :: Text -> Text
rubygems q = "https://rubygems.org/gems/" <> q

-- | __Python__ – <https://pypi.python.org/pypi PyPI>
--
-- Link example:
-- @[Markdown]@ →
-- <https://pypi.python.org/pypi/Markdown Markdown>
pypi :: Text -> Text
pypi q = "https://pypi.python.org/pypi/" <> q

-- | __Perl__ – <https://metacpan.org MetaCPAN> (by module)
--
-- Link example:
-- @[Text::Markdown]@ →
-- <https://metacpan.org/pod/Text::Markdown Text::Markdown>
metacpanPod :: Text -> Text
metacpanPod q = "https://metacpan.org/pod/" <> q

-- | __Perl__ – <https://metacpan.org MetaCPAN> (by release)
--
-- Link example:
-- @[Text-Markdown]@ →
-- <https://metacpan.org/release/Text-Markdown Text-Markdown>
metacpanRelease :: Text -> Text
metacpanRelease q = "https://metacpan.org/release/" <> q

-- | __Haskell__ – <https://hackage.haskell.org Hackage>
--
-- Link example:
-- @[cmark]@ →
-- <https://hackage.haskell.org/package/cmark cmark>
hackage :: Text -> Text
hackage q = "https://hackage.haskell.org/package/" <> q

-- | __Rust__ – <https://crates.io Cargo>
--
-- Link example:
-- @[hoedown]@ →
-- <https://crates.io/crates/hoedown hoedown>
cargo :: Text -> Text
cargo q = "https://crates.io/crates/" <> q

-- | __PHP__ – <http://pear.php.net PEAR>
--
-- Link example:
-- @[Text_Wiki_Doku]@ →
-- <http://pear.php.net/package/Text_Wiki_Doku Text_Wiki_Doku>
pear :: Text -> Text
pear q = "http://pear.php.net/package/" <> q

-- | __Dart__ – <https://pub.dartlang.org pub>
--
-- Link example:
-- @[md_proc]@ →
-- <https://pub.dartlang.org/packages/md_proc md_proc>
pub :: Text -> Text
pub q = "https://pub.dartlang.org/packages/" <> q

-- | __R__ – <http://cran.r-project.org/web/packages/ CRAN>
--
-- Link example:
-- @[markdown]@ →
-- <http://cran.r-project.org/web/packages/markdown markdown>
cran :: Text -> Text
cran q = "http://cran.r-project.org/web/packages/" <> q

-- | __Erlang__ – <https://hex.pm Hex>
--
-- Link example:
-- @[earmark]@ →
-- <https://hex.pm/packages/earmark earmark>
hex :: Text -> Text
hex q = "https://hex.pm/packages/" <> q

-- | __SWI-Prolog__ – <http://www.swi-prolog.org/pack/list packages>
--
-- Link example:
-- @[markdown]@ →
-- <http://www.swi-prolog.org/pack/list?p=markdown markdown>
swiprolog :: Text -> Text
swiprolog q = "http://www.swi-prolog.org/pack/list?p=" <> q

-- | __D__ – <http://code.dlang.org DUB>
--
-- Link example:
-- @[dmarkdown]@ →
-- <http://code.dlang.org/packages/dmarkdown dmarkdown>
dub :: Text -> Text
dub q = "http://code.dlang.org/packages/" <> q

-- | __Bash__ – <http://bpkg.io bpkg>
--
-- Link example:
-- @[markdown]@ →
-- <http://www.bpkg.io/pkg/markdown markdown>
bpkg :: Text -> Text
bpkg q = "http://bpkg.io/pkg/" <> q

-- | __Android__ – <https://play.google.com Google Play> (formerly Play Market)
--
-- Link example:
-- @[com.opera.mini.native]@ →
-- <https://play.google.com/store/apps/details?id=com.opera.mini.native Opera Mini>
googleplay :: Text -> Text
googleplay q = "https://play.google.com/store/apps/details?id=" <> q

-- | <http://braumeister.org Braumeister> (Homebrew formulas)
--
-- Link example:
-- @[multimarkdown]@ →
-- <http://braumeister.org/formula/multimarkdown multimarkdown>
braumeister :: Text -> Text
braumeister q = "http://braumeister.org/formula/" <> q

-- | <http://brewformulas.org Brewformulas> (Homebrew formulas)
--
-- Link example:
-- @[multimarkdown]@ →
-- <http://brewformulas.org/multimarkdown multimarkdown>
brewformulas :: Text -> Text
brewformulas q = "http://brewformulas.org/" <> q

-- | <https://chocolatey.org Chocolatey>
--
-- Link example:
-- @[Opera]@ →
-- <https://chocolatey.org/packages/Opera Opera>
chocolatey :: Text -> Text
chocolatey q = "https://chocolatey.org/packages/" <> q

-- | __Debian__ – <https://debian.org/distrib/packages packages (stable)>
debian :: Text -> Text
debian q = "https://packages.debian.org/stable/" <> q

-- | __Arch Linux__ – <https://aur.archlinux.org AUR> (“user repository”)
aur :: Text -> Text
aur q = "https://aur.archlinux.org/packages/" <> q

-- | __Gentoo__ – <https://packages.gentoo.org packages>
gentoo :: Text -> Text
gentoo q = "https://packages.gentoo.org/package/" <> q

-- | __openSUSE__ – <http://software.opensuse.org packages>
opensuse :: Text -> Text
opensuse q = "http://software.opensuse.org/package/" <> q

-- | __Linux Mint__ – <http://community.linuxmint.com/software/browse packages>
mint :: Text -> Text
mint q = "http://community.linuxmint.com/software/view/" <> q

-- | __Mageia__ – <http://mageia.madb.org packages>
mageia :: Text -> Text
mageia q = "http://mageia.madb.org/package/show/name/" <> q

-- | __Fedora__ – <https://admin.fedoraproject.org/pkgdb packages>
fedora :: Text -> Text
fedora q = "https://admin.fedoraproject.org/pkgdb/package/" <> q

-- | __Emacs__ – <https://marmalade-repo.org Marmalade>
marmalade :: Text -> Text
marmalade q = "https://marmalade-repo.org/packages/" <> q

-- | __Emacs__ – <http://melpa.org MELPA>
melpa :: Text -> Text
melpa q = "http://melpa.org/#/" <> q

-- | __Emacs__ – <https://elpa.gnu.org ELPA>
elpa :: Text -> Text
elpa q = "https://elpa.gnu.org/packages/" <> q

-- | __Sublime Text__ – <https://packagecontrol.io Package Control>
packagecontrol :: Text -> Text
packagecontrol q = "https://packagecontrol.io/packages/" <> q

-- | __Atom__ – <https://atom.io/packages packages>
--
-- Link example:
-- @[tidy-markdown]@ →
-- <https://atom.io/packages/tidy-markdown tidy-markdown>
atom :: Text -> Text
atom q = "https://atom.io/packages/" <> q

-- | __jEdit__ – <http://plugins.jedit.org packages>
--
-- Link example:
-- @[MarkdownPlugin]@ →
-- <http://plugins.jedit.org/plugins/?MarkdownPlugin MarkdownPlugin>
jedit :: Text -> Text
jedit q = "http://plugins.jedit.org/plugins/?" <> q

-- | __Opera__ – <https://addons.opera.com extensions>
--
-- Link example:
-- @[amazon-for-opera]@ →
-- <https://addons.opera.com/extensions/details/amazon-for-opera Amazon for Opera>
opera :: Text -> Text
opera q = "https://addons.opera.com/extensions/details/" <> q

-- | __Firefox__ – <https://addons.mozilla.org/firefox Add-ons> (extensions, themes)
--
-- Link example:
-- @[tree-style-tab]@ →
-- <https://addons.mozilla.org/firefox/addon/tree-style-tab Tree Style Tab>
firefox :: Text -> Text
firefox q = "https://addons.mozilla.org/firefox/addon/" <> q

-- | __Chrome__ – <https://chrome.google.com/webstore Chrome Web Store> (extensions, apps, themes)
--
-- Link example:
-- @[hdokiejnpimakedhajhdlcegeplioahd]@ →
-- <https://chrome.google.com/webstore/detail/hdokiejnpimakedhajhdlcegeplioahd LastPass>
chrome :: Text -> Text
chrome q = "https://chrome.google.com/webstore/detail/" <> q
