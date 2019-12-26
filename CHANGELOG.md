# Changelog

`shortcut-links` uses [PVP Versioning][1].
The changelog is available [on GitHub][2].

## 0.5.0.0 - Dec 26, 2019

* Support GHC-8.8.1.
* Update maintenance info.

## 0.4.2.1

* Fixed links to GHC extensions.

## 0.4.2.0

* Added `useShortcutFrom`.
* `@` is now allowed in Github/Gitlab/Bitbucket nicks.

## 0.4.1.0

* Exported `useShortcut` (gee).

## 0.4.0.0

* Exported `Shortcut`.
* Renamed “braumeister” to “brew” (it still uses Braumeister, but this can change in the future).

## 0.3.0.0

* Added shortcut names to their descriptions.

## 0.2.0.0

* Descriptions of shortcuts look much better now.
* “sublime” was renamed to “sublimepc”, since it's possible that some other package manager for Sublime Text would emerge in the future.
* Shortcut “tvt” for TV Tropes was removed – it's just “tvtropes” now.
* “facebook” can link to profiles by their ID.
* “tvtropes” properly capitalises names of categories (if given as an option) – previously you needed to write e.g. “SugarWiki”, now “sugar wiki” works too.

## 0.1.0.0

First release.

[1]: https://pvp.haskell.org
[2]: https://github.com/kowainik/shortcut-links/releases
