# starlightr 0.2.2 

* README.md generated from `usethis` sets the logo img height attribute. Starlight does not work well with that creating full-width images. Now the height attribute in img tags are swapped to width.
* `\pkg` now emitted as bold text instead of dropped.
* `\source` now lowered from Rd files.

# starlightr 0.2.0

* `\link{}` targets to dotted internal functions (e.g. `.bsa_dubois`) now
  resolve to their `dot-` prefixed reference pages.
* `reference.include_internal = true` now renders internal topics even when
  they are not listed in `sidebar.reference`, so links to them resolve
  without adding sidebar entries.
* Wide display equations (`\deqn{}`) now scroll horizontally instead of
  overflowing the page.

# starlightr 0.1.0

* Initial release.
