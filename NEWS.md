# starlightr 0.2.1 

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
