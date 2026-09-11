# starlightr 0.2.2 

* README.md generated from `usethis` sets the logo img height attribute. Starlight does not work well with that creating full-width images. Now the height attribute in img tags are swapped to width.
* `\pkg` now emitted as bold text instead of dropped.
* `\preformatted` now lowered from Rd files.
* `\out` payload now emitted as escaped text instead of dropped.
* Cross-vignette `other.html` links now point at the sibling article page.
* `audit_config()` now checks references against documented topics, not just NAMESPACE exports, so datasets and package doc pages no longer report as warnings.

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
