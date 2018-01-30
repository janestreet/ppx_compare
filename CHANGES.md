## v0.10

- Disallowed `[%equal]`; use `[%compare.equal]`

- Added `@compare.ignore` record-field attribute; `ppx_compare` and `ppx_hash`
  skip record fields annotated with `@compare.ignore`.

- Added support to `%compare` syntax for underscore (`_`) as meaning a
  comparison function that ignores both its arguments and returns zero.

## v0.9

## 113.43.00

- use the new context-free API

## 113.24.00

- Follow evolution of `Ppx_core` and `Type_conv`.
