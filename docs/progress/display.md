# Display

Tracks `$display` / `$write` / `$strobe` format-specifier coverage and file-sink support against the
runtime print pipeline (`src/lyra/runtime/integral_format.cpp`,
`src/lyra/lowering/hir_to_mir/lower_print.cpp`). Each item is a single PR-sized step; the file is
deleted when all sub-steps land.

The format-specifier sub-steps gate the `expect.variables` shape of the R7 test-framework work
(`architecture-reset.md`), since the test harness can only probe a variable whose type has an
implemented specifier.

## Sub-Steps

- [ ] DI1 -- `%c` (char). Decode the low 7 bits of an integral arg as ASCII. `lower_print.cpp`
      currently returns `kFormatSpecifierNotImplemented` for `kChar`.
- [ ] DI2 -- `%t` (time). Format a time value against the active timescale. `lower_print.cpp`
      currently returns `kFormatSpecifierNotImplemented` for `kTime`. Requires a time variant on
      `RuntimeValueView` and time-unit awareness on the format spec.
- [x] DI3 -- `%f` / `%e` / `%g` (real). Shipped via real-types C1 in PR #789. `FormatKind` split
      into `kRealDecimal` / `kRealExponential` / `kRealGeneral`; `RuntimeValueView` gained
      `Real64ValueView` and `Real32ValueView`; `value::FormatValue` dispatches each kind through
      `std::format` (`{:.{}f}`, `{:.{}e}`, `{:.{}g}`) with default precision 6 per LRM Table 21-2.
      Coverage tracked under `datatypes.md` "Real" -> C1.
- [ ] DI4 -- `%m` (hierarchical name). Requires runtime exposure of the current scope's hierarchical
      path. `lower_print.cpp` currently returns `kFormatModulePathNotImplemented`. Shares its
      underlying mechanism with the archive item `hierarchy/refs`, which the `expect.variables`
      Phase 2 also depends on for hierarchical probe paths.
- [ ] DI5 -- File sink (`$fdisplay`, `$fwrite`). `lower_print.cpp` currently returns
      `kFileDisplayNotImplemented` for any non-stdout sink. Requires file-descriptor runtime state
      (`$fopen` returning an MCD/FD, `$fclose`), per-descriptor output dispatch, and threading the
      descriptor argument through `RuntimePrintCall`.
- [ ] DI6 -- `$strobe` postponed-region semantics. `support::SystemSubroutineDesc` already carries
      `is_strobe`, but `lower_print.cpp` lowers strobe identically to `$display`. Strobe must defer
      its read-and-print to the postponed region of the same time slot.

## Out of Scope

- Format-string parse diagnostics (`kFormatStringTrailingPercent`, `kFormatStringMissingSpecifier`,
  `kFormatStringWidthOverflow`, `kFormatStringUnknownSpecifier`) -- already implemented, not gaps.
- `$monitor`. Not modelled today; add an entry when a concrete consumer needs it.
- `%u` / `%z` (binary-packed unsigned/signed) and `%v` (strength). Not on the immediate roadmap; add
  entries when concrete consumers appear.
