# Display

Tracks `$display` / `$write` / `$strobe` format-specifier coverage and file-sink support.

The format-specifier sub-steps gate the `expect.variables` shape of `architecture-reset.md` R7,
since the test harness can only probe a variable whose type has an implemented specifier.

## Sub-Steps

- [ ] DI1 -- `%c` (char): low 7 bits of an integral arg as ASCII.
- [ ] DI2 -- `%t` (time): formatted against the active timescale. Requires a time variant in the
      runtime value surface and time-unit awareness on the format spec.
- [x] DI3 -- `%f` / `%e` / `%g` (real). Default precision 6 per LRM Table 21-2. Coverage tracked
      under `datatypes.md` Real C1.
- [ ] DI4 -- `%m` (hierarchical name). Requires runtime exposure of the current scope's hierarchical
      path; shares its mechanism with archive item `hierarchy/refs`.
- [ ] DI5 -- File sink (`$fdisplay`, `$fwrite`). Requires file-descriptor runtime state (`$fopen`
      returning an MCD / FD, `$fclose`) and per-descriptor output dispatch.
- [ ] DI6 -- `$strobe` postponed-region semantics. Strobe must defer its read-and-print to the
      postponed region of the same time slot (LRM 21.2).
- [ ] DI7 -- `%p` / `%0p` assignment-pattern format for aggregate types (LRM 21.2.1.6). Initial
      scope: unpacked array of integral elements, with output `'{<elem>, <elem>, ...}` and each
      element formatted by the LRM 21.2.1.6 element rule for its singular type. Multi-dimensional
      arrays nest naturally. Struct / union / enum / string / real element formats land with their
      respective type workstreams. Drives the test framework's whole-array `expect.variables`
      assertion path; see `unpacked.md` for the first consumer.

## Out of Scope

- Format-string parse diagnostics (trailing `%`, missing specifier, width overflow, unknown
  specifier) -- already implemented, not gaps.
- `$monitor`. Not modelled today; add an entry when a concrete consumer needs it.
- `%u` / `%z` (binary-packed unsigned / signed) and `%v` (strength). Not on the immediate roadmap;
  add entries when concrete consumers appear.
