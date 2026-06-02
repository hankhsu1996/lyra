# Display

Tracks `$display` / `$write` / `$strobe` format-specifier coverage and file-sink support.

The format-specifier sub-steps gate the `expect.variables` shape of `architecture-reset.md` R7,
since the test harness can only probe a variable whose type has an implemented specifier.

## Sub-Steps

- [x] DI1 -- `%c` (char): low byte of an integral argument as ASCII (LRM 21.2.1.1 example). The low
      byte's X/Z poison collapses to a single character following simulator convention -- any X bit
      yields `x`, otherwise any Z bit yields `z`. Width / precision modifiers are rejected by the
      slang frontend, so the lowered form is always a single character.
- [ ] DI2 -- `%t` (time): formatted against the active timescale. Requires a time variant in the
      runtime value surface and time-unit awareness on the format spec.
- [x] DI3 -- `%f` / `%e` / `%g` (real). Default precision 6 per LRM Table 21-2. Coverage tracked
      under `datatypes.md` Real C1.
- [ ] DI4 -- `%m` (hierarchical name). Requires runtime exposure of the current scope's hierarchical
      path; shares its mechanism with archive item `hierarchy/refs`.
- [x] DI5 -- File sink. Twelve `$display` / `$write` / `$fdisplay` / `$fwrite` variants
      (default-decimal plus `b` / `h` / `o` radix variants per LRM 21.2.1.1); descriptors per LRM
      21.3.1 (MCD with bit 0 = stdout, FD with bit 31 set, OR-able MCDs that fan output across
      sinks, channel reuse on close); `$fopen` / `$fclose` with the full mode-string family (`r` /
      `rb` / `w` / `wb` / `a` / `ab` plus their `+` update variants). Read-mode `$fopen` opens a
      real `FILE*` so the FD shape is genuine even though the read tasks themselves are out of
      scope. The test framework gained `expect.files` with strict matching (any undeclared file in
      the per-case sandbox fails the case) and per-case sandbox cwd so `$fopen("foo.txt")` writes
      inside the sandbox.
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
- `$monitor` / `$fmonitor`. Not modelled today; add an entry when a concrete consumer needs it.
- `$strobe` / `$fstrobe` radix variants. The radix-dispatch mechanism is shared with DI5 but
  strobe's "drain at end of time slot" semantic waits on DI6 (postponed region).
- `$fscanf` / `$sscanf`. Tracked separately; the format-string scanner that handles SV's 4-state
  input vocabulary (`x` / `z` / `?` / `_` in numeric fields), sized literals, and the variadic
  output-arg pipeline is a self-contained cut on top of the file-read surface. Other file read tasks
  (`$fgetc` / `$ungetc` / `$fgets` / `$fread` / `$fseek` / `$rewind` / `$ftell` / `$feof` /
  `$ferror` / `$fflush`) are implemented per LRM 21.3.4..21.3.8. The output-arg tasks ride the same
  LRM 13.5 copy-out shape used by user-defined functions with `output` formals; only
  statement-position calls are supported. When `$fscanf` lands, fold its variadic copy-out
  desugaring with the existing UDF F4 path and the file IO output-arg path into one shared helper --
  three call sites with stable shape will be the right time to extract the abstraction.
- `$sformat` / `$sformatf` / `$swrite` family (formatting into a string variable). Independent
  feature.
- `%u` / `%z` (binary-packed unsigned / signed) and `%v` (strength). Not on the immediate roadmap;
  add entries when concrete consumers appear.
