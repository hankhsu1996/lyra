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
- [x] DI7 -- `%p` / `%0p` assignment-pattern format for aggregate types (LRM 21.2.1.6). Scope: fixed
      unpacked array of integral elements. Output is `'{<elem>, <elem>, ...}` with `, ` between
      elements; multi-dimensional arrays nest naturally. Singular integral elements follow the LRM
      "as it would unformatted" rule (default `$display` radix, i.e. decimal); singular string
      elements print quoted. `%0p` produces identical text in this scope; LRM 21.2.1.6 allows it.
      Struct / union / enum / string-typed / real element formats land with their respective type
      workstreams. Drives the test framework's whole-array `expect.variables` assertion path:
      sequence-valued YAML entries (`a: [10, 20, 30]`) lower to a recursive `UnpackedArrayValueView`
      and round-trip through the same `FormatValue` the runtime uses.
- [x] DI8 -- `$sscanf` and `$fscanf` over a shared scanner core (LRM 21.3.4.3). Statement-position
      call (bare or blocking assign-RHS); conversions `%d` / `%h` / `%x` / `%b` / `%o` / `%s` / `%c`
      / `%%`; 4-state vocabulary (`x` / `z` / `?` / `_`) inside the integer conversions; single-char
      `x`/`z`/`?` fill for `%d`. Output-arg copy-out uses LRM 13.5 with copy-in initialization so
      unmatched slots preserve the actual's prior value (LRM 21.3.4.3 only writes successfully
      matched outputs). `$sscanf` takes a string or string-literal input; `$fscanf` takes an int FD,
      honours "offending input character is left unread" via the underlying FD's putback buffer, and
      stamps `$ferror` on invalid / closed descriptors.

## Scan family follow-ups

Tracks remaining LRM 21.3.4.3 corners explicitly rejected by the scan family. Each item is a
user-observable feature gap (lowering-time `diag::Unsupported` or runtime rejection) that should
close as the corresponding behaviour lands.

- [ ] Expression-position `$sscanf` / `$fscanf` (e.g. `if ($sscanf(...))`). Rejected at lowering;
      only statement position (bare call or blocking assign-RHS) supports the LRM 13.5 copy-out
      desugaring today.
- [ ] Field width (`%5d`) and assignment suppression (`%*d`) for both scan functions (LRM 21.3.4.3
      Table 21-7). Rejected at runtime so the matched count never silently desyncs from the user's
      argument list.
- [ ] `$sscanf` input source of unpacked-array-of-byte type (LRM 21.3.4.3). String and integral
      inputs work; the unpacked aggregate variant is rejected at lowering.
- [ ] `$sscanf` NUL-as-whitespace (LRM 21.3.4.3 sscanf-only). NUL is currently treated as a literal
      byte rather than a separator.
- [ ] `$sscanf` "format string or str argument contains x/z bits implies EOF (-1)" (LRM 21.3.4.3
      sscanf-only). Slang binds str / format expressions to value bytes on this pipeline so the
      corner cannot be observed in practice yet; revisit once 4-state string surfaces exist.
- [ ] `$fseek` / `$rewind` cancelling pending `$ungetc` operations (LRM 21.3.5). Independent
      file-positioning gap; not triggered by the scan family but shares the FD surface.

## Out of Scope

- Format-string parse diagnostics (trailing `%`, missing specifier, width overflow, unknown
  specifier) -- already implemented, not gaps.
- `$monitor` / `$fmonitor`. Not modelled today; add an entry when a concrete consumer needs it.
- `$strobe` / `$fstrobe` radix variants. The radix-dispatch mechanism is shared with DI5 but
  strobe's "drain at end of time slot" semantic waits on DI6 (postponed region).
- Other file read tasks (`$fgetc` / `$ungetc` / `$fgets` / `$fread` / `$fseek` / `$rewind` /
  `$ftell` / `$feof` / `$ferror` / `$fflush`) are implemented per LRM 21.3.4..21.3.8.
- `$sformat` / `$sformatf` / `$swrite` family (formatting into a string variable). Independent
  feature.
- `%u` / `%z` (binary-packed unsigned / signed) and `%v` (strength). Not on the immediate roadmap;
  add entries when concrete consumers appear.
