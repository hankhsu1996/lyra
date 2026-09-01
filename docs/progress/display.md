# Display

Tracks `$display` / `$write` / `$strobe` format-specifier coverage and file-sink support.

## Sub-Steps

- [x] DI1 -- `%c` (char): low byte of an integral argument as ASCII (LRM 21.2.1.1 example). The low
      byte's X/Z poison collapses to a single character following simulator convention -- any X bit
      yields `x`, otherwise any Z bit yields `z`. Width / precision modifiers are rejected by the
      slang frontend, so the lowered form is always a single character.
- [x] DI2 -- `%t` (time): formatted against the active timescale. The display unit, precision,
      suffix and field width come from `$timeformat` (LRM 20.4.3), applied design-wide.
- [x] DI3 -- `%f` / `%e` / `%g` (real). Default precision 6 per LRM Table 21-2. Coverage tracked
      under `datatypes.md` Real C1.
- [x] DI4 -- `%m` (hierarchical name, LRM 21.2.1.5). The reported name is the scope the call was
      written in: a named block, a task or function, or the enclosing design element. It is one
      ordinary SV string, so the format pipeline carries no `%m`-specific arm and a format string
      parsed at run time reports the same name a literal one does. The part of the name fixed by
      object-tree ownership comes from the run-time tree (closes `hierarchy.md` D5) and the part
      below it -- the named scopes that need no run-time object of their own -- is fixed when the
      body is compiled. Width modifiers on `%m` are rejected at the slang frontend.
- [x] DI5 -- File sink. Twelve `$display` / `$write` / `$fdisplay` / `$fwrite` variants
      (default-decimal plus `b` / `h` / `o` radix variants per LRM 21.2.1.1); descriptors per LRM
      21.3.1 (MCD with bit 0 = stdout, FD with bit 31 set, OR-able MCDs that fan output across
      sinks, channel reuse on close); `$fopen` / `$fclose` with the full mode-string family (`r` /
      `rb` / `w` / `wb` / `a` / `ab` plus their `+` update variants). Read-mode `$fopen` opens a
      real `FILE*` so the FD shape is genuine even though the read tasks themselves are out of
      scope.
- [x] DI6 -- `$strobe` / `$strobeb` / `$strobeh` / `$strobeo` (LRM 21.2.2). Lowers to a closure
      submitted via the postponed-region builtin; the closure body builds and writes the same print
      items the `$display` family produces. Lambda init-capture snapshots procedural locals by value
      (safe even when the issuing `initial` frame has already returned), and module-signal operands
      resolve through the closure's `self` capture and are read at fire time -- so they see
      NBA-committed values, which is the LRM 21.2.2 semantic. The postponed queue uses the same
      swap-and-drain shape NBA uses; re-entrant submission during the drain is rejected.
- [x] DI7 -- `%p` / `%0p` assignment-pattern format for aggregate types (LRM 21.2.1.6). Scope: fixed
      unpacked and dynamic array of integral elements, including mixed-container nesting
      (`int     arr[3][]`, `int arr[][3]`, `int arr[][]`). Output is `'{<elem>, <elem>, ...}` with
      `, ` between elements; empty containers print `'{}`; multi-dimensional and mixed-container
      forms nest naturally. Singular integral elements follow the LRM "as it would unformatted" rule
      (default `$display` radix, i.e. decimal); singular string elements print quoted. `%0p`
      produces identical text in this scope; LRM 21.2.1.6 allows it. Struct / union / enum /
      string-typed / real element formats land with their respective type workstreams.
- [x] DI8 -- `$sscanf` and `$fscanf` over a shared scanner core (LRM 21.3.4.3). Statement-position
      call (bare or blocking assign-RHS); conversions `%d` / `%h` / `%x` / `%b` / `%o` / `%s` / `%c`
      / `%%`; 4-state vocabulary (`x` / `z` / `?` / `_`) inside the integer conversions; single-char
      `x`/`z`/`?` fill for `%d`. An output the parse never reached keeps its prior value, since LRM
      21.3.4.3 writes only successfully matched outputs. `$sscanf` takes a string or string-literal
      input; `$fscanf` takes an int FD, honours "offending input character is left unread" via the
      underlying FD's putback buffer, and stamps `$ferror` on invalid / closed descriptors.
- [x] DI9 -- `$sformat` / `$sformatf` / `$swrite` family (LRM 21.3.3). Six subroutines: `$swrite` /
      `$swriteb` / `$swriteh` / `$swriteo` with auto-format (per-task default radix per LRM
      21.2.1.1), `$sformat` with explicit literal format, and `$sformatf` returning the formatted
      string as a function result. Reuses the print engine's literal-format walker and the
      `value::Format` runtime so the conversion-spec set is identical to `$display` / `$write` (`%d`
      / `%h` / `%x` / `%b` / `%o` / `%s` / `%c` / `%%` / `%p` / `%0p` / `%f` / `%e` / `%g` plus
      width / precision / zero-pad / left-align modifiers). No newline appended (LRM 21.3.3 is a
      string-producer; newline policy belongs to the display / write family).

## Scan family follow-ups

Tracks remaining LRM 21.3.4.3 corners explicitly rejected by the scan family. Each item is a
user-observable feature gap (a lowering-time unsupported diagnostic or runtime rejection) that
should close as the corresponding behaviour lands.

- [x] Expression-position `$sscanf` / `$fscanf` (if-condition, blocking assign-RHS, case selector,
      arithmetic operand). The scan family is modelled per LRM 21.3.4.3 as a system function whose
      `integer` return is observable in any expression position; writes to output args are runtime
      side effects routed through `Var::Set` for observable structural lvalues.
- [x] Complex-lvalue scan output args (bit-select `a[3:0]`, element index `arr[i]`, struct field
      `s.f`, multi-dim packed element `m[i]`, etc., per LRM 21.3.4.3). Any lvalue an ordinary
      assignment can write is a scan output, because the parse lands in a temporary and reaches the
      lvalue by ordinary assignment. Output lvalues that are not reducible to writable expressions
      (e.g., the result of a non-lvalue function call) remain rejected.
- [x] Field width (`%5d`) and assignment suppression (`%*d`) for both scan functions (LRM 21.3.4.3
      Table 21-7). The runtime format parser handles the optional `*` and decimal width modifiers;
      suppressed conversions advance the input but do not bump the matched count or consume an
      output slot (LRM "success of suppressed assignments is not directly determinable"); the C
      scanf convention applies for `%d`'s sign (not counted toward the width). `%c` retains a
      one-byte read; the scanf "`%5c` reads 5 bytes" extension needs a string-slot output and is
      tracked separately.
- [x] `$sscanf` input source of unpacked-array-of-byte type (LRM 21.3.4.3). The HIR-to-MIR boundary
      inserts an implicit conversion to string for unpacked arrays whose element is an 8-bit
      integral; the backend emits a `value::String::FromByteArray` call that linearises the
      element-order byte stream (embedded NULs preserved) into the scanner's input. Any source type
      outside LRM 21.3.4.3's three permitted shapes (string, integral, unpacked array of byte) is an
      upstream-validation invariant; the lowering throws `InternalError` because slang's type-check
      is expected to reject it.
- [x] NUL-as-whitespace (LRM 21.3.4.3(a), which grants the rule to `$sscanf` alone). A null
      character separates input fields under `$sscanf` and is ordinary input under `$fscanf`. The
      white-space set is fixed by which system function the source names, not by the bytes that
      reach the parser, so the same input scans differently under the two.
- [x] `$sscanf` / `$fscanf` integral str / format and the LRM 21.3.4.3 x/z -> EOF (-1) corner.
      Packed bit vectors lift to string via a shared `value::String::FromPackedArray` conversion
      (LRM 5.9 MSB-first byte order), so any integral expression in str or format position now
      reaches the scanner unchanged. A source or format operand carrying x or z makes the call
      report EOF without scanning; an operand whose type has no unknown state is known by its type,
      so the check costs nothing where there is no x/z to find. The rule names `$sscanf` literally
      but the format argument's role is identical under `$fscanf`, so the guard fires on `$fscanf`'s
      format as well; `$fscanf`'s file descriptor has no string semantics and is exempt. The same
      conversion path unblocks `$display("%s", x)` on a packed integral operand, which previously
      built but threw at runtime; x/z bits in that path render as `'\0'` since LRM does not pin `%s`
      behaviour for 4-state operands.
- [x] `$fseek` / `$rewind` cancelling pending `$ungetc` operations (LRM 21.3.5). The Lyra-owned
      per-FD putback slot is cleared whenever the file position is repositioned, so any subsequent
      read consults the underlying stream rather than the stale pushback byte.

## String-format family follow-ups

Tracks the LRM 21.3.3 corners of `$sformat` / `$sformatf` / `$swrite*` beyond the literal-format,
string-output core.

- [x] Runtime-evaluated format string for `$sformat` / `$sformatf` (LRM 21.3.3 NOTE: format string
      may be a non-constant expression). A format string carried as a value reaches the same
      conversion set a literal one does -- every specifier and modifier, including `%m` and `%t`,
      which resolve against the calling scope. A count mismatch does not stop the run: a directive
      with no operand left contributes nothing and a surplus operand is ignored.
- [x] `$sformat` / `$swrite*` output_var of integral or unpacked-array-of-byte type (LRM 21.3.3 +
      LRM 5.9 assignment rules). An integral destination takes the text right-justified -- padding
      its leftmost bits with zeros when wider, truncating the leftmost characters when narrower --
      and an unpacked byte array takes it left-justified from its left bound, an element past the
      end of the text keeping the element default.
- [x] `$sformat` / `$sformatf` format_string of integral or unpacked-array-of-byte type (LRM
      21.3.3). The text carried as bytes reaches the parse through the same bits-to-text conversion
      any other such operand takes.

Not yet closed: LRM 21.3.3 asks for a warning when the operand count does not match the format
string's directives. The compile-time-parsed path reports the mismatch as an error, which LRM 21.3.3
sanctions; the runtime-parsed path continues silently.

## Strobe family follow-ups

- [x] `$fstrobe` / `$fstrobeb` / `$fstrobeh` / `$fstrobeo` (LRM 21.2.2 + 21.3.2, file sink). Four
      descriptor rows with `sink_kind = kFile`, `is_strobe = true`, and `min_args = 1` were enough
      to light up the whole family end-to-end: the HIR -> MIR lowering, the C++ backend, and the
      runtime were already orthogonal across the `sink_kind` and `is_strobe` axes after DI5 and DI6
      shipped. First argument is an MCD or FD per LRM 21.3.1; the rest is the print payload at the
      per-task default radix. Postponed lambdas reference the descriptor through their capture list,
      so module-scope FDs read NBA-committed values at fire time and procedural- local FDs snapshot
      at submit time.
- [x] LRM 21.3.2 implicit cancel on `$fclose`. Cancellation is modelled as a resource event: every
      FD and every MCD channel owns a cancel signal that `$fclose` fires (and replaces with a fresh
      one) on every affected channel, so the next `$fopen` reusing the channel starts with a clean
      signal while any observer captured before the close keeps seeing the original cancelled state
      -- channel reuse cannot revive a dead submission. The strobe runtime acquires the cancel
      observer at submit time and short-circuits the wrapped print if any participating channel
      reports cancelled; an OR-ed MCD resolves as cancelled when any participating channel does,
      matching the literal LRM wording "operations on a ... multichannel descriptor are implicitly
      cancelled". Cancel state lives on the channel rather than per submission, so memory stays
      bounded for free. The same per-channel signal will carry future `$fmonitor` cancellation when
      that lands.

## File read family follow-ups

- [ ] Expression-position use of the output-argument reads `$fgets` / `$fread` / `$ferror` (LRM
      21.3.4). Each returns a value that LRM permits in any expression position
      (`if ($fgets(s, fd))`, a blocking-assign condition), but Lyra supports them only in statement
      position (a bare call or the right-hand side of a blocking assignment), where the LRM 13.5
      output-argument copy-out has a statement boundary to desugar into; a nested-expression call is
      rejected with a diagnostic. Closing this needs the copy-out to run inside an
      immediately-invoked closure, the same shape the scan family already uses for its
      expression-position support. `$readmemh` / `$readmemb` do not share this gap: they are void
      tasks, so they never appear in expression position.

## Conformance gaps the corpus records

Behaviour the corpus asks for and does not get. Each is a case that runs and keeps every check it
makes, recorded against the path it answers wrongly on; the day the answer becomes right the case
passes and that record fails until its entry goes. What is written here is what the standard
requires.

- [ ] **A conversion with no field width does not pad to the operand's size** (LRM 21.2.1.2). The
      clause's own worked example fixes `%d` of a 32-bit value in ten right-justified columns and
      `%3h` of `32'h5` as `005`; Lyra prints `10` and `  5`. Two independent readings of the clause
      reached the same conclusion. Decimal pads with spaces and the other radices with zeros, so the
      two halves are separate requirements that happen to fail together.
- [ ] **Format flags C provides but the standard does not.** `%-4h` (left-justify) and `%05d`
      (zero-pad a decimal) are accepted and behave as C's `printf` does. 21.2.1.2 admits only a
      non-negative field width, and fixes decimal padding as leading spaces, so `%05d` of -5 is
      required to give four leading spaces rather than `-0005`. Whether to reject these or define
      them is open; today they silently follow C.
- [ ] **`%p` of an enumeration prints the integer** (LRM 21.2.1.6), where the clause requires the
      enumeration name.

## Out of Scope

- Format-string parse diagnostics (trailing `%`, missing specifier, width overflow, unknown
  specifier) -- already implemented, not gaps.
- `$monitor` / `$fmonitor`. Not modelled today; add an entry when a concrete consumer needs it.
- File read / positioning tasks (`$fgetc` / `$ungetc` / `$fseek` / `$rewind` / `$ftell` / `$feof` /
  `$fflush`) are implemented per LRM 21.3.4..21.3.8. The output-argument reads `$fgets` / `$fread` /
  `$ferror` are implemented in statement position; their expression-position gap is tracked above.
- `%u` / `%z` (binary-packed unsigned / signed) and `%v` (strength). Not on the immediate roadmap;
  add entries when concrete consumers appear.
