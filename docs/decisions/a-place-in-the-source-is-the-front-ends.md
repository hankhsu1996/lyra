# A place in the source is the front end's

Status: Accepted, 2026-10-09.

## Requirement

A message about the source tells its reader where the text is and how it came to be there -- which
file included it, which macro wrote it -- whichever layer raised the message. The reader often wrote
neither the compiler nor the design. The front end knows all of that; the layers after it know none
of it, and they raise most of what a user meets, a construct not carried out yet. Lyra had kept a
second account of the source beside the front end's, with its own file numbers, line starts, spans
and printer, so one run printed the front end's warnings in one form and Lyra's refusals in another,
without the macro or the include behind them.

## Choice

- **A place is held as the front end gave it.** What a construct carries is the front end's own
  value for where it starts and ends, kept whole and opened by nothing that carries it, and resolved
  only to be shown, since resolving it is what loses its history. The forms that hold it name no
  front end. A place a running program reports is the one exception, resolved where the program's
  constant is made, since a run has no front end to ask.
- **The front end shows every message.** A message of Lyra's is handed to the front end to show, as
  the front end shows its own. Position, the source line, the underline, the macro and the including
  files are the front end's to work out, and Lyra keeps no copy of a file, no line index and no
  printer. A message about no place takes the same path.
- **A message opens with `error:`, `warning:` or `note:`, and says its own kind in its words.** A
  construct not carried out says so in its text, and a failure of Lyra's own opens with
  `internal error:` after the first word. The same holds of a report a running design makes, except
  the line a design's own severity task prints, which keeps that task's severity (LRM 20.10).

This is clang's shape. Its code generator reports what it cannot compile at the front end's own
location through the front end's engine (`CodeGenModule::ErrorUnsupported`), and rustc's middle form
carries the front end's span on every statement. LLVM resolves a place early because it serves many
front ends; Lyra's first form past the front end has one.

## Alternatives turned down

- **Keeping Lyra's own account of the source, with a correct converter.** It fails naming the macro
  and the include. The failure rests on the converted-to account having no way to hold a macro, so
  every correction moved a question about text from one line of the converter to another.
- **A printer of Lyra's own, fed by the front end.** It fails one form for every line a run prints.
  The failure rests on drawing source lines being the part that must agree with the front end's.
- **A word or label of Lyra's own before or after a message** (`unsupported:`, a bracketed class,
  the tool's name before a message about no place). It fails one form for every line, and nothing
  reads a message's kind off its text.

## Consequences across layers

- Every layer after the front end holds the place of the source it was lowered from -- each MIR
  expression and statement, each LIR instruction and terminator -- so what any layer refuses or
  fails on is reported where the source wrote it ([`mir.md`](../architecture/mir.md),
  [`lir.md`](../architecture/lir.md)).
- A place means something only in the run that read the source, so it is written into nothing kept
  across runs.
- How a running design is handed the place of each operation that can fail, and how the library
  carries it, is [`mir.md`](../architecture/mir.md)'s and
  [`runtime_distribution.md`](../architecture/runtime_distribution.md)'s. The form of a message's
  text is [`messages.md`](../messages.md)'s.
