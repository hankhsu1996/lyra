# A Place In The Source Is The Front End's

## Date

2026-10-09

## Status

Accepted.

## Why this decision matters

A message about the source is read by someone who did not write the compiler and, with a design
taken from elsewhere, often did not write the text either. What they need from it is where the text
is and how it came to be there: which file included it, which macro wrote it.

The front end knows all of that. It read the files, it keeps their lines, and it holds, for every
piece of text a macro produced, where the macro was used and where its body was written. The layers
after it know none of it, and they still raise most of what a user of a new simulator sees: this
construct is not carried out yet.

So there is a choice of who shows a message that a later layer raised, and it decides what a place
is while those layers hold it.

## What was there

The compiler kept a second account of the source beside the front end's: file numbers of its own, a
copy of every file's text, line starts of its own, a span of file, begin and end, and a renderer. A
converter stood at the boundary. Everything that converter did was a question about text the front
end had already answered, asked again: where text inside a macro lands in a file, whether the two
ends of a range land together, what to do when they do not. One run printed the front end's warnings
in one form and the compiler's refusals in another, the second without the macro or the include that
explained it.

## What others do

- **clang, in the layers after its front end.** The code generator reports a construct it cannot
  compile with the front end's own location and range, through the front end's engine
  (`CodeGenModule::ErrorUnsupported`). It owns no table and converts nothing.
- **LLVM, which does not know clang.** It keeps a file name, a line and a column of its own. When it
  has something to report, clang turns that back into one of its own locations and reports it
  through its own engine (`BackendConsumer::getBestLocationFromDebugLoc`). One engine still shows
  every message; what is lost is the macro, since the position was resolved before it was kept.
- **rustc.** The middle form carries the front end's span on every statement.
- **CIRCT, whose SystemVerilog front end is the one this compiler uses.** It sends the front end's
  messages into its own engine and resolves a location to file, line and column at the boundary. Its
  own layers' messages carry no macro history, and text from a macro argument is reported at the
  macro's name.

In every one of them a run has one thing that shows messages. They differ in whether the layers
after the front end hold its value or a resolved one. LLVM holds a resolved one because it serves
many front ends. That condition is not ours: the first form past the front end here has one.

## Decisions

### D1. A place is held as the front end gave it

What a construct carries is the front end's own value for where it starts and where it ends, kept
whole and never opened by what carries it. Resolving a place is what loses its history, so it is not
resolved until it is shown. The value is two words of the compiler's own, so the forms that hold it
name no front end.

### D2. The front end shows every message

A message is handed to the front end to show, as the front end shows its own. Position, the line of
text, what is underlined, the macro that produced the text and the files that included it are the
front end's to work out. The compiler keeps no copy of a file, no line index and no drawing of its
own.

A message about no place is shown the same way. To the front end no place is a place like any other,
at which a message is shown without the lines that say where, so the compiler writes no line of its
own and has no second way to show a message.

### D3. A message opens with one of three words

`error:`, `warning:` and `note:`, which are the front end's and every other compiler's. What kind of
error one is belongs to the message: a construct not yet carried out says so in its own words, and a
failure of the compiler's own opens with `internal error:` after the first word. The kinds
themselves are unchanged where the compiler counts, decides an exit status and records what a path
refuses.

### D4. The one reader inside lowering asks

A program is told where a statement came from as a file name, a line and a column. That text is the
front end's answer too, asked through an interface the lowering can hold without knowing which front
end is behind it.

## Consequences

- A refusal of text a macro wrote names the macro, and one in an included file names what included
  it.
- A design's sources are in memory once.
- A place means something only in the run that read the source, so it is never written into anything
  kept across runs.
- The forms after the first hold no place, so what they refuse is still reported with none. That is
  a limit of those forms and not of this decision; the same value can be carried further when they
  need it.

## Rejected

- **Keeping the compiler's own account and making the converter correct.** It was built first. Each
  correction moved a question about text from one line of the converter to another and none of them
  removed the question, because the account being converted into could not hold a macro.
- **Showing every message through a printer of the compiler's own, fed by the front end.** It keeps
  labels of the compiler's own, and it keeps the compiler drawing source lines, which is the part
  that has to agree with the front end's and has no reason to exist twice.
- **A word of the compiler's own in front of a message, or after it.** `unsupported:` before the
  message is a word no other compiler opens with, and a class in brackets after it was tried and
  removed the same day. Nothing reads a message's kind off its text.
- **The tool's name in front of a message about no place.** clang and GCC open such a line with
  their name; rustc and the front end's own command do not. Writing it meant a line of the
  compiler's own for one case, with the words, the color and the test for which case it is that such
  a line needs, beside the path every other message takes.
