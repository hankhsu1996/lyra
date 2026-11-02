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

A program that reports where it failed while it runs is handed that place by its compiler:

- **clang.** `__builtin_source_location()` answers the address of a constant its AST context keeps
  once per place (`ASTContext::getUnnamedGlobalConstantDecl`). A sanitizer's check is generated
  inline, and the handler it calls is handed a record of its own data holding the place, so the
  report is made where the place arrives.
- **rustc.** A function marked `#[track_caller]` is handed its caller's `Location` as a hidden
  argument and hands it on; any other call hands a constant of its own place. Through a function
  pointer the mark stops, and the report names the library's own line, which is Rust its user can
  read.
- **Verilator.** Its run-time checks are generated inline and call `vl_fatal` with the file and the
  line as arguments.

Each hands the place to the report as an argument, and none keeps a current place to read. Where the
code between the entry and the report is a library's rather than a generated check, others do keep
one:

- **folly's `RequestContext`.** A guard at the entry sets a thread-local and restores what was there
  when it ends, what runs inside reads it and nothing passes it, and work put off keeps the context
  and restores it around itself (`RequestContextScopeGuard`, `EventBase::runInLoop`).
- **tokio's runtime context.** The same guard over the current handle, and a task carries its own.

## Decisions

### D1. A place is held as the front end gave it

What a construct carries is the front end's own value for where it starts and where it ends, kept
whole and never opened by what carries it. Resolving a place is what loses its history, so it is not
resolved until it is shown. The value is two words of the compiler's own, so the forms that hold it
name no front end. What a program states for a report it makes while it runs is the other case: a
run has no front end to ask, so that place is resolved where the constant is made (D4).

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

The same holds while a design runs: a report the tool makes of its own opens with one of the three
words, placed at the statement that failed, with the instance and the time in a note after it. Only
the line a design's own severity task prints keeps the severity that task names (LRM 20.10), since
that line is the design's. The form of the text after the word is `docs/messages.md`'s.

### D4. A program is handed where each operation that can fail was written

A report a running design makes names the statement that failed (LRM 20.10 asks this of a severity
task, and a run-time error of the design is reported the same way), however far from that statement
the failure is found, and nothing between the two has to know about it. So a failure's place is
stated where the design wrote the operation, bound at the library's door for the call's extent, and
kept as a value by whatever runs later; nothing between the door and the report carries it.

Each operation that can fail the design hands the library where it was written, as its last operand:
a value of the library's location type, one word, the address of a constant of the program's own
holding a file name, a line and a column, one per place an operation starts at, as
`std::source_location` holds what clang's `__builtin_source_location` answers. MIR states it, as a
rewrite of the stage, for every call of a door, so no site composing such a call knows which entries
are doors; opening a class handle is one of those calls, since a handle naming no object fails the
design there (LRM 8.4). Every entry of the library is a door unless its declaration says it takes
none, so an entry nobody thought about takes a place, and one that reads no bound place states so.

The constant is a record of the library's, and the unit holds it as it holds any constant: an entry
of a pool, interned by what it says, a use naming the entry. The file, the line and the column are
asked of the front end while MIR is built, through an interface that does not say which front end
that is, so what the entry holds is the constant's content and no backend holds the front end. The
value a call hands is the record's address read as the library's location type, which MIR states
with the ordinary reference, address-of and cast, and which each target translates as it translates
those: a backend writes the record and names it, and what a location means is the library's.

The place is the last input of a door and of every body that reports at its caller, as rustc's
`fn_abi` chains the caller location after a function's inputs with the return apart from them. What
a target adds after every operand follows it: what a library entry is told of a type, and the
storage of an answer.

A door is the function program code calls, an entry of the C ABI or a method the C++ backend names.
It opens one scope with the place, which binds it for the call's extent and restores what was bound
before, and past it the library takes no place at all: a function of the library's interior, a
type's table among them, knows nothing of where it was called from, and a raise or a report reads
what is bound. A door over a door hands the place on, so each path opens one scope. No door's extent
spans a suspension, because a door is a plain function, so a binding is never in force across one.

What the library keeps past the door keeps the place as a value, as it keeps its operands, and binds
it when it runs. A call of `randomize()` is several entries, so the problem it fills keeps the place
and what goes on to fail binds it. A dump the library opens at the end of a time step keeps the
place of the task that asked in its own capture. A `context` import keeps its call's place in the
chain of calls, since an imported task can suspend inside it, and what foreign code does wrong under
that call is reported there; a plain import states no extent to keep it in (LRM 35.5.3), so what its
foreign code does wrong is the run's own. The scheduler keeps no place: a deferred update is a
closure of the program, whose own calls hand theirs. A raise keeps the place it read, since it is
reported after the door is left.

The trade is where a missing place is found. Under the parameter it was refused by the build; here a
path to a report under no binding is found when it runs, as a failure of the tool's own and never as
a report at the wrong statement. That is the price of the library's interior carrying nothing, since
a precompiled library's interior is not visible to the verifier. What the verifier still holds is
everything on the program's side: every call of a door has its place, every body that reports at its
caller has its parameter, and no call states an empty place. A report the run makes for no operation
of the design, such as a time slot that never settles, says so where it is made, and reads nothing.

A generated operation that can report is a door of its own, reached two ways. Program code hands it
the place, and a type's table, whose functions take none, hands it no place, which is the location
type's default value: a door handed no place binds nothing, so the place bound where the table was
reached stands. In LLVM that is the function a table holds; in the C++ path it is an element's
policy calling the type's method. The library's own code handing an operation on from inside a door
hands no place either, for the same reason. These are the one thing a door can be handed that is no
place, and a report under no binding at all is still a failure.

Whether an operation takes the place is the operation's and not the kind of value it acts on, as
rustc's `should_inherit_track_caller` gives every impl the mark on the trait method, so a kind that
cannot fail takes it and binds it as any door does. The one operation that is two entries is a write
to an element, which every array makes on every such statement and only a queue's can report: over a
queue it is an entry that takes the place, and over the others one that takes none, as an operation
over integral values is an entry of its own. A body no caller hands a place states its own, and the
verifier refuses one that states none.

A body of the tool's own text -- the library classes of LRM 15, the coverage library -- reports at
the design's call that reached it, since its own lines mean nothing to whoever wrote the call. Such
a body is handed where it was called as a parameter of its own, last, and every call it makes hands
that on, as does a closure it builds, exactly as a Rust function marked `#[track_caller]` is handed
its caller's location and forwards it. A body generated for a type's operation that can report is
one of these, and so is the body written for a subroutine an interface leaves to a connected module
(LRM 25.7), which the source gives none: a call of an `extern forkjoin` task no module defines is
that call's error (LRM 25.7.4). Whether a subroutine reports at its caller is stated on its
declaration, which a unit's signature carries, so every unit calling it hands the same arguments, as
rustc carries the mark in a crate's metadata and gives it to every shim standing for the function. A
body the design wrote hands each call its own place.

The file name a program states is what `` `__FILE__ `` expands to at that place, which the front end
answers, so a report and the directive never differ. LRM 20.10 requires exactly that of a severity
task ("shall be the same as the `` `__FILE__ `` and `` `__LINE__ `` compiler directives"), and LRM
22.13 says the directive expands to the path by which a tool opened the file, in a format the tool
may choose, so a design named by a path reports that path, and a `` `line `` directive moves both.

## Consequences

- A refusal of text a macro wrote names the macro, and one in an included file names what included
  it.
- A design's sources are in memory once.
- A place means something only in the run that read the source, so it is never written into anything
  kept across runs.
- The forms after the first carry the same value: each expression and statement of MIR, and each
  instruction and terminator of LIR, holds the place of the source it was lowered from, so what a
  later layer refuses or fails on is reported where the source wrote it. Until they carried it, this
  line recorded that they held none, which left refusals and compiler failures past the first form
  with no line to point at.
- A report a running design makes names the statement that failed, inside a constructor, after a
  wait, in a task another procedure called, alike; until the program handed the library where each
  operation was written, every run-time error was placed at the procedure that was running, and a
  failure inside a library class at the library's own line.
- A failure added anywhere in the library's interior reads what is bound and changes no signature.
- Opening a class handle is a call MIR states, which can be unwound out of like any other; a target
  that opened handles by a call of its own left a frame unwound through it unsettled.

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
- **A binding that falls back to the running procedure, opened by a list of the entries marked to
  open one.** It was built, on the ground that an argument would be paid by every element copy and
  comparison a table reaches. That ground was false, since neither can report. What was wrong was
  not the binding but the silent fallback and the positive list: an entry nobody marked read nothing
  and was reported at the running procedure with no sign of it, which is what nine of the eleven
  ways of writing a bounded queue did until they were swept by hand. With the default reversed an
  entry takes a place unless it states it reads none, and a report under no binding is a failure
  rather than a guess.
- **The place as a parameter of every function between the operation and its failure.** It was
  built, and a path to a report that stated no place did not compile, which is the one thing it has
  over the binding. It took some 460 parameters across 80 files for about 90 failure sites, through
  the table a type's operations are reached by, and a failure added deep in the library changed
  every signature above it. Every other caller of those functions passes the same value through,
  which is what a dynamic extent is. Rust's own library does not do this: `track_caller` goes on the
  operation whose failure is the caller's, and a deep panic names the library's line, which Rust's
  user can read. Our library's lines are the tool's, which the design's author cannot act on, so the
  place has to reach the report, and it reaches it without the interior knowing.
- **Catching a failure at the entry and adding the place to it.** A report that does not throw never
  passes the entry.
- **The tool's name in front of a message about no place.** clang and GCC open such a line with
  their name; rustc and the front end's own command do not. Writing it meant a line of the
  compiler's own for one case, with the words, the color and the test for which case it is that such
  a line needs, beside the path every other message takes.
