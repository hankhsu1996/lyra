# Rendered text is written once, into the artifact it belongs to

Date: 2026-09-23 Status: accepted

## Context

The C++ backend composed its output by having every node answer with its own text. A node returned a
string, and whoever asked for it copied that string into the string it was building, all the way up.
Two costs follow from that arrangement rather than from anything a renderer decides.

**A byte is copied once for every level above it.** The text of a leaf is copied into its parent's
string, that string into its parent's, and so on, so a design whose expressions nest deeply pays
more per emitted byte than one whose expressions are flat -- measured at 3,066 instructions per byte
against 1,578. **And the punctuation between the parts was a format string parsed while the program
ran**, once per node, though the string is a literal fixed when the compiler is built and the
arguments are known where it is written.

Measured on 2026-09-22 with an optimized build, four standard-library primitives were 27% of an
emission run with no function of this compiler among them: scanning format strings 9.2%, `memcpy`
6.4%, writing into a format sink 6.0%, freeing 5.5%. Rendering is 69.6% of that run and rendering
expressions 59.7% of it -- and the share goes **up** when this compiler is built optimized, because
the front end and the lowering give back proportionally more.

What makes this worth a record rather than a fix is that the arrangement reads as the obvious one.
It is what a high-level language writes, and there it is not slow: a JavaScript engine represents
`a + b` as a node holding references to the two halves and flattens the tree into contiguous
characters only when something reads them, so each character is still written once. The C++ standard
library has no such representation -- `std::format` and `+` produce real, contiguous, allocated
bytes every time -- so the same arrangement that is free in one language is quadratic in the other.

## Decision

**A render entry contributes to the artifact it is writing and answers with no text.** The
destination is passed down, every node writes its own punctuation and asks its children to write
theirs, in the order the text will be read, and each byte is written once.

**Target syntax is written as bytes; a format call converts a value and never places text.** At an
interior node a format call cannot survive in any case, because its arguments would have to be the
children's text. At a leaf, turning a number into text is real work and is done by a conversion
writing straight into the destination.

**What is genuinely a value is a name.** A name has readers besides the artifact -- the file a class
is written into is composed from one -- so naming and type mapping answer with names, and those are
built as values. Everything that reaches a reader only as part of the artifact is written.

**Three things belong to the destination rather than to whoever writes into it**: what column a line
opens at, the blank line that sets one section apart from the next, and the depth a body opens at
whatever depth the text stood at where the body was reached. A separator is owed while its section
is open and paid by the first byte that lands, so a section with nothing in it costs nothing --
separator included -- and no contributor answers in advance whether its own section is there.

## Consequences

**The emitted text is identical, which is what makes this checkable.** Emitting the whole
conformance corpus before and after gives the same bytes for all but four of 8,216 files, and each
of those four is a case whose emitted text already varies between two runs of one unchanged
compiler.

**Emission costs about a sixth fewer instructions**, measured over one emission of each of two
synthetic designs: 15.6% fewer over 1.3 MB of C++ from many small units, 17.2% fewer over 2.0 MB
whose expressions nest two hundred deep. Both readings are from the default build of this compiler,
where its own code is inflated and the library primitives this removes are not, so neither is the
figure a released build would show.

**A new arm is right by construction.** What someone adding a MIR alternative copies is the arm
beside it, so the shape that is there is the shape that spreads. The entries answer with `void`, so
the arrangement this record replaces no longer compiles at the place it would be written.

**The residual is gated rather than written down.** A format call composing the program's text
cannot be told from one composing a name by any rule a type system can state -- both answer with a
string -- so it is told apart by the spelling, and `tools/policy/check_architecture.py` refuses
`std::format` where a node's text is written.

## Rejected alternatives

**Keep the arrangement and make the format calls cheaper.** This removes at most the scanning, and
leaves the copy per level and the allocation per node -- which together are the larger half of the
four primitives measured above.

**Answer with a lazy concatenation instead of a string.** A rope holding references to its parts,
flattened once at the top, is what the high-level language does for free and what LLVM's own string
concatenation offers. It is not available to a recursive render: that type is documented as one that
must never be stored and may only be taken as a parameter, because it holds pointers to temporaries
that die at the end of the statement -- so a node cannot answer with one.

## Cross-references

- [backend_contract](../architecture/backend_contract.md) -- a render entry is a fixed function of
  one MIR node. This record changes where its output goes and nothing about what it decides.
- [unit-signature](unit-signature.md) -- which artifact a piece of text belongs in.
