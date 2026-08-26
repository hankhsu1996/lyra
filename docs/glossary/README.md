# Glossary

Canonical definitions for terminology used across architecture docs. A term earns an entry when its
everyday meaning is wide enough that two readers would take a contract differently -- "class",
"member", and "object type" are here because SystemVerilog and C++ each supply a plausible reading
and the docs mean one of them. A term the surrounding prose already pins needs no entry, so this
directory is deliberately small and grows only where an ambiguity has actually cost someone.

Where an entry exists it is the single definition: an architecture doc using the term means what the
entry says, and a doc that needs a different meaning names a different term.

## File Naming

One file per term or closely related cluster of terms, using kebab-case.

## Entry Contents

Each entry contains:

- Term
- Definition (one sentence)
- Contrast (terms it is not synonymous with)
- Usage notes

If a term has meaningful nuance, split it into distinct terms with distinct entries. Do not overload
a single entry with multiple meanings.
