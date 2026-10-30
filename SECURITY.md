# Security

Lyra has no stable release yet. A fix lands on `main`, and that is the version to use.

## What counts as a vulnerability

Lyra compiles the SystemVerilog it is given and runs the program it builds, and it is not hardened
against a design written to attack it. A design can link C code through DPI-C, and the program it
builds can read and write files and use as much memory as it asks for. So a crash, an internal
error, a hang or running out of memory on some input is an ordinary bug: open a bug report for it.

A vulnerability is something that reaches past what you asked Lyra to build and run. For example,
the build fetching or running something other than what `MODULE.bazel` pins, or a CI workflow or
release artifact that someone outside the project could alter.

## Reporting one

Report it privately, through the repository's **Security** tab and **Report a vulnerability**, not
in a public issue. Say which commit you used, how to reproduce it, and what an attacker gains. You
will get an answer in the advisory, and the fix and the advisory are published together.
