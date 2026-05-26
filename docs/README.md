# Lyra Documentation

Lyra is a SystemVerilog compiler. This directory defines the compiler's architecture and tracks the
work in flight against it.

Architecture docs are **contracts**: they describe what each layer is, what it owns, and what shapes
are forbidden. Current code must conform to these contracts; if it does not, the code is wrong, not
the doc.

## Structure

- `architecture/` -- binding contracts for each layer and cross-cutting concept. Start here.
- `progress/` -- live working queues tracking the delta between current code and the target
  architecture.
- `decisions/` -- logged architectural decisions (one file per decision).
- `glossary/` -- canonical definitions of terminology used across docs.
- `ci/` -- CI workflow inventory.
- `style.md` -- writing contract for docs in this directory.

## Reading Order

Read `architecture/README.md` first. It lists the architecture docs in the intended reading order
and includes a **Concept Index** mapping topics (`generate`, `lvalue`, `scheduler`,
`constructor context`, ...) to the doc that owns them.

## Finding Things Fast

If you know what concept you are looking for, jump straight to the Concept Index in
`architecture/README.md`. If you are not sure what to call the concept, start at the top of the
Reading Order: `north_star.md` then `compiler_overview.md` give the framing in under five minutes.
