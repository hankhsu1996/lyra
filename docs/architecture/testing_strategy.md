# Testing Strategy

## Purpose

Define the test taxonomy: which tests exist, what each is allowed to check, and what shapes are
forbidden.

## Owns

- The two test categories, and what decides which one a new test belongs to.
- The contract of each category: scope, input form, expected output form.
- The rule that all SystemVerilog-semantic coverage is case coverage.
- The rule that no test asserts on the text of an intermediate form.

## Does Not Own

- Individual test contents.
- Test runner implementation details.
- Benchmark methodology (covered elsewhere).

## Core Invariants

1. **Any test that validates SystemVerilog semantics is a case.** There is no partial exemption for
   "small", "minimal", or "helper" coverage. What decides membership is the question the test
   answers, never the size of the input it needs.
2. **A case is a directory of SystemVerilog sources plus a manifest declaring the expected
   behavior.** It runs the whole pipeline and simulates; what it asserts is the design's observable
   result, not any intermediate the compiler passed through on the way.
3. **A case asserts the thing the feature produces, not a downstream effect of it.** A feature whose
   contract is "this variable now holds X" is checked against the variable. Formatted output is
   asserted only where formatting is itself the feature under test.
4. **A case names the backends that run it, and every backend claiming it answers to the same
   expectations.** A case is written once; a backend that has not reached a construct leaves it
   unclaimed rather than lowering it to a weaker assertion. Coverage is therefore measured rather
   than asserted, and the claimed set is what grows as a backend fills in.
5. **Everything outside the case corpus tests machinery with no SystemVerilog-semantic dimension**
   -- a runtime data structure, diagnostic rendering, the driver's own command-line behavior. Such a
   test may never stand in for language coverage.
6. **Every SystemVerilog behavior has exactly one owning case.** Coverage is not duplicated between
   cases, and never mirrored outside the corpus.
7. **One case per meaningful semantic group, not per assertion.** A case pays the full fixed cost of
   elaboration, lowering, emission, host compilation, and a run, so sharding one code path across
   many cases buys wall-clock and no signal. Split only when the assertions exercise genuinely
   different paths.
8. **No test asserts on the text of an HIR, MIR, or LIR dump.** A dump is a debugging view whose
   wording is free to change; pinning it tests the printer and obstructs every later refactor. What
   a lowering produced is proven by what the program does.

## Boundary to Adjacent Layers

- A case exercises the full pipeline, so a failure in one may point at any layer. Bisecting it is
  done by reading the dumps, not by pinning them in another test.
- A test outside the corpus isolates one piece of machinery and must not depend on the shape of any
  intermediate form.

## Forbidden Shapes

- A C++ test that constructs compiler objects and asserts on their fields to check SystemVerilog
  semantics.
- A case without a manifest.
- Any assertion on a dump's text, in any category, to prove a language behavior.
- Labels like "partial semantics", "small behavior", or "minimal repro" used to justify placing
  SystemVerilog-semantic coverage outside the corpus.
- A case one backend is held to more weakly than another.
- A new case duplicating a path an existing case already covers, where extending that case would
  serve.
- Tests that exercise code outside the current source tree.
- Relying on a file-path regex as the primary classification between categories. Membership follows
  from what the test answers, not from what it is named.

## Notes / Examples

Current implementation: cases live under `tests/cases/`, each a directory holding `main.sv`, any
support sources, and a `case.yaml` manifest declaring expected variables, expected output, and the
backend tags that claim it. `tests/suites.yaml` maps a suite to the tags it collects. The tests
outside the corpus are the diagnostic renderer, a runtime reference type, the driver's command-line
behavior, and an audit of the precompiled header's contents.

Adding a SystemVerilog feature means adding or extending a case first, then implementing until it
passes.
