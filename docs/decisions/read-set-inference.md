# Read-Set Inference via slang Flow Analysis

## Date

2026-05-29

## Status

Accepted. How a procedure's own implicit list takes in what the functions it calls read is revised
by [an-implicit-list-asks-each-function-once](an-implicit-list-asks-each-function-once.md).

## Why this decision matters

Several SystemVerilog features require a read set -- the set of symbols whose change should
re-trigger evaluation of a procedural fragment:

- `always_comb` / `always_latch` body (LRM 9.2.2.2.1)
- `always @*` region (LRM 9.4.2.2)
- `assign lhs = rhs;` continuous assignment (LRM 10.3)
- `wait (cond)` cond expression (LRM 9.4.3)
- (future) concurrent assertions and properties (LRM 16)
- (future) `wait_order(...)` (LRM 15.6)

Each of these features asks the same underlying question and -- absent a load-bearing architectural
rule -- has historically each grown its own answer. `always_comb` already routes through slang's
`AnalysisManager` (the listener path in `compile.cpp`). The first cut of `wait` and the first cut of
continuous assignment both grew hand-coded `ASTVisitor`-derived walkers that duplicated parts of
slang's leaf-set logic.

The cost of getting the answer wrong is not a behavioural bug. Naive walkers do not under-collect --
they over-collect, by recording symbols that slang's data-flow analysis would have excluded
(must-def writes, locally-declared helpers, lvalue positions inside calls). The wrong-shaped read
set causes spurious wake-ups, which causes redundant body re-evaluation, which is exactly the gap
separating us from production simulators. The project principle is that **performance is
correctness**: an answer that produces the right value but at the wrong cost is the wrong answer.
Treating "the simulation still produces the right value" as good enough is incompatible with the
project's reason for existing.

This document fixes the inference path so subsequent features inherit one infrastructure and the
older hand-coded walkers can be removed.

## Findings that shaped the design

### F1. slang's flow-analysis framework was designed for caller composition

Two public extension points exist on `slang::analysis::AnalysisManager`:

`slang/include/slang/analysis/AnalysisManager.h:100-118`:

```cpp
void addListener(std::function<void(const AnalyzedProcedure&)> listener);
```

The procedure listener fires after each procedure-like symbol has been analyzed. It receives the
finished `AnalyzedProcedure` (which already carries `getSensitivityList()`,
`getImplicitEventReadSets()`, and `getTimedStatements()`).

`slang/include/slang/analysis/AnalysisManager.h:120-130`:

```cpp
using CustomDFAProvider = std::function<AnalyzedProcedure(AnalysisContext&,
                                                          const ast::Symbol&,
                                                          const AnalyzedProcedure*)>;
void setCustomDFAProvider(CustomDFAProvider provider);
```

The custom DFA provider replaces slang's default flow analysis construction. slang calls it for each
procedure-like symbol with the worker-local `AnalysisContext`, the symbol, and the parent procedure
(if any). The callback is expected to build and run whatever flow analysis the caller wants, then
return an `AnalyzedProcedure` instance.

**Consequence:** every piece we need is reachable through public API. The listener handles
post-analysis result extraction; the provider gives us access to `AnalysisContext` and the
construction site we need for any secondary analysis that must run in the same context.

### F2. slang treats continuous assignments as procedures for analysis

`slang/include/slang/analysis/AnalyzedProcedure.h:81-82`:

> Note that this can include continuous assignments, which are not technically procedures but are
> treated as such for analysis purposes.

`slang/source/analysis/AnalyzedProcedure.cpp:295-299` confirms it -- the same `addReads` pipeline
that builds the sensitivity list for `always_comb` also runs for `ContinuousAssignSymbol`,
controlled by `AnalysisFlags::ContAssignUsesLSPs`.

`slang/source/analysis/AnalysisScopeVisitor.h:115-122` shows both kinds go through the same listener
pipeline:

```cpp
template<typename T>
    requires(IsAnyOf<T, ProceduralBlockSymbol, ContinuousAssignSymbol>)
void visit(const T& symbol) {
    result.procedures.emplace_back(manager.analyzeProcedure(...));
    // ...
    for (auto& listener : manager.procListeners)
        listener(result.procedures.back());
}
```

**Consequence:** continuous assignment sensitivity is available through the listener path with no
extra work beyond recognising the `ContinuousAssignSymbol` symbol kind.

### F3. The abstract flow-analysis pass accepts arbitrary subtree roots

`slang/include/slang/analysis/AbstractFlowAnalysis.h:97-107`:

```cpp
void run(const Statement& stmt) { state = (DERIVED).topState(); visit(stmt); }
void run(const Expression& expr) { state = (DERIVED).topState(); visit(expr); }
```

Both overloads are public and symmetric. The framework will analyze any statement or expression a
caller hands it -- the symbol-kind switch in `DataFlowAnalysis::run()` is convenience, not a
restriction.

**Consequence:** any sub-expression read set (a `wait` cond, an assertion clause, a property
argument) can be analyzed by instantiating a flow-analysis pass and calling `run(expr)` on the
sub-expression directly. The procedure's overall analysis does not need to track sub-region read
sets separately -- we re-run the analysis on the sub-tree we care about.

### F4. The default flow-analysis subclass is exported and ready to use

`slang/include/slang/analysis/DataFlowAnalysis.h:675-679`:

```cpp
class SLANG_EXPORT DefaultDFA : public DataFlowAnalysis<DefaultDFA, DataFlowState> {
public:
    DefaultDFA(AnalysisContext& context, const Symbol& symbol, bool reportDiags) :
        DataFlowAnalysis(context, symbol, reportDiags) {}
};
```

`DefaultDFA` is slang's own concrete subclass that inherits the framework unchanged. It is
`SLANG_EXPORT` -- a public API entry. slang's internal fallback when no custom provider is set is
itself a `DefaultDFA` instance (`AnalysisManager.cpp:397-403`).

**Consequence:** for our use case ("run full flow analysis on a subtree, no behaviour overrides"),
no subclass is needed. We instantiate `DefaultDFA` directly. There is no state type to design, no
hooks to override, no template parameters to choose.

### F5. Must-def precision matters even inside a single Expression subtree

The intuitive split "Statements need DFA, Expressions do not" is wrong. SystemVerilog allows side
effects inside expressions:

- Embedded assignment expressions: `(tmp = a + b) + tmp`
- Function calls with `output` / `inout` / `ref` arguments: `f(in, output buf) + buf`
- Increment / decrement: `++counter + counter`
- Compound assignment: `(x += y) + x`

In each case, a leaf appears in rvalue position after a definite write to the same symbol. Lvalue
tracking alone (classifying leaves as read vs write) does not exclude the second-occurrence read;
only must-def does. A lite walker covers the common case but not these.

For `wait` cond specifically, LRM 11.3.6 forbids assignment operators in timing-control expressions,
ruling out the first three. Function calls with output arguments remain reachable: a walker that
does not understand them will over-collect.

**Consequence:** any path that handles read-set inference must come from a data-flow analysis that
distinguishes lvalue from rvalue position and excludes must-def reads. Hand walkers, no matter how
careful about leaf enumeration, cannot reach this without re-implementing DFA.

### F6. Local-symbol exclusion alone closes most but not all of the gap

slang's `isLocal` filter (`slang/source/analysis/AnalyzedProcedure.cpp:248-271`) excludes symbols
declared inside the procedure's scope chain. For `always_comb` bodies this is the dominant source of
bloat that a hand walker would introduce: procedure-local helpers are common.

The remaining gap -- non-local must-def, where a module-level symbol is written-then-read inside the
procedure -- only causes spurious wake-ups when the symbol is also driven by another process. In
well-formed SystemVerilog this is uncommon, because most signals have a single driver.

**Consequence:** a hand walker that adds local-symbol filtering claws back the visible majority of
slang's precision. However, the project's `perf = correctness` principle makes the residual gap
unacceptable on principle, even if its quantitative impact is small. Locking in a "good enough"
walker now forces a second migration later when the residual case starts mattering.

## The decision

All read-set inference is driven by slang's existing flow-analysis framework, through one analyzer
the lowering owns. We do not write a subclass; we do not write a hand walker.

- **A procedure's own sensitivity** (`always_comb` / `always_latch`, LRM 9.2.2.2.1) is a
  `DefaultDFA` run over the procedure's own text, less what it declares and the bits it writes; what
  a function it calls reads is that function's report, as
  [an-implicit-list-asks-each-function-once](an-implicit-list-asks-each-function-once.md) records.
  This record first took slang's `AnalyzedProcedure::getSensitivityList()`, which reads every called
  function's body wherever it is declared -- a dependency on another unit's body this record did not
  weigh.
- **Any other node** -- an expression a wait or a sampled value function or a continuous assignment
  reads, a statement an `@*` gates, a subroutine's body a wait asks about -- is analyzed by running
  a fresh `DefaultDFA` on that node through `AbstractFlowAnalysis::run`. Feeding a wait's condition
  in directly bypasses slang's `visitStmt(WaitStatement)` (`AbstractFlowAnalysis.h:603-612`), which
  would suppress rvalue tracking inside a timing control, so its reads land in `rvalues` like any
  other expression's.

**What the analyzer answers is the state a node reads from outside itself, and every consumer asks
exactly that.** slang's `rvalues` hold every symbol the node reads, including those the node brings
into being: an automatic it declares, a subroutine's formals and result, an array method's iterator
(LRM 7.12), a pattern's binding (LRM 12.6). None of these holds anything before the node runs, and
none can be written from outside it (LRM 6.21), so a read of one is no dependency. The analyzer
drops them -- F6's local-symbol exclusion, applied to every node rather than only to a procedure --
and keeps a static variable declared inside, which exists from time zero. slang draws the same line
after its own analysis for `always_comb` (`isLocal`). Leaving it to each consumer was tried: one
consumer filtered, one did not and crashed on `always @(*) for (int j ...)`, and one counted an
iterator as an automatic input and refused a legal `$changed`.

**The analysis rules no path out by a value.** slang's flow analysis evaluates a condition against
the elaborated instance and leaves out what is only read on a path a constant excludes: the side of
an `if`, a `case` or a conditional operator that a parameter or a generate index never takes, the
operand a logical operator skips, and the bits a `for` with known bounds never reaches, which it
finds by unrolling the loop. Its own source calls that a heuristic and says no rule in the standard
defines it. The standard's list is what is read within the block (LRM 9.2.2.2.1) or appears in the
statement (9.4.2.2), with no exception for a branch that cannot be taken, and a select indexed by a
loop variable is not a static prefix (11.5.3). So the analyzer asks for that list: the front end's
`IgnoreConstantConditions` option, which the fork carries.

The requirement it serves is that one body compiled for many constructions reads the same thing in
each. A generate index or a parameter read as a value is supplied at construction, so a read set
that depends on what one of them settles differs between constructions whose text is identical, and
they stop being one body: a loop whose procedure branched on its index was one set of scope classes
per iteration, which on a design measured outside the project was most of its largest unit. slang
and Verilator both compute sensitivity after every instance is elaborated apart, where the constant
is simply known; compiling once for all of them is the condition that differs here.

It costs wake-ups and nothing else. Ruling no path out can only add reads, so a procedure wakes on a
read in a branch its constants exclude and recomputes the same values, which is the behaviour the
clause states and is visible where the body has an effect besides its writes. Measured on Ibex
running its hello program to the software's own `$finish`, optimized, on the execution backend:
15,312,161,333 instructions before and 15,465,115,671 after, 1.0% more, with the same instruction
trace; the design's watched entries go from 1838 to 1867. Deciding at construction which reads an
instance arms would give the narrower set back to a body with no such effect; nothing does that
today.

**A read set arrives in the order the text makes its reads.** What a body waits on is a set (LRM
9.4.2.1), but turning each read into this compiler's own form allocates as it goes, and two forms
are compared position for position to decide whether two copies of one body are one artifact. The
front end kept reads in a hash table keyed on each symbol's address, so two copies of one text
enumerated them differently and compared unequal: a loop whose blocks each read three of their own
variables in an `always_comb` was 8 scope classes per iteration (68 at 8 blocks, 260 at 32, and 12
at both once fixed), and a module handed different values of a parameter lost its sharing. The fork
keeps reads the way it already keeps writes, in the order each symbol is first read, and nothing
here sorts them.

**A read keeps the names the text reached it by.** The analysis states the symbol a read landed on
in the instance analyzed, and that is not where the same text lands in another instance: a name
through an interface port reaches whatever the port is bound to (LRM 25.3). Routing a wait from the
symbol made every instance of a unit watch the first instance's interface. So each read carries the
expression every name reaching it starts at, taken from the value paths of the analyzed text; what a
called function reads arrives as that function's report, already stated where the call reaches it. A
name through a port is watched through the port, exactly as an expression through one is read, and
any other name from where it starts. Searching the reader's ports for the one bound to the symbol's
interface guesses the name from the landing, and is wrong where two ports carry one instance or the
text named it another way.

The project-owned code is the analyzer: the flattening of `getRValues()` into a flat list of the
parts read (mirrors `AnalyzedProcedure.cpp:223-227`), the collection of what a node declares, and a
cache per node. No subclass, no state type, no hook overrides, no use of slang's `detail::`
namespace.

## Rejected alternatives

### A. Two paths: slang's listener for symbol-level + hand walker for sub-expression

Use the `addListener` path where it gives us what we need (procedural blocks, `@*` regions,
continuous assignments), and a hand-coded `ASTVisitor`-derived walker for `wait` cond. Initial
implementation of both `wait` (in `WaitCondReadCollector`) and continuous assignment (in
`ContinuousAssignReadCollector` and later `ExpressionReadCollector`) took this shape.

Rejected because the hand-walker side cannot reach slang's precision (F5 says must-def matters even
inside expressions; F6 says local-symbol filtering does not close the gap). The architectural split
also has no remaining justification once F1 + F3 + F4 show that slang's framework is already
composable for the sub-expression case.

### B. Single hand walker for everything

Drop slang entirely and run a hand walker on `always_comb` bodies too. Cheapest path. Rejected
because losing must-def and local-symbol exclusion on procedural bodies makes the read set
dramatically wrong on real designs. Project principle: silent over-collection is not acceptable on
principle, not just when the cost shows up.

### B'. Single hand walker plus local-symbol filter

Add the `isLocal` filter from F6 to a hand walker. Closes most of the gap (estimated 90-95%) for a
small effort. Rejected because the residual gap (non-local must-def under multi-driver conditions,
F6) is silently incorrect under the project's principle. Locking in a "good enough" walker now
creates known-stale interim code that has to be unwound the first time a benchmark exposes the
residual.

### C. Subclass `DataFlowAnalysis` with custom hooks and state

The first version of this decision (before reading slang's source end-to-end) proposed building a
project-owned subclass of `DataFlowAnalysis<TDerived, TState>`, designing a custom state type, and
overriding `enterTimingControlExpr` / `leaveTimingControlExpr` to extract per-wait sub-region read
sets during the procedure-level analysis.

Rejected as unnecessary. F1 + F3 + F4 show that the same outcome is reachable through slang's public
API without any of the subclass machinery: `DefaultDFA` is exported,
`AbstractFlowAnalysis::run(Expression)` accepts arbitrary sub-trees, and `setCustomDFAProvider` is
the official integration hook. Sub-region reads come from running a second `DefaultDFA` on the
sub-tree, not from intercepting the procedure-level pass.

This rejection is the load-bearing simplification of this revision of the document. Earlier drafts
-- and the cuts they would have produced -- assumed work that was never required.

### C'. Vendor a patch to slang adding per-construct sensitivity APIs

File a slang PR adding `getWaitSensitivity(WaitStatement*)` and similar to `AnalyzedProcedure`.
Deferred -- not categorically rejected. slang's current API is composable enough that a downstream
consumer (us) can implement the per-wait extraction in a few lines. Upstreaming an "is-per-wait"
accessor is a reasonable contribution to make later for reusability, but we do not gate on it. If we
contribute upstream, the project-owned analyzer shrinks to a few lines of API translation.

## Consequences

### Immediate

- Sensitivity inference lives in `lowering/ast_to_hir/sensitivity.{hpp,cpp}`: one analyzer the
  lowering asks per node as it reaches it, caching by node, and handing what it finds to the
  slang-to-HIR translation. Nothing is precomputed over the whole compilation.
- The hand-coded walkers in `include/lyra/lowering/ast_to_hir/sensitivity.hpp` (the
  `ExpressionReadCollector`) and `src/lyra/lowering/ast_to_hir/statement/lower.cpp` (the
  `WaitCondReadCollector`) are deleted.
- Wait-statement and continuous-assignment lowering switch from walker invocation to asking the
  analyzer, symmetric with how `always_comb` asks for its procedure's list.
- Continuous assignment gains correctness it did not have before: function calls with output
  arguments in the RHS, embedded assignments, and compound expressions are now read-set-correct.
- Wait cond inherits the same correctness.

### Future features

- `wait_order(...)` (LRM 15.6) reuses the same shape: it is already in
  `AbstractFlowAnalysis.h:614-619`'s timing-control path, surfaces via `getTimedStatements()`, and
  feeds the same per-sub-tree DFA pattern.
- Concurrent assertions and properties (LRM 16) feed into the same framework: slang has
  `enterAssertionActionBlock` / `leaveAssertionActionBlock` hooks already and the existing flow
  analysis handles assertion bodies. Per-assertion read-set extraction follows the same per-sub-tree
  `DefaultDFA::run` pattern.
- Other compile-time analyses that would otherwise re-walk the AST (unused-write detection,
  dead-code analysis, lint-style checks) can drive the same flow analysis per node and extract other
  result kinds from it.

### Operational

- Project owns no DFA implementation. Updates to slang's `DataFlowState`, flow-analysis internals,
  or read-set tracking are transparent. The surface we depend on is `slang::analysis::DefaultDFA`,
  `slang::analysis::DataFlowState`, `AnalyzedProcedure`'s public accessors, and
  `AbstractFlowAnalysis::run` -- all public, all `SLANG_EXPORT`.
- Each DFA instance is single-use: the flow-analysis result fields (`rvalues`, `lvalues`,
  `symbolToSlot`, `timedStatements`, ...) accumulate across `run()` calls and are never cleared by
  the framework. Every node is analyzed by a freshly constructed one.
