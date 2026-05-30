# Read-Set Inference via slang Flow Analysis

## Date

2026-05-29

## Status

Accepted

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

All read-set inference is driven by slang's existing flow-analysis framework. We do not write a
subclass; we do not write a hand walker. We drive two slang surfaces:

1. **`AnalysisManager::setCustomDFAProvider`** is set to a callback that:
   - Constructs `DefaultDFA(context, symbol, /*reportDiags=*/true)`
   - Calls `dfa.run()` to do the procedure-level analysis (always_comb / always_latch / `@*` /
     continuous assignment, depending on the symbol kind)
   - Iterates `dfa.getTimedStatements()` to find every `WaitStatement`. For each one, constructs a
     **fresh** `DefaultDFA` and calls `DefaultDFA::AbstractFlowAnalysis::run(wait.cond)` directly.
     The resulting `getRValues()` is the per-wait sensitivity list.
   - Records both procedure-level and per-wait results into a project-owned `SensitivityReadStore`
     under a mutex (the callback may be invoked concurrently from worker threads).
   - Returns the same `AnalyzedProcedure` slang's default path would have returned, so downstream
     listeners and consumers see no behavioural difference.

2. **`AnalysisManager::addListener`** is set to a callback that only reads from the already-built
   `AnalyzedProcedure` -- it harvests `getSensitivityList()` and `getImplicitEventReadSets()` for
   the procedure-level cases. It does not need `AnalysisContext`, which is why it is split from the
   provider above. Listener and provider coexist.

Per-wait analysis works precisely because the cond expression is fed to `AbstractFlowAnalysis::run`
directly. slang's `visitStmt(WaitStatement)` (`AbstractFlowAnalysis.h:603-612`) -- which wraps the
cond in `enterTimingControlExpr` / `leaveTimingControlExpr` and would suppress rvalue tracking per
LRM 9.4.2.2 -- never fires, because we bypass that visitor entry point. The cond is analyzed as an
ordinary expression, and its reads land in `rvalues` like any other expression's reads.

The `SensitivityReadStore` is keyed by slang's two analyzable AST hierarchies:
`const ast::Statement*` for whole-statement read sets (procedural-block bodies and `@*` regions,
where slang's listener attaches the read set to a statement) and `const ast::Expression*` for
sub-expression read sets (the wait cond expression and the continuous assignment's
`AssignmentExpression`, where we ran or harvested the analysis on an expression subtree). The keying
split mirrors slang's own AST hierarchy split rather than enumerating specific source-language
features, so future per-expression analyses (LRM 16 property / assertion expressions, LRM 15.6
`wait_order` event-list expressions) plug into the existing Expression bucket without API changes.
Both buckets flow into `TranslateSensitivityReads` on the way to HIR `SensitivityEntry`.

The only project-owned code involved is:

- The two callback lambdas (~30 lines each)
- A `FlattenReadSet` helper that walks `getRValues()` into `vector<SensitivityRead>` (mirrors
  `AnalyzedProcedure.cpp:223-227`)
- The map-shape change and consumer-side lookup migrations

No subclass, no state type, no hook overrides, no use of slang's `detail::` namespace.

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
contribute upstream, the project-owned callback shrinks to a few lines of API translation.

## Consequences

### Immediate

- Sensitivity inference lives in `lowering/ast_to_hir/sensitivity.{hpp,cpp}`, alongside the store
  types and the slang-to-HIR `TranslateSensitivityReads`. The producer is
  `BuildSensitivityReadStore(compilation)`, which sets a `CustomDFAProvider` (per-wait DFA, store
  writes) and an `addListener` callback (procedure-level + `@*` region + continuous assignment store
  writes). The previous single `unordered_map<const Statement*, vector<SensitivityRead>>` becomes a
  two-keyed `SensitivityReadStore` keyed by `Statement*` (procedure body / `@*` region) and
  `Expression*` (wait cond / continuous-assignment expression). `compile.cpp` shrinks to
  orchestration -- it just calls `BuildSensitivityReadStore` and threads the result through
  `LowerCompilationFacts`.
- The hand-coded walkers in `include/lyra/lowering/ast_to_hir/sensitivity.hpp` (the
  `ExpressionReadCollector`) and `src/lyra/lowering/ast_to_hir/statement/lower.cpp` (the
  `WaitCondReadCollector`) are deleted.
- Wait-statement and continuous-assignment lowering switch from walker invocation to map lookup,
  symmetric with how always_comb already looks up by procedure-body statement.
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
  dead-code analysis, lint-style checks) can use the same custom-DFA-provider hook to drive flow
  analysis once and extract multiple result kinds in a single pass.

### Operational

- Project owns no DFA implementation. Updates to slang's `DataFlowState`, flow-analysis internals,
  or read-set tracking are transparent. The surface we depend on is `slang::analysis::DefaultDFA`,
  `slang::analysis::DataFlowState`, `setCustomDFAProvider`, `addListener`, `AnalyzedProcedure`'s
  public accessors, and `AbstractFlowAnalysis::run` -- all public, all `SLANG_EXPORT`.
- Listener and provider may be invoked from multiple worker threads
  (`AnalysisScopeVisitor.h:117-122` runs inside thread-pool worker tasks). The store-write side is
  protected by a single `std::mutex`. Per-listener cost is dominated by DFA execution; the
  store-write critical section is trivial, so contention is negligible.
- Each DFA instance is single-use: the flow-analysis result fields (`rvalues`, `lvalues`,
  `symbolToSlot`, `timedStatements`, ...) accumulate across `run()` calls and are never cleared by
  the framework. Per-wait DFAs are freshly constructed inside the provider callback.
