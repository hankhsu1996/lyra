#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/loop_label_id.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/support/event_edge.hpp"

namespace lyra::hir {

struct StmtId {
  std::uint32_t value;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

struct EmptyStmt {};

// The SystemVerilog point of declaration, which is what its position in the
// statement stream marks. What is declared -- including the declaration
// assignment -- is the declaration's own content; this statement names it.
struct VarDeclStmt {
  ProceduralVarId var = {};
};

struct ExprStmt {
  ExprId expr;
};

// A `begin ... end` (LRM 9.3.4): statements run in sequence in a lexical
// declaration scope. It always has one. Whether the source named it, and
// whether it declares anything, are properties recorded on the scope -- never
// reasons for the scope to be absent -- so a block with no name and no
// declaration is the same shape as one with both. The scope record holds the
// block's segment name, its direct declarations, and the scopes nested inside
// it; runtime addressability is a separate axis on that record.
//
// This is also the sequence a lowering composes when it expands one source
// statement into several and the surrounding slot admits one (LRM 12.7.3 gives
// `foreach` such a shape). Such a block declares nothing and carries no name,
// which is what makes it transparent -- not a kind of its own.
struct BlockStmt {
  std::vector<StmtId> statements;
  ProceduralScopeId scope;
};

// LRM 9.3.2 Table 9-1: which join keyword controls when the forking process
// resumes.
enum class JoinMode : std::uint8_t {
  kAll,
  kAny,
  kNone,
};

// LRM 9.3.2 parallel block. `locals` are the fork's block_item_declarations
// (VarDeclStmt) -- initialized at block entry, before any branch spawns, to
// give each branch a by-value snapshot; they precede the parallel statements in
// the fork's scope. Each branch in `branches` is a statement run as its own
// concurrent process; `mode` sets when the parent resumes. `scope` is the
// lexical declaration scope the fork opens, which owns the locals above and
// exists whether or not any were written.
struct ForkStmt {
  JoinMode mode;
  std::vector<StmtId> locals;
  std::vector<StmtId> branches;
  ProceduralScopeId scope;
};

enum class UniquePriorityCheck : std::uint8_t {
  kUnique,
  kUnique0,
  kPriority,
};

// LRM 12.5 plain case (`===` exact compare), the LRM 12.5.1 do-not-care forms
// casez (Z bidirectional wildcard) and casex (Z + X bidirectional wildcard),
// and the LRM 12.5.4 set-membership form `case (X) inside`. All four share the
// cascade shape -- selector snapshot, label list, first-match-wins, optional
// default -- and differ only in the per-label compare HIR->MIR picks: an
// equality primitive for the first three, the asymmetric wildcard membership
// of LRM 11.4.13 for inside.
enum class CaseCondition : std::uint8_t {
  kNormal,
  kWildcardJustZ,
  kWildcardXOrZ,
  kInside,
};

// LRM 12.4 / 12.6.2. `conditions` is the predicate's clause sequence, always
// at least one entry; a plain `if (expr)` is the single pattern-free clause.
struct IfStmt {
  std::vector<ConditionClause> conditions;
  StmtId then_stmt;
  std::optional<StmtId> else_stmt;
  std::optional<UniquePriorityCheck> check;
};

struct CaseItem {
  std::vector<ExprId> labels;
  StmtId stmt;
};

struct CaseStmt {
  CaseCondition condition_kind;
  ExprId condition;
  std::vector<CaseItem> items;
  std::optional<StmtId> default_stmt;
  std::optional<UniquePriorityCheck> check;
};

// LRM 12.6.1 pattern-matching case item: a pattern plus an optional Boolean
// filter (the `&&& filter` suffix) plus the statement body. Pattern-bound
// identifiers are in scope for the filter and the body.
struct PatternCaseItem {
  PatternId pattern = {};
  std::optional<ExprId> filter;
  StmtId stmt = {};
};

// LRM 12.6.1 pattern-matching case statement (`case (expr) matches ... /
// casez (expr) matches ... / casex (expr) matches ...`). Distinct from
// `CaseStmt` because both the per-item label shape (patterns, not
// expressions) and the per-item match semantics (a recursive match that binds
// identifiers) differ.
struct PatternCaseStmt {
  CaseCondition condition_kind;
  ExprId condition;
  std::vector<PatternCaseItem> items;
  std::optional<StmtId> default_stmt;
  std::optional<UniquePriorityCheck> check;
};

struct ForInitDecl {
  ProceduralVarId var = {};
  std::optional<ExprId> init;
};

struct ForInitExpr {
  ExprId expr;
};

using ForInit = std::variant<ForInitDecl, ForInitExpr>;

struct ForStmt {
  std::vector<ForInit> init;
  std::optional<ExprId> condition;
  std::vector<ExprId> step;
  StmtId body;
  std::optional<LoopLabelId> break_label = std::nullopt;
};

struct WhileStmt {
  ExprId condition;
  StmtId body;
};

struct RepeatStmt {
  ExprId count;
  StmtId body;
};

struct DoWhileStmt {
  ExprId condition;
  StmtId body;
};

struct ForeverStmt {
  StmtId body;
};

struct BreakStmt {
  std::optional<LoopLabelId> target = std::nullopt;
};

struct ContinueStmt {};

// LRM 13.4.1 `return [expr];`. `value` carries the returned expression for a
// non-void function; it is absent for `return;` and for void functions / tasks.
struct ReturnStmt {
  std::optional<ExprId> value;
};

struct DelayControl {
  ExprId duration;
};

// One leaf entry of a wait's projection set. Identity-only: which cell, the
// flat-bit footprint of its packed encoding the leaf observes, and what edge
// polarity the leaf was subscribed under. An absent footprint
// means the whole signal is observed; an edge then reduces to its LSB. Implicit
// sensitivity sources (always_comb / always_latch / `@*` / wait cond /
// continuous assignment) supply leaves with `edge_kind == kAnyChange`. Explicit
// event control `@(posedge ...)` / `@(negedge ...)` / `@(edge ...)` set the
// per-leaf edge.
struct SensitivityEntry {
  ValueTarget ref;
  std::optional<std::pair<std::uint64_t, std::uint64_t>> footprint;
  support::EventEdge edge_kind = support::EventEdge::kAnyChange;
};

// One entry of an explicit `@(...)` event control. `signal` is the SV
// expression being monitored; `edge` is the optional edge identifier; the
// per-leaf `sensitivity_list` is the read set of `signal` (slang DFA) used
// for subscription. For a compound expression (concat / arithmetic / dynamic
// index) the leaves are over-broad relative to "fire only when the result
// changes" -- HIR -> MIR builds a snapshot + re-eval loop around the leaf
// wait that enforces LRM 9.4.2 correctness.
struct EventTrigger {
  ExprId signal;
  support::EventEdge edge;
  std::vector<SensitivityEntry> sensitivity_list;
};

struct EventControl {
  std::vector<EventTrigger> triggers;
};

// LRM 9.4.2.2 `@*` / `@(*)`. Sensitivity for the controlled body is
// computed by slang's AnalysisManager (write-before-read exclusion via
// must-def) and looked up at AST -> HIR via the precomputed read-set facts.
struct ImplicitEventControl {
  std::vector<SensitivityEntry> sensitivity_list;
};

// LRM 15.5.2 `@e;`. The controlled timing is a wait on a named event rather
// than a value-change event. HIR mirrors slang's TimingControl shape; HIR ->
// MIR collapses this onto a method call (`event.Await()`) on the named-event
// data type. The `event` ExprId resolves to a PrimaryExpr of a direct or routed
// reference pointing at the event variable.
struct NamedEventControl {
  ExprId event;
};

using TimingControl = std::variant<
    DelayControl, EventControl, ImplicitEventControl, NamedEventControl>;

struct TimedStmt {
  TimingControl timing;
  StmtId stmt;
};

// LRM 15.5.1 `-> e;`. Source-aligned with slang's EventTriggerStatement.
// HIR -> MIR collapses this onto a method call (`event.Trigger()`) on the
// named-event data type. The `event` ExprId resolves to a PrimaryExpr of a
// direct or routed reference pointing at the event variable.
struct EventTriggerStmt {
  ExprId event;
};

// LRM 9.4.3 level-sensitive `wait (cond) body`. `sensitivity_list` is the
// precomputed read set of `cond`, populated at AST -> HIR from a slang-side
// ASTVisitor over WaitStatement.cond -- symmetric with how `@*` and
// always_comb carry slang-derived sensitivity.
struct WaitStmt {
  ExprId cond;
  StmtId body;
  std::vector<SensitivityEntry> sensitivity_list;
};

// LRM 9.6.1 `wait fork`: block the enclosing process until all of its immediate
// child subprocesses have terminated. Carries no operand -- the child set is
// the executing process's, resolved at runtime.
struct WaitForkStmt {};

// LRM 9.6.3 `disable fork`: terminate every descendant of the enclosing
// process, including the descendants of subprocesses that have already
// terminated. Like `wait fork` it carries no operand -- the descendant set is
// the executing process's, resolved at runtime -- but it does not block the
// caller.
struct DisableForkStmt {};

// LRM 9.6.2 `disable <named block or task>`: terminate the activity of the
// named scope so execution resumes at the statement following it. `target` is a
// typed reference to that scope's declaration -- selected by static identity,
// so the target may sit in another process. How the termination is realized --
// the scope's runtime endpoint, the resumption gate, the unwind -- is
// synthesized at HIR-to-MIR, not carried here.
struct DisableStmt {
  ProceduralScopeId target;
};

using StmtData = std::variant<
    EmptyStmt, VarDeclStmt, ExprStmt, BlockStmt, ForkStmt, IfStmt, CaseStmt,
    PatternCaseStmt, ForStmt, WhileStmt, RepeatStmt, DoWhileStmt, ForeverStmt,
    BreakStmt, ContinueStmt, ReturnStmt, TimedStmt, EventTriggerStmt, WaitStmt,
    WaitForkStmt, DisableForkStmt, DisableStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
