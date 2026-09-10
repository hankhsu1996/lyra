#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/base/pool_id.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/assertion.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/loop_label_id.hpp"
#include "lyra/hir/pattern.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/timing.hpp"

namespace lyra::hir {

struct StmtId {
  std::uint32_t value = base::kUnassignedId;

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

// LRM 16.2 verification directive of an immediate assertion. The two state
// different things -- an obligation the design must meet, and an assumption
// about the environment around it -- and a simulation tool checks both the same
// way, so what the directive decides is which of the two a failure names.
enum class AssertionDirective : std::uint8_t {
  kAssert,
  kAssume,
};

// When an immediate assertion's action runs relative to the step that reaches
// it (LRM 16.3, 16.4). A simple assertion runs its action inline; a deferred
// one evaluates its expression inline but holds the action for a later region
// of the same time step, to suppress reports from transient combinational
// values -- an observed (`#0`) one maturing in Observed and acting in Reactive,
// a final one maturing and acting in Postponed.
enum class AssertionTiming : std::uint8_t {
  kSimple,
  kObserved,
  kFinal,
};

// LRM 16.3 / 16.4 immediate assert / assume: the expression is tested where the
// statement executes, read the way the condition of a procedural if is read.
// Either arm of the action block may be omitted, and the two omissions mean
// different things -- with no pass statement a true expression runs nothing,
// while with no fail statement a false expression still reaches the tool's own
// failure report. `timing` says whether that action runs inline or is deferred
// to a later region of the time step.
struct AssertStmt {
  AssertionDirective directive;
  AssertionTiming timing;
  ExprId condition;
  std::optional<StmtId> pass_stmt;
  std::optional<StmtId> fail_stmt;
};

// LRM 16.3 simple immediate cover: success of the expression is a coverage
// goal, which inverts the disposition an assert has. A false expression is not
// a failure, so the grammar gives this form a single statement rather than an
// action block and there is no fail arm to carry.
struct CoverStmt {
  ExprId condition;
  std::optional<StmtId> pass_stmt;
};

// LRM 16.14 concurrent assert / assume, written where a procedure reaches it.
// Its enabling condition is that control arrived here, which is no function of
// the trace and so is recorded by being at this position rather than evaluated.
// Reaching the statement therefore queues an instance rather than evaluating
// the property; the instance matures in the Observed region of that time step
// and begins an attempt at the tick of the leading clock (LRM 16.14.6).
//
// Each attempt carries its own result and selects its own arm, and the two
// omissions mean what they mean for an immediate assertion -- with no pass
// statement a true result runs nothing, with no fail statement a false one
// still reaches the tool's own report. An attempt the disable condition
// preempted selects neither (LRM 16.14.1).
struct ConcurrentAssertStmt {
  AssertionDirective directive;
  PropertySpec spec;
  std::optional<StmtId> pass_stmt;
  std::optional<StmtId> fail_stmt;
};

// LRM 16.14.3 concurrent cover: the statement runs once for each attempt that
// succeeds, at most once per attempt. An attempt that does not succeed is not a
// failure, so this form has no fail arm and reaches no report.
struct ConcurrentCoverStmt {
  PropertySpec spec;
  std::optional<StmtId> pass_stmt;
};

// The two dispositions a concurrent assertion carries, named apart from the
// statement stream so the assertion whose enabling condition is 1 -- which a
// scope declares rather than a procedure running it -- is the same concept
// spelled once.
using ConcurrentAssertion =
    std::variant<ConcurrentAssertStmt, ConcurrentCoverStmt>;

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

struct TimedStmt {
  TimingControl timing;
  StmtId stmt;
};

// LRM 15.5.1 `-> e;` and `->> [ delay_or_event_control ] e;`. The `event`
// ExprId resolves to a PrimaryExpr of a direct or routed reference pointing at
// the event variable. `timing` says when the trigger happens: `->` triggers
// where the statement is reached, and `->>` makes it a nonblocking update event
// due in the NBA region of the slot its control names.
struct EventTriggerStmt {
  ExprId event;
  EffectTiming timing;
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
// so the target may sit in another process, and it indexes the registry of the
// declaration scope this body belongs to. How the termination is realized --
// the scope's runtime endpoint, the resumption gate, the unwind -- is
// synthesized at HIR-to-MIR, not carried here.
struct DisableStmt {
  ProceduralScopeId target;
};

using StmtData = std::variant<
    EmptyStmt, VarDeclStmt, ExprStmt, BlockStmt, ForkStmt, IfStmt, CaseStmt,
    PatternCaseStmt, AssertStmt, CoverStmt, ConcurrentAssertStmt,
    ConcurrentCoverStmt, ForStmt, WhileStmt, RepeatStmt, DoWhileStmt,
    ForeverStmt, BreakStmt, ContinueStmt, ReturnStmt, TimedStmt,
    EventTriggerStmt, WaitStmt, WaitForkStmt, DisableForkStmt, DisableStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
