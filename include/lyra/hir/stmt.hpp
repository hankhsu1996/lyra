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
#include "lyra/hir/inside_item.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

struct StmtId {
  std::uint32_t value;

  auto operator<=>(const StmtId&) const -> std::strong_ordering = default;
};

struct EmptyStmt {};

// VarDeclStmt has ordering semantics in HIR -- its position in the statement
// stream marks the SystemVerilog point of declaration. The actual storage is
// allocated on Process.procedural_vars; ProceduralVarId.value indexes into
// that vector. The optional init expression holds the initial value written in
// the declaration (e.g. `int x = 7`); it is evaluated at the point of the
// statement.
struct VarDeclStmt {
  ProceduralVarId var = {};
  std::optional<ExprId> init;
};

struct ExprStmt {
  ExprId expr;
};

struct BlockStmt {
  std::vector<StmtId> statements;
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
// concurrent process; `mode` sets when the parent resumes.
struct ForkStmt {
  JoinMode mode;
  std::vector<StmtId> locals;
  std::vector<StmtId> branches;
};

enum class UniquePriorityCheck : std::uint8_t {
  kUnique,
  kUnique0,
  kPriority,
};

// LRM 12.5 plain case (`===` exact compare) and the LRM 12.5.1 do-not-care
// forms casez (Z bidirectional wildcard) and casex (Z + X bidirectional
// wildcard). All three share the cascade shape (selector snapshot, value-list
// labels, first-match-wins, optional default); they differ only in the
// per-label compare primitive HIR->MIR picks. case-inside is a separate
// CaseInsideStmt because its labels are range_list entries.
enum class CaseCondition : std::uint8_t {
  kNormal,
  kWildcardJustZ,
  kWildcardXOrZ,
};

struct IfStmt {
  ExprId condition;
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

// LRM 12.5.4 set-membership form `case (X) inside { ... }`. Distinct from
// CaseStmt because both the per-item label shape (range_list, not value list)
// and the per-item match semantics (asymmetric wildcard inside-membership per
// LRM 11.4.13 / 11.4.6) differ -- a match here is only the deterministic
// `1'b1` from the inside operator; `1'b0` and `1'bx` are both no-match.
struct CaseInsideItem {
  std::vector<InsideItem> items;
  StmtId stmt;
};

struct CaseInsideStmt {
  ExprId condition;
  std::vector<CaseInsideItem> items;
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

struct BreakStmt {};

struct ContinueStmt {};

// LRM 13.4.1 `return [expr];`. `value` carries the returned expression for a
// non-void function; it is absent for `return;` and for void functions / tasks.
struct ReturnStmt {
  std::optional<ExprId> value;
};

struct DelayControl {
  ExprId duration;
};

enum class EventEdge : std::uint8_t {
  kAnyChange,
  kPosedge,
  kNegedge,
  kBothEdges,
};

// One leaf entry of a wait's projection set. Identity-only: which structural
// variable, which flat bit range of its packed encoding, and what edge
// polarity the leaf was subscribed under. Implicit sensitivity sources
// (always_comb / always_latch / `@*` / wait cond / continuous assignment)
// supply leaves with `edge_kind == kAnyChange`. Explicit event control
// `@(posedge ...)` / `@(negedge ...)` / `@(edge ...)` set the per-leaf edge.
struct SensitivityEntry {
  SensitivityRef ref;
  std::pair<std::uint64_t, std::uint64_t> bit_range;
  EventEdge edge_kind = EventEdge::kAnyChange;
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
  EventEdge edge;
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
// data type. The `event` ExprId resolves to a PrimaryExpr of StructuralVarRef
// pointing at the event variable.
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
// named-event data type. The `event` ExprId resolves to a PrimaryExpr of
// StructuralVarRef pointing at the event variable.
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

using StmtData = std::variant<
    EmptyStmt, VarDeclStmt, ExprStmt, BlockStmt, ForkStmt, IfStmt, CaseStmt,
    CaseInsideStmt, ForStmt, WhileStmt, RepeatStmt, DoWhileStmt, ForeverStmt,
    BreakStmt, ContinueStmt, ReturnStmt, TimedStmt, EventTriggerStmt, WaitStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
