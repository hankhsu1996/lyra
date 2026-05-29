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

enum class UniquePriorityCheck : std::uint8_t {
  kUnique,
  kUnique0,
  kPriority,
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
  ExprId condition;
  std::vector<CaseItem> items;
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

struct DelayControl {
  ExprId duration;
};

enum class EventEdge : std::uint8_t {
  kAnyChange,
  kPosedge,
  kNegedge,
  kBothEdges,
};

struct EventTrigger {
  ExprId signal;
  EventEdge edge;
};

struct EventControl {
  std::vector<EventTrigger> triggers;
};

// One entry of an implicit sensitivity list (LRM 9.2.2.2.1 for always_comb /
// always_latch; 9.4.2.2 for `@*`; 9.4.3 for `wait (expr)`). Identity-only:
// which structural variable, which flat bit range of its packed encoding.
// The current runtime collapses to whole-variable subscription at HIR ->
// MIR; bit_range is preserved here so the collapse can be lifted once the
// runtime gains bit-level Observable.
struct SensitivityEntry {
  StructuralVarRef ref;
  std::pair<std::uint64_t, std::uint64_t> bit_range;
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
    EmptyStmt, VarDeclStmt, ExprStmt, BlockStmt, IfStmt, CaseStmt, ForStmt,
    WhileStmt, RepeatStmt, DoWhileStmt, ForeverStmt, BreakStmt, ContinueStmt,
    TimedStmt, EventTriggerStmt, WaitStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
