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
// always_latch; 9.4.2.2 for `always @*`). Identity-only: which structural
// variable, which flat bit range of its packed encoding. bit_range matches
// slang's selectable-width representation; for packed types it is lossless
// w.r.t. longest static prefix. The current runtime collapses to whole-variable
// subscription at HIR -> MIR, but the precision is preserved here so the
// collapse can be removed when the runtime gains bit-level Observable.
struct SensitivityEntry {
  StructuralVarRef ref;
  std::pair<std::uint64_t, std::uint64_t> bit_range;
};

// LRM 9.4.2.2 `@*` / `@(*)`. Identity-shaped (distinct from EventControl's
// expression-shaped triggers): the sensitivity list is derived by slang's
// AnalysisManager from the controlled statement's reads, never reassembled
// into expressions. HIR keeps the slang AST shape -- the TimedStmt wraps the
// controlled body -- and HIR -> MIR expands this into the runtime-oriented
// "wait then body" sequence at lowering time.
struct ImplicitEventControl {
  std::vector<SensitivityEntry> sensitivity_list;
};

using TimingControl =
    std::variant<DelayControl, EventControl, ImplicitEventControl>;

struct TimedStmt {
  TimingControl timing;
  StmtId stmt;
};

using StmtData = std::variant<
    EmptyStmt, VarDeclStmt, ExprStmt, BlockStmt, IfStmt, CaseStmt, ForStmt,
    WhileStmt, RepeatStmt, DoWhileStmt, ForeverStmt, BreakStmt, ContinueStmt,
    TimedStmt>;

struct Stmt {
  std::optional<std::string> label;
  StmtData data;
  diag::SourceSpan span;
};

}  // namespace lyra::hir
