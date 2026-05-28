#pragma once

#include <compare>
#include <cstdint>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"

namespace lyra::hir {

struct ProcessId {
  std::uint32_t value;

  auto operator<=>(const ProcessId&) const -> std::strong_ordering = default;
};

enum class ProcessKind : std::uint8_t {
  kInitial,
  kFinal,
  kAlways,
  kAlwaysComb,
  kAlwaysLatch,
  kAlwaysFf,
};

struct Process {
  ProcessKind kind = ProcessKind::kInitial;
  diag::SourceSpan span;
  StmtId root_stmt{};
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
  std::vector<ProceduralVarDecl> procedural_vars;
  // LRM 9.2.2.2.1; non-empty only for always_comb / always_latch. `@*` carries
  // its own sensitivity inside hir::ImplicitEventControl on the body's
  // TimedStmt, so it does not populate this field.
  std::vector<SensitivityEntry> implicit_sensitivity_list;
};

}  // namespace lyra::hir
