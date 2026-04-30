#pragma once

#include <compare>
#include <cstdint>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/local_var.hpp"
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
  StmtId body{};
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
  std::vector<LocalVar> local_vars;
};

}  // namespace lyra::hir
