#pragma once

#include <compare>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/value_ref.hpp"

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

// One entry of an always_comb / always_latch implicit sensitivity list
// (LRM 9.2.2.2.1). Identity-only: which structural variable, which flat bit
// range of its packed encoding. bit_range matches slang's selectable-width
// representation; for packed types it is lossless w.r.t. longest static
// prefix. The current runtime collapses to whole-variable subscription at
// HIR -> MIR, but the precision is preserved here so the collapse can be
// removed when the runtime gains bit-level Observable.
struct SensitivityEntry {
  StructuralVarRef ref;
  std::pair<std::uint64_t, std::uint64_t> bit_range;
};

struct Process {
  ProcessKind kind = ProcessKind::kInitial;
  diag::SourceSpan span;
  StmtId root_stmt{};
  std::vector<Expr> exprs;
  std::vector<Stmt> stmts;
  std::vector<ProceduralVarDecl> procedural_vars;
  // LRM 9.2.2.2.1; non-empty only for always_comb / always_latch.
  std::vector<SensitivityEntry> implicit_sensitivity_list;
};

}  // namespace lyra::hir
