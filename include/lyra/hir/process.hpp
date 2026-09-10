#pragma once

#include <compare>
#include <cstdint>
#include <vector>

#include "lyra/base/pool_id.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/timing.hpp"

namespace lyra::hir {

struct ProcessId {
  std::uint32_t value = base::kUnassignedId;

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
  ProceduralBody body;
  StmtId root_stmt{};
  // LRM 9.2.2.2.1 implicit sensitivity for always_comb / always_latch
  // (procedure-level; the body is a plain statement). Empty for every other
  // process kind -- `always @*` carries its sensitivity inside
  // hir::ImplicitEventControl on the body's TimedStmt instead.
  std::vector<SensitivityEntry> implicit_sensitivity_list;
};

}  // namespace lyra::hir
