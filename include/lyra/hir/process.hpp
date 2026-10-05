#pragma once

#include <compare>
#include <cstdint>
#include <variant>

#include "lyra/base/pool_id.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/timing.hpp"

namespace lyra::hir {

struct ProcessId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const ProcessId&) const -> std::strong_ordering = default;
};

// The procedure kinds of LRM 9.2. A general `always` and an `always_ff` carry
// whatever timing they have inside the body -- `always @*` its implicit list
// inside the body's timing control -- while an `always_comb` and an
// `always_latch` wait, after the body, on a list the procedure states
// (LRM 9.2.2.2.1, 9.2.2.3): what it reads, what it writes, and the function
// calls it makes, which report what their functions read and write.
struct InitialProcess {
  auto operator==(const InitialProcess&) const -> bool = default;
};
struct FinalProcess {
  auto operator==(const FinalProcess&) const -> bool = default;
};
struct AlwaysProcess {
  auto operator==(const AlwaysProcess&) const -> bool = default;
};
struct AlwaysFfProcess {
  auto operator==(const AlwaysFfProcess&) const -> bool = default;
};
struct AlwaysCombProcess {
  Reads implicit_reads;

  auto operator==(const AlwaysCombProcess&) const -> bool = default;
};
struct AlwaysLatchProcess {
  Reads implicit_reads;

  auto operator==(const AlwaysLatchProcess&) const -> bool = default;
};

using ProcessKind = std::variant<
    InitialProcess, FinalProcess, AlwaysProcess, AlwaysFfProcess,
    AlwaysCombProcess, AlwaysLatchProcess>;

struct Process {
  ProcessKind kind;
  diag::SourceSpan span;
  ProceduralBody body;
  StmtId root_stmt{};

  auto operator==(const Process&) const -> bool = default;
};

}  // namespace lyra::hir
