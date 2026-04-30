#pragma once

#include <compare>
#include <cstdint>

#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

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
  Body body;
};

}  // namespace lyra::mir
