#pragma once

#include <compare>
#include <cstdint>

#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

struct ProcessId {
  std::uint32_t value;

  auto operator<=>(const ProcessId&) const -> std::strong_ordering = default;
};

enum class ProcessKind { kInitial };

struct Process {
  ProcessKind kind;
  Body body;
};

}  // namespace lyra::mir
