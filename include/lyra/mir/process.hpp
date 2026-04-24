#pragma once

#include <compare>
#include <cstdint>
#include <variant>

#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

struct ProcessId {
  std::uint32_t value;

  auto operator<=>(const ProcessId&) const -> std::strong_ordering = default;
};

struct Initial {};

using ProcessData = std::variant<Initial>;

struct Process {
  ProcessData data;
  Body body;
};

}  // namespace lyra::mir
