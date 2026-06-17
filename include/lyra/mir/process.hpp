#pragma once

#include <compare>
#include <cstdint>
#include <string>

#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

struct ProcessId {
  std::uint32_t value;

  auto operator<=>(const ProcessId&) const -> std::strong_ordering = default;
};

enum class ProcessKind : std::uint8_t {
  kInitial,
  kFinal,
};

struct Process {
  ProcessKind kind = ProcessKind::kInitial;
  std::string name;
  ProceduralScope root_procedural_scope;
};

}  // namespace lyra::mir
