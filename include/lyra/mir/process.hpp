#pragma once

#include <compare>
#include <cstdint>

#include "lyra/mir/method.hpp"

namespace lyra::mir {

struct ProcessId {
  std::uint32_t value;

  auto operator<=>(const ProcessId&) const -> std::strong_ordering = default;
};

enum class ProcessKind : std::uint8_t {
  kInitial,
  kFinal,
};

// An activation registration: the scope's constructor-time decision to run
// `code` (a coroutine callable) at simulation start (`kInitial`) or shutdown
// (`kFinal`, LRM 9.2.3). The kind is the lifecycle registration, not a property
// of the callable body; `code` is an ordinary `MethodDecl` rendered like any
// other method.
struct Process {
  ProcessKind kind = ProcessKind::kInitial;
  MethodDecl code;
};

}  // namespace lyra::mir
