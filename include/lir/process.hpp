#pragma once

#include <vector>

#include "common/trigger.hpp"
#include "fmt/format.h"
#include "lir/instruction.hpp"
#include "lir/value.hpp"

namespace lyra::lir {

// Use common::Trigger with Value directly
using Trigger = common::Trigger<Value>;

enum class ProcessKind { kInitial, kAlways };

inline auto ToString(ProcessKind kind) -> std::string {
  switch (kind) {
    case ProcessKind::kInitial:
      return "initial";
    case ProcessKind::kAlways:
      return "always";
    default:
      return "(unknown)";
  }
}

struct Process {
  ProcessKind kind;

  // Flat list of executable instructions
  std::vector<Instruction> instructions;

  // Sensitivity list for the process (empty for initial processes)
  std::vector<Trigger> trigger_list;

  [[nodiscard]] auto ToString() const -> std::string {
    std::string out;

    out += fmt::format("  Process {}\n", lyra::lir::ToString(kind));
    for (const auto& instr : instructions) {
      out += fmt::format("    {}\n", instr);
    }

    return out;
  }
};

inline auto operator<<(std::ostream& os, const Process& process)
    -> std::ostream& {
  return os << process.ToString();
}

}  // namespace lyra::lir

template <>
struct fmt::formatter<lyra::lir::Process> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::Process& process, FormatContext& ctx) {
    return fmt::format_to(ctx.out(), "{}", process.ToString());
  }
};
