#pragma once

#include <memory>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/ranges.h>

#include "common/formatting.hpp"
#include "common/trigger.hpp"
#include "lir/basic_block.hpp"
#include "lir/instruction.hpp"

namespace lyra::lir {

// Use string for variable names in triggers
using Trigger = common::Trigger<std::string>;

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

  // List of basic blocks
  std::vector<std::unique_ptr<BasicBlock>> blocks;

  // Flat list of executable instructions (legacy, for backward compatibility)
  std::vector<Instruction> instructions;

  // Sensitivity list for the process (empty for initial processes)
  std::vector<Trigger> trigger_list;

  [[nodiscard]] auto ToString(int indentation_level = 0) const -> std::string {
    std::string out;

    out += fmt::format(
        "{}Process {}", common::Indent(indentation_level),
        lyra::lir::ToString(kind));
    if (!trigger_list.empty()) {
      out += fmt::format(" @({})", fmt::join(trigger_list, ", "));
    }
    out += "\n";

    // If using basic blocks
    if (!blocks.empty()) {
      for (const auto& block : blocks) {
        out += fmt::format("{}", block->ToString(indentation_level + 1));
      }
    }
    // Fallback to flat instruction list (legacy)
    else if (!instructions.empty()) {
      for (const auto& instr : instructions) {
        out +=
            fmt::format("{}{}\n", common::Indent(indentation_level + 1), instr);
      }
    }

    return out;
  }

  // Find the index of a basic block by its label
  [[nodiscard]] auto FindBlockIndexByLabel(const std::string& label) const
      -> size_t {
    for (size_t i = 0; i < blocks.size(); ++i) {
      if (blocks[i]->label == label) {
        return i;
      }
    }
    throw std::runtime_error(
        fmt::format("Basic block with label '{}' not found", label));
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
