#pragma once

#include <memory>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/ranges.h>

#include "lyra/common/formatting.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/lir/basic_block.hpp"

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
  std::string name;
  ProcessKind kind;

  // List of basic blocks
  std::vector<std::unique_ptr<BasicBlock>> blocks;

  [[nodiscard]] auto ToString(
      common::FormatMode mode = common::FormatMode::kPlain,
      int indentation_level = 0) const -> std::string {
    std::string out;

    // Process header with kind and trigger list
    if (mode == common::FormatMode::kContextual) {
      out += fmt::format(
          "{}Process {} {}", common::Indent(indentation_level), name,
          lyra::lir::ToString(kind));
    } else {
      out += fmt::format("Process {} {}", name, lyra::lir::ToString(kind));
    }

    out += "\n";

    // If using basic blocks
    if (!blocks.empty()) {
      for (const auto& block : blocks) {
        out += block->ToString(mode, indentation_level + 1);
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
  return os << process.ToString(common::FormatMode::kContextual);
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
    return fmt::format_to(
        ctx.out(), "{}",
        process.ToString(lyra::common::FormatMode::kContextual));
  }
};
