#pragma once

#include <string>
#include <vector>

#include <fmt/format.h>

#include "lyra/common/indent.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::lir {

struct BasicBlock {
  // Label for this basic block, used for branch targets
  LabelRef label;

  // Instructions contained in this basic block
  std::vector<Instruction> instructions;

  [[nodiscard]] auto ToString(
      common::FormatMode mode = common::FormatMode::kPlain,
      int indentation_level = 0) const -> std::string {
    std::string result;

    // Format the block label
    if (mode == common::FormatMode::kContextual) {
      result = fmt::format("{}{}:\n", common::Indent(indentation_level), label);
    } else {
      result = fmt::format("{}:\n", label);
    }

    // Format each instruction
    for (const auto& instr : instructions) {
      if (mode == common::FormatMode::kContextual) {
        result += fmt::format(
            "{}{}\n", common::Indent(indentation_level + 1), instr.ToString());
      } else {
        result += fmt::format("{}\n", instr.ToString());
      }
    }
    return result;
  }
};

inline auto operator<<(std::ostream& os, const BasicBlock& block)
    -> std::ostream& {
  return os << block.ToString(common::FormatMode::kContextual);
}

}  // namespace lyra::lir

template <>
struct fmt::formatter<lyra::lir::BasicBlock> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::BasicBlock& block, FormatContext& ctx) const {
    return fmt::format_to(
        ctx.out(), "{}", block.ToString(lyra::common::FormatMode::kContextual));
  }
};
