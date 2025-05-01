#pragma once

#include <string>
#include <vector>

#include <fmt/format.h>

#include "lir/instruction.hpp"

namespace lyra::lir {

struct BasicBlock {
  // Label for this basic block, used for branch targets
  std::string label;

  // Instructions contained in this basic block
  std::vector<Instruction> instructions;

  [[nodiscard]] auto ToString() const -> std::string {
    std::string result = fmt::format("{}:\n", label);
    for (const auto& instr : instructions) {
      result += fmt::format("  {}\n", instr.ToString());
    }
    return result;
  }
};

inline auto operator<<(std::ostream& os, const BasicBlock& block)
    -> std::ostream& {
  return os << block.ToString();
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
    return fmt::format_to(ctx.out(), "{}", block.ToString());
  }
};
