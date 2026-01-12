#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/symbol.hpp"

namespace lyra::common {

// Single representation for a hierarchical path element.
// Used throughout the codebase for instance paths and generate array indexing.
struct HierarchicalPathElement {
  SymbolRef symbol;                    // The scope/instance symbol
  std::optional<int32_t> array_index;  // Index for generate arrays

  explicit HierarchicalPathElement(SymbolRef sym)
      : symbol(sym), array_index(std::nullopt) {
  }

  HierarchicalPathElement(SymbolRef sym, int32_t idx)
      : symbol(sym), array_index(idx) {
  }

  auto operator==(const HierarchicalPathElement& other) const -> bool {
    return symbol == other.symbol && array_index == other.array_index;
  }
};

// Format a hierarchical path for SV-style output (debugging, error messages).
// When a path element has an empty name (common for unnamed generate blocks),
// its array index is appended to the previous element's output.
// Example: [{gen_block, null}, {"", 0}] -> "gen_block[0]"
inline auto FormatHierarchicalPath(
    const std::vector<HierarchicalPathElement>& elements, SymbolRef target)
    -> std::string {
  std::string result;
  for (const auto& elem : elements) {
    if (elem.symbol->name.empty()) {
      // Empty name: append index directly (no dot separator)
      if (elem.array_index) {
        result += fmt::format("[{}]", *elem.array_index);
      }
    } else {
      // Non-empty name: add dot separator if needed
      if (!result.empty()) {
        result += ".";
      }
      result += elem.symbol->name;
      if (elem.array_index) {
        result += fmt::format("[{}]", *elem.array_index);
      }
    }
  }
  if (!result.empty()) {
    result += ".";
  }
  result += target->name;
  return result;
}

}  // namespace lyra::common
