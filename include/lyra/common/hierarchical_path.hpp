#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include <fmt/core.h>

#include "lyra/common/symbol.hpp"

namespace lyra::common {

// Single representation for a hierarchical path element.
// Used throughout the codebase for instance paths and generate array indexing.
struct HierarchicalPathElement {
  SymbolId symbol;                     // The scope/instance symbol
  std::optional<int32_t> array_index;  // Index for generate arrays

  explicit HierarchicalPathElement(SymbolId sym)
      : symbol(sym), array_index(std::nullopt) {
  }

  HierarchicalPathElement(SymbolId sym, int32_t idx)
      : symbol(sym), array_index(idx) {
  }

  auto operator==(const HierarchicalPathElement& other) const -> bool {
    return symbol == other.symbol && array_index == other.array_index;
  }
};

// Format a hierarchical path (shows sym#id format).
inline auto FormatHierarchicalPath(
    const std::vector<HierarchicalPathElement>& elements, SymbolId target)
    -> std::string {
  std::string result;
  for (const auto& elem : elements) {
    if (!result.empty()) {
      result += ".";
    }
    result += fmt::format("sym#{}", elem.symbol);
    if (elem.array_index) {
      result += fmt::format("[{}]", *elem.array_index);
    }
  }
  if (!result.empty()) {
    result += ".";
  }
  result += fmt::format("sym#{}", target);
  return result;
}

}  // namespace lyra::common
