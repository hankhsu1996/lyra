#pragma once

#include <string>
#include <vector>

#include <slang/ast/Symbol.h>

namespace lyra::common {

using SymbolRef = const slang::ast::Symbol*;

// Format hierarchical path for display: [inst1, inst2], target ->
// "inst1.inst2.target"
inline auto FormatHierarchicalPath(
    const std::vector<SymbolRef>& instance_path, SymbolRef target_symbol)
    -> std::string {
  std::string result;
  for (const auto& inst_sym : instance_path) {
    result += inst_sym->name;
    result += ".";
  }
  result += target_symbol->name;
  return result;
}

}  // namespace lyra::common
