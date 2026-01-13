#pragma once

#include <cstdint>
#include <string>
#include <unordered_map>
#include <vector>

#include <slang/ast/Symbol.h>

namespace lyra::common {

using SymbolRef = const slang::ast::Symbol*;

using SymbolId = uint32_t;
inline constexpr SymbolId kInvalidSymbolId = UINT32_MAX;

struct SymbolInfo {
  std::string name;
};

class SymbolTable {
 public:
  auto Register(const slang::ast::Symbol* slang_sym) -> SymbolId {
    auto it = slang_to_id_.find(slang_sym);
    if (it != slang_to_id_.end()) {
      return it->second;
    }
    auto id = static_cast<SymbolId>(symbols_.size());
    symbols_.push_back(SymbolInfo{.name = std::string(slang_sym->name)});
    slang_to_id_.emplace(slang_sym, id);
    return id;
  }

  [[nodiscard]] auto GetInfo(SymbolId id) const -> const SymbolInfo& {
    return symbols_[id];
  }

  [[nodiscard]] auto Size() const -> size_t {
    return symbols_.size();
  }

 private:
  std::vector<SymbolInfo> symbols_;
  std::unordered_map<const slang::ast::Symbol*, SymbolId> slang_to_id_;
};

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
