#include "lyra/lowering/hir_to_mir/dpi_registry.hpp"

#include <utility>

namespace lyra::lowering::hir_to_mir {

auto DesignDpiImports::Insert(DpiImportInfo info) -> bool {
  auto symbol = info.symbol;
  auto [it, inserted] = entries_.emplace(symbol, std::move(info));
  return inserted;
}

auto DesignDpiImports::Find(SymbolId symbol) const -> const DpiImportInfo* {
  auto it = entries_.find(symbol);
  if (it == entries_.end()) {
    return nullptr;
  }
  return &it->second;
}

}  // namespace lyra::lowering::hir_to_mir
