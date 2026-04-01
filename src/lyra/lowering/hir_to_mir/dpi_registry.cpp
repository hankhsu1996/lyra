#include "lyra/lowering/hir_to_mir/dpi_registry.hpp"

#include <format>
#include <utility>

#include "lyra/common/internal_error.hpp"

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

auto DesignDpiExports::Insert(DpiExportInfo info) -> bool {
  auto symbol = info.symbol;

  // 1. Check duplicate symbol first.
  if (entries_.contains(symbol)) {
    return false;
  }

  // 2. Check c_name uniqueness across all exports.
  auto c_name = info.c_name;
  if (auto it = c_name_to_symbol_.find(c_name);
      it != c_name_to_symbol_.end() && it->second != symbol) {
    throw common::InternalError(
        "DesignDpiExports::Insert",
        std::format(
            "duplicate DPI export C name '{}' (symbols {} and {})", c_name,
            it->second.value, symbol.value));
  }

  // 3. Both checks passed: insert into both maps.
  entries_.emplace(symbol, std::move(info));
  c_name_to_symbol_.emplace(std::move(c_name), symbol);
  return true;
}

auto DesignDpiExports::Find(SymbolId symbol) const -> const DpiExportInfo* {
  auto it = entries_.find(symbol);
  if (it == entries_.end()) {
    return nullptr;
  }
  return &it->second;
}

}  // namespace lyra::lowering::hir_to_mir
