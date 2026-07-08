#pragma once

#include <cstdint>
#include <optional>
#include <vector>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/InstanceSymbols.h>

namespace lyra::lowering::ast_to_hir {

// The resolved shape of an instance array: per-dimension element counts,
// outermost first, and the per-element leaf instance that names the target
// unit.
struct InstanceArrayShape {
  std::vector<std::uint32_t> dims;
  const slang::ast::InstanceSymbol* leaf;
};

// Resolves a (possibly multi-dimensional) instance array to its shape. A
// zero-element dimension (`Child c[0:-1]`, LRM 23.3.2) has no element to
// descend into or to name the unit from and constructs nothing, so the array
// contributes no member: nullopt. The declaration pass (which assigns each
// array member its identity) and the member-population pass (which builds the
// member) resolve the shape through this one predicate, so the identity a
// reference resolves through and the member the body builds cannot drift.
inline auto ResolveInstanceArrayShape(
    const slang::ast::InstanceArraySymbol& array)
    -> std::optional<InstanceArrayShape> {
  std::vector<std::uint32_t> dims;
  const slang::ast::Symbol* level = &array;
  while (level->kind == slang::ast::SymbolKind::InstanceArray) {
    const auto& arr = level->as<slang::ast::InstanceArraySymbol>();
    if (arr.elements.empty()) {
      return std::nullopt;
    }
    dims.push_back(static_cast<std::uint32_t>(arr.elements.size()));
    level = arr.elements.front();
  }
  return InstanceArrayShape{
      .dims = std::move(dims),
      .leaf = &level->as<slang::ast::InstanceSymbol>()};
}

}  // namespace lyra::lowering::ast_to_hir
