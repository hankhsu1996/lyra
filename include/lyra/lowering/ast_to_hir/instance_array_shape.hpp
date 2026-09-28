#pragma once

#include <optional>
#include <vector>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/numeric/ConstantValue.h>

namespace lyra::lowering::ast_to_hir {

// The resolved shape of an instance array: the range each dimension declares,
// outermost first, and the per-element leaf instance that names the target
// unit. The range is what the declaration states and what the member's type
// carries; how many elements it has is derived from it, since everything below
// reaches an element by position.
struct InstanceArrayShape {
  std::vector<slang::ConstantRange> ranges;
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
  std::vector<slang::ConstantRange> ranges;
  const slang::ast::Symbol* level = &array;
  while (level->kind == slang::ast::SymbolKind::InstanceArray) {
    const auto& arr = level->as<slang::ast::InstanceArraySymbol>();
    if (arr.elements.empty()) {
      return std::nullopt;
    }
    ranges.push_back(arr.range);
    level = arr.elements.front();
  }
  return InstanceArrayShape{
      .ranges = std::move(ranges),
      .leaf = &level->as<slang::ast::InstanceSymbol>()};
}

// What the scope declaring `instance` declared for it: the instance itself, or
// the whole array it is an element of. A multi-dimensional array nests one
// array symbol per dimension, but the declaration is one member spanning them
// all, and the instance's `arrayPath` carries every index.
inline auto OwnerOfInstance(const slang::ast::InstanceSymbol& instance)
    -> const slang::ast::Symbol& {
  const slang::ast::Symbol* owner = &instance;
  while (owner->getParentScope() != nullptr &&
         owner->getParentScope()->asSymbol().kind ==
             slang::ast::SymbolKind::InstanceArray) {
    owner = &owner->getParentScope()->asSymbol();
  }
  return *owner;
}

}  // namespace lyra::lowering::ast_to_hir
