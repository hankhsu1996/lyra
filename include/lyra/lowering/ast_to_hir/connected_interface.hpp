#pragma once

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>

#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"

namespace lyra::lowering::ast_to_hir {

// What an interface port is bound to: the interface instance, and the modport
// narrowing which of its members the port reaches (LRM 25.3, 25.5). Either is
// null when the connection supplies none -- an unconnected port has no
// instance, and a port reaching the whole interface has no modport, which is
// how the language spells that view.
struct ConnectedInterface {
  const slang::ast::InstanceSymbol* instance;
  const slang::ast::ModportSymbol* modport;
};

// A port declared with a range names as many instances as the range has
// elements, every one of them the same interface specialization, so any element
// answers for all of them.
//
// The connection is passed in rather than looked up, because which one applies
// is the caller's question: a parent deducing what it fixed for a child reads
// the connection it wrote, while a unit reading its own port reaches the one
// belonging to the instance whose body is being compiled. Those are the same
// connection only when the two agree on which instance is meant.
inline auto ConnectedInterfaceOf(
    const slang::ast::InterfacePortSymbol::IfaceConn& connection)
    -> ConnectedInterface {
  const auto [connected, modport] = connection;
  const auto leaf = [&]() -> const slang::ast::InstanceSymbol* {
    if (connected == nullptr) {
      return nullptr;
    }
    if (const auto* instance = connected->as_if<slang::ast::InstanceSymbol>()) {
      return instance;
    }
    const auto* array = connected->as_if<slang::ast::InstanceArraySymbol>();
    if (array == nullptr) {
      return nullptr;
    }
    const auto shape = ResolveInstanceArrayShape(*array);
    return shape.has_value() ? shape->leaf : nullptr;
  };
  return ConnectedInterface{.instance = leaf(), .modport = modport};
}

}  // namespace lyra::lowering::ast_to_hir
