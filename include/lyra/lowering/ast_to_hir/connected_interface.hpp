#pragma once

#include <vector>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>

#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"

namespace lyra::lowering::ast_to_hir {

// What an interface port is bound to: the interface instances, in row-major
// order of their positions, and the modport narrowing which of their members
// the port reaches (LRM 25.3, 25.5). An unconnected port has no instance, and a
// port reaching the whole interface has no modport, which is how the language
// spells that view.
//
// A port declared with a range names as many instances as the range has
// elements. They take the one assignment their instantiation wrote (LRM
// 23.3.2), but something written elsewhere may reach one of them (LRM 23.10.1),
// so each is its own. Their order needs no correction: what a connection
// resolves to is already rebased onto the range the port declared, matched
// left index to left index (LRM 23.3.3.5), so the position each takes here is
// the port's own position for it. The unit publishing the port and the parent
// binding it both read the instances from here, so the two count the port's
// positions alike.
//
// The connection is passed in rather than looked up, because which one applies
// is the caller's question: a parent deducing what it fixed for a child reads
// the connection it wrote, while a unit reading its own port reaches the one
// belonging to the instance whose body is being compiled. Those are the same
// connection only when the two agree on which instance is meant.
struct ConnectedInterface {
  std::vector<const slang::ast::InstanceSymbol*> instances;
  const slang::ast::ModportSymbol* modport;
};

inline auto ConnectedInterfaceOf(
    const slang::ast::InterfacePortSymbol::IfaceConn& connection)
    -> ConnectedInterface {
  const auto [connected, modport] = connection;
  ConnectedInterface out{.instances = {}, .modport = modport};
  if (connected == nullptr) {
    return out;
  }
  if (const auto* instance = connected->as_if<slang::ast::InstanceSymbol>()) {
    out.instances.push_back(instance);
    return out;
  }
  if (const auto* array = connected->as_if<slang::ast::InstanceArraySymbol>()) {
    if (auto shape = ResolveInstanceArrayShape(*array)) {
      out.instances = std::move(shape->elements);
    }
  }
  return out;
}

}  // namespace lyra::lowering::ast_to_hir
