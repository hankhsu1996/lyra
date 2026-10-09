#pragma once

// What follows for an instance from where it stands in the hierarchy, as
// opposed to what its instantiation hands it.
//
// A body's meaning is not settled by its definition and parameters alone,
// since a name resolves per instance (LRM 23.8) and what is written elsewhere
// reaches one instance and not another (LRM 23.10.1, 23.11, 33.4), and a
// unit's code names the class of every instance it builds. So whatever tells
// an instance apart tells apart every instance holding it: a name concerns
// each instance it leaves on its way to where it lands, and something written
// elsewhere concerns each instance above the one it is about.

#include <cstdint>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/lowering/ast_to_hir/climb.hpp"
#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"

namespace slang::ast {
class InstanceBodySymbol;
class InstanceSymbol;
class Scope;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// A hierarchical name an instance below writes that leaves this one too:
// which of its writer's names it is, in the order the writer's body writes
// those that leave it, the scope it lands in, and the instance that scope
// stands in.
struct NameLanding {
  std::uint32_t written = 0;
  const slang::ast::Scope* scope = nullptr;
  const slang::ast::InstanceBodySymbol* instance = nullptr;
};

// One thing an instance below fixes for this one: `path` leads from this
// instance to that one, and `what` is the name that instance writes or what
// was written elsewhere about it.
struct FixedBelow {
  std::string path;
  std::variant<NameLanding, OverrideEffect> what;
};

// `climbs` is where each name the instance's own body writes lands once it
// leaves the instance, in the order the body writes them. `below` is what the
// instances below it fix for it.
struct InstanceContext {
  std::vector<ClimbAnchor> climbs;
  std::vector<FixedBelow> below;
};

using InstanceContexts =
    std::unordered_map<const slang::ast::InstanceSymbol*, InstanceContext>;

// The context of `inst`, worked out once and kept in `known`. An instance's
// answer reads its own body and, for each instance that body holds, that
// instance's answer; it never reads the body of an instance it holds. So what
// an instance passes upward is something it states about itself, and one
// compiled alone asks for exactly the answers it needs.
[[nodiscard]] auto InstanceContextOf(
    const slang::ast::InstanceSymbol& inst, InstanceContexts& known)
    -> const InstanceContext&;

}  // namespace lyra::lowering::ast_to_hir
