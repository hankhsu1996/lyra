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
#include <functional>
#include <string>
#include <unordered_map>
#include <variant>
#include <vector>

#include "lyra/lowering/ast_to_hir/climb.hpp"
#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"

namespace slang::ast {
class GenerateBlockSymbol;
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
// instances below it fix for it. `a_name_leaves_its_writer` is whether the
// instance or any instance below it writes a name that leaves the instance
// writing it, wherever that name lands: one landing in this instance fixes
// nothing for it, and still lands somewhere else for another instance of the
// same text.
struct InstanceContext {
  std::vector<ClimbAnchor> climbs;
  std::vector<FixedBelow> below;
  bool a_name_leaves_its_writer = false;
};

using InstanceContexts =
    std::unordered_map<const slang::ast::InstanceSymbol*, InstanceContext>;

// Whether an instance is read through a body of its own. One that is not was
// left by the front end pointing at another's, which it does only where no
// name leaves the instance and nothing written elsewhere reaches it or an
// instance below it.
using HasBodyOfItsOwnFn =
    std::function<bool(const slang::ast::InstanceSymbol&)>;

// The context of `inst`, worked out once and kept in `known`. An instance's
// answer reads its own body and, for each instance that body holds, that
// instance's answer; it never reads the body of an instance it holds. So what
// an instance passes upward is something it states about itself, and one
// compiled alone asks for exactly the answers it needs. An instance with no
// body of its own has nothing to state, and no body is read for it.
[[nodiscard]] auto InstanceContextOf(
    const slang::ast::InstanceSymbol& inst, InstanceContexts& known,
    const HasBodyOfItsOwnFn& has_body_of_its_own) -> const InstanceContext&;

// Something written elsewhere about an instance a generate block holds, or
// about one below such an instance: `path` leads from the block to it.
struct OverriddenBelow {
  std::string path;
  OverrideEffect effect;
};

// Everything written elsewhere that reaches an instance standing in `block`
// or below one, in the order the block holds them. It tells block instances of
// one text apart the way it tells instances of one definition apart: the scope
// names the class of every instance it builds.
[[nodiscard]] auto OverridesBelow(
    const slang::ast::GenerateBlockSymbol& block, InstanceContexts& known,
    const HasBodyOfItsOwnFn& has_body_of_its_own)
    -> std::vector<OverriddenBelow>;

}  // namespace lyra::lowering::ast_to_hir
