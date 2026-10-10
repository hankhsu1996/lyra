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
// `path` leads from this instance to its writer, `written` is which of the
// writer's names it is, in the order the writer's body writes those that
// leave it, and it lands in `scope`, which stands in `instance`.
struct NameLandingBelow {
  std::string path;
  std::uint32_t written = 0;
  const slang::ast::Scope* scope = nullptr;
  const slang::ast::InstanceBodySymbol* instance = nullptr;
};

// Something written elsewhere about an instance below this one, or below a
// generate block: `path` leads to that instance.
struct OverriddenBelow {
  std::string path;
  OverrideEffect effect;
};

// `climbs` is where each name the instance's own body writes lands once it
// leaves the instance, in the order the body writes them. The other two are
// what the instances below it fix for it: the names they write that leave it
// too, and what is written elsewhere about them.
struct InstanceContext {
  std::vector<ClimbAnchor> climbs;
  std::vector<NameLandingBelow> names_below;
  std::vector<OverriddenBelow> overridden_below;
};

using InstanceContexts =
    std::unordered_map<const slang::ast::InstanceSymbol*, InstanceContext>;

// Whether an instance is read through a body of its own. One that is not was
// left by the front end pointing at another's, which it does only where every
// name leaving the body resolves alike from both instances and nothing
// written elsewhere reaches either or an instance below it.
using HasBodyOfItsOwnFn =
    std::function<bool(const slang::ast::InstanceSymbol&)>;

// The context of `inst`, worked out once and kept in `known`. An instance's
// answer reads its own body and, for each instance that body holds, that
// instance's answer; it never reads the body of an instance it holds. So what
// an instance passes upward is something it states about itself, and one
// compiled alone asks for exactly the answers it needs. An instance with no
// body of its own has the context of the instance whose body it shares, and
// no body is read for it.
[[nodiscard]] auto InstanceContextOf(
    const slang::ast::InstanceSymbol& inst, InstanceContexts& known,
    const HasBodyOfItsOwnFn& has_body_of_its_own) -> const InstanceContext&;

// Everything written elsewhere that reaches an instance standing in `block`
// or below one, in the order the block holds them. It tells block instances of
// one text apart the way it tells instances of one definition apart: the scope
// names the class of every instance it builds.
[[nodiscard]] auto OverridesBelow(
    const slang::ast::GenerateBlockSymbol& block, InstanceContexts& known,
    const HasBodyOfItsOwnFn& has_body_of_its_own)
    -> std::vector<OverriddenBelow>;

}  // namespace lyra::lowering::ast_to_hir
