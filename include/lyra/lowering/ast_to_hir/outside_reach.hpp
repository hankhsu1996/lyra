#pragma once

// What follows for an instance from where it stands in the hierarchy, as
// opposed to what its instantiation hands it.
//
// A body's meaning is not settled by its definition and parameters alone,
// since a name resolves per instance (LRM 23.8), and what a scope publishes
// names the instances its names and its children's names reach. So a name
// concerns each instance it leaves on its way to where it lands, and an
// instance states, for the one holding it, every name below it that leaves
// that one too.

#include <optional>
#include <unordered_map>
#include <vector>

#include "lyra/lowering/ast_to_hir/climb.hpp"

namespace slang::ast {
class GenerateBlockSymbol;
class InstanceBodySymbol;
class InstanceSymbol;
class ParameterSymbol;
class Scope;
class Symbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// One name the instance's own body writes: the declaration the front end found,
// the symbol each step of the path written to it names, and where it lands once
// it leaves the instance, nothing where it stays inside.
struct NameUse {
  const slang::ast::Symbol* found = nullptr;
  std::vector<const slang::ast::Symbol*> path;
  std::optional<ClimbAnchor> landing;
};

// What one walk of the instance's own body finds it reaching outside itself:
// `uses`, every name it writes, in the order it writes them; `class_scopes`,
// the scope of each class and covergroup another instance declares that a type
// it states holds; and `foreign_parameters`, each parameter of another
// instance it reads. `names_below` is what the instances below it fix for
// it: where each name they write that leaves this one too lands.
struct OutsideReach {
  std::vector<NameUse> uses;
  std::vector<const slang::ast::Scope*> class_scopes;
  std::vector<const slang::ast::ParameterSymbol*> foreign_parameters;
  std::vector<ClimbAnchor> names_below;

  // Where each name of `uses` that leaves the instance lands, in the order the
  // body writes them.
  [[nodiscard]] auto Climbs() const -> std::vector<ClimbAnchor>;
};

using OutsideReaches =
    std::unordered_map<const slang::ast::InstanceSymbol*, OutsideReach>;

// The instances standing in `scope`, an instance's body or a generate block,
// wherever its generate blocks and instance arrays place them, in the order
// the source writes them. A held instance's body is not entered.
[[nodiscard]] auto InstancesHeldIn(const slang::ast::Scope& scope)
    -> std::vector<const slang::ast::InstanceSymbol*>;

// The context of `inst`, worked out once and kept in `known`. An instance's
// answer reads the body it is read through and, for each instance that body
// holds, that instance's answer; it never reads the body of an instance it
// holds. So what an instance passes upward is something it states about
// itself, and one compiled alone asks for exactly the answers it needs. An
// instance the front end left pointing at another's body has that instance's
// context, and no body is read for it.
[[nodiscard]] auto OutsideReachOf(
    const slang::ast::InstanceSymbol& inst, OutsideReaches& known)
    -> const OutsideReach&;

}  // namespace lyra::lowering::ast_to_hir
