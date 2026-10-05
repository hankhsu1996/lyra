#pragma once

#include <optional>
#include <span>
#include <variant>
#include <vector>

namespace slang::ast {
class HierarchicalReference;
class InstanceBodySymbol;
class Scope;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Where a hierarchical name lands first once it leaves the instance it is
// written in: the scope the upward search found it in (LRM 23.8), or the
// top-level instance a path from the top starts at (LRM 23.6). `scope` is an
// instance's body or a generate block, `instance` the body of the instance it
// stands in, and the name continues downward from there.
//
// Which scope that is depends on where the instance stands, so two instances
// of one definition may land differently; everything the name reaches after it
// is a declaration of whatever stands there. `carries` holds the scopes the
// classes the name passes through belong to (LRM 6.22) -- the class of each
// handle its path holds, and the class of each member it names -- since what
// the name spells below its landing is how the reader reaches them.
struct ClimbAnchor {
  const slang::ast::Scope* scope = nullptr;
  const slang::ast::InstanceBodySymbol* instance = nullptr;
  std::vector<const slang::ast::Scope*> carries;
};

// Where `reference`, written in `reader`, lands once it leaves `reader`, or
// nothing when it never does -- a name resolved inside the instance, or through
// one of its interface ports.
[[nodiscard]] auto ClimbOutOf(
    const slang::ast::HierarchicalReference& reference,
    const slang::ast::InstanceBodySymbol& reader) -> std::optional<ClimbAnchor>;

// The same for every hierarchical name `reader` writes, in the order the
// source writes them, leaving out those that never leave. A child instance's
// body is the child's, but what the instantiation hands the child is written
// here.
[[nodiscard]] auto ClimbsOutOf(const slang::ast::InstanceBodySymbol& reader)
    -> std::vector<ClimbAnchor>;

// Where a reader starts reaching a scope it writes no path to -- the scope a
// class it uses belongs to (LRM 6.22), which may stand in another instance:
// from the reader itself, or from the instance whose body is `body`.
struct FromReader {};
struct FromInstance {
  const slang::ast::InstanceBodySymbol* body = nullptr;
};
using ReachStart = std::variant<FromReader, FromInstance>;

// Where `reader`, whose names land at `climbs`, starts reaching `target`. From
// the reader where `target` stands in it; from where the first of its names
// passing through that class landed once it left the instance; or from the
// instance enclosing the reader that `target` stands in, where a type handed
// down from there belongs to it (LRM 6.20.3). Nothing where none applies.
//
// Each is chosen by what the source writes and by where its names land, which
// tells a unit apart already, so every instance of one unit starts from the
// same place and reaches the scope along the same path.
[[nodiscard]] auto StartOfReach(
    const slang::ast::Scope& target,
    const slang::ast::InstanceBodySymbol& reader,
    std::span<const ClimbAnchor> climbs) -> std::optional<ReachStart>;

}  // namespace lyra::lowering::ast_to_hir
