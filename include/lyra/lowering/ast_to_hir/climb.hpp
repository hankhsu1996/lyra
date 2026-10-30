#pragma once

#include <optional>
#include <span>
#include <string>
#include <variant>
#include <vector>

namespace slang::ast {
class HierarchicalReference;
class InstanceBodySymbol;
class Scope;
class Symbol;
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
//
// `blocks_passed` holds, for each run of generate blocks the name passes below
// its landing, the last block of the run, in the order the name passes them.
// Which block a construct built is each object's construction and no part of
// the class of the scope holding it (LRM 27.4, 27.5), so the reader compiles
// against the class of each block it passes as well as the landing's, and two
// instances whose names land in scopes of one class compile differently where
// the blocks they pass below it differ. The last block of a run names those
// before it, which enclose it in one instance.
//
// `reads` is the value the name reads where it names a constant -- a parameter
// or an enumeration's member -- which the reader's code is compiled with, so
// two instances landing in scopes of one class whose constants differ there
// compile differently.
struct ClimbAnchor {
  const slang::ast::Scope* scope = nullptr;
  std::vector<const slang::ast::Scope*> blocks_passed;
  const slang::ast::InstanceBodySymbol* instance = nullptr;
  std::vector<const slang::ast::Scope*> carries;
  std::optional<std::string> reads;
};

// Where a name written in `reader` lands once it leaves `reader`, or nothing
// when it never does -- a name resolved inside the instance, or through one of
// its interface ports. The name is `found`, the declaration the search found,
// reached by `written`, the path the source wrote to it, which is empty for a
// name written plainly. A plain name lands where the search found it, in the
// scope enclosing the reader that declares it, as a module declared inside
// another reaches what that module declares (LRM 23.4, 24.3).
[[nodiscard]] auto ClimbOutOf(
    const slang::ast::Symbol& found,
    const slang::ast::HierarchicalReference& written,
    const slang::ast::InstanceBodySymbol& reader) -> std::optional<ClimbAnchor>;

// Where a reader starts reaching a scope it writes no path to -- the scope a
// class it uses belongs to (LRM 6.22), which may stand in another instance:
// from the reader itself, or from `scope`, an instance's body or a generate
// block outside the reader.
struct FromReader {};
struct FromScope {
  const slang::ast::Scope* scope = nullptr;
};
using ReachStart = std::variant<FromReader, FromScope>;

// Where `reader`, whose names land at `climbs`, starts reaching `target`. From
// the reader where `target` stands in it; from `target` itself where the
// reader stands in it, as a type handed down from there does (LRM 6.20.3);
// from where the first of its names passing through that class landed once it
// left the instance; or from the instance enclosing the reader that `target`
// stands in. Nothing where none applies.
//
// Each is chosen by what the source writes and by where its names land, which
// tells a unit apart already, so every instance of one unit starts from the
// same place and reaches the scope along the same path.
[[nodiscard]] auto StartOfReach(
    const slang::ast::Scope& target,
    const slang::ast::InstanceBodySymbol& reader,
    std::span<const ClimbAnchor> climbs) -> std::optional<ReachStart>;

}  // namespace lyra::lowering::ast_to_hir
