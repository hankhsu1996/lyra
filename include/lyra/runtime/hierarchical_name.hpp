#pragma once

// How a hierarchical name that reaches the run from outside the design -- from
// foreign code (LRM 35.5.3, 38.21) or the run's own command line -- is read
// into the names it is made of, and how one of those names is matched against
// a scope's own. Every reader of such a name reads it here, so a name means
// the same scope whoever is handed it.

#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <vector>

#include "lyra/abi/records.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"

namespace lyra::runtime {

class Scope;

// One of the names a hierarchical name is made of (LRM 23.6): an identifier,
// and the constant indices selecting one element of what it declares.
struct NamePart {
  // Without the backslash and the white space an escaped identifier is
  // written with, which are no part of it (LRM 5.6.1).
  std::string_view identifier;
  std::vector<std::int64_t> indices;
};

struct HierarchicalName {
  // Where the name begins with a package's name and `::` (LRM 26.3), that
  // package's.
  std::optional<std::string_view> package;
  std::vector<NamePart> parts;
};

// `text` read as a hierarchical name, none where it is not one.
[[nodiscard]] auto ReadHierarchicalName(std::string_view text)
    -> std::optional<HierarchicalName>;

// The identifier `written` spells as a hierarchical name writes it: the text
// itself, or, for an escaped identifier, what lies between the backslash and
// the white space ending it (LRM 5.6.1).
[[nodiscard]] auto IdentifierOf(std::string_view written) -> std::string_view;

// The child of `scope` that `part` names, none where no child is it.
[[nodiscard]] auto ChildNamed(Scope& scope, const NamePart& part) -> Scope*;

// How far `parts` reach down the constructed tree from `from`: the scope the
// longest leading run of them names, `from` itself where none does, and how
// many parts that run is.
struct ScopeReached {
  Scope* scope = nullptr;
  std::size_t parts = 0;
};
[[nodiscard]] auto LongestNamedPrefix(
    Scope& from, std::span<const NamePart> parts) -> ScopeReached;

// The declarations the source named that `innermost` lies in, `innermost`
// included where it is named, outermost first: what a name written inside it
// is made of below the object holding them (LRM 23.6). A scope the source gave
// no name adds none to a name.
[[nodiscard]] auto NamedEnclosingScopes(
    const abi::LexicalScopeRecord* innermost)
    -> std::vector<const abi::LexicalScopeRecord*>;

// The declaration among `declarations` that `identifier` names directly in
// `enclosing`, or directly in the object holding them where it is none: a
// name reaches only what lies directly in the scope it is relative to, so
// nothing a scope the source gave no name declares is reached from outside it
// (LRM 9.3.4). None where no declaration is so named.
[[nodiscard]] auto LexicalDeclarationNamed(
    std::span<const abi::LexicalScopeRecord* const> declarations,
    const abi::LexicalScopeRecord* enclosing, std::string_view identifier)
    -> const abi::LexicalScopeRecord*;

}  // namespace lyra::runtime
