#pragma once

#include <functional>
#include <string>

#include "lyra/hir/published_scope.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::hir {

// How the bytes of a published structure write a reference to a scope or a
// declaration of a unit: the unit by the name the structure gives it and the
// path inside it. A structure states its references by name and its own
// content by value, so whoever compares two structures decides what makes two
// references alike, and the rest is compared as it stands.
using ReferenceBytes = std::function<std::string(
    const std::string& unit_name, const support::DefPath& path)>;

// What the class of a generate block of `signature` publishes, as bytes that
// two such classes share exactly when they publish alike: every type, class
// and covergroup declared in the block, and every member, callable, construct,
// view and clocking block, each type written out by what it is rather than by
// where its pool holds it, and every reference through `reference`. A generate
// construct inside it is written as the source wrote it: which of its blocks a
// construction built is the construction's, and no part of the class
// (LRM 27.5).
[[nodiscard]] auto BlockClassBytes(
    const UnitSignature& signature, const ScopeClassSignature& scope,
    const ReferenceBytes& reference) -> std::string;

// What `signature` publishes, as bytes two signatures share exactly when they
// publish alike, the same way, of what is declared outside every generate
// block. Which blocks its own generate constructs built is no part of it
// (LRM 27.5), and the class of a block is written only where something names
// it, through `reference`, and not by its content.
[[nodiscard]] auto StructureBytes(
    const UnitSignature& signature, const ReferenceBytes& reference)
    -> std::string;

}  // namespace lyra::hir
