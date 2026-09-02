#pragma once

#include <cstdint>
#include <vector>

#include "lyra/lir/class_id.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// Whether instances of this class are nodes of the runtime object tree.
// Extending a base the runtime library defines is what puts an object in that
// tree, and a scope is the only thing that extends one. Extending a class --
// this unit's or another's -- is what a class of the source language does, and
// says nothing about the tree.
auto IsObjectTreeNode(const Class& cls) -> bool;

// A value carries the members its class's bases declare before the ones its own
// class declares. The next two queries are that one order read twice, as a list
// and as a position, so neither may state it differently from the other.

// The members a value of this class owns, in the order its storage holds them.
auto StorageMembers(const CompilationUnit& unit, ClassId cls)
    -> std::vector<Member>;

// Where the member named by `member` sits in the storage of any value that
// carries it, which is the same position in the declaring class and in every
// class extending it.
auto MemberPosition(const CompilationUnit& unit, MemberRef member)
    -> std::uint32_t;

// Whether a value of `type` carries the members `declaration` declares -- true
// when the two are the same declaration, and when `type` is a class reaching
// `declaration` through the bases it extends. This is what makes a member step
// valid over a type other than the one that declares the member.
auto CarriesMembersOf(
    const CompilationUnit& unit, TypeId type, TypeId declaration) -> bool;

}  // namespace lyra::lir
