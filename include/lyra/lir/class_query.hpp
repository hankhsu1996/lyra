#pragma once

#include "lyra/lir/compilation_unit.hpp"

namespace lyra::lir {

// Whether instances of this class are nodes of the runtime object tree.
// Extending a base the runtime library defines is what puts an object in that
// tree, and a scope is the only thing that extends one. Extending a class --
// this unit's or another's -- is what a class of the source language does, and
// says nothing about the tree.
auto IsObjectTreeNode(const Class& cls) -> bool;

}  // namespace lyra::lir
