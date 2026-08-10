#pragma once

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// The type of the storage a place names. The base contributes the storage the
// chain starts from: a place local names its own storage, and any other base is
// a value, which names storage only once dereferenced. Each dereference names
// the storage behind what the chain has reached -- a reference's referent, or
// what a capability wrapper represents; each member step selects a member of
// the class the chain has reached.
auto PlaceType(
    const CompilationUnit& unit, const Function& fn, const Place& place)
    -> TypeId;

}  // namespace lyra::lir
