#pragma once

#include <vector>

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir {

// What `unit`'s code and declarations name of other units' declarations, each
// once, in the order they first name it, which is what a target writing the
// unit's text has to bring into view: a member reached, a method called or
// dispatched on, a construction, a static property, a member of another
// unit's namespace, a value of a record another unit declares, an object
// owned, which has to be complete to move or end, and the entry making an
// object of another unit's body. Holding a handle names nothing of its class,
// and neither does asking whether an object is of it (LRM 8.16), which names
// only its definition. A class named is named with its lineage, the classes it
// extends and the interfaces it is a value of, because its layout and its
// dispatch positions follow theirs; what another unit's class holds is that
// unit's to build and end. It is not what the unit depends on: a lowering can
// decide what a construct means from another unit's declaration and write
// nothing that names it.
[[nodiscard]] auto ReferencedDeclarationsOf(const CompilationUnit& unit)
    -> std::vector<ReferencedDeclaration>;

}  // namespace lyra::mir
