#pragma once

#include <vector>

#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir {

// What `unit` reads of other units' signatures, each once, in the order its
// code and its own declarations first read it. It is read off the finished
// unit, as a C++ compiler's dependency file lists the files it actually read,
// so no read can happen without being listed: a member reached, a method
// called or dispatched on, a construction, a conversion between two classes, a
// static property, a member of another unit's namespace, a value of a record
// another unit declares, an object owned, which has to be complete to move or
// end, and the entry making an object of another unit's body. Holding a handle
// reads nothing of its class. A class read is read with its lineage, the
// classes it extends and the interface classes it is a value of, because its
// layout and its dispatch positions follow theirs; what another unit's class
// holds is that unit's to build and end.
[[nodiscard]] auto ConsumedSignaturesOf(const CompilationUnit& unit)
    -> std::vector<ConsumedSignature>;

}  // namespace lyra::mir
