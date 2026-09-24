#pragma once

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// A body reached through a table that holds entries of every prototype -- a
// class's record of its behaviors (LRM 8.20), a scope's answer to a name a
// hierarchical reference spells (LRM 23.6). The table erases the receiver to
// `receiver_type`, so the entry takes it as that, reads it back as `cls`, and
// forwards its own formals to `target`, a body of `cls`. The caller restores
// the prototype from the same declaration this is built from, which is what
// makes the two agree without a promise between them.
auto BuildForwardingEntry(
    mir::CompilationUnit& unit, const mir::Class& cls, mir::ClassId cls_id,
    mir::CallableId target, mir::TypeId receiver_type) -> mir::CallableCode;

}  // namespace lyra::lowering::hir_to_mir
