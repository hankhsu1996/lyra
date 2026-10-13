#pragma once

#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The runtime library holds an object as a type it knows -- a scope -- while a
// body of a class takes the object as that class. So what the library enters is
// a body of the class that takes no receiver: it is handed the object as the
// library holds it, views it as the class, and does one thing on it.
//
//   static R entry(Library* object, Args... args) {
//     return static_cast<Class*>(object)->method(args...);
//   }
//
// The body the library enters for `callable` of `cls`, class `id` of `unit`:
// the callable itself where it takes no receiver -- a static method (LRM 8.10)
// -- and otherwise one such body calling exactly that callable on the object,
// added to `cls`. `object` is the type the library hands the object over as.
auto EntryOf(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    mir::CallableId callable, mir::TypeId object) -> mir::CallableId;

}  // namespace lyra::lowering::hir_to_mir
