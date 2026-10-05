#pragma once

#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
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

// The body handed the object as `object`, viewing it as `cls` -- class `id` of
// `unit` -- and calling exactly `callable` on it with what it was handed after
// the object, completing as that call does. It is entered on the object, which
// is its receiver, so a class the object is also of can state it as a method
// of its own.
//
//   auto Published::f(Args... args) -> R {
//     return static_cast<Class*>(this)->f_body(args...);
//   }
auto ForwardingMethod(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& cls,
    mir::CallableId callable, mir::TypeId object) -> mir::CallableCode;

}  // namespace lyra::lowering::hir_to_mir
