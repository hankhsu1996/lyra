#pragma once

#include <span>

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

// The body handed the object as `object`, a class each of `bodies` is a body
// of a class extending, which enters the body of whichever of those classes the
// object is with what it was handed after the object, completing as that call
// does. It is entered on the object, which is its receiver, so the class the
// object was handed over as states it as a method of its own. Every class of
// `bodies` is settled in `unit`, and their bodies take the same things after
// the receiver, being bodies of one subroutine.
//
//   auto Published::f(Args... args) -> R {
//     if (IsOfClass(&First::sv_definition)) {
//       return static_cast<First*>(this)->f_body(args...);
//     }
//     return static_cast<Last*>(this)->f_body(args...);
//   }
//
// The object is of exactly one of them, so the last is entered untested, and a
// class realized one way states no test at all.
auto ForwardingMethod(
    const mir::CompilationUnit& unit,
    std::span<const mir::CallableTarget> bodies, mir::TypeId object)
    -> mir::CallableCode;

}  // namespace lyra::lowering::hir_to_mir
