#pragma once

#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The runtime library and a dispatching call hold an object as the root every
// object is, while a body of a class takes the object as that class. So what
// they enter is a body of the class that takes no receiver: it is handed the
// object as the root, views it as the class, and does one thing on it.
//
//   static R entry(Root* object, Args... args) {
//     return static_cast<Class*>(object)->method(args...);
//   }
//
// The body the library enters for `callable` of `cls`, class `id` of `unit`:
// the callable itself where it takes no receiver -- a static method (LRM 8.10)
// -- and otherwise one such body calling exactly that callable on the object,
// added to `cls`, of the prototype `entry`: handed the object first, then what
// the callable takes after its receiver.
auto EntryOf(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    mir::CallableId callable, mir::TypeId entry) -> mir::CallableId;

// The prototype of the body the library enters for `target`, the code of a
// callable taking its object as a receiver, handed the object as `object`: the
// object, then what the callable takes after its receiver, answering what it
// answers.
[[nodiscard]] auto EntryPrototypeOf(
    const mir::CompilationUnit& unit, const mir::CallableCode& target,
    mir::TypeId object) -> mir::TypeId;

// Ends `body` with `call`, a call resulting in `result`, completing as that
// call does. A call completing as a coroutine is awaited, which completes the
// body as a coroutine too, with what that one completed with.
void CompleteWith(
    const mir::CompilationUnit& unit, mir::Builder body, mir::ExprId call,
    mir::TypeId result);

}  // namespace lyra::lowering::hir_to_mir
