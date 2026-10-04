#pragma once

#include <span>
#include <vector>

#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The runtime library holds an object as a type it knows -- a scope, or nothing
// in particular -- while a body of a class takes the object as that class. So
// what the library enters is a body of the class that takes no receiver: it is
// handed the object as the library holds it, views it as the class, and does
// one thing on it.
//
//   static R entry(Library* object, Args... args) {
//     return static_cast<Class*>(object)->method(args...);
//   }
//
// Each function below adds one such body to `cls`, class `id` of `unit`, and
// answers which callable it is. `object` is the type the library hands the
// object over as.

// What a callable takes after its receiver, and what it results in.
struct Prototype {
  std::vector<mir::TypeId> params;
  mir::TypeId result;
};

// The body answering where field `slot` is.
//
//   static void* entry(Library* object) {
//     return &static_cast<Class*>(object)->field;
//   }
auto AddFieldAddressEntry(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    mir::FieldId slot, mir::TypeId object) -> mir::CallableId;

// The body making the call the object decides (LRM 8.20) on `slot`, with what
// `prototype` takes. It is the behavior's own call, so it runs what the
// object's own class answers.
//
//   static R entry(Library* object, Args... args) {
//     return static_cast<Introducer*>(object)->behavior(args...);
//   }
auto AddDispatchingEntry(
    const mir::CompilationUnit& unit, mir::Class& cls,
    const mir::VirtualSlot& slot, const Prototype& prototype,
    mir::TypeId object) -> mir::CallableId;

// The body the library enters for `callable`: the callable itself where it
// takes no receiver -- a static method (LRM 8.10) -- and otherwise one calling
// exactly that callable on the object.
auto EntryOf(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    mir::CallableId callable, mir::TypeId object) -> mir::CallableId;

// The body the library enters for each name of `named`: for a method the
// object decides, one making that call on the object; otherwise the entry of
// the body the class defines. A name whose body the class does not define is
// left out.
auto NamedEntries(
    const mir::CompilationUnit& unit, mir::ClassId id, mir::Class& cls,
    std::span<const mir::NamedCallable> named, mir::TypeId object)
    -> std::vector<mir::NamedCallable>;

}  // namespace lyra::lowering::hir_to_mir
