#pragma once

#include <optional>
#include <span>

#include "lyra/hir/class_id.hpp"
#include "lyra/hir/method_id.hpp"
#include "lyra/lowering/hir_to_mir/class_shape.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/library_callback.hpp"

namespace lyra::lowering::hir_to_mir {

// A prototype taking `params`, in order, and answering `result`, entered on no
// object of its own: what the code of one operation of a class takes, the
// object the operation runs on among the rest.
[[nodiscard]] auto OperationPrototype(
    std::span<const mir::TypeId> params, mir::TypeId result)
    -> mir::CallableCode;

// What the code of each operation of the class `cls` takes and answers, read
// off the class's declaration alone, so it is the same to every body of the
// unit and to a unit that only reaches the class. One run on an object takes it
// as its receiver. The construction (LRM 8.7) of a class that belongs to an
// instance (LRM 6.22) takes the object, the instance and its formals; a method,
// or the form a waited evaluation calls of a function (LRM 9.4.2), what its
// prototype states after its object, or for a static one (LRM 8.10) after the
// instance, where its class belongs to one; what `randomize()` asks of the
// class (LRM 18.6), the object and, where constraints are stated to it, the
// problem; the instance an interface class the class names is of (LRM 6.22),
// the instance the class belongs to, answering the one the name reaches.
[[nodiscard]] auto ClassConstructorPrototype(
    UnitLowerer& unit_lowerer, hir::ClassId cls) -> mir::CallableCode;
// The constructor of `cls` itself, as a construction of an object of it
// enters it -- where the operation above is what a scope's published class
// introduces for every body realizing the scope to answer, this is the
// class's own: the object, the instance the class belongs to where it belongs
// to one, then what every construction takes.
[[nodiscard]] auto ClassBodyConstructorPrototype(
    UnitLowerer& unit_lowerer, hir::ClassId cls,
    std::optional<mir::TypeId> declaring_instance) -> mir::CallableCode;
[[nodiscard]] auto ClassMethodPrototype(
    UnitLowerer& unit_lowerer, hir::ClassId cls, hir::MethodId method)
    -> mir::CallableCode;
[[nodiscard]] auto ClassHookPrototype(
    UnitLowerer& unit_lowerer, hir::ClassId cls,
    support::LibraryCallback callback) -> mir::CallableCode;
[[nodiscard]] auto ClassInterfaceReachPrototype(UnitLowerer& unit_lowerer)
    -> mir::CallableCode;
// What answers, of an object reached as `self_pointer_type`, the instance it
// keeps for its class (LRM 6.22), of the type `instance`.
[[nodiscard]] auto ClassDeclaringInstancePrototype(
    mir::TypeId self_pointer_type, mir::TypeId instance) -> mir::CallableCode;

// The method a scope's published class introduces for an operation whose code
// takes and answers what `operation` does, entered on an object reached as
// `self`: the object of the scope, then everything the operation takes.
[[nodiscard]] auto SlotPrototype(
    const mir::CallableCode& operation, mir::TypeId self) -> mir::CallableCode;

// A method's form, as its dispatch states it: a static method (LRM 8.10)
// belongs to the class and runs on no object, one run on an object being built
// runs as its constructor does, and every other runs on the object it was
// called through.
auto CallableFormOf(const hir::MethodDispatch& dispatch) -> CallableForm;

// The operation `point` of the class `id` as the class states it: taking what
// the operation does, it reads the instance the operation runs for -- the one
// a body of the form `form` is handed where it is entered by a construction or
// runs on no object (LRM 8.7, 8.10), and otherwise the one the object recorded
// where `instance` says -- and enters the code that instance's body compiled,
// through the method `published`, the class published of the scope declaring
// the class, introduced for it.
[[nodiscard]] auto EntryOf(
    UnitLowerer& unit_lowerer, mir::ClassId id,
    const DeclaringInstance& instance, const ClassCodePoint& point,
    CallableForm form, mir::ClassId published) -> mir::CallableCode;

// The class `cls` a design element's unit declares, as every body of the unit
// states it: its layout, what it answers of the interface classes it
// implements, and for each of its operations an entry into the code of the
// body its object's instance is (LRM 6.22), all read off its declaration and
// nothing off any code. A body whose construction did not build the scope
// declaring it states it too, since the class is the unit's and not one
// body's (LRM 27.5).
[[nodiscard]] auto StateClassFromDeclaration(
    UnitLowerer& unit_lowerer, hir::ClassId cls) -> mir::Class;

// Moves the code a body compiled for each operation of the class `id`, which
// belongs to an instance (LRM 6.22), out of `compiled` and into the body class
// `declaring_frame` stands in, whose published class is `published`: each
// becomes that body class's answer to the method `published` introduced for
// it, with the instance as its object and what the operation took after it.
// Which body's code runs is then the instance's (LRM 8.25).
void MoveCodeIntoBody(
    UnitLowerer& unit_lowerer, mir::ClassId id, const mir::Class& compiled,
    const WalkFrame& declaring_frame, mir::ClassId published);

}  // namespace lyra::lowering::hir_to_mir
