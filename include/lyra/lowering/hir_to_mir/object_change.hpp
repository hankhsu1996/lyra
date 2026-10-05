#pragma once

#include <variant>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The entries below act on an object and are handed whatever reaches it, as
// the source reached it: a class handle (LRM 8.3), or the running method's own
// object. Which object a handle names is opened from it by each target, as an
// access's receiver is.

// The event source of the object `object` reaches, which a wait reaching the
// object subscribes to: one per object, covering every property (LRM 9.4.2).
[[nodiscard]] auto ObjectEventSourceOf(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object)
    -> mir::ExprId;

// Which property of an object an access reaches: the class declaring it, this
// unit's or one another unit published, and the slot it has there.
using PropertyName =
    std::variant<mir::ClassFieldTarget, mir::CrossUnitClassFieldTarget>;

// The storage `property`, holding a value of `type`, occupies on the object
// `object` reaches (LRM 8.4). `object` is a class handle, the running method's
// own object, or a write in progress into the object, which is dereferenced to
// the object as a guard is.
[[nodiscard]] auto PropertyStorage(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId object,
    const PropertyName& property, mir::TypeId type) -> mir::Expr;

// A write opened on the object `receiver` reaches, alone, for a write to one of
// its properties: it lasts as long as the full-expression that writes the
// property and ending it tells the object (LRM 9.4.2), and it is dereferenced
// to the object as the class the property is reached through.
[[nodiscard]] auto OpenObjectWrite(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId receiver)
    -> mir::ExprId;

// A reference to `property`, holding a value of `type`, of the object
// `receiver` reaches (LRM 13.5.2): a step taken on the object, so the object
// travels with the reference and a write through it tells the object as it
// lands (LRM 9.4.2), which no write held open around the call that is lent it
// could do.
[[nodiscard]] auto PropertyReference(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId receiver,
    const PropertyName& property, mir::TypeId type) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
