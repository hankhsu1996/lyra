#pragma once

#include "lyra/hir/type_id.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// The bodies through which a reader reaching the design by name reaches a part
// of a value of one type (LRM 37.17 to 37.19, 38.15, 38.34). Each takes the
// one step a selection states at its depth into the value -- a member, an
// element, an entry -- and hands the rest of the selection to the bodies of
// the part's type, as a type's derived serialization hands each field to the
// field type's own, so what the unit holds for them follows its types and
// never the paths through them:
//
// - `settle` takes the selection alone, settling what its steps come to by the
//   types: the key each entry it names is kept by, converted from what foreign
//   code wrote as the design converts an index (LRM 7.8.4), and the positions a
//   takeover of the variable counts before the part (LRM 10.6.2).
// - `read` takes a reference to the value and hands the part's value or the
//   object it names, or, at an array whose size the run sets, its size and,
//   where asked, its keys.
// - `write` takes a reference belonging to the variable and lands the value
//   handed on the part, as the design's own write through a reference does.
struct PartBodies {
  mir::CallableId settle;
  mir::CallableId read;
  mir::CallableId write;
};

// One parameter a body reaching data objects is handed by the reader: its
// binding and its type, named as a place in whichever block uses it.
struct Handed {
  mir::LocalId local;
  mir::TypeId type;

  [[nodiscard]] auto PlaceIn(mir::Builder block) const -> mir::ExprId {
    return block.LocalPlace(local, type);
  }
  [[nodiscard]] auto ReadIn(mir::Builder block) const -> mir::ExprId {
    return block.ReadLocal(local, type);
  }
};

// The bodies of `type`, made the first time any reaches it and shared by every
// part of that type the unit describes.
[[nodiscard]] auto PartBodiesOf(UnitLowerer& owner, hir::TypeId type)
    -> PartBodies;

// Whether a part of type `type` is itself one a write from a reader lands a
// value on whole (LRM 38.34, Table 38-3): an integral value, a real or a
// string.
[[nodiscard]] auto TakesAWrittenValue(
    const UnitLowerer& owner, hir::TypeId type) -> bool;

// Whether a part of type `type`, or a part of one, holds a value a write from a
// reader lands (LRM 38.34, Table 38-3): an integral value, a real or a string.
[[nodiscard]] auto HasWrittenPart(const UnitLowerer& owner, hir::TypeId type)
    -> bool;

}  // namespace lyra::lowering::hir_to_mir
