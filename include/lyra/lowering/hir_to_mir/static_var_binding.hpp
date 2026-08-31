#pragma once

#include <cstdint>
#include <span>
#include <string_view>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// The MIR field one static-lifetime body local became, and the procedural scope
// that declared it. A static's storage outlives every activation of its body
// (LRM 13.3.1) and is one per instance, so the field is the enclosing class's
// like any other and a body reaches it directly on `self`. The scope is what
// names it: a hierarchical path descends to that scope's node and asks for the
// spelling the source wrote.
struct StaticVarBinding {
  hir::ProceduralVarId var;
  hir::ProceduralScopeId scope;
  mir::FieldId field;
};

// One entry per static-lifetime local a body declares. A body that declares
// none has an empty one; nothing about the shape tells that apart from a body
// that declares one or many, and no reader has to.
using StaticVarBindings = std::vector<StaticVarBinding>;

// Whether the storage this walk declares can be watched from outside the body
// that declares it. Storage in the design hierarchy can: a hierarchical
// reference reads it and an event control can wait on it, so a write has to
// reach subscribers. Storage on a class cannot, because a class object is
// reached by member select rather than by scope name (LRM 23.7), so it is a
// plain cell.
enum class ObservedStorage : std::uint8_t { kNo, kYes };

// The vars a callable's signature already binds: its formals and, for a
// non-void function, the implicit result variable (LRM 13.4.1). Their storage
// is the call's own data flow -- a parameter, or a component of the completion
// payload -- so the walk below gives them none, whatever lifetime their
// declarations carry. A process binds none.
[[nodiscard]] auto SignatureBoundVars(const hir::SubroutineDecl& decl)
    -> std::vector<hir::ProceduralVarId>;

// Gives every static-lifetime local one body declares its MIR field on the
// class enclosing that body, and records which scope wrote it. A static is one
// cell per instance (LRM 13.3.1), so the enclosing class is where it goes
// whichever block it was written in; the scope it came from settles only what a
// hierarchical path can call it. The walk descends the body's scope tree
// because that is where a declaration's scope is stated -- a declaration holds
// no link back up to it.
//
// The mangled field name carries the callable and the declaration's id, so
// sibling callables sharing a source identifier, and nested blocks repeating
// one, stay distinct on the arena they all share.
auto BindBodyStatics(
    const UnitLowerer& unit_lowerer,
    const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
        scopes,
    base::Arena<mir::FieldDecl, mir::FieldId>& fields, ObservedStorage observed,
    const hir::ProceduralBody& body,
    std::span<const hir::ProceduralVarId> signature_bound,
    std::string_view callable_name) -> StaticVarBindings;

}  // namespace lyra::lowering::hir_to_mir
