#pragma once

#include <span>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;
struct WalkFrame;

// Where a static-lifetime local keeps its one cell (LRM 6.21). What decides it
// is what the declaration belongs to, never what reaches it. A body in the
// design hierarchy is replicated with its instance, so its cell is a field of
// that instance's class and every instance has its own. A class method's
// declaration belongs to the class: the standard makes such a variable
// "roughly equivalent to C static variables declared within a function", so one
// cell serves every call however many objects exist, and a method being
// automatic (LRM 8.6) governs its ordinary locals rather than this one. A
// package subroutine's belongs to the unit's namespace, which owns one
// program-global cell (LRM 26.2).
struct InstanceFieldHome {
  mir::FieldId field;
};

struct ClassCellHome {
  mir::StaticPropertyId property;
};

// A cell the unit's namespace owns is reached by name, never by its
// declaration id, so the name is what a reference needs and what this carries.
struct UnitCellHome {
  std::string name;
};

using StaticStorageHome =
    std::variant<InstanceFieldHome, ClassCellHome, UnitCellHome>;

// The pool a declaration scope declares its bodies' static-lifetime cells in,
// one per home above. Whoever lowers a declaration scope states its own, so
// nothing here works out where a body sits from the body.
struct InstanceStorage {
  base::Arena<mir::FieldDecl, mir::FieldId>* fields;
};

struct ClassStorage {
  base::Arena<mir::StaticPropertyDecl, mir::StaticPropertyId>* properties;
};

struct UnitStorage {
  base::Arena<mir::StaticVariableDecl, mir::StaticVariableId>* variables;
};

using StaticStorageOwner =
    std::variant<InstanceStorage, ClassStorage, UnitStorage>;

// The cell one static-lifetime body local became, and the procedural scope that
// declared it. The scope is what names it: a hierarchical path descends to that
// scope's node and asks for the spelling the source wrote, which only a cell on
// an object can answer. `cell_type` is the type the home was declared with --
// an observable cell where something outside the body can reach it, the plain
// value type where nothing can -- settled once here so no reader recomputes it.
struct StaticVarBinding {
  hir::ProceduralVarId var;
  hir::ProceduralScopeId scope;
  StaticStorageHome home;
  mir::TypeId cell_type;
};

// One entry per static-lifetime local a body declares. A body that declares
// none has an empty one; nothing about the shape tells that apart from a body
// that declares one or many, and no reader has to.
using StaticVarBindings = std::vector<StaticVarBinding>;

// Declares one static-lifetime cell in `owner` and answers with the home a body
// reaches it through. Every cell that outlives an activation is minted here,
// whichever construct asked for one.
[[nodiscard]] auto DeclareStaticCell(
    const StaticStorageOwner& owner, std::string name, mir::TypeId cell_type)
    -> StaticStorageHome;

// The vars a callable's signature already binds: its formals and, for a
// non-void function, the implicit result variable (LRM 13.4.1). Their storage
// is the call's own data flow -- a parameter, or a component of the completion
// payload -- so the walk below gives them none, whatever lifetime their
// declarations carry. A process binds none.
[[nodiscard]] auto SignatureBoundVars(const hir::SubroutineDecl& decl)
    -> std::vector<hir::ProceduralVarId>;

// Gives every static-lifetime local one body declares its cell in `owner`, and
// records which scope wrote it. The walk descends the body's scope tree because
// that is where a declaration's scope is stated -- a declaration holds no link
// back up to it.
//
// The mangled cell name carries the callable and the declaration's id, so
// sibling callables sharing a source identifier, and nested blocks repeating
// one, stay distinct in the pool they all share.
auto BindBodyStatics(
    const UnitLowerer& unit_lowerer,
    const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
        scopes,
    const StaticStorageOwner& owner, const hir::ProceduralBody& body,
    std::span<const hir::ProceduralVarId> signature_bound,
    std::string_view callable_name) -> StaticVarBindings;

// The expression a body reaches one of its static-lifetime cells through. Each
// home is one access from where the body stands: a field off the body's own
// `self`, the cell its class owns, or the cell the unit's namespace owns.
[[nodiscard]] auto BuildStaticStorageAccess(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    const StaticStorageHome& home, mir::TypeId cell_type) -> mir::Expr;

// The field a static of a body in the design hierarchy took. Such a body is
// replicated with its instance, so its cells are that instance's fields and no
// other home arises there. Asked by whoever already holds the object: a route
// that descended to it, and the registration of the name a hierarchical path
// reaches it by (LRM 6.21).
[[nodiscard]] auto InstanceFieldOf(const StaticVarBinding& binding)
    -> mir::FieldId;

}  // namespace lyra::lowering::hir_to_mir
