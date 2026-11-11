#pragma once

#include <map>
#include <span>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/procedural_scope.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/static_property_id.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/kept_piece.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/static_property_id.hpp"
#include "lyra/mir/static_variable_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;
struct WalkFrame;
struct ClassShape;

// Where a static-lifetime cell of a body lives: a variable's (LRM 6.21), and
// the control of a statement the assertion control tasks select, which lives
// as the statement's other static storage does. What decides it is what the
// declaration belongs to, never what reaches it. A body in the
// design hierarchy is replicated with its instance, so its cell is a field of
// that instance's class and every instance has its own. A class method's
// declaration belongs to the class: the standard makes such a variable "roughly
// equivalent to C static variables declared within a function", so one cell
// serves every call however many objects exist, and a method being automatic
// (LRM 8.6) governs its ordinary locals rather than this one. A package
// subroutine's belongs to the unit's namespace, which owns one program-global
// cell (LRM 26.2).
//
// "One cell for the class" is one cell per class declaration, which is not the
// same as one for the program: a class declared inside a structural scope is a
// type of that scope's instance (LRM 6.22), so it is replicated with it and
// each instance carries the class's cells as fields of its own.
//
// The field is named with the class declaring it, because a cell a name can
// reach is a field of the class the scope publishes, where its signature put
// it, rather than of the class extending that one.
struct InstanceFieldHome {
  mir::ClassFieldTarget field;
};

// A cell its class owns is named by that class and the cell's position in its
// pool. Both are on the home because a statement reaching the cell may stand
// anywhere -- a namespace unit brings its classes' cells up in a body that is
// in no class at all -- so nothing about where the statement sits can answer
// which class owns what it reaches.
struct ClassCellHome {
  mir::ClassId owner;
  mir::StaticPropertyId property;
};

// A cell the unit's namespace owns for one of its bodies -- what a `disable`
// or an assertion control of a package subroutine acts on. Only bodies of that
// same unit reach one of these, so its position is what a reference needs.
struct UnitCellHome {
  mir::StaticVariableId variable;
};

using StaticStorageHome =
    std::variant<InstanceFieldHome, ClassCellHome, UnitCellHome>;

// A cell already placed for a static-lifetime variable a scope published, which
// the walk binding the body's variables takes rather than declaring a second
// one.
struct PlacedStatic {
  hir::ProceduralVarId var;
  mir::ClassFieldTarget field;
};

// The same for a static property of a class the scope declares, which the
// class takes rather than declaring a cell of its own (LRM 6.22, 8.9).
struct PlacedProperty {
  hir::StaticPropertyId property;
  mir::ClassFieldTarget field;
};

// The pool a declaration scope declares its bodies' static-lifetime cells in,
// one per home above. Whoever lowers a declaration scope states its own, so
// nothing here works out where a body sits from the body. `placed` lists the
// cells of this body's locals the scope published, each already a field of the
// class the scope publishes.
struct InstanceStorage {
  mir::ClassId owner;
  ClassShape* shape = nullptr;
  std::span<const PlacedStatic> placed = {};
};

struct ClassStorage {
  mir::ClassId owner;
  ClassShape* shape = nullptr;
};

struct UnitStorage {
  base::Arena<mir::StaticVariableDecl, mir::StaticVariableId>* variables;
};

using StaticStorageOwner =
    std::variant<InstanceStorage, ClassStorage, UnitStorage>;

// The control the assertion control tasks set for one statement a body holds
// (LRM 20.11), which lives as long as the statement's other static storage:
// one per instance of the declaring scope, or one for the class or the
// program. `within` is every scope it stands in, since a task naming a block or
// a task it stands in selects it.
struct AssertionControlCell {
  hir::ControlledAssertionId assertion;
  std::vector<hir::ProceduralScopeId> within;
};

// The attempts one concurrent assertion statement has in flight (LRM 16.14.1),
// which live as long as its control: whatever replicates the statement holds
// them, whether the statement is a declaration of the scope or stands in a
// procedure (LRM 16.14.6).
struct ConcurrentAttemptsCell {
  hir::ControlledAssertionId assertion;
};

// What the sampled value function `call` of the body keeps of one piece of its
// operand, a value per tick of its clock (LRM 16.9.3). It has to have been
// kept at every tick whether or not the body reached the call then, so it
// lives as the body's other static storage does.
struct SampledHistoryCell {
  hir::ExprId call;
  KeptPiece kept;
};

// The cell one static-lifetime body variable, one statement's assertion
// control, one concurrent assertion's attempts, or one kept piece of a sampled
// value function's operand became. `cell_type` is the type the home was
// declared with -- a variable's observable-cell type, and for the others the
// library type that holds them -- settled once here so no reader recomputes
// it. `scope` is the
// scope declaring it, which a variable's initializer is written in though it
// runs apart from every activation of that scope (LRM 6.21).
struct StaticCellBinding {
  std::variant<
      hir::ProceduralVarId, AssertionControlCell, ConcurrentAttemptsCell,
      SampledHistoryCell>
      of;
  hir::ProceduralScopeId scope;
  StaticStorageHome home;
  mir::TypeId cell_type;
};

// One entry per static-lifetime cell a body declares. A body that declares
// none has an empty one; nothing about the shape tells that apart from a body
// that declares one or many, and no reader has to.
using StaticCellBindings = std::vector<StaticCellBinding>;

// Declares one static-lifetime cell in `owner` and answers with the home a body
// reaches it through. Every cell that outlives an activation is declared here,
// whichever construct asked for one, except one a scope published, which was
// placed where its publication put it. The cell answers to no identifier: the
// source declared a local of a body.
[[nodiscard]] auto DeclareStaticCell(
    const StaticStorageOwner& owner, mir::TypeId cell_type)
    -> StaticStorageHome;

// Gives every static-lifetime variable one body declares, every statement of
// it the assertion control tasks select, and every piece its sampled value
// functions keep, its cell in `owner`. The walk descends the body's scope tree
// because that is where the declarations are -- a scope lists what it
// declares.
//
// Sibling callables sharing a source identifier, and nested blocks repeating
// one, need nothing done to stay distinct: each cell is a position in the pool
// they share, and a position is distinct by being one.
auto BindBodyStatics(
    const UnitLowerer& unit_lowerer,
    const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
        scopes,
    const StaticStorageOwner& owner, const hir::ProceduralBody& body)
    -> StaticCellBindings;

// The type of what keeps the past values of a piece of type `value`, one per
// tick of a clock, as deep as its reads reach (LRM 16.9.3).
[[nodiscard]] auto SampledHistoryTypeOf(
    const UnitLowerer& unit_lowerer, hir::TypeId value) -> mir::TypeId;

// The expression a body reaches one of these cells through. Each home is one
// access from where the body stands: a field off the instance `hops` out from
// the one this body counts from, the cell its class owns, or the cell the
// unit's namespace owns. A body reaching its own declaration scope's cell
// passes no hops; a body naming another scope's class passes the distance to
// that scope, which is what the reference states.
[[nodiscard]] auto BuildStaticStorageAccess(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    const StaticStorageHome& home, mir::TypeId cell_type,
    mir::EnclosingHops hops) -> mir::Expr;

// A borrowed pointer to the cell of type `cell_type` at `home`, reached from
// where `frame`'s body stands, for what holds the cell past the statement that
// reaches it. The access is added to `block`, and the pointer is answered
// unadded, for the caller to place where it is consumed.
[[nodiscard]] auto BuildStaticStorageAddress(
    mir::CompilationUnit& unit, mir::Builder block, const WalkFrame& frame,
    const StaticStorageHome& home, mir::TypeId cell_type) -> mir::Expr;

// The same pointer to the cell `cell` binds.
[[nodiscard]] auto BuildStaticStorageAddress(
    mir::CompilationUnit& unit, mir::Builder block, const WalkFrame& frame,
    const StaticCellBinding& cell) -> mir::Expr;

// Where the sampled value functions of one body, or among one scope's own
// expressions, keep each piece of their operands (LRM 16.9.3), by call: a cell
// of the body's static storage, or a field of the scope's instance, each a
// home a body reaches as it reaches any other static cell.
class KeptHistories {
 public:
  void Add(
      hir::ExprId call, KeptPiece kept, StaticStorageHome home,
      mir::TypeId cell_type);

  // The pieces `call` keeps, none where its operand keeps none.
  [[nodiscard]] auto PiecesOf(hir::ExprId call) const -> std::vector<KeptPiece>;

  // The cell keeping `kept` for `call`, as the place a call on it acts
  // through, reached from where `frame`'s body stands.
  [[nodiscard]] auto CellOf(
      const mir::CompilationUnit& unit, const WalkFrame& frame,
      hir::ExprId call, const KeptPiece& kept) const -> mir::Expr;

 private:
  struct Cell {
    KeptPiece kept;
    StaticStorageHome home;
    mir::TypeId cell_type;
  };

  std::map<hir::ExprId, std::vector<Cell>> by_call_;
};

// The field a static of a body in the design hierarchy took. Such a body is
// replicated with its instance, so its cells are that instance's fields and no
// other home arises there. Asked by a route that already holds the object.
[[nodiscard]] auto InstanceFieldOf(const StaticCellBinding& binding)
    -> mir::ClassFieldTarget;

}  // namespace lyra::lowering::hir_to_mir
