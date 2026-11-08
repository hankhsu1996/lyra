#pragma once

#include <cstddef>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/passing.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::hir {
struct CallExpr;
}  // namespace lyra::hir

namespace lyra::lowering::hir_to_mir {

class ProcessLowerer;
class UnitLowerer;

// What a call holds of its formals, position by position, for the functions
// evaluating the defaults it leaves to the callee: the place of what it binds
// each formal, for as long as the call runs, which each of those functions
// takes by reference, so a default answering a place may answer one of these.
// A formal handed a value holds it in a local of the call, read for the
// callee; a formal bound by reference is held as that reference; and a formal
// the call hands nothing holds what it starts as, an `output`'s starting value
// (LRM 13.3.2). A call that leaves nothing to a default holds nothing.
class HeldActuals {
 public:
  HeldActuals(
      const mir::CompilationUnit& unit, std::size_t positions, bool omits_any)
      : unit_(&unit), omits_any_(omits_any) {
    if (omits_any) earlier_.resize(positions);
  }

  auto HoldValue(
      const WalkFrame& frame, std::size_t position, mir::ExprId value)
      -> mir::ExprId;
  auto HoldReference(
      const WalkFrame& frame, std::size_t position, mir::ExprId reference)
      -> mir::ExprId;
  void HoldStartValue(
      const WalkFrame& frame, std::size_t position, mir::TypeId type);

  // What the positions before `position` hold.
  [[nodiscard]] auto Before(std::size_t position) const
      -> std::span<const mir::ExprId> {
    return std::span{earlier_}.first(position);
  }

 private:
  const mir::CompilationUnit* unit_;
  bool omits_any_;
  std::vector<mir::ExprId> earlier_;
};

// Whether a call of `operands` leaves any of them to a default.
[[nodiscard]] auto OmitsAny(
    const base::Arena<hir::Expr, hir::ExprId>& exprs,
    std::span<const hir::ExprId> operands) -> bool;

// Visits the positions of a call in the order it evaluates them: what the
// source wrote, left to right, then what it left to a default, left to right,
// so a default reads every formal before it whichever of them were written, and
// an object a default is evaluated on need not exist for what the source wrote
// (LRM 13.5 leaves the order of a call's arguments open).
template <typename Visit>
auto ForEachInCallOrder(
    const base::Arena<hir::Expr, hir::ExprId>& exprs,
    std::span<const hir::ExprId> operands, Visit visit) -> diag::Result<void> {
  for (const bool omitted : {false, true}) {
    for (std::size_t i = 0; i < operands.size(); ++i) {
      if (std::holds_alternative<hir::CalleeDefaultExpr>(
              exprs.Get(operands[i]).data) != omitted) {
        continue;
      }
      if (auto visited = visit(i); !visited) return visited;
    }
  }
  return {};
}

// The place an omitted `output` or `inout` formal binds: what `reference`, the
// answer of its default, refers to, a place of `type`.
auto PlaceAnswered(
    const WalkFrame& frame, mir::ExprId reference, mir::TypeId type,
    const hir::Expr& omitted) -> Lvalue;

// What the function evaluating a default answers, and the type of the value or
// the place that answer is.
struct EvaluatedDefault {
  mir::ExprId answer;
  mir::TypeId type;
};

// What the function evaluating the default of an import's formal at `position`
// answers, called on `scope`, the scope declaring the import, with `earlier`,
// what the call binds each formal before it that the function takes (LRM
// 13.5.3, 35.5.4).
template <ExprLowerer Lowerer>
auto EvaluateImportDefault(
    Lowerer& lowerer, const WalkFrame& frame,
    const hir::ImportDeclaringScope& scope, std::size_t position,
    std::span<const mir::ExprId> earlier) -> diag::Result<EvaluatedDefault>;

// A call of the method `callee` names where no source expression stands for
// it: on the object the handle `receiver` holds, or on none for a
// type-associated method, with `arguments` in the frame's block -- an input's
// value, brought here to its formal's type, and a `ref` one's reference. It
// answers the method's result, or the call where the method answers none.
auto BuildMethodCall(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::ClassMethodTarget& callee, std::optional<mir::ExprId> receiver,
    std::span<const mir::ExprId> arguments, hir::TypeId result_type)
    -> mir::ExprId;

// Lowers a call to a user subroutine -- a structural one of this unit, a
// package / `$unit` one (LRM 26.3), or a class method whether it
// carries a receiver or not (LRM 8.6, 8.10) -- to the expression it yields
// (LRM 13.4, 13.5). The four differ in how the callee is named and in nothing
// after that. A call is an expression wherever it is written: statement
// position is that expression under an expression statement, and
// `lhs = f(...)` is an assignment over it, so neither is a shape of its own.
//
// The expression binds the actuals and calls the callee, typed with the
// protocol the callee states: a task's completion is a coroutine its caller
// awaits, a function's is its payload outright. A call that takes steps of its
// own -- one writing values back once the callee completes, or one holding the
// object it is made on through a class handle while the defaults it leaves are
// evaluated -- is one block expression of the call, those steps and the yield,
// or, where the callee is a task, a coroutine the enabler awaits; what it
// holds ends with that scope. Any other call has nothing to sequence and is
// the call itself.
//
// Returns nullopt for a callee that is not a user subroutine -- a system,
// builtin, imported, or foreign one -- each of which has its own boundary.
// `result_type` is the call's own result type (the enclosing expression's
// type); it is the completion payload's result component unless it is void, in
// which case the callee is a task or void function that contributes none.
template <ExprLowerer Lowerer>
auto LowerSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    mir::TypeId result_type) -> std::optional<diag::Result<mir::Expr>>;

// The call above written as a statement, where it is a function that takes
// steps of its own and settles no value: the steps and the call are the whole
// of what it does, so what the source wrote lowers to a sequence of
// statements. Returns nullopt for every other call, which the expression form
// lowers, as it lowers such a call standing anywhere else a call settling no
// value may -- a loop's step (LRM 12.7.1).
auto LowerSubroutineCallStmtForm(
    ProcessLowerer& lowerer, WalkFrame frame,
    const std::optional<std::string>& label, const hir::CallExpr& call,
    mir::TypeId result_type) -> std::optional<diag::Result<mir::Stmt>>;

// Makes the call `call` names, of a function an implicit list follows, so it
// states what it reads and writes into the report `report` holds a pointer to
// rather than running (LRM 9.2.2.2.1), appending it to the block `frame` is
// writing. What the function reads is fixed by its text, so every argument is
// handed as its type's default -- a `ref` one as storage holding it -- and
// what the function would write back is dropped.
template <ExprLowerer Lowerer>
auto EmitReportingCall(
    Lowerer& lowerer, const WalkFrame& frame, hir::ExprId call,
    mir::LocalId report) -> diag::Result<void>;

// Emits into `frame` the argument an `input` or a `ref` formal of `type` is
// handed for `actual`: its value, or what it designates (LRM 13.5.2), which is
// the whole of a variable or a class property or a part of one, so a write
// through the formal is a write of what holds that storage, told to whoever
// waits on it when it lands. A formal generic over a type takes the actual seen
// as any. A return binds its value to the result the same way.
template <ExprLowerer Lowerer>
auto EmitBoundActual(
    Lowerer& lowerer, const WalkFrame& frame, const hir::Expr& actual,
    Passing passing, mir::TypeId type) -> diag::Result<mir::ExprId>;

// Emits into `frame` the arguments a construction of `cls` is entered with
// (LRM 8.7) for `actuals`, each bound by its formal's direction as a call
// binds it: an input as its value, a `ref` as what it names (LRM 13.5.2). The
// same for `new` and for a constructor's call of its base's. Those the source
// wrote come first, then each it left to a default, evaluated in order on the
// object `object` answers a pointer to, which is asked only where there is one,
// and handed `leading`, what the constructor takes ahead of its formals.
template <ExprLowerer Lowerer>
auto EmitConstructionActuals(
    Lowerer& lowerer, const WalkFrame& frame, hir::ClassId cls,
    std::span<const hir::ExprId> actuals, std::span<const mir::ExprId> leading,
    const std::function<mir::ExprId()>& object)
    -> diag::Result<std::vector<mir::ExprId>>;

}  // namespace lyra::lowering::hir_to_mir
