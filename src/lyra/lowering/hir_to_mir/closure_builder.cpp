#include "lyra/lowering/hir_to_mir/closure_builder.hpp"

#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_signature.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/keeper.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// `closure` added to `block` made in a box of its own.
auto BuildClosureBox(
    mir::CompilationUnit& unit, mir::Builder block, mir::Expr closure)
    -> mir::ExprId {
  const mir::TypeId box = mir::BoxOf(unit.types, closure.type);
  return block.Add(mir::MakeNewExpr(block.Add(std::move(closure)), box));
}

// What a closure of `closure_type` is once its type is no longer known: a
// callable of its kind, taking what its body takes and answering what its
// body answers.
auto ErasedOf(const mir::CompilationUnit& unit, mir::TypeId closure_type)
    -> mir::TypeId {
  return mir::ErasedTypeOf(
      unit.types,
      unit.GetClosure(
          unit.types.Get(closure_type).Get<mir::ClosureType>().closure_id));
}

// `held`, a box or a borrow of a closure, made one of the closure of no known
// type `erased`, holding it as `type` does.
auto Unsized(
    mir::Builder block, mir::ExprId held, mir::TypeId erased, mir::TypeId type)
    -> mir::ExprId {
  return block.Add(
      mir::MakeCallExpr(
          mir::Direct{
              .target = support::BuiltinFn::kUnsize,
              .type_arguments = {erased}},
          {held}, type));
}

}  // namespace

ClosureBuilder::ClosureBuilder(
    mir::CompilationUnit& unit, const WalkFrame& enclosing,
    support::ClosureKind kind, CapturePolicy policy)
    : unit_(&unit),
      outer_(enclosing.Builder()),
      closure_decl_{
          .fields = {},
          .field_order = {},
          .invoke = mir::CallableCode::Defined(enclosing.span)},
      closure_id_(unit.DeclareClosure()),
      bindings_(
          unit, closure_decl_, closure_id_, kind, *enclosing.bindings, outer_,
          std::move(policy)),
      frame_(enclosing.WithBlock(&closure_decl_.invoke.Body())
                 .WithBindings(&bindings_)
                 .WithReportingReachedTo(std::nullopt)) {
}

auto ClosureBuilder::AddParam(mir::TypeId type) -> mir::LocalId {
  const mir::LocalId binding = bindings_.DeclareAnonymous(type);
  invocation_params_.push_back(binding);
  return binding;
}

auto ClosureBuilder::Finish(mir::TypeId result_type) -> mir::Expr {
  closure_decl_.invoke.result_type = result_type;
  closure_decl_.invoke.params = invocation_params_;
  std::vector<mir::FieldInit> field_inits = bindings_.Finalize();
  const mir::TypeId value_type = unit_->types.Intern(
      mir::Type{mir::ClosureType{.closure_id = closure_id_}});
  unit_->DefineClosure(closure_id_, std::move(closure_decl_));
  return mir::Expr{
      .data =
          mir::ClosureExpr{
              .closure = closure_id_, .field_inits = std::move(field_inits)},
      .type = value_type};
}

auto ClosureBuilder::Build(mir::ExprId result) -> mir::Expr {
  const mir::TypeId result_type =
      closure_decl_.invoke.Body().exprs.Get(result).type;
  Body().AppendStmt(mir::ReturnStmt{.value = result});
  return Finish(result_type);
}

auto ClosureBuilder::BuildVoid() -> mir::Expr {
  return Finish(unit_->builtins.void_type);
}

auto ClosureBuilder::BuildCallOnce() -> mir::Expr {
  Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  const mir::TypeId result_type = unit_->builtins.coroutine_void;
  const mir::ExprId box = BuildClosureBox(*unit_, outer_, Finish(result_type));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = mir::ClosureTarget{.closure = closure_id_},
                      .receiver = box},
              .arguments = {}},
      .type = result_type};
}

auto BuildKeptClosure(
    mir::CompilationUnit& unit, mir::Builder block, mir::Expr closure)
    -> mir::ExprId {
  const mir::TypeId erased = ErasedOf(unit, closure.type);
  const mir::ExprId box = BuildClosureBox(unit, block, std::move(closure));
  return Unsized(block, box, erased, mir::BoxOf(unit.types, erased));
}

auto BuildEmplacedClosure(
    mir::CompilationUnit& unit, const WalkFrame& frame, mir::Expr keeper,
    mir::Expr closure) -> mir::Expr {
  const mir::TypePool& types = unit.types;
  mir::Builder block = frame.Builder();
  auto& effect = std::get<mir::ClosureExpr>(closure.data);

  // A value the effect is made of that its source makes is held in this frame
  // before the keeper is found, whose finding may evaluate a delay (LRM
  // 9.4.5). One already lying in storage is read where the effect is made: what
  // lies so is a variable of this frame, which no delay may name (LRM 13.3.2),
  // and storage a delay can write -- a design's variable, a property, what a
  // `ref` binds -- is read by a call that makes its value.
  for (mir::FieldInit& init : effect.field_inits) {
    if (!mir::ValueLiesInStorage(unit, block.Body(), init.value)) {
      const mir::LocalId local = DeclareLocal(frame, init.value);
      init.value = block.ReadLocal(local, block.Get(init.value).type);
    }
  }

  // The keeper is found once and named twice, by the storage the effect is
  // made in and by the commit that keeps it there.
  const mir::TypeId keeper_pointer = keeper.type;
  const mir::TypeId keeper_type =
      types.Get(keeper_pointer).Get<mir::PointerType>().pointee;
  const std::optional<mir::KeeperEntries> entries =
      mir::KeeperEntriesOf(types, keeper_type);
  if (!entries.has_value()) {
    throw InternalError(
        "hir_to_mir: an effect is made in what keeps no deferred effects -- "
        "please report this as a bug");
  }
  const mir::LocalId held = DeclareLocal(frame, block.Add(std::move(keeper)));
  const mir::TypeId effect_type = closure.type;
  const mir::ExprId closure_value = block.Add(std::move(closure));
  const mir::ExprId keeper_value = block.ReadLocal(held, keeper_pointer);
  const mir::TypeId effect_pointer = types.Intern(
      mir::Type{mir::PointerType{
          .pointee = effect_type,
          .ownership = mir::PointerOwnership::kBorrowed,
          .mutability = mir::Mutability::kMutable}});
  const mir::ExprId made = block.Add(
      mir::MakePlacementNewExpr(
          closure_value,
          mir::PlacementAllocation{
              .allocate = entries->allocate, .arguments = {keeper_value}},
          effect_pointer));
  return mir::MakeCallExpr(
      mir::Direct{
          .target = entries->commit,
          .receiver = block.Deref(
              types, block.LocalPlace(held, keeper_pointer), keeper_type)},
      {made}, unit.builtins.void_type);
}

auto BuildBorrowedClosure(
    mir::CompilationUnit& unit, mir::Builder block, mir::ExprId closure)
    -> mir::ExprId {
  const mir::TypeId closure_type = block.Get(closure).type;
  const mir::ExprId borrowed = mir::Borrow(unit, block, closure);
  const mir::TypeId erased = ErasedOf(unit, closure_type);
  return Unsized(
      block, borrowed, erased,
      unit.types.Intern(
          mir::Type{mir::ReferenceType{
              .referent = erased, .mutability = mir::Mutability::kReadOnly}}));
}

}  // namespace lyra::lowering::hir_to_mir
