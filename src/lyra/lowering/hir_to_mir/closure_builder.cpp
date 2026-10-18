#include "lyra/lowering/hir_to_mir/closure_builder.hpp"

#include <optional>
#include <utility>
#include <vector>

#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_signature.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// `closure` added to `block` made in a box of its own.
auto BuildClosureBox(
    mir::CompilationUnit& unit, mir::Block& block, mir::Expr closure)
    -> mir::ExprId {
  const mir::TypeId box = mir::BoxOf(unit.types, closure.type);
  return block.exprs.Add(
      mir::MakeNewExpr(block.exprs.Add(std::move(closure)), box));
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

}  // namespace

ClosureBuilder::ClosureBuilder(
    mir::CompilationUnit& unit, const WalkFrame& enclosing,
    support::ClosureKind kind, CapturePolicy policy)
    : unit_(&unit),
      outer_(enclosing.current_block),
      closure_decl_{},
      closure_id_(unit.DeclareClosure()),
      bindings_(
          unit, closure_decl_, closure_id_, kind, *enclosing.bindings, *outer_,
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
  closure_decl_.invoke.Body().AppendStmt(mir::ReturnStmt{.value = result});
  return Finish(result_type);
}

auto ClosureBuilder::BuildVoid() -> mir::Expr {
  return Finish(unit_->builtins.void_type);
}

auto ClosureBuilder::BuildCallOnce() -> mir::Expr {
  closure_decl_.invoke.Body().AppendStmt(
      mir::ReturnStmt{.value = std::nullopt});
  const mir::TypeId result_type = unit_->builtins.coroutine_void;
  const mir::ExprId box = BuildClosureBox(*unit_, *outer_, Finish(result_type));
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
    mir::CompilationUnit& unit, mir::Block& block, mir::Expr closure)
    -> mir::ExprId {
  const mir::TypeId erased = ErasedOf(unit, closure.type);
  const mir::ExprId box = BuildClosureBox(unit, block, std::move(closure));
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = box},
          .type = mir::BoxOf(unit.types, erased)});
}

auto BuildBorrowedClosure(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId closure)
    -> mir::ExprId {
  const mir::TypeId closure_type = block.exprs.Get(closure).type;
  const mir::ExprId borrowed = block.exprs.Add(
      mir::Expr{
          .data = mir::BorrowExpr{.operand = closure},
          .type = unit.types.Intern(
              mir::Type{mir::ReferenceType{
                  .referent = closure_type,
                  .mutability = mir::Mutability::kReadOnly}})});
  return block.exprs.Add(
      mir::Expr{
          .data = mir::CastExpr{.operand = borrowed},
          .type = unit.types.Intern(
              mir::Type{mir::ReferenceType{
                  .referent = ErasedOf(unit, closure_type),
                  .mutability = mir::Mutability::kReadOnly}})});
}

}  // namespace lyra::lowering::hir_to_mir
