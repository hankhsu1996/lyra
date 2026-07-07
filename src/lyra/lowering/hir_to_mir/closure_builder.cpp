#include "lyra/lowering/hir_to_mir/closure_builder.hpp"

#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

ClosureBuilder::ClosureBuilder(
    mir::CompilationUnit& unit, const WalkFrame& enclosing, bool coroutine,
    CapturePolicy policy)
    : unit_(&unit),
      outer_(enclosing.current_block),
      closure_decl_{},
      closure_id_(unit.DeclareClosure()),
      bindings_(
          unit, closure_decl_, closure_id_, *enclosing.bindings, *outer_,
          std::move(policy)),
      frame_(enclosing.WithBlock(&closure_decl_.invoke.body)
                 .WithBindings(&bindings_)
                 .WithCoroutineBody(coroutine)) {
}

auto ClosureBuilder::AddParam(
    BindingOriginId origin, std::string_view name, mir::TypeId type)
    -> mir::LocalId {
  const mir::LocalId binding = bindings_.Declare(
      origin, mir::LocalDecl{.name = std::string(name), .type = type});
  invocation_params_.push_back(binding);
  return binding;
}

auto ClosureBuilder::AddParamAnonymous(std::string_view name, mir::TypeId type)
    -> mir::LocalId {
  const mir::LocalId binding = bindings_.DeclareAnonymous(
      mir::LocalDecl{.name = std::string(name), .type = type});
  invocation_params_.push_back(binding);
  return binding;
}

auto ClosureBuilder::Finish(mir::TypeId result_type) -> mir::Expr {
  closure_decl_.invoke.result_type = result_type;
  closure_decl_.invoke.params = invocation_params_;
  std::vector<mir::FieldInit> field_inits = bindings_.Finalize();
  const mir::TypeId closure_type =
      unit_->types.Intern(mir::ClosureType{.closure_id = closure_id_});
  unit_->DefineClosure(closure_id_, std::move(closure_decl_));
  return mir::Expr{
      .data =
          mir::ClosureExpr{
              .closure = closure_id_, .field_inits = std::move(field_inits)},
      .type = closure_type};
}

auto ClosureBuilder::Build(mir::ExprId result) -> mir::Expr {
  const mir::TypeId result_type =
      closure_decl_.invoke.body.exprs.Get(result).type;
  closure_decl_.invoke.body.AppendStmt(mir::ReturnStmt{.value = result});
  return Finish(result_type);
}

auto ClosureBuilder::BuildCoroutine() -> mir::Expr {
  closure_decl_.invoke.body.AppendStmt(
      mir::ReturnStmt{.value = std::nullopt, .is_coroutine_return = true});
  return Finish(unit_->builtins.coroutine_void);
}

auto ClosureBuilder::BuildVoid() -> mir::Expr {
  return Finish(unit_->builtins.void_type);
}

auto BuildClosureCallExpr(
    mir::CompilationUnit& unit, mir::Block& block, mir::Expr closure)
    -> mir::Expr {
  const auto& construct = std::get<mir::ClosureExpr>(closure.data);
  const mir::TypeId result_type =
      unit.GetClosure(construct.closure).invoke.result_type;
  const mir::ExprId closure_id = block.exprs.Add(std::move(closure));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Indirect{.closure = closure_id}, .arguments = {}},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
