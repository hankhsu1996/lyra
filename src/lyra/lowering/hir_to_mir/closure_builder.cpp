#include "lyra/lowering/hir_to_mir/closure_builder.hpp"

#include <memory>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

ClosureBuilder::ClosureBuilder(
    mir::CompilationUnit& unit, const WalkFrame& enclosing, bool coroutine,
    CapturePolicy policy)
    : unit_(&unit),
      outer_(enclosing.current_block),
      bindings_(unit, code_, *enclosing.bindings, *outer_, std::move(policy)),
      frame_(enclosing.WithBlock(&code_.body)
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

auto ClosureBuilder::CaptureByValue(mir::ExprId outer_id, std::string_view name)
    -> mir::ExprId {
  const mir::Expr outer_expr = outer_->exprs.Get(outer_id);
  if (std::holds_alternative<mir::IntegerLiteral>(outer_expr.data)) {
    return code_.body.exprs.Add(outer_expr);
  }
  const mir::TypeId field_type = outer_expr.type;
  const mir::CaptureId field = bindings_.Capture(outer_id, name);
  return code_.body.exprs.Add(mir::MakeCaptureRefExpr(field, field_type));
}

auto ClosureBuilder::Finish(mir::TypeId result_type) -> mir::Expr {
  code_.result_type = result_type;
  code_.params = invocation_params_;
  mir::ClosureExpr closure;
  closure.capture_inits = bindings_.TakeCaptureInits();
  closure.code = std::make_unique<mir::CallableCode>(std::move(code_));
  return mir::Expr{.data = std::move(closure), .type = result_type};
}

auto ClosureBuilder::Build(mir::ExprId result) -> mir::Expr {
  const mir::TypeId result_type = code_.body.exprs.Get(result).type;
  code_.body.AppendStmt(mir::ReturnStmt{.value = result});
  return Finish(result_type);
}

auto ClosureBuilder::BuildCoroutine() -> mir::Expr {
  code_.body.AppendStmt(
      mir::ReturnStmt{.value = std::nullopt, .is_coroutine_return = true});
  return Finish(unit_->builtins.coroutine_void);
}

auto ClosureBuilder::BuildVoid() -> mir::Expr {
  return Finish(unit_->builtins.void_type);
}

auto BuildClosureCallExpr(mir::Block& block, mir::Expr closure) -> mir::Expr {
  const mir::TypeId result_type = closure.type;
  const mir::ExprId closure_id = block.exprs.Add(std::move(closure));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Indirect{.closure = closure_id}, .arguments = {}},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
