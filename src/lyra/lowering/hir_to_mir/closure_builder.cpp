#include "lyra/lowering/hir_to_mir/closure_builder.hpp"

#include <memory>
#include <string>
#include <utility>
#include <variant>

#include "lyra/lowering/hir_to_mir/capture_sink.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

ClosureBuilder::ClosureBuilder(
    mir::CompilationUnit& unit, const WalkFrame& enclosing, bool coroutine,
    std::optional<BlockDepth> by_value_depth)
    : unit_(&unit),
      outer_(enclosing.current_block),
      sink_(
          enclosing.block_depth.Inner(), body_, *outer_, unit, by_value_depth) {
  const mir::TypeId self_pointer_type =
      enclosing.current_class->self_pointer_type;
  self_binding_ =
      body_.vars.Add(mir::LocalDecl{.name = "self", .type = self_pointer_type});
  const mir::ExprId outer_self_read =
      outer_->exprs.Add(MakeSelfRefExpr(enclosing, self_pointer_type));
  environment_.push_back(
      mir::EnvBinding{.param = self_binding_, .value = outer_self_read});
  frame_ = enclosing.WithBlock(&body_)
               .Deeper()
               .WithCaptureSink(&sink_)
               .WithSelfBinding(self_binding_, enclosing.block_depth.Inner())
               .WithCoroutineBody(coroutine);
}

auto ClosureBuilder::AddParam(std::string_view name, mir::TypeId type)
    -> mir::LocalId {
  const mir::LocalId binding =
      body_.vars.Add(mir::LocalDecl{.name = std::string(name), .type = type});
  invocation_params_.push_back(binding);
  return binding;
}

auto ClosureBuilder::CaptureByValue(mir::ExprId outer_id, std::string_view name)
    -> mir::ExprId {
  const mir::Expr& outer_expr = outer_->exprs.Get(outer_id);
  if (std::holds_alternative<mir::IntegerLiteral>(outer_expr.data)) {
    return body_.exprs.Add(outer_expr);
  }
  const mir::LocalId binding = body_.vars.Add(
      mir::LocalDecl{.name = std::string(name), .type = outer_expr.type});
  environment_.push_back(mir::EnvBinding{.param = binding, .value = outer_id});
  return body_.exprs.Add(
      mir::Expr{
          .data =
              mir::LocalRef{.hops = mir::BlockHops{.value = 0}, .var = binding},
          .type = outer_expr.type});
}

auto ClosureBuilder::Finish(mir::TypeId result_type) -> mir::Expr {
  for (const CaptureRequest& request : sink_.TakeRequests()) {
    environment_.push_back(
        mir::EnvBinding{.param = request.binding, .value = request.source});
  }
  // The signature is `self` (always `params[0]`) followed by the per-invocation
  // parameters. Captured locals are not parameters -- they are the bound
  // environment, reached as fields, distinct from the call signature.
  std::vector<mir::LocalId> params;
  params.reserve(1 + invocation_params_.size());
  params.push_back(self_binding_);
  for (const mir::LocalId param : invocation_params_) {
    params.push_back(param);
  }
  auto code = std::make_unique<mir::CallableCode>(mir::CallableCode{
      .params = std::move(params),
      .result_type = result_type,
      .body = std::move(body_)});
  mir::ClosureExpr closure;
  closure.code = std::move(code);
  closure.environment = std::move(environment_);
  return mir::Expr{.data = std::move(closure), .type = result_type};
}

auto ClosureBuilder::Build(mir::ExprId result) -> mir::Expr {
  const mir::TypeId result_type = body_.exprs.Get(result).type;
  body_.AppendStmt(mir::ReturnStmt{.value = result});
  return Finish(result_type);
}

auto ClosureBuilder::BuildCoroutine() -> mir::Expr {
  body_.AppendStmt(
      mir::ReturnStmt{.value = std::nullopt, .is_coroutine_return = true});
  return Finish(unit_->builtins.coroutine);
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
              .callee = mir::ClosureRef{.closure = closure_id},
              .arguments = {}},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
