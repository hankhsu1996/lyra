#include "lyra/lowering/hir_to_mir/closure_builder.hpp"

#include <memory>
#include <string>
#include <utility>
#include <variant>

#include "lyra/lowering/hir_to_mir/capture_sink.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
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
      body_.AddLocal(mir::LocalDecl{.name = "self", .type = self_pointer_type});
  const mir::ExprId outer_self_read =
      outer_->AddExpr(BuildSelfRefExpr(enclosing, self_pointer_type));
  captures_.push_back(
      mir::Capture{.value = outer_self_read, .binding = self_binding_});
  frame_ = enclosing.WithBlock(&body_)
               .Deeper()
               .WithCaptureSink(&sink_)
               .WithSelfBinding(self_binding_, enclosing.block_depth.Inner())
               .WithCoroutineBody(coroutine);
}

auto ClosureBuilder::AddParam(std::string_view name, mir::TypeId type)
    -> mir::LocalId {
  const mir::LocalId binding =
      body_.AddLocal(mir::LocalDecl{.name = std::string(name), .type = type});
  params_.push_back(mir::Parameter{.binding = binding});
  return binding;
}

auto ClosureBuilder::CaptureByValue(mir::ExprId outer_id, std::string_view name)
    -> mir::ExprId {
  const mir::Expr& outer_expr = outer_->GetExpr(outer_id);
  if (std::holds_alternative<mir::IntegerLiteral>(outer_expr.data)) {
    return body_.AddExpr(outer_expr);
  }
  const mir::LocalId binding = body_.AddLocal(
      mir::LocalDecl{.name = std::string(name), .type = outer_expr.type});
  captures_.push_back(mir::Capture{.value = outer_id, .binding = binding});
  return body_.AddExpr(
      mir::Expr{
          .data =
              mir::LocalRef{.hops = mir::BlockHops{.value = 0}, .var = binding},
          .type = outer_expr.type});
}

auto ClosureBuilder::Finish(mir::TypeId result_type) -> mir::Expr {
  for (const CaptureRequest& request : sink_.TakeRequests()) {
    captures_.push_back(
        mir::Capture{.value = request.source, .binding = request.binding});
  }
  mir::ClosureExpr closure;
  closure.captures = std::move(captures_);
  closure.params = std::move(params_);
  closure.body = std::make_unique<mir::Block>(std::move(body_));
  return mir::Expr{.data = std::move(closure), .type = result_type};
}

auto ClosureBuilder::Build(mir::ExprId result) -> mir::Expr {
  const mir::TypeId result_type = body_.GetExpr(result).type;
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
  const mir::ExprId closure_id = block.AddExpr(std::move(closure));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::ClosureRef{.closure = closure_id},
              .arguments = {}},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
