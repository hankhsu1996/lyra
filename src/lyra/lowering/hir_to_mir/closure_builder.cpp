#include "lyra/lowering/hir_to_mir/closure_builder.hpp"

#include <memory>
#include <utility>
#include <vector>

#include "lyra/lowering/hir_to_mir/capture_sink.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

ClosureBuilder::ClosureBuilder(
    mir::CompilationUnit& unit, const WalkFrame& enclosing,
    mir::TypeId result_type)
    : unit_(&unit),
      outer_(enclosing.current_procedural_scope),
      result_type_(result_type),
      sink_(enclosing.procedural_depth.Inner(), body_, *outer_, unit) {
  const mir::TypeId self_ptr_type = unit_->builtins.self_pointer;
  self_binding_ = body_.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "self", .type = self_ptr_type});
  outer_self_read_ =
      outer_->AddExpr(BuildSelfRefExpr(enclosing, self_ptr_type));
  // A synchronous IIFE aliases the caller's storage, live throughout the
  // closure's evaluation, so every sink capture is a reference (no by-value
  // depth) and the body never suspends.
  frame_ =
      enclosing.WithProceduralScope(&body_)
          .Deeper()
          .WithCaptureSink(&sink_)
          .WithSelfBinding(self_binding_, enclosing.procedural_depth.Inner())
          .WithCoroutineBody(false);
}

auto ClosureBuilder::Build(mir::ExprId result) -> mir::Expr {
  body_.AppendStmt(mir::ReturnStmt{.value = result});

  std::vector<mir::Capture> captures;
  captures.push_back(
      mir::Capture{.value = outer_self_read_, .binding = self_binding_});
  for (const CaptureRequest& request : sink_.TakeRequests()) {
    captures.push_back(
        mir::Capture{.value = request.source, .binding = request.binding});
  }
  mir::ClosureExpr closure;
  closure.captures = std::move(captures);
  closure.body = std::make_unique<mir::ProceduralScope>(std::move(body_));

  return mir::Expr{.data = std::move(closure), .type = result_type_};
}

auto BuildClosureCallExpr(mir::ProceduralScope& scope, mir::Expr closure)
    -> mir::Expr {
  const mir::TypeId result_type = closure.type;
  const mir::ExprId closure_id = scope.AddExpr(std::move(closure));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::ClosureRef{.closure = closure_id},
              .arguments = {}},
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
