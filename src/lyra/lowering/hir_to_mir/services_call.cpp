#include "lyra/lowering/hir_to_mir/services_call.hpp"

#include <filesystem>
#include <format>
#include <string>

#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

auto FormatRuntimeOriginString(
    diag::SourceSpan span, const diag::SourceManager& mgr) -> std::string {
  const diag::FileInfo* file = mgr.GetFile(span.file_id);
  if (file == nullptr) return {};
  const auto loc = mgr.OffsetToLineCol(span.file_id, span.begin);
  return std::format(
      "{}:{}:{}", std::filesystem::path{file->path}.filename().string(),
      loc.line, loc.col);
}

auto BuildServicesCallExpr(const ModuleLowerer& module, const WalkFrame& frame)
    -> mir::Expr {
  auto& body = *frame.current_block;
  const auto& builtins = module.Unit().builtins;
  const mir::ExprId self_id = body.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  return mir::MakeServicesCallExpr(self_id, builtins.services);
}

auto BuildFilesCallExpr(const ModuleLowerer& module, const WalkFrame& frame)
    -> mir::Expr {
  auto& body = *frame.current_block;
  const auto& builtins = module.Unit().builtins;
  const mir::ExprId services_id =
      body.exprs.Add(BuildServicesCallExpr(module, frame));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::BuiltinFnCallee{.id = support::BuiltinFn::kFiles},
              .arguments = {services_id}},
      .type = builtins.files};
}

auto BuildDiagnosticCallExpr(
    const ModuleLowerer& module, const WalkFrame& frame) -> mir::Expr {
  auto& body = *frame.current_block;
  const auto& builtins = module.Unit().builtins;
  const mir::ExprId services_id =
      body.exprs.Add(BuildServicesCallExpr(module, frame));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::BuiltinFnCallee{.id = support::BuiltinFn::kDiagnostic},
              .arguments = {services_id}},
      .type = builtins.diagnostic};
}

}  // namespace lyra::lowering::hir_to_mir
