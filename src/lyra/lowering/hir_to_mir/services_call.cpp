#include "lyra/lowering/hir_to_mir/services_call.hpp"

#include <filesystem>
#include <format>
#include <string>

#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
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

auto BuildServicesCallExpr(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame) -> mir::Expr {
  auto& body = *frame.current_block;
  const auto& builtins = unit_lowerer.Unit().builtins;
  // A receiver-less callable (a package function or task, LRM 26.3) reaches the
  // engine through its own leading services binding, not through a `self` it
  // does not have. An instance callable derives the handle from its receiver.
  if (frame.current_class == nullptr) {
    const BodyBindingRef services =
        frame.bindings->EnsureCarrier(BindingOriginId::Services());
    return frame.bindings->MakeReadExpr(services, body);
  }
  const mir::ExprId self_id = body.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  return mir::MakeServicesCallExpr(self_id, builtins.services);
}

auto BuildFilesCallExpr(const UnitLowerer& unit_lowerer, const WalkFrame& frame)
    -> mir::Expr {
  auto& body = *frame.current_block;
  const auto& builtins = unit_lowerer.Unit().builtins;
  const mir::ExprId services_id =
      body.exprs.Add(BuildServicesCallExpr(unit_lowerer, frame));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kFiles},
              .arguments = {services_id}},
      .type = builtins.files};
}

auto BuildDiagnosticCallExpr(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame) -> mir::Expr {
  auto& body = *frame.current_block;
  const auto& builtins = unit_lowerer.Unit().builtins;
  const mir::ExprId services_id =
      body.exprs.Add(BuildServicesCallExpr(unit_lowerer, frame));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kDiagnostic},
              .arguments = {services_id}},
      .type = builtins.diagnostic};
}

auto BuildFormatCallExpr(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::ExprId services_id, mir::ExprId items_array) -> mir::Expr {
  const auto& builtins = unit.builtins;
  const mir::ExprId time_format_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kTimeFormat},
                  .arguments = {services_id}},
          .type = builtins.time_format});
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kFormat},
              .arguments = {items_array, time_format_id}},
      .type = builtins.string};
}

}  // namespace lyra::lowering::hir_to_mir
