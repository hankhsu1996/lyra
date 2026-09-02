#include "lyra/lowering/hir_to_mir/runtime_call.hpp"

#include <filesystem>
#include <format>
#include <string>

#include "lyra/diag/source_manager.hpp"
#include "lyra/diag/source_span.hpp"
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

auto BuildCurrentRuntimeCallExpr(const UnitLowerer& unit_lowerer) -> mir::Expr {
  return mir::MakeCurrentRuntimeCallExpr(unit_lowerer.Unit().builtins.effects);
}

auto BuildStringValueExpr(
    const mir::CompilationUnit& unit, mir::Block& block, std::string text)
    -> mir::ExprId {
  const mir::TypeId string_type = unit.builtins.string;
  const mir::ExprId literal = block.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = std::move(text)},
          .type = string_type});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {literal}},
          .type = string_type});
}

auto BuildFilesCallExpr(const UnitLowerer& unit_lowerer, mir::Block& block)
    -> mir::Expr {
  const auto& builtins = unit_lowerer.Unit().builtins;
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kFiles},
              .arguments = {runtime_id}},
      .type = builtins.files};
}

auto BuildDiagnosticCallExpr(
    const mir::CompilationUnit& unit, mir::ExprId runtime_id) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kDiagnostic},
              .arguments = {runtime_id}},
      .type = unit.builtins.diagnostic};
}

auto BuildFormatCallExpr(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId runtime_id,
    mir::ExprId items_array) -> mir::Expr {
  const auto& builtins = unit.builtins;
  const mir::ExprId time_format_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kTimeFormat},
                  .arguments = {runtime_id}},
          .type = builtins.time_format});
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kFormat},
              .arguments = {items_array, time_format_id}},
      .type = builtins.string};
}

}  // namespace lyra::lowering::hir_to_mir
