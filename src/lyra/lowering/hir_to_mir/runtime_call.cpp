#include "lyra/lowering/hir_to_mir/runtime_call.hpp"

#include <filesystem>
#include <format>
#include <string>
#include <utility>
#include <vector>

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

void AppendRuntimeEffectStmt(
    const UnitLowerer& unit_lowerer, mir::Block& block,
    support::BuiltinFn entry, std::vector<mir::ExprId> operands) {
  std::vector<mir::ExprId> arguments;
  arguments.reserve(operands.size() + 1);
  arguments.push_back(
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer)));
  arguments.insert(arguments.end(), operands.begin(), operands.end());
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data =
                      mir::CallExpr{
                          .callee = mir::Direct{.target = entry},
                          .arguments = std::move(arguments)},
                  .type = unit_lowerer.Unit().builtins.void_type})});
}

auto BuildWaitStmt(
    const UnitLowerer& unit_lowerer, mir::Block& block,
    mir::ExprId registration) -> mir::Stmt {
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data = mir::WaitExpr{.registration = registration},
                  .type = unit_lowerer.Unit().builtins.void_type})}};
}

auto BuildAwaitStmt(
    const UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId execution)
    -> mir::Stmt {
  const auto& awaited = unit_lowerer.Unit()
                            .types.Get(block.exprs.Get(execution).type)
                            .Get<mir::CoroutineType>();
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data = mir::AwaitExpr{.execution = execution},
                  .type = awaited.payload})}};
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
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kFiles,
                      .receiver = runtime_id},
              .arguments = {}},
      .type = builtins.files};
}

auto BuildDiagnosticCallExpr(
    const mir::CompilationUnit& unit, mir::ExprId runtime_id) -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kDiagnostic,
                      .receiver = runtime_id},
              .arguments = {}},
      .type = unit.builtins.diagnostic};
}

auto BuildReportCallExpr(
    const mir::CompilationUnit& unit, support::BuiltinFn severity,
    mir::ExprId diagnostic_id, mir::ExprId origin_id, mir::ExprId text_id)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{.target = severity, .receiver = diagnostic_id},
              .arguments = {origin_id, text_id}},
      .type = unit.builtins.void_type};
}

void AppendToolReportStmt(
    const UnitLowerer& unit_lowerer, mir::Block& block,
    support::BuiltinFn severity, std::string text, diag::SourceSpan span) {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer));
  const mir::ExprId diagnostic_id =
      block.exprs.Add(BuildDiagnosticCallExpr(unit, runtime_id));
  const mir::ExprId origin_id = BuildStringValueExpr(
      unit, block,
      FormatRuntimeOriginString(span, unit_lowerer.SourceManager()));
  const mir::ExprId text_id =
      BuildStringValueExpr(unit, block, std::move(text));
  block.AppendStmt(
      mir::ExprStmt{
          .expr = block.exprs.Add(BuildReportCallExpr(
              unit, severity, diagnostic_id, origin_id, text_id))});
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
                      mir::Direct{
                          .target = support::BuiltinFn::kTimeFormat,
                          .receiver = runtime_id},
                  .arguments = {}},
          .type = builtins.time_format});
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kFormat},
              .arguments = {items_array, time_format_id}},
      .type = builtins.string};
}

}  // namespace lyra::lowering::hir_to_mir
