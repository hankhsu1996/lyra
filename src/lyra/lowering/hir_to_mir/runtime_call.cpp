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

auto BuildSuspendingCallStmt(
    const UnitLowerer& unit_lowerer, mir::Block& block, mir::ExprId call)
    -> mir::Stmt {
  const mir::CompilationUnit& unit = unit_lowerer.Unit();
  // Awaiting yields what the awaited thing completes with, which its own type
  // answers: an execution hands over the value it produced, and a call that
  // arranged a wait hands over nothing.
  const mir::Type& awaited = unit.types.Get(block.exprs.Get(call).type);
  const mir::TypeId result = awaited.Is<mir::CoroutineType>()
                                 ? awaited.Get<mir::CoroutineType>().payload
                                 : unit.builtins.void_type;
  return mir::Stmt{
      .label = std::nullopt,
      .data = mir::ExprStmt{
          .expr = block.exprs.Add(
              mir::Expr{
                  .data = mir::AwaitExpr{.awaitable = call}, .type = result})}};
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
