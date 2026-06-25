#include "lyra/lowering/hir_to_mir/expression/system/diagnostic.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/primary.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Materializes `origin` as a `value::String` MIR expression: a `StringLiteral`
// renders as a raw C string in C++, so a `Construct` of `string` type
// wraps it to satisfy the runtime method's `const value::String&` parameter.
auto BuildOriginStringExpr(
    const mir::CompilationUnit& unit, mir::Block& block, std::string origin)
    -> mir::ExprId {
  const mir::TypeId string_type = unit.builtins.string;
  const mir::ExprId origin_lit = block.exprs.Add(
      mir::Expr{
          .data = mir::StringLiteral{.value = std::move(origin)},
          .type = string_type});
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Construct{}, .arguments = {origin_lit}},
          .type = string_type});
}

auto TryExtractLiteralInt(const hir::Expr& expr)
    -> std::optional<std::int64_t> {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
  if (primary == nullptr) return std::nullopt;
  const auto* lit = std::get_if<hir::IntegerLiteral>(&primary->data);
  if (lit == nullptr) return std::nullopt;
  const auto& c = lit->value;
  if (c.state_kind == hir::IntegralStateKind::kFourState) return std::nullopt;
  if (c.value_words.empty()) return 0;
  return static_cast<std::int64_t>(c.value_words[0]);
}

}  // namespace

auto LowerDiagnosticSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::DiagnosticSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  // $info / $warning / $error / $fatal format their items with decimal as the
  // bare-arg default radix (LRM 20.10 / 21.2.1.4), then route through the
  // diagnostic dispatcher rather than a print sink. The call's source span
  // becomes the origin tag the dispatcher prepends and uses as its per-site
  // dedup key. $fatal additionally consumes its first argument as the
  // `finish_number` (LRM 20.2 verbosity level) and chains an implicit $finish
  // after the EmitFatal record.
  const bool is_fatal = info.builtin_fn == support::BuiltinFn::kEmitFatal;

  // LRM 20.2 default verbosity level. Honored only by the $fatal path.
  int finish_level = 1;
  std::size_t items_offset = 0;
  if (is_fatal && !call.arguments.empty()) {
    if (!call.arguments.front().has_value()) {
      throw InternalError("$fatal finish_number argument unexpectedly elided");
    }
    const hir::Expr& level_expr =
        process.HirBody().exprs.Get(*call.arguments.front());
    const auto literal = TryExtractLiteralInt(level_expr);
    if (!literal.has_value()) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "$fatal first argument (finish_number) must be an integer literal");
    }
    if (*literal != 0 && *literal != 1 && *literal != 2) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "$fatal finish_number must be 0, 1, or 2");
    }
    finish_level = static_cast<int>(*literal);
    items_offset = 1;
  }

  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      process, frame, call, support::PrintRadix::kDecimal, items_offset,
      FormatStringRequirement::kOptional, span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  auto& unit = process.Module().Unit();
  auto& block = *frame.current_block;
  const auto time_unit_power =
      static_cast<std::int64_t>(process.Resolution().unit_power);
  const mir::ExprId items_array = block.exprs.Add(
      BuildPrintItemsArray(unit, block, *items_or, time_unit_power));

  const mir::ExprId services_id =
      block.exprs.Add(BuildServicesCallExpr(process.Module(), frame));
  const mir::ExprId text_id = block.exprs.Add(
      BuildFormatCallExpr(unit, block, services_id, items_array));
  const mir::ExprId diagnostic_id =
      block.exprs.Add(BuildDiagnosticCallExpr(process.Module(), frame));
  const mir::ExprId origin_id = BuildOriginStringExpr(
      unit, block,
      FormatRuntimeOriginString(span, process.Module().SourceManager()));

  mir::Expr emit_call{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = info.builtin_fn},
              .arguments = {diagnostic_id, origin_id, text_id}},
      .type = unit.builtins.void_type};

  if (!is_fatal) return emit_call;

  // $fatal: emit happens as its own statement, then the implicit $finish call
  // becomes this lowering's returned expression. The caller wraps the finish
  // call in an AwaitStmt (the descriptor sets `suspends = true`) and the two
  // statements land in the enclosing block in source order.
  const mir::ExprId emit_call_id = block.exprs.Add(std::move(emit_call));
  block.AppendStmt(mir::ExprStmt{.expr = emit_call_id});

  const mir::ExprId finish_services_id =
      block.exprs.Add(BuildServicesCallExpr(process.Module(), frame));
  const mir::ExprId level_id = block.exprs.Add(
      mir::MakeInt32Literal(
          unit.builtins.int32, static_cast<std::int64_t>(finish_level)));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kFatalFinish},
              .arguments = {finish_services_id, level_id}},
      .type = unit.builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir
