#include "lyra/lowering/hir_to_mir/expression/system/diagnostic.hpp"

#include <cstdint>
#include <expected>
#include <string>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Materializes `origin` as a `value::String` MIR expression: a `StringLiteral`
// renders as a raw C string in C++, so a `ConstructorCallee` of `string` type
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
                  .callee = mir::ConstructorCallee{},
                  .arguments = {origin_lit}},
          .type = string_type});
}

}  // namespace

auto LowerDiagnosticSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::DiagnosticSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  // $info / $warning / $error format their items with decimal as the bare-arg
  // default radix (LRM 20.10 / 21.2.1.4), then route through the diagnostic
  // dispatcher rather than a print sink. The call's source span becomes the
  // origin tag the dispatcher prepends and uses as its per-site dedup key.
  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      process, frame, call, support::PrintRadix::kDecimal, 0,
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
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::BuiltinFnCallee{.id = support::BuiltinFn::kFormat},
                  .arguments = {services_id, items_array}},
          .type = unit.builtins.string});
  const mir::ExprId diagnostic_id =
      block.exprs.Add(BuildDiagnosticCallExpr(process.Module(), frame));
  const mir::ExprId origin_id = BuildOriginStringExpr(
      unit, block,
      FormatRuntimeOriginString(span, process.Module().SourceManager()));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::BuiltinFnCallee{.id = info.builtin_fn},
              .arguments = {diagnostic_id, origin_id, text_id}},
      .type = unit.builtins.void_type};
}

}  // namespace lyra::lowering::hir_to_mir
