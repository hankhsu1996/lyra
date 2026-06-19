#include "lyra/lowering/hir_to_mir/expression/system/print.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_print.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto ToValuePrintKind(const support::PrintSystemSubroutineInfo& info)
    -> value::PrintKind {
  if (info.sink_kind == support::PrintSinkKind::kStdout) {
    return info.append_newline ? value::PrintKind::kDisplay
                               : value::PrintKind::kWrite;
  }
  return info.append_newline ? value::PrintKind::kFDisplay
                             : value::PrintKind::kFWrite;
}

// LRM 21.2.1.3: a %t directive scales by the enclosing scope's time unit, which
// is known only at lowering -- so its power is materialized here as the spec's
// sixth field rather than read from the directive like the others. Fields pass
// as `int` literals the runtime FormatSpec constructor converts; the modifier
// fields are omitted when none differs from its default.
auto BuildFormatSpecExpr(
    mir::CompilationUnit& unit, mir::ProceduralScope& scope,
    const mir::FormatSpec& spec, std::int64_t time_unit_power) -> mir::ExprId {
  const auto int_lit = [&](std::int64_t v) {
    return scope.AddExpr(mir::MakeInt32Literal(unit.builtins.int32, v));
  };
  std::vector<mir::ExprId> args;
  args.push_back(int_lit(static_cast<std::int64_t>(spec.kind)));
  const bool is_time = spec.kind == value::FormatKind::kTime;
  const bool all_default =
      spec.modifiers.width == -1 && spec.modifiers.precision == -1 &&
      !spec.modifiers.zero_pad && !spec.modifiers.left_align && !is_time;
  if (!all_default) {
    args.push_back(int_lit(spec.modifiers.width));
    args.push_back(int_lit(spec.modifiers.precision));
    args.push_back(int_lit(spec.modifiers.zero_pad ? 1 : 0));
    args.push_back(int_lit(spec.modifiers.left_align ? 1 : 0));
    args.push_back(int_lit(is_time ? time_unit_power : 0));
  }
  return scope.AddExpr(
      mir::Expr{
          .data = mir::ConstructExpr{.args = std::move(args)},
          .type = unit.builtins.format_spec});
}

auto BuildPrintItemExpr(
    mir::CompilationUnit& unit, mir::ProceduralScope& scope,
    const mir::RuntimePrintItem& item, std::int64_t time_unit_power)
    -> mir::ExprId {
  return std::visit(
      Overloaded{
          [&](const mir::RuntimePrintLiteral& lit) -> mir::ExprId {
            const mir::ExprId text = scope.AddExpr(
                mir::Expr{
                    .data = mir::StringLiteral{.value = lit.text},
                    .type = unit.builtins.string});
            return scope.AddExpr(
                mir::Expr{
                    .data = mir::ConstructExpr{.args = {text}},
                    .type = unit.builtins.print_literal_item});
          },
          [&](const mir::RuntimePrintValue& v) -> mir::ExprId {
            const mir::ExprId spec =
                BuildFormatSpecExpr(unit, scope, v.spec, time_unit_power);
            return scope.AddExpr(
                mir::Expr{
                    .data = mir::ConstructExpr{.args = {v.value, spec}},
                    .type = unit.builtins.print_value_item});
          }},
      item);
}

// The array's MIR type supplies only the element type to the renderer; its
// declared size is not read there, so it merely mirrors the element count.
auto BuildPrintItemsArray(
    mir::CompilationUnit& unit, mir::ProceduralScope& scope,
    const std::vector<mir::RuntimePrintItem>& items,
    std::int64_t time_unit_power) -> mir::ExprId {
  std::vector<mir::ExprId> elements;
  elements.reserve(items.size());
  for (const mir::RuntimePrintItem& item : items) {
    elements.push_back(BuildPrintItemExpr(unit, scope, item, time_unit_power));
  }
  const mir::TypeId array_type = unit.AddType(
      mir::TypeData{mir::UnpackedArrayType{
          .element_type = unit.builtins.print_item, .size = items.size()}});
  return scope.AddExpr(
      mir::Expr{
          .data = mir::ArrayLiteralExpr{.elements = std::move(elements)},
          .type = array_type});
}

}  // namespace

auto LowerPrintSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id,
    const support::PrintSystemSubroutineInfo& print, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  const mir::TypeId void_type = process.Module().Unit().builtins.void_type;

  std::optional<mir::ExprId> descriptor = std::nullopt;
  std::size_t arg_offset = 0;
  if (print.sink_kind == support::PrintSinkKind::kFile) {
    // LRM 21.3.2: the first argument of $fdisplay / $fwrite is an MCD/FD
    // descriptor; remaining arguments are the print payload. The runtime
    // decodes the bit pattern (MCD vs FD per LRM 21.3.1) at dispatch time.
    if (!call.arguments[0].has_value()) {
      throw InternalError("$f-print descriptor argument unexpectedly elided");
    }
    auto lowered_or =
        process.LowerExpr(hir_proc.exprs.at(call.arguments[0]->value), frame);
    if (!lowered_or) return std::unexpected(std::move(lowered_or.error()));
    descriptor = proc_scope.AddExpr(*std::move(lowered_or));
    arg_offset = 1;
  }

  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      process, frame, call, print.radix, arg_offset,
      FormatStringRequirement::kOptional, span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  // LRM 21.2.2: $strobe defers the same print to the postponed region. The
  // items keep their outer-scope ExprIds; the backend wraps the rendered print
  // in a postponed-region lambda whose init-capture list snapshots any
  // procedural-local operands.
  if (print.is_strobe) {
    mir::RuntimePrintCall print_call(
        ToValuePrintKind(print), descriptor, std::move(*items_or));
    return mir::Expr{
        .data =
            mir::RuntimeCallExpr{
                .call =
                    mir::RuntimeSubmitPostponedCall{
                        .print = std::move(print_call)}},
        .type = void_type};
  }

  auto& unit = process.Module().Unit();
  const auto time_unit_power =
      static_cast<std::int64_t>(process.Resolution().unit_power);
  const mir::ExprId items_array =
      BuildPrintItemsArray(unit, proc_scope, *items_or, time_unit_power);

  std::vector<mir::ExprId> args;
  args.push_back(proc_scope.AddExpr(BuildServicesCallExpr(process, frame)));
  if (descriptor.has_value()) {
    args.push_back(*descriptor);
  }
  args.push_back(items_array);

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::SystemSubroutineCallee{.id = id},
              .arguments = std::move(args)},
      .type = void_type};
}

}  // namespace lyra::lowering::hir_to_mir
