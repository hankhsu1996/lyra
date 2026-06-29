#include "lyra/lowering/hir_to_mir/expression/system/print.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/print_items.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/file_descriptor.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 21.3.1 stdout pre-bound FD literal, used as the sink for $display /
// $write / $strobe lowerings that don't carry a user-supplied descriptor.
auto BuildStdoutFdLiteral(mir::Block& block, mir::TypeId int32_type)
    -> mir::ExprId {
  return block.exprs.Add(mir::MakeInt32Literal(int32_type, support::kStdoutFd));
}

auto LowerDescriptor(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  if (!call.arguments[0].has_value()) {
    throw InternalError("$f-print descriptor argument unexpectedly elided");
  }
  return process.LowerExpr(
      process.HirBody().exprs.Get(*call.arguments[0]), frame);
}

auto WriteCalleeFor(bool append_newline) -> support::BuiltinFn {
  return append_newline ? support::BuiltinFn::kWriteln
                        : support::BuiltinFn::kWrite;
}

// Emits the two-step composition: format the items, then write to the sink.
// Both calls land in `block`; the returned id is the void write call.
auto EmitFormatThenWrite(
    ProcessLowerer& process, const WalkFrame& frame, mir::Block& block,
    mir::ExprId items_array, mir::ExprId fd, bool append_newline)
    -> mir::ExprId {
  auto& unit = process.Module().Unit();
  const mir::ExprId services =
      block.exprs.Add(BuildServicesCallExpr(process.Module(), frame));
  const mir::ExprId text =
      block.exprs.Add(BuildFormatCallExpr(unit, block, services, items_array));
  const mir::ExprId files =
      block.exprs.Add(BuildFilesCallExpr(process.Module(), frame));
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = WriteCalleeFor(append_newline)},
                  .arguments = {files, fd, text}},
          .type = unit.builtins.void_type});
}

// LRM 21.2.2 / 21.3.1: $strobe family. The postponed-region closure formats
// and writes at fire time; for a user-supplied descriptor, the body first
// gates on a channel-cancellation token snapshotted at submit time so an
// intervening $fclose silences the print.
auto LowerStrobeCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::PrintSystemSubroutineInfo& print, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  auto& unit = process.Module().Unit();
  const mir::TypeId void_type = unit.builtins.void_type;
  const mir::TypeId int32_type = unit.builtins.int32;
  const std::int64_t time_unit_power =
      static_cast<std::int64_t>(process.Resolution().unit_power);
  const bool is_file_sink = print.sink_kind == support::PrintSinkKind::kFile;

  std::optional<mir::ExprId> outer_user_descriptor;
  std::optional<mir::ExprId> outer_cancellation;
  if (is_file_sink) {
    auto desc_or = LowerDescriptor(process, frame, call);
    if (!desc_or) return std::unexpected(std::move(desc_or.error()));
    outer_user_descriptor = block.exprs.Add(*std::move(desc_or));
    const mir::ExprId outer_files =
        block.exprs.Add(BuildFilesCallExpr(process.Module(), frame));
    outer_cancellation = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{
                            .target = support::BuiltinFn::kCancellationFor},
                    .arguments = {outer_files, *outer_user_descriptor}},
            .type = unit.builtins.channel_cancellation});
  }

  ClosureBuilder closure(unit, frame);
  mir::Block& body = closure.Body();
  const WalkFrame& body_frame = closure.Frame();

  std::optional<mir::ExprId> body_user_descriptor;
  if (outer_user_descriptor.has_value()) {
    body_user_descriptor =
        closure.CaptureByValue(*outer_user_descriptor, "descriptor");
  }
  if (outer_cancellation.has_value()) {
    const mir::ExprId cancellation =
        closure.CaptureByValue(*outer_cancellation, "cancellation");
    const mir::ExprId is_cancelled = body.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kIsCancelled},
                    .arguments = {cancellation}},
            .type = unit.builtins.bit1});
    mir::Block guard;
    guard.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
    body.AppendStmt(
        mir::IfStmt{
            .condition = is_cancelled,
            .then_scope = body.child_scopes.Add(std::move(guard)),
            .else_scope = std::nullopt});
  }

  // Items lower through the closure's frame: an enclosing procedural-local
  // read becomes a by-value capture, while a module-signal read flows
  // through the closure's own `self` capture and re-reads at fire time
  // (LRM 21.2.2 sample-at-end-of-slot for signals).
  const std::size_t arg_offset = is_file_sink ? 1 : 0;
  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      process, body_frame, call, print.radix, arg_offset,
      FormatStringRequirement::kOptional, span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));
  const mir::ExprId items_array = body.exprs.Add(
      BuildPrintItemsArray(unit, body, *items_or, time_unit_power));

  const mir::ExprId body_fd = body_user_descriptor.has_value()
                                  ? *body_user_descriptor
                                  : BuildStdoutFdLiteral(body, int32_type);
  const mir::ExprId write_call = EmitFormatThenWrite(
      process, body_frame, body, items_array, body_fd, print.append_newline);
  body.AppendStmt(mir::ExprStmt{.expr = write_call});

  const mir::ExprId closure_id = block.exprs.Add(closure.BuildVoid());
  const mir::ExprId services =
      block.exprs.Add(BuildServicesCallExpr(process.Module(), frame));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{.target = support::BuiltinFn::kSubmitPostponed},
              .arguments = {services, closure_id}},
      .type = void_type};
}

}  // namespace

auto LowerPrintSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const support::PrintSystemSubroutineInfo& print, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  if (print.is_strobe) {
    return LowerStrobeCall(process, frame, call, print, span);
  }

  auto& block = *frame.current_block;
  auto& unit = process.Module().Unit();
  const mir::TypeId int32_type = unit.builtins.int32;
  const bool is_file_sink = print.sink_kind == support::PrintSinkKind::kFile;

  std::optional<mir::ExprId> user_descriptor;
  std::size_t arg_offset = 0;
  if (is_file_sink) {
    auto desc_or = LowerDescriptor(process, frame, call);
    if (!desc_or) return std::unexpected(std::move(desc_or.error()));
    user_descriptor = block.exprs.Add(*std::move(desc_or));
    arg_offset = 1;
  }

  auto items_or = BuildRuntimePrintItemsFromCallArgs(
      process, frame, call, print.radix, arg_offset,
      FormatStringRequirement::kOptional, span);
  if (!items_or) return std::unexpected(std::move(items_or.error()));

  const auto time_unit_power =
      static_cast<std::int64_t>(process.Resolution().unit_power);
  const mir::ExprId items_array = block.exprs.Add(
      BuildPrintItemsArray(unit, block, *items_or, time_unit_power));

  const mir::ExprId fd = user_descriptor.has_value()
                             ? *user_descriptor
                             : BuildStdoutFdLiteral(block, int32_type);
  const mir::ExprId write_call = EmitFormatThenWrite(
      process, frame, block, items_array, fd, print.append_newline);
  return block.exprs.Get(write_call);
}

}  // namespace lyra::lowering::hir_to_mir
