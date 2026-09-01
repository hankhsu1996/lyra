#include "lyra/lowering/hir_to_mir/expression/system/file_io.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/param_direction.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Assembles the file-IO call as an instance method on the `files` broker.
// `files = runtime.Files()` is interned first; subsequent operands flow
// after it in their runtime-signature order.
auto BuildFileIoCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    support::BuiltinFn builtin_fn, std::vector<mir::ExprId> operands,
    mir::TypeId result_type) -> mir::Expr {
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> args;
  args.reserve(operands.size() + 1);
  args.push_back(block.exprs.Add(BuildFilesCallExpr(process.Owner(), block)));
  for (const mir::ExprId operand : operands) {
    args.push_back(operand);
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = builtin_fn},
              .arguments = std::move(args)},
      .type = result_type};
}

auto LowerOperand(
    ProcessLowerer& process, const WalkFrame& frame, hir::ExprId operand)
    -> diag::Result<mir::Expr> {
  return process.LowerExpr(process.HirBody().exprs.Get(operand), frame);
}

auto LowerFixedOperandCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::BuiltinFn builtin_fn, std::size_t operand_count,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const std::vector<hir::ExprId> operands =
      RequiredOperands(call, operand_count);
  std::vector<mir::ExprId> lowered;
  lowered.reserve(operands.size());
  for (const hir::ExprId operand : operands) {
    auto operand_or = LowerOperand(process, frame, operand);
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    lowered.push_back(block.exprs.Add(*std::move(operand_or)));
  }
  return BuildFileIoCall(
      process, frame, builtin_fn, std::move(lowered), result_type);
}

// Opening with a mode yields a file descriptor and opening without one yields a
// multichannel descriptor (LRM 21.3.1). Which was written is known here, so the
// call names the form it means and nothing downstream recovers it by counting
// operands.
auto LowerFileOpenCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const std::vector<hir::ExprId> head = RequiredLeadingOperands(call, 1);
  auto name_or = LowerOperand(process, frame, head[0]);
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  std::vector<mir::ExprId> operands{block.exprs.Add(*std::move(name_or))};
  const std::optional<hir::ExprId> mode = OptionalOperand(call, 1);
  if (mode) {
    auto mode_or = LowerOperand(process, frame, *mode);
    if (!mode_or) return std::unexpected(std::move(mode_or.error()));
    operands.push_back(block.exprs.Add(*std::move(mode_or)));
  }
  return BuildFileIoCall(
      process, frame,
      mode ? support::BuiltinFn::kFileOpenMode : support::BuiltinFn::kFileOpen,
      std::move(operands), process.Owner().Unit().builtins.int_type);
}

// Flushing an addressed channel and flushing every open one (LRM 21.3.6) are
// two requests, told apart here by which the source wrote.
auto LowerFileFlushCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  std::vector<mir::ExprId> operands;
  const std::optional<hir::ExprId> fd = OptionalOperand(call, 0);
  if (fd) {
    auto fd_or = LowerOperand(process, frame, *fd);
    if (!fd_or) return std::unexpected(std::move(fd_or.error()));
    operands.push_back(frame.current_block->exprs.Add(*std::move(fd_or)));
  }
  return BuildFileIoCall(
      process, frame,
      fd ? support::BuiltinFn::kFileFlush : support::BuiltinFn::kFileFlushAll,
      std::move(operands), process.Owner().Unit().builtins.void_type);
}

// The text a read delivers is an SV string; LRM 21.3.4.2 also admits an
// integral destination, whose width bounds the read, and no entry carries that
// sizing rule yet.
auto RequireStringDestination(
    const mir::CompilationUnit& unit, mir::TypeId destination,
    std::string_view name, diag::SourceSpan span) -> diag::Result<void> {
  if (unit.types.Get(destination).Is<mir::StringType>()) return {};
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedSubroutineArgument,
      std::format(
          "{} into an integral destination is not yet supported, where the "
          "destination's width is what bounds the read (LRM 21.3.4.2)",
          std::string{name}));
}

// Emits the call, stores the component it settles for the destination into the
// place the source named, and yields the count it settled for itself -- the
// steps of one block expression, so a read that writes back stands wherever the
// source wrote it.
//
// The destination is an `output` for either of these, so nothing crosses in
// (LRM 13.5): what the place held before says nothing about the text being
// delivered, and all of it is replaced.
auto BuildTextRead(
    ProcessLowerer& process, BlockBuilder& steps, support::BuiltinFn builtin_fn,
    std::vector<mir::ExprId> operands, mir::ExprId destination,
    mir::TypeId destination_type) -> mir::Expr {
  auto& unit = process.Owner().Unit();
  const mir::TypeId count_type = unit.builtins.int_type;
  const CompletionLayout layout = BuildCompletionLayout(
      {CalleeFormal{
          .direction = hir::ParamDirection::kOutput, .type = destination_type}},
      count_type);
  const mir::TypeId payload = CompletionPayloadType(unit, layout.components);
  const std::array writebacks{CompletionWriteback{
      .place = destination,
      .component = *layout.formals.front().component,
      .type = destination_type}};
  const mir::LocalId completion = BindCompletion(
      unit, steps.Frame(),
      BuildFileIoCall(
          process, steps.Frame(), builtin_fn, std::move(operands), payload),
      payload, writebacks);
  return steps.Build(ProjectCompletionComponent(
      steps.Body(), completion, payload, base::ComponentIndex{}, count_type));
}

// $fgets(str, fd) -- LRM 21.3.4.2.
auto LowerFileGetsCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    std::string_view name, diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const std::vector<hir::ExprId> operands = RequiredOperands(call, 2);
  BlockBuilder steps(frame);

  const hir::Expr& line_hir = process.HirExprs().Get(operands[0]);
  const mir::TypeId line_type = process.Owner().TranslateType(line_hir.type);
  auto valid_or =
      RequireStringDestination(process.Owner().Unit(), line_type, name, span);
  if (!valid_or) return std::unexpected(std::move(valid_or.error()));
  auto line_or = process.LowerLhsExpr(line_hir, steps.Frame());
  if (!line_or) return std::unexpected(std::move(line_or.error()));
  const mir::ExprId line_place = steps.Body().exprs.Add(*std::move(line_or));

  auto fd_or = LowerOperand(process, steps.Frame(), operands[1]);
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = steps.Body().exprs.Add(*std::move(fd_or));

  return BuildTextRead(
      process, steps, support::BuiltinFn::kFileGets, {fd_id}, line_place,
      line_type);
}

// $fread -- LRM 21.3.4.4. Which destination the source named says which of the
// two requests it is: a packed variable takes the bytes whole, a memory takes
// them word by word from an address it is told, and the clause gives the two
// different addressing. The destination crosses in as well as back, because
// its own shape decides how many bytes a word takes and what the file does not
// reach keeps what it held.
auto LowerFileReadCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto& unit_lowerer = process.Owner();
  auto& unit = unit_lowerer.Unit();
  const auto& hir_proc = process.HirBody();
  const std::vector<hir::ExprId> head = RequiredLeadingOperands(call, 2);
  const auto& dest_hir_ty =
      unit_lowerer.Hir().types.Get(hir_proc.exprs.Get(head[0]).type);
  const auto* memory = dest_hir_ty.As<hir::UnpackedArrayType>();

  if (memory == nullptr) {
    // LRM 21.3.4.4 says start and count "are ignored if $fread is loading an
    // integral variable"; refusing rather than dropping them surfaces the
    // mistake instead of running a call the source did not write.
    if (call.arguments.size() != 2) {
      return diag::Fail(
          diag::DiagCode::kUnsupportedSubroutineArgument,
          "$fread: the integral form takes no start / count arguments (LRM "
          "21.3.4.4 ignores them)");
    }
  } else if (!unit_lowerer.Hir()
                  .types.Get(memory->element_type)
                  .IsBitVector()) {
    return diag::Fail(
        diag::DiagCode::kUnsupportedSubroutineArgument,
        "$fread into a memory is supported for a one-dimensional unpacked "
        "array of integral words (LRM 21.3.4.4)");
  }

  BlockBuilder steps(frame);
  const WalkFrame& step_frame = steps.Frame();
  mir::Block& body = steps.Body();

  // The destination is an `inout`: its own shape decides how many bytes a word
  // takes and what the file does not reach keeps what it held, so its value
  // crosses in as well as riding the completion back (LRM 13.5, 21.3.4.4).
  const hir::Expr& dest_hir = hir_proc.exprs.Get(head[0]);
  const mir::TypeId dest_type = unit_lowerer.TranslateType(dest_hir.type);
  auto dest_or = process.LowerLhsExpr(dest_hir, step_frame);
  if (!dest_or) return std::unexpected(std::move(dest_or.error()));
  const mir::ExprId dest_place = body.exprs.Add(*std::move(dest_or));
  const CompletionLayout layout = BuildCompletionLayout(
      {CalleeFormal{
          .direction = hir::ParamDirection::kInOut, .type = dest_type}},
      unit.builtins.int_type);
  auto incoming_or = LowerOperand(process, step_frame, head[0]);
  if (!incoming_or) return std::unexpected(std::move(incoming_or.error()));
  std::vector<mir::ExprId> operands{body.exprs.Add(*std::move(incoming_or))};

  auto fd_or = LowerOperand(process, step_frame, head[1]);
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  operands.push_back(body.exprs.Add(*std::move(fd_or)));

  if (memory != nullptr) {
    operands.push_back(BuildIntLiteral(unit, body, memory->dim.left));
    operands.push_back(BuildIntLiteral(unit, body, memory->dim.right));
    const std::int64_t lowest = std::min(memory->dim.left, memory->dim.right);
    const std::int64_t highest = std::max(memory->dim.left, memory->dim.right);
    // The start the source left out is the lowest declared index, and the
    // count it left out is the whole memory -- which the entry then clamps to
    // what stands between the start and the end. Materializing both keeps one
    // entry for every form the source may write.
    if (const std::optional<hir::ExprId> start = OptionalOperand(call, 2)) {
      auto start_or = LowerOperand(process, step_frame, *start);
      if (!start_or) return std::unexpected(std::move(start_or.error()));
      operands.push_back(body.exprs.Add(*std::move(start_or)));
    } else {
      operands.push_back(BuildIntLiteral(unit, body, lowest));
    }
    if (const std::optional<hir::ExprId> count = OptionalOperand(call, 3)) {
      auto count_or = LowerOperand(process, step_frame, *count);
      if (!count_or) return std::unexpected(std::move(count_or.error()));
      operands.push_back(body.exprs.Add(*std::move(count_or)));
    } else {
      operands.push_back(BuildIntLiteral(unit, body, highest - lowest + 1));
    }
  }

  const mir::TypeId payload = CompletionPayloadType(unit, layout.components);
  const std::array writebacks{CompletionWriteback{
      .place = dest_place,
      .component = *layout.formals.front().component,
      .type = dest_type}};
  const mir::LocalId completion = BindCompletion(
      unit, step_frame,
      BuildFileIoCall(
          process, step_frame,
          memory != nullptr ? support::BuiltinFn::kFileReadMemory
                            : support::BuiltinFn::kFileRead,
          std::move(operands), payload),
      payload, writebacks);
  return steps.Build(ProjectCompletionComponent(
      body, completion, payload, base::ComponentIndex{},
      unit.builtins.int_type));
}

// $ferror(fd, str) -- LRM 21.3.7.
auto LowerFileErrorCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    std::string_view name, diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const std::vector<hir::ExprId> operands = RequiredOperands(call, 2);
  BlockBuilder steps(frame);

  auto fd_or = LowerOperand(process, steps.Frame(), operands[0]);
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = steps.Body().exprs.Add(*std::move(fd_or));

  const hir::Expr& message_hir = process.HirExprs().Get(operands[1]);
  const mir::TypeId message_type =
      process.Owner().TranslateType(message_hir.type);
  auto valid_or = RequireStringDestination(
      process.Owner().Unit(), message_type, name, span);
  if (!valid_or) return std::unexpected(std::move(valid_or.error()));
  auto message_or = process.LowerLhsExpr(message_hir, steps.Frame());
  if (!message_or) return std::unexpected(std::move(message_or.error()));
  const mir::ExprId message_place =
      steps.Body().exprs.Add(*std::move(message_or));

  return BuildTextRead(
      process, steps, support::BuiltinFn::kFileError, {fd_id}, message_place,
      message_type);
}

}  // namespace

auto LowerFileIOSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    std::string_view name, const support::FileIOSystemSubroutineInfo& info,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  const auto& builtins = process.Owner().Unit().builtins;
  switch (info.builtin_fn) {
    case support::BuiltinFn::kFileOpen:
      return LowerFileOpenCall(process, frame, call);
    case support::BuiltinFn::kFileClose:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 1, builtins.void_type);
    case support::BuiltinFn::kFileGetc:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 1, builtins.int_type);
    case support::BuiltinFn::kFileUngetc:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 2, builtins.int_type);
    case support::BuiltinFn::kFileSeek:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 3, builtins.int_type);
    case support::BuiltinFn::kFileRewind:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 1, builtins.int_type);
    case support::BuiltinFn::kFileTell:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 1, builtins.int_type);
    case support::BuiltinFn::kFileEof:
      return LowerFixedOperandCall(
          process, frame, call, info.builtin_fn, 1, builtins.int_type);
    case support::BuiltinFn::kFileFlush:
      return LowerFileFlushCall(process, frame, call);
    case support::BuiltinFn::kFileGets:
      return LowerFileGetsCall(process, frame, call, name, span);
    case support::BuiltinFn::kFileError:
      return LowerFileErrorCall(process, frame, call, name, span);
    case support::BuiltinFn::kFileRead:
      return LowerFileReadCall(process, frame, call);
    default:
      throw InternalError(
          "LowerFileIOSystemSubroutineCall: unexpected file-IO BuiltinFn");
  }
}

}  // namespace lyra::lowering::hir_to_mir
