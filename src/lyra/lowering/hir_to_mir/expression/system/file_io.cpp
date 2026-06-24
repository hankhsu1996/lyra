#include "lyra/lowering/hir_to_mir/expression/system/file_io.hpp"

#include <algorithm>
#include <cstddef>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/copy_out_desugar.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Assembles the generic file-IO call: the engine handle (self.Services()) as
// argument 0, then the task operands in runtime-signature order. Every
// file-IO runtime entry takes RuntimeServices& first, so the handle is a
// leading argument, not a backend-injected fact.
auto BuildFileIoCall(
    const ProcessLowerer& process, const WalkFrame& frame,
    support::SystemSubroutineId id, std::vector<mir::ExprId> operands,
    mir::TypeId result_type) -> mir::Expr {
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> args;
  args.reserve(operands.size() + 1);
  args.push_back(
      block.exprs.Add(BuildServicesCallExpr(process.Module(), frame)));
  for (const mir::ExprId operand : operands) {
    args.push_back(operand);
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::SystemSubroutineCallee{.id = id},
              .arguments = std::move(args)},
      .type = result_type};
}

auto LowerOperand(
    ProcessLowerer& process, const WalkFrame& frame, const hir::CallExpr& call,
    std::size_t index) -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  return process.LowerExpr(hir_proc.exprs.Get(*call.arguments[index]), frame);
}

auto LowerFixedOperandCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id, std::size_t operand_count,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> operands;
  operands.reserve(operand_count);
  for (std::size_t i = 0; i < operand_count; ++i) {
    auto operand_or = LowerOperand(process, frame, call, i);
    if (!operand_or) return std::unexpected(std::move(operand_or.error()));
    operands.push_back(block.exprs.Add(*std::move(operand_or)));
  }
  return BuildFileIoCall(process, frame, id, std::move(operands), result_type);
}

// LRM 21.3.1: the one-argument MCD form and the two-argument FD form select the
// runtime overload by argument count alone.
auto LowerFileOpenCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  auto name_or = LowerOperand(process, frame, call, 0);
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  std::vector<mir::ExprId> operands{block.exprs.Add(*std::move(name_or))};
  if (call.arguments.size() == 2) {
    auto mode_or = LowerOperand(process, frame, call, 1);
    if (!mode_or) return std::unexpected(std::move(mode_or.error()));
    operands.push_back(block.exprs.Add(*std::move(mode_or)));
  }
  return BuildFileIoCall(
      process, frame, id, std::move(operands),
      process.Module().Unit().builtins.int32);
}

// LRM 21.3.6: the no-argument flush-all form and the addressed form select the
// runtime overload by argument count alone.
auto LowerFileFlushCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id) -> diag::Result<mir::Expr> {
  std::vector<mir::ExprId> operands;
  if (!call.arguments.empty()) {
    auto fd_or = LowerOperand(process, frame, call, 0);
    if (!fd_or) return std::unexpected(std::move(fd_or.error()));
    operands.push_back(frame.current_block->exprs.Add(*std::move(fd_or)));
  }
  return BuildFileIoCall(
      process, frame, id, std::move(operands),
      process.Module().Unit().builtins.void_type);
}

// LRM 13.5: $fgets / $fread / $ferror write into an actual lvalue argument
// and must desugar to copy-out semantics at the statement boundary, exactly
// like user-defined functions with `output` args (see
// `LowerSubroutineCallWithWritebacks`). At expression position we cannot
// safely synthesize the temp+writeback block, so reject and direct the user
// to a statement-position call.
auto RejectOutputArgFileCallInExprPosition(
    std::string_view name, diag::SourceSpan span) -> diag::Result<mir::Expr> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedSubroutineArgument,
      std::format(
          "{} writes through an output argument and is only supported as a "
          "bare call or as the right-hand side of a blocking assignment "
          "(LRM 13.5 copy-out semantics)",
          std::string{name}));
}

}  // namespace

auto LowerFileIOSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    support::SystemSubroutineId id, std::string_view name,
    const support::FileIOSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  const auto& builtins = process.Module().Unit().builtins;
  switch (info.kind) {
    case support::FileIOKind::kOpen:
      return LowerFileOpenCall(process, frame, call, id);
    case support::FileIOKind::kClose:
      return LowerFixedOperandCall(
          process, frame, call, id, 1, builtins.void_type);
    case support::FileIOKind::kGetc:
      return LowerFixedOperandCall(process, frame, call, id, 1, builtins.int32);
    case support::FileIOKind::kUngetc:
      return LowerFixedOperandCall(process, frame, call, id, 2, builtins.int32);
    case support::FileIOKind::kSeek:
      return LowerFixedOperandCall(process, frame, call, id, 3, builtins.int32);
    case support::FileIOKind::kRewind:
      return LowerFixedOperandCall(process, frame, call, id, 1, builtins.int32);
    case support::FileIOKind::kTell:
      return LowerFixedOperandCall(process, frame, call, id, 1, builtins.int32);
    case support::FileIOKind::kEof:
      return LowerFixedOperandCall(process, frame, call, id, 1, builtins.int32);
    case support::FileIOKind::kFlush:
      return LowerFileFlushCall(process, frame, call, id);
    case support::FileIOKind::kGets:
    case support::FileIOKind::kRead:
    case support::FileIOKind::kError:
      // Output-arg tasks land here only when invoked in a nested expression
      // context (e.g., `if ($fgets(s, fd)) ...`). Statement-position calls
      // (`code = $fgets(s, fd);` / `$fgets(s, fd);`) get desugared upstream
      // in `statement/assignment.cpp` (ExprStmt path) before reaching this
      // dispatch.
      return RejectOutputArgFileCallInExprPosition(name, span);
  }
  throw InternalError("LowerFileIOSystemSubroutineCall: unknown FileIOKind");
}

auto LowerFileIOSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, support::SystemSubroutineId id,
    const support::FileIOSystemSubroutineInfo& info,
    std::optional<hir::ExprId> assign_target, mir::TypeId result_type)
    -> diag::Result<mir::Stmt> {
  if (!support::FileIOHasOutputArg(info.kind)) {
    throw InternalError(
        "LowerFileIOSystemSubroutineCallStmt: kind has no output arg");
  }

  const auto& module = process.Module();
  const auto& hir_proc = process.HirBody();

  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper).Deeper();

  std::vector<OutputArgSlot> slots;
  mir::Expr call_expr{};

  switch (info.kind) {
    case support::FileIOKind::kGets: {
      // $fgets(str_lvalue, fd) -- arg[0] is the output string lvalue.
      auto slot_or = BuildOutputArgSlot(
          process, wrapper_frame, *call.arguments[0], "_lyra_fgets_dest");
      if (!slot_or) return std::unexpected(std::move(slot_or.error()));
      slots.push_back(*slot_or);
      auto fd_or = process.LowerExpr(
          hir_proc.exprs.Get(*call.arguments[1]), wrapper_frame);
      if (!fd_or) return std::unexpected(std::move(fd_or.error()));
      const mir::ExprId fd_id = wrapper.exprs.Add(*std::move(fd_or));
      const mir::ExprId temp_ref = wrapper.exprs.Add(
          mir::Expr{.data = slots[0].temp, .type = slots[0].type});
      call_expr = BuildFileIoCall(
          process, wrapper_frame, id, {temp_ref, fd_id},
          process.Module().Unit().builtins.int32);
      break;
    }
    case support::FileIOKind::kRead: {
      // LRM 21.3.4.4: arg[0] is either a packed integral lvalue (integral
      // form) or an unpacked array (memory form). Both write the destination,
      // so both round-trip through a copy-out temp; the runtime call is a
      // generic CallExpr whose operands are the temp, the descriptor, and --
      // for the memory form -- the declared bounds plus start / count.
      const auto& dest_hir = hir_proc.exprs.Get(*call.arguments[0]);
      const auto& dest_hir_ty = module.Hir().GetType(dest_hir.type);
      const auto& builtins = process.Module().Unit().builtins;
      const auto* unpacked =
          std::get_if<hir::UnpackedArrayType>(&dest_hir_ty.data);

      if (unpacked == nullptr) {
        // Integral form. LRM "start and count are ignored if $fread is
        // loading an integral variable" -- reject the tolerant case so the
        // user gets a clear diagnostic instead of silent arg-dropping.
        if (call.arguments.size() != 2) {
          return diag::Fail(
              diag::DiagCode::kUnsupportedSubroutineArgument,
              "$fread: integral form does not accept start/count "
              "arguments (LRM 21.3.4.4 says they are ignored; we reject "
              "to surface the mistake)");
        }
      } else {
        // Memory form. Only 1D unpacked of integral packed elements is in
        // scope; struct / union / multi-dim / dynamic-array element types
        // are deferred.
        const auto& elem_ty = module.Hir().GetType(unpacked->element_type);
        if (!std::holds_alternative<hir::PackedArrayType>(elem_ty.data)) {
          return diag::Fail(
              diag::DiagCode::kUnsupportedSubroutineArgument,
              "$fread memory form: only 1D unpacked arrays of integral "
              "packed elements are supported (LRM 21.3.4.4)");
        }
      }

      auto fd_or = process.LowerExpr(
          hir_proc.exprs.Get(*call.arguments[1]), wrapper_frame);
      if (!fd_or) return std::unexpected(std::move(fd_or.error()));
      const mir::ExprId fd_id = wrapper.exprs.Add(*std::move(fd_or));

      auto slot_or = BuildOutputArgSlot(
          process, wrapper_frame, *call.arguments[0], "_lyra_fread_dest");
      if (!slot_or) return std::unexpected(std::move(slot_or.error()));
      slots.push_back(*slot_or);
      const mir::ExprId temp_ref = wrapper.exprs.Add(
          mir::Expr{.data = slots[0].temp, .type = slots[0].type});

      std::vector<mir::ExprId> operands{temp_ref, fd_id};
      if (unpacked != nullptr) {
        operands.push_back(wrapper.exprs.Add(
            mir::MakeInt32Literal(builtins.int32, unpacked->dim.left)));
        operands.push_back(wrapper.exprs.Add(
            mir::MakeInt32Literal(builtins.int32, unpacked->dim.right)));
        // start: the SV index, or the lowest declared index when omitted
        // (LRM 21.3.4.4 default). Synthesizing it keeps count the only
        // trailing-optional operand.
        if (call.arguments.size() > 2 && call.arguments[2].has_value()) {
          auto start_or = process.LowerExpr(
              hir_proc.exprs.Get(*call.arguments[2]), wrapper_frame);
          if (!start_or) return std::unexpected(std::move(start_or.error()));
          operands.push_back(wrapper.exprs.Add(*std::move(start_or)));
        } else {
          operands.push_back(wrapper.exprs.Add(
              mir::MakeInt32Literal(
                  builtins.int32,
                  std::min(unpacked->dim.left, unpacked->dim.right))));
        }
        if (call.arguments.size() > 3 && call.arguments[3].has_value()) {
          auto count_or = process.LowerExpr(
              hir_proc.exprs.Get(*call.arguments[3]), wrapper_frame);
          if (!count_or) return std::unexpected(std::move(count_or.error()));
          operands.push_back(wrapper.exprs.Add(*std::move(count_or)));
        }
      }
      call_expr = BuildFileIoCall(
          process, wrapper_frame, id, std::move(operands), builtins.int32);
      break;
    }
    case support::FileIOKind::kError: {
      // $ferror(fd, str_lvalue) -- arg[1] is the output string lvalue.
      auto fd_or = process.LowerExpr(
          hir_proc.exprs.Get(*call.arguments[0]), wrapper_frame);
      if (!fd_or) return std::unexpected(std::move(fd_or.error()));
      const mir::ExprId fd_id = wrapper.exprs.Add(*std::move(fd_or));
      auto slot_or = BuildOutputArgSlot(
          process, wrapper_frame, *call.arguments[1], "_lyra_ferror_dest");
      if (!slot_or) return std::unexpected(std::move(slot_or.error()));
      slots.push_back(*slot_or);
      const mir::ExprId temp_ref = wrapper.exprs.Add(
          mir::Expr{.data = slots[0].temp, .type = slots[0].type});
      call_expr = BuildFileIoCall(
          process, wrapper_frame, id, {fd_id, temp_ref},
          process.Module().Unit().builtins.int32);
      break;
    }
    default:
      throw InternalError(
          "LowerFileIOSystemSubroutineCallStmt: unexpected FileIOKind");
  }

  std::optional<mir::ExprId> assign_target_id = std::nullopt;
  if (assign_target.has_value()) {
    auto lhs_or =
        process.LowerLhsExpr(hir_proc.exprs.Get(*assign_target), wrapper_frame);
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    assign_target_id = wrapper.exprs.Add(*std::move(lhs_or));
  }

  const mir::ExprId services_id =
      wrapper.exprs.Add(BuildServicesCallExpr(process.Module(), wrapper_frame));
  return BuildCopyOutBlock(
      process.Module().Unit(), services_id, frame, std::move(wrapper),
      std::move(label), result_type, std::move(call_expr), false,
      assign_target_id, slots);
}

}  // namespace lyra::lowering::hir_to_mir
