#include "lyra/lowering/hir_to_mir/lower_file_io.hpp"

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
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_file_io.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerFileOpenCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;

  auto name_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[0]->value));
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  const mir::ExprId name_id = proc_scope.AddExpr(*std::move(name_or));

  std::optional<mir::ExprId> mode_id = std::nullopt;
  if (call.arguments.size() == 2) {
    auto mode_or =
        LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[1]->value));
    if (!mode_or) return std::unexpected(std::move(mode_or.error()));
    mode_id = proc_scope.AddExpr(*std::move(mode_or));
  }

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call =
                  mir::RuntimeFileOpenCall{.name = name_id, .mode = mode_id}},
      .type = process.Module().Unit().builtins.int32};
}

auto LowerFileCloseCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;

  auto desc_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[0]->value));
  if (!desc_or) return std::unexpected(std::move(desc_or.error()));
  const mir::ExprId descriptor_id = proc_scope.AddExpr(*std::move(desc_or));

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeFileCloseCall{.descriptor = descriptor_id}},
      .type = process.Module().Unit().builtins.void_type};
}

auto LowerFileGetcCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  auto fd_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[0]->value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope.AddExpr(*std::move(fd_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{.call = mir::RuntimeFileGetcCall{.fd = fd_id}},
      .type = process.Module().Unit().builtins.int32};
}

auto LowerFileUngetcCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  auto c_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[0]->value));
  if (!c_or) return std::unexpected(std::move(c_or.error()));
  const mir::ExprId c_id = proc_scope.AddExpr(*std::move(c_or));
  auto fd_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[1]->value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope.AddExpr(*std::move(fd_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeFileUngetcCall{.c = c_id, .fd = fd_id}},
      .type = process.Module().Unit().builtins.int32};
}

auto LowerFileSeekCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  auto fd_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[0]->value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope.AddExpr(*std::move(fd_or));
  auto off_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[1]->value));
  if (!off_or) return std::unexpected(std::move(off_or.error()));
  const mir::ExprId off_id = proc_scope.AddExpr(*std::move(off_or));
  auto op_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[2]->value));
  if (!op_or) return std::unexpected(std::move(op_or.error()));
  const mir::ExprId op_id = proc_scope.AddExpr(*std::move(op_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call =
                  mir::RuntimeFileSeekCall{
                      .fd = fd_id, .offset = off_id, .operation = op_id}},
      .type = process.Module().Unit().builtins.int32};
}

auto LowerFileRewindCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  auto fd_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[0]->value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope.AddExpr(*std::move(fd_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{.call = mir::RuntimeFileRewindCall{.fd = fd_id}},
      .type = process.Module().Unit().builtins.int32};
}

auto LowerFileTellCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  auto fd_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[0]->value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope.AddExpr(*std::move(fd_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{.call = mir::RuntimeFileTellCall{.fd = fd_id}},
      .type = process.Module().Unit().builtins.int32};
}

auto LowerFileEofCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  auto fd_or =
      LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[0]->value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope.AddExpr(*std::move(fd_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{.call = mir::RuntimeFileEofCall{.fd = fd_id}},
      .type = process.Module().Unit().builtins.int32};
}

auto LowerFileFlushCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  const auto& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  std::optional<mir::ExprId> descriptor = std::nullopt;
  if (!call.arguments.empty()) {
    auto fd_or =
        LowerExpr(process, frame, hir_proc.exprs.at(call.arguments[0]->value));
    if (!fd_or) return std::unexpected(std::move(fd_or.error()));
    descriptor = proc_scope.AddExpr(*std::move(fd_or));
  }
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeFileFlushCall{.descriptor = descriptor}},
      .type = process.Module().Unit().builtins.void_type};
}

// LRM 13.5: $fgets / $fread / $ferror write into an actual lvalue argument
// and must desugar to copy-out semantics at the statement boundary, exactly
// like user-defined functions with `output` args (see
// `LowerSubroutineCallWithWritebacks`). At expression position we cannot
// safely synthesize the temp+writeback block, so reject and direct the user
// to a statement-position call.
auto RejectOutputArgFileCallInExprPosition(
    std::string_view name, diag::SourceSpan span) -> diag::Result<mir::Expr> {
  return diag::Unsupported(
      span, diag::DiagCode::kUnsupportedSubroutineArgument,
      std::format(
          "{} writes through an output argument and is only supported as a "
          "bare call or as the right-hand side of a blocking assignment "
          "(LRM 13.5 copy-out semantics)",
          std::string{name}),
      diag::UnsupportedCategory::kFeature);
}

}  // namespace

auto LowerFileIOSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    std::string_view name, const support::FileIOSystemSubroutineInfo& info,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  switch (info.kind) {
    case support::FileIOKind::kOpen:
      return LowerFileOpenCall(process, frame, call);
    case support::FileIOKind::kClose:
      return LowerFileCloseCall(process, frame, call);
    case support::FileIOKind::kGetc:
      return LowerFileGetcCall(process, frame, call);
    case support::FileIOKind::kUngetc:
      return LowerFileUngetcCall(process, frame, call);
    case support::FileIOKind::kSeek:
      return LowerFileSeekCall(process, frame, call);
    case support::FileIOKind::kRewind:
      return LowerFileRewindCall(process, frame, call);
    case support::FileIOKind::kTell:
      return LowerFileTellCall(process, frame, call);
    case support::FileIOKind::kEof:
      return LowerFileEofCall(process, frame, call);
    case support::FileIOKind::kFlush:
      return LowerFileFlushCall(process, frame, call);
    case support::FileIOKind::kGets:
    case support::FileIOKind::kRead:
    case support::FileIOKind::kError:
      // Output-arg tasks land here only when invoked in a nested expression
      // context (e.g., `if ($fgets(s, fd)) ...`). Statement-position calls
      // (`code = $fgets(s, fd);` / `$fgets(s, fd);`) get desugared upstream
      // in lower_stmt.cpp before reaching this dispatch.
      return RejectOutputArgFileCallInExprPosition(name, span);
  }
  throw InternalError("LowerFileIOSystemSubroutineCall: unknown FileIOKind");
}

auto LowerFileIOSystemSubroutineCallStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const support::FileIOSystemSubroutineInfo& info,
    std::optional<hir::ExprId> assign_target, mir::TypeId result_type)
    -> diag::Result<mir::Stmt> {
  if (!support::FileIOHasOutputArg(info.kind)) {
    throw InternalError(
        "LowerFileIOSystemSubroutineCallStmt: kind has no output arg");
  }

  const auto& module = process.Module();
  const auto& hir_proc = process.HirBody();

  mir::ProceduralScope wrapper;
  const WalkFrame wrapper_frame = frame.WithProceduralScope(&wrapper).Deeper();

  std::vector<OutputArgSlot> slots;
  mir::Expr call_expr{};

  switch (info.kind) {
    case support::FileIOKind::kGets: {
      // $fgets(str_lvalue, fd) -- arg[0] is the output string lvalue.
      auto slot_or = BuildOutputArgSlot(
          process, wrapper_frame, *call.arguments[0], "_lyra_fgets_dest");
      if (!slot_or) return std::unexpected(std::move(slot_or.error()));
      slots.push_back(*slot_or);
      auto fd_or = LowerExpr(
          process, wrapper_frame, hir_proc.exprs.at(call.arguments[1]->value));
      if (!fd_or) return std::unexpected(std::move(fd_or.error()));
      const mir::ExprId fd_id = wrapper.AddExpr(*std::move(fd_or));
      const mir::ExprId temp_ref = wrapper.AddExpr(
          mir::Expr{.data = slots[0].temp, .type = slots[0].type});
      call_expr = mir::Expr{
          .data =
              mir::RuntimeCallExpr{
                  .call =
                      mir::RuntimeFileGetsCall{
                          .str_dest = temp_ref, .fd = fd_id}},
          .type = process.Module().Unit().builtins.int32};
      break;
    }
    case support::FileIOKind::kRead: {
      // LRM 21.3.4.4: arg[0] is either a packed integral lvalue
      // (integral form) or an unpacked array (memory form).
      const auto& dest_hir = hir_proc.exprs.at(call.arguments[0]->value);
      const auto& dest_hir_ty = module.Hir().GetType(dest_hir.type);
      auto fd_or = LowerExpr(
          process, wrapper_frame, hir_proc.exprs.at(call.arguments[1]->value));
      if (!fd_or) return std::unexpected(std::move(fd_or.error()));
      const mir::ExprId fd_id = wrapper.AddExpr(*std::move(fd_or));

      const auto* unpacked =
          std::get_if<hir::UnpackedArrayType>(&dest_hir_ty.data);
      if (unpacked == nullptr) {
        // Integral form. LRM "start and count are ignored if $fread is
        // loading an integral variable" -- we reject the tolerant case
        // outright so the user gets a clear diagnostic instead of silent
        // arg-dropping.
        if (call.arguments.size() != 2) {
          return diag::Unsupported(
              diag::DiagCode::kUnsupportedSubroutineArgument,
              "$fread: integral form does not accept start/count "
              "arguments (LRM 21.3.4.4 says they are ignored; we reject "
              "to surface the mistake)",
              diag::UnsupportedCategory::kFeature);
        }
        auto slot_or = BuildOutputArgSlot(
            process, wrapper_frame, *call.arguments[0], "_lyra_fread_dest");
        if (!slot_or) return std::unexpected(std::move(slot_or.error()));
        slots.push_back(*slot_or);
        const mir::ExprId temp_ref = wrapper.AddExpr(
            mir::Expr{.data = slots[0].temp, .type = slots[0].type});
        call_expr = mir::Expr{
            .data =
                mir::RuntimeCallExpr{
                    .call =
                        mir::RuntimeFileReadCall{
                            .target =
                                mir::RuntimeFileReadCall::PackedTarget{
                                    .dest = temp_ref},
                            .fd = fd_id}},
            .type = process.Module().Unit().builtins.int32};
        break;
      }
      // Unpacked-array destination (LRM 21.3.4.4 "memory form"). Only 1D
      // unpacked of integral packed elements is in scope; struct / union /
      // multi-dim / dynamic-array element types are deferred.
      const auto& elem_ty = module.Hir().GetType(unpacked->element_type);
      if (!std::holds_alternative<hir::PackedArrayType>(elem_ty.data)) {
        return diag::Unsupported(
            diag::DiagCode::kUnsupportedSubroutineArgument,
            "$fread memory form: only 1D unpacked arrays of integral "
            "packed elements are supported (LRM 21.3.4.4)",
            diag::UnsupportedCategory::kFeature);
      }
      auto extract_optional =
          [&](std::size_t idx) -> diag::Result<std::optional<mir::ExprId>> {
        if (call.arguments.size() <= idx) return std::nullopt;
        if (!call.arguments[idx].has_value()) return std::nullopt;
        const auto& a = hir_proc.exprs.at(call.arguments[idx]->value);
        auto lowered = LowerExpr(process, wrapper_frame, a);
        if (!lowered) return std::unexpected(std::move(lowered.error()));
        return wrapper.AddExpr(*std::move(lowered));
      };
      auto start_or = extract_optional(2);
      if (!start_or) return std::unexpected(std::move(start_or.error()));
      auto count_or = extract_optional(3);
      if (!count_or) return std::unexpected(std::move(count_or.error()));
      // The unpacked-array lvalue flows in without copy-out: the runtime
      // helper mutates elements through the live reference, so the
      // BuildOutputArgSlot temp the integral path uses isn't needed here.
      auto dest_or = LowerExpr(process, wrapper_frame, dest_hir);
      if (!dest_or) return std::unexpected(std::move(dest_or.error()));
      const mir::ExprId dest_id = wrapper.AddExpr(*std::move(dest_or));
      call_expr = mir::Expr{
          .data =
              mir::RuntimeCallExpr{
                  .call =
                      mir::RuntimeFileReadCall{
                          .target =
                              mir::RuntimeFileReadCall::UnpackedTarget{
                                  .dest = dest_id,
                                  .start = *start_or,
                                  .count = *count_or,
                                  .declared_left = unpacked->dim.left,
                                  .declared_right = unpacked->dim.right},
                          .fd = fd_id}},
          .type = process.Module().Unit().builtins.int32};
      break;
    }
    case support::FileIOKind::kError: {
      // $ferror(fd, str_lvalue) -- arg[1] is the output string lvalue.
      auto fd_or = LowerExpr(
          process, wrapper_frame, hir_proc.exprs.at(call.arguments[0]->value));
      if (!fd_or) return std::unexpected(std::move(fd_or.error()));
      const mir::ExprId fd_id = wrapper.AddExpr(*std::move(fd_or));
      auto slot_or = BuildOutputArgSlot(
          process, wrapper_frame, *call.arguments[1], "_lyra_ferror_dest");
      if (!slot_or) return std::unexpected(std::move(slot_or.error()));
      slots.push_back(*slot_or);
      const mir::ExprId temp_ref = wrapper.AddExpr(
          mir::Expr{.data = slots[0].temp, .type = slots[0].type});
      call_expr = mir::Expr{
          .data =
              mir::RuntimeCallExpr{
                  .call =
                      mir::RuntimeFileErrorCall{
                          .fd = fd_id, .str_dest = temp_ref}},
          .type = process.Module().Unit().builtins.int32};
      break;
    }
    default:
      throw InternalError(
          "LowerFileIOSystemSubroutineCallStmt: unexpected FileIOKind");
  }

  std::optional<mir::ExprId> assign_target_id = std::nullopt;
  if (assign_target.has_value()) {
    auto lhs_or = LowerExpr(
        process, wrapper_frame, hir_proc.exprs.at(assign_target->value));
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    assign_target_id = wrapper.AddExpr(*std::move(lhs_or));
  }

  return BuildCopyOutBlock(
      frame, std::move(wrapper), std::move(label), result_type,
      std::move(call_expr), assign_target_id, slots);
}

}  // namespace lyra::lowering::hir_to_mir
