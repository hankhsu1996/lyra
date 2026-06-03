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
#include "lyra/hir/stmt.hpp"
#include "lyra/lowering/hir_to_mir/copy_out_desugar.hpp"
#include "lyra/lowering/hir_to_mir/lower_expr.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_file_io.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerFileOpenCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto name_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0].value));
  if (!name_or) return std::unexpected(std::move(name_or.error()));
  const mir::ExprId name_id = proc_scope_state.AddExpr(*std::move(name_or));

  std::optional<mir::ExprId> mode_id = std::nullopt;
  if (call.arguments.size() == 2) {
    auto mode_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(call.arguments[1].value));
    if (!mode_or) return std::unexpected(std::move(mode_or.error()));
    mode_id = proc_scope_state.AddExpr(*std::move(mode_or));
  }

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call =
                  mir::RuntimeFileOpenCall{.name = name_id, .mode = mode_id}},
      .type = unit_state.Builtins().int32};
}

auto LowerFileCloseCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto desc_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0].value));
  if (!desc_or) return std::unexpected(std::move(desc_or.error()));
  const mir::ExprId descriptor_id =
      proc_scope_state.AddExpr(*std::move(desc_or));

  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeFileCloseCall{.descriptor = descriptor_id}},
      .type = unit_state.Builtins().void_type};
}

auto LowerFileGetcCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto fd_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0].value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope_state.AddExpr(*std::move(fd_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{.call = mir::RuntimeFileGetcCall{.fd = fd_id}},
      .type = unit_state.Builtins().int32};
}

auto LowerFileUngetcCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto c_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0].value));
  if (!c_or) return std::unexpected(std::move(c_or.error()));
  const mir::ExprId c_id = proc_scope_state.AddExpr(*std::move(c_or));
  auto fd_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[1].value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope_state.AddExpr(*std::move(fd_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeFileUngetcCall{.c = c_id, .fd = fd_id}},
      .type = unit_state.Builtins().int32};
}

auto LowerFileSeekCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto fd_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0].value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope_state.AddExpr(*std::move(fd_or));
  auto off_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[1].value));
  if (!off_or) return std::unexpected(std::move(off_or.error()));
  const mir::ExprId off_id = proc_scope_state.AddExpr(*std::move(off_or));
  auto op_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[2].value));
  if (!op_or) return std::unexpected(std::move(op_or.error()));
  const mir::ExprId op_id = proc_scope_state.AddExpr(*std::move(op_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call =
                  mir::RuntimeFileSeekCall{
                      .fd = fd_id, .offset = off_id, .operation = op_id}},
      .type = unit_state.Builtins().int32};
}

auto LowerFileRewindCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto fd_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0].value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope_state.AddExpr(*std::move(fd_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{.call = mir::RuntimeFileRewindCall{.fd = fd_id}},
      .type = unit_state.Builtins().int32};
}

auto LowerFileTellCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto fd_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0].value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope_state.AddExpr(*std::move(fd_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{.call = mir::RuntimeFileTellCall{.fd = fd_id}},
      .type = unit_state.Builtins().int32};
}

auto LowerFileEofCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  auto fd_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
      hir_proc.exprs.at(call.arguments[0].value));
  if (!fd_or) return std::unexpected(std::move(fd_or.error()));
  const mir::ExprId fd_id = proc_scope_state.AddExpr(*std::move(fd_or));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{.call = mir::RuntimeFileEofCall{.fd = fd_id}},
      .type = unit_state.Builtins().int32};
}

auto LowerFileFlushCall(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call)
    -> diag::Result<mir::Expr> {
  std::optional<mir::ExprId> descriptor = std::nullopt;
  if (!call.arguments.empty()) {
    auto fd_or = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
        hir_proc.exprs.at(call.arguments[0].value));
    if (!fd_or) return std::unexpected(std::move(fd_or.error()));
    descriptor = proc_scope_state.AddExpr(*std::move(fd_or));
  }
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeFileFlushCall{.descriptor = descriptor}},
      .type = unit_state.Builtins().void_type};
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
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_proc, const hir::CallExpr& call,
    const support::SystemSubroutineDesc& desc,
    const support::FileIOSystemSubroutineInfo& info, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  switch (info.kind) {
    case support::FileIOKind::kOpen:
      return LowerFileOpenCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
          call);
    case support::FileIOKind::kClose:
      return LowerFileCloseCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
          call);
    case support::FileIOKind::kGetc:
      return LowerFileGetcCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
          call);
    case support::FileIOKind::kUngetc:
      return LowerFileUngetcCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
          call);
    case support::FileIOKind::kSeek:
      return LowerFileSeekCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
          call);
    case support::FileIOKind::kRewind:
      return LowerFileRewindCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
          call);
    case support::FileIOKind::kTell:
      return LowerFileTellCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
          call);
    case support::FileIOKind::kEof:
      return LowerFileEofCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
          call);
    case support::FileIOKind::kFlush:
      return LowerFileFlushCall(
          unit_state, scope_state, proc_state, proc_scope_state, hir_proc,
          call);
    case support::FileIOKind::kGets:
    case support::FileIOKind::kRead:
    case support::FileIOKind::kError:
      // Output-arg tasks land here only when invoked in a nested expression
      // context (e.g., `if ($fgets(s, fd)) ...`). Statement-position calls
      // (`code = $fgets(s, fd);` / `$fgets(s, fd);`) get desugared upstream
      // in lower_stmt.cpp before reaching this dispatch.
      return RejectOutputArgFileCallInExprPosition(desc.name, span);
  }
  throw InternalError("LowerFileIOSystemSubroutineCall: unknown FileIOKind");
}

auto LowerFileIOSystemSubroutineCallStmt(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProcessLoweringState& proc_state, const hir::ProceduralBody& hir_proc,
    const hir::Stmt& stmt, [[maybe_unused]] diag::SourceSpan span,
    const hir::CallExpr& call,
    [[maybe_unused]] const support::SystemSubroutineDesc& desc,
    const support::FileIOSystemSubroutineInfo& info,
    std::optional<hir::ExprId> assign_target, mir::TypeId result_type)
    -> diag::Result<mir::Stmt> {
  if (!support::FileIOHasOutputArg(info.kind)) {
    throw InternalError(
        "LowerFileIOSystemSubroutineCallStmt: kind has no output arg");
  }

  ProceduralScopeLoweringState wrapper;
  ProceduralDepthGuard depth_guard{proc_state};

  std::vector<OutputArgSlot> slots;
  mir::Expr call_expr{};

  switch (info.kind) {
    case support::FileIOKind::kGets: {
      // $fgets(str_lvalue, fd) -- arg[0] is the output string lvalue.
      auto slot_or = BuildOutputArgSlot(
          unit_state, scope_state, proc_state, wrapper, hir_proc,
          call.arguments[0], "_lyra_fgets_dest");
      if (!slot_or) return std::unexpected(std::move(slot_or.error()));
      slots.push_back(*slot_or);
      auto fd_or = LowerExpr(
          unit_state, scope_state, proc_state, wrapper, hir_proc,
          hir_proc.exprs.at(call.arguments[1].value));
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
          .type = unit_state.Builtins().int32};
      break;
    }
    case support::FileIOKind::kRead: {
      // $fread(integral_lvalue, fd) -- arg[0] is the output integral lvalue.
      auto slot_or = BuildOutputArgSlot(
          unit_state, scope_state, proc_state, wrapper, hir_proc,
          call.arguments[0], "_lyra_fread_dest");
      if (!slot_or) return std::unexpected(std::move(slot_or.error()));
      slots.push_back(*slot_or);
      auto fd_or = LowerExpr(
          unit_state, scope_state, proc_state, wrapper, hir_proc,
          hir_proc.exprs.at(call.arguments[1].value));
      if (!fd_or) return std::unexpected(std::move(fd_or.error()));
      const mir::ExprId fd_id = wrapper.AddExpr(*std::move(fd_or));
      const mir::ExprId temp_ref = wrapper.AddExpr(
          mir::Expr{.data = slots[0].temp, .type = slots[0].type});
      call_expr = mir::Expr{
          .data =
              mir::RuntimeCallExpr{
                  .call =
                      mir::RuntimeFileReadCall{
                          .int_dest = temp_ref, .fd = fd_id}},
          .type = unit_state.Builtins().int32};
      break;
    }
    case support::FileIOKind::kError: {
      // $ferror(fd, str_lvalue) -- arg[1] is the output string lvalue.
      auto fd_or = LowerExpr(
          unit_state, scope_state, proc_state, wrapper, hir_proc,
          hir_proc.exprs.at(call.arguments[0].value));
      if (!fd_or) return std::unexpected(std::move(fd_or.error()));
      const mir::ExprId fd_id = wrapper.AddExpr(*std::move(fd_or));
      auto slot_or = BuildOutputArgSlot(
          unit_state, scope_state, proc_state, wrapper, hir_proc,
          call.arguments[1], "_lyra_ferror_dest");
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
          .type = unit_state.Builtins().int32};
      break;
    }
    default:
      throw InternalError(
          "LowerFileIOSystemSubroutineCallStmt: unexpected FileIOKind");
  }

  std::optional<mir::ExprId> assign_target_id = std::nullopt;
  if (assign_target.has_value()) {
    auto lhs_or = LowerExpr(
        unit_state, scope_state, proc_state, wrapper, hir_proc,
        hir_proc.exprs.at(assign_target->value));
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    assign_target_id = wrapper.AddExpr(*std::move(lhs_or));
  }

  return BuildCopyOutBlock(
      wrapper, stmt, result_type, std::move(call_expr), assign_target_id,
      slots);
}

}  // namespace lyra::lowering::hir_to_mir
