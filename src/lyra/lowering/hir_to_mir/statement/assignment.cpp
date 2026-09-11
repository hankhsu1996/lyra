#include "lyra/lowering/hir_to_mir/statement/assignment.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/mem_file.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/sformat.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 11.4.12 LHS destructuring desugar. Triggered when an ExprStmt wraps an
// AssignExpr whose LHS is a ConcatExpr -- the only context in which
// destructuring is grammatically legal. Emits a block that snapshots the RHS
// into a single packed temp then distributes per-part slices to each LHS
// operand. The source wrote one assignment, so a nonblocking one carries every
// part into one deferred effect: a control on it is read once, and every part's
// share lands in the same slot.
auto LowerDestructuringAssign(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::AssignExpr& assign, const hir::ConcatExpr& lhs_concat,
    diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  std::vector<std::uint64_t> part_widths;
  part_widths.reserve(lhs_concat.operands.size());
  mir::IntegralStateKind state_kind = mir::IntegralStateKind::kTwoState;
  std::uint64_t total_width = 0;
  const auto& hir_types = process.Owner().Hir().types;
  const auto& mir_types = process.Owner().Unit().types;
  for (const hir::ExprId op_id : lhs_concat.operands) {
    const hir::Expr& op = hir_proc.exprs.Get(op_id);
    if (!hir_types.Get(op.type).IsIntegral()) {
      throw InternalError(
          "LowerDestructuringAssign: destructuring operand is not "
          "an integral type");
    }
    // Width and state domain are properties of the operand's MIR type, which
    // is what the snapshot is sliced against.
    const auto& packed =
        mir_types.Get(process.Owner().TranslateType(op.type)).PackedShape();
    const std::uint64_t w = packed.BitWidth();
    part_widths.push_back(w);
    total_width += w;
    if (packed.state_kind == mir::IntegralStateKind::kFourState) {
      state_kind = mir::IntegralStateKind::kFourState;
    }
  }
  if (total_width == 0) {
    throw InternalError(
        "LowerDestructuringAssign: destructuring total width must be positive");
  }

  const mir::TypeId temp_type = mir::PackedVectorOf(
      process.Owner().Unit().types, total_width, state_kind);

  const mir::ExprId temp_default_init = wrapper.exprs.Add(
      BuildDefaultValueExpr(process.Owner().Unit(), wrapper, temp_type));
  const mir::LocalId snapshot_var =
      wrapper_frame.bindings->DeclareAnonymous(temp_type);
  wrapper.AppendStmt(
      mir::LocalDeclStmt{.target = snapshot_var, .init = temp_default_init});

  // RHS is evaluated once; the snapshot temp is what gets distributed,
  // which is what makes `{a, b} = {b, a}` swap correctly.
  auto rhs_or =
      process.LowerExpr(hir_proc.exprs.Get(assign.rhs), wrapper_frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  mir::ExprId rhs_id = wrapper.exprs.Add(*std::move(rhs_or));
  if (wrapper.exprs.Get(rhs_id).type != temp_type) {
    rhs_id = wrapper.exprs.Add(BuildValueConversion(
        process.Owner().Unit(), wrapper, rhs_id, temp_type));
  }

  const mir::ExprId temp_assign_target =
      wrapper.exprs.Add(mir::MakeLocalRefExpr(snapshot_var, temp_type));
  const mir::ExprId temp_assign_id = wrapper.exprs.Add(
      mir::Expr{
          .data =
              mir::AssignExpr{.target = temp_assign_target, .value = rhs_id},
          .type = temp_type});
  wrapper.AppendStmt(mir::ExprStmt{.expr = temp_assign_id});

  // MSB-first per LRM 11.4.12: operands[0] occupies the high bits of the
  // snapshot, operands.back() the low bits.
  std::vector<DestructuredPart> parts;
  parts.reserve(lhs_concat.operands.size());
  std::uint64_t offset = total_width;
  for (std::size_t i = 0; i < lhs_concat.operands.size(); ++i) {
    const std::uint64_t w = part_widths[i];
    offset -= w;

    auto part_lhs_or = process.LowerLhsExpr(
        hir_proc.exprs.Get(lhs_concat.operands[i]), wrapper_frame);
    if (!part_lhs_or) {
      return std::unexpected(std::move(part_lhs_or.error()));
    }
    const mir::TypeId part_mir_type = process.Owner().TranslateType(
        hir_proc.exprs.Get(lhs_concat.operands[i]).type);

    const mir::ExprId temp_ref =
        wrapper.exprs.Add(mir::MakeLocalRefExpr(snapshot_var, temp_type));
    const mir::TypeId slice_type =
        mir::PackedVectorOf(process.Owner().Unit().types, w, state_kind);
    const mir::ExprId slice_id = wrapper.exprs.Add(BuildPackedRunRead(
        process.Owner(), wrapper, temp_ref, offset, w, slice_type));
    mir::ExprId rhs_for_part = slice_id;
    if (part_mir_type != slice_type) {
      rhs_for_part = wrapper.exprs.Add(BuildValueConversion(
          process.Owner().Unit(), wrapper, slice_id, part_mir_type));
    }

    parts.push_back(
        DestructuredPart{
            .target = *std::move(part_lhs_or),
            .value = rhs_for_part,
            .type = part_mir_type});
  }

  if (const auto* deferred =
          std::get_if<hir::NonBlockingEffect>(&assign.timing)) {
    auto effect_or = BuildDestructuredDeferredAssign(
        process, wrapper_frame, span, deferred->control, parts);
    if (!effect_or) return std::unexpected(std::move(effect_or.error()));
    wrapper.AppendStmt(
        mir::ExprStmt{.expr = wrapper.exprs.Add(*std::move(effect_or))});
  } else {
    for (const DestructuredPart& part : parts) {
      wrapper.AppendStmt(
          mir::ExprStmt{
              .expr = wrapper.exprs.Add(BuildStoreExpr(
                  process.Owner().Unit(), wrapper, part.target, part.value,
                  std::nullopt, part.type))});
    }
  }

  const mir::BlockId wrapper_scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

// One target of an unpack, with the width it takes off the stream. The width
// comes from the target's type, so it is read where the targets are gathered
// and carried to where each share is cut.
struct UnpackTarget {
  mir::TypeId type;
  std::uint64_t width;
};

// LRM 11.4.14.3 unpack. The source is a bit-stream value, or the stream another
// streaming concatenation built; either way it is read out as bits, and the
// targets are filled from the stream's most significant end in the order the
// source wrote them. Where the stream carries more bits than the targets need,
// the surplus is at its least significant end and is dropped -- which is why
// the usable run is taken before the re-ordering rather than after.
//
// The shape follows the LRM 11.4.12 destructuring beside it: the source is
// snapshotted once and distributed, so a target appearing on both sides reads
// what the assignment started with.
auto LowerStreamingUnpackAssign(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::AssignExpr& assign, const hir::StreamingConcatExpr& lhs_stream,
    diag::SourceSpan span) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  const mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  // What each target is and how wide, and never its state domain: the bits come
  // from the source, so the stream's domain is the source's, and a target's own
  // is applied where its share is read back at its shape.
  std::vector<UnpackTarget> targets;
  targets.reserve(lhs_stream.operands.size());
  std::uint64_t targets_width = 0;
  for (const hir::ExprId op_id : lhs_stream.operands) {
    const hir::Expr& target = hir_proc.exprs.Get(op_id);
    const mir::TypeId target_type = process.Owner().TranslateType(target.type);
    const std::optional<StreamShape> shape =
        FixedStreamShapeOf(unit.types, target_type);
    if (!shape.has_value()) {
      return diag::Fail(
          target.span, diag::DiagCode::kUnsupportedExpressionForm,
          "filling a value of this type from a stream of bits is not yet "
          "supported (LRM 11.4.14.3)");
    }
    targets.push_back(UnpackTarget{.type = target_type, .width = shape->width});
    targets_width += shape->width;
  }

  const hir::Expr& rhs = hir_proc.exprs.Get(assign.rhs);
  auto rhs_or = process.LowerExpr(rhs, wrapper_frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  auto source_or = BuildToBitstream(
      unit, wrapper, wrapper.exprs.Add(*std::move(rhs_or)), rhs.span);
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  const mir::ExprId source_id = *source_or;
  // The stream's own shape is on the type the pack just gave it, so it is read
  // there rather than worked out again from the value it came from. By value:
  // the pool's view does not survive the interning below.
  const mir::PackedArrayType source =
      unit.types.Get(wrapper.exprs.Get(source_id).type).PackedShape();
  if (source.BitWidth() < targets_width) {
    throw InternalError(
        "LowerStreamingUnpackAssign: the front end refuses a source with "
        "fewer bits than the targets need (LRM 11.4.14.3) -- please report "
        "this as a bug");
  }

  // The bits the targets will take, in the order they will take them: the
  // surplus dropped and the blocks re-ordered once, ahead of any target's
  // share, so the source is evaluated once and no part recomputes the whole.
  const mir::TypeId stream_type =
      mir::PackedVectorOf(unit.types, targets_width, source.state_kind);
  const mir::ExprId distributable_id = BuildReorderedStream(
      unit, wrapper,
      wrapper.exprs.Add(BuildPackedRunRead(
          process.Owner(), wrapper, source_id,
          source.BitWidth() - targets_width, targets_width, stream_type)),
      lhs_stream.block_bits);
  const mir::LocalId stream_var =
      wrapper_frame.bindings->DeclareAnonymous(stream_type);
  wrapper.AppendStmt(
      mir::LocalDeclStmt{
          .target = stream_var,
          .init = wrapper.exprs.Add(
              BuildDefaultValueExpr(unit, wrapper, stream_type))});
  wrapper.AppendStmt(
      mir::ExprStmt{
          .expr = wrapper.exprs.Add(
              mir::Expr{
                  .data =
                      mir::AssignExpr{
                          .target = wrapper.exprs.Add(
                              mir::MakeLocalRefExpr(stream_var, stream_type)),
                          .value = distributable_id},
                  .type = stream_type})});

  std::vector<DestructuredPart> parts;
  parts.reserve(targets.size());
  std::uint64_t consumed = 0;
  for (std::size_t i = 0; i < targets.size(); ++i) {
    const UnpackTarget& target = targets[i];
    const hir::Expr& target_expr = hir_proc.exprs.Get(lhs_stream.operands[i]);
    auto part_lhs_or = process.LowerLhsExpr(target_expr, wrapper_frame);
    if (!part_lhs_or) {
      return std::unexpected(std::move(part_lhs_or.error()));
    }
    const mir::TypeId segment_type =
        mir::PackedVectorOf(unit.types, target.width, source.state_kind);
    const mir::ExprId segment_id = wrapper.exprs.Add(BuildPackedRunRead(
        process.Owner(), wrapper,
        wrapper.exprs.Add(mir::MakeLocalRefExpr(stream_var, stream_type)),
        targets_width - consumed - target.width, target.width, segment_type));
    consumed += target.width;
    auto value_or = BuildFromBitstream(
        unit, wrapper, segment_id, target.type, target_expr.span);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    parts.push_back(
        DestructuredPart{
            .target = *std::move(part_lhs_or),
            .value = wrapper.exprs.Add(*std::move(value_or)),
            .type = target.type});
  }

  if (const auto* deferred =
          std::get_if<hir::NonBlockingEffect>(&assign.timing)) {
    auto effect_or = BuildDestructuredDeferredAssign(
        process, wrapper_frame, span, deferred->control, parts);
    if (!effect_or) return std::unexpected(std::move(effect_or.error()));
    wrapper.AppendStmt(
        mir::ExprStmt{.expr = wrapper.exprs.Add(*std::move(effect_or))});
  } else {
    for (const DestructuredPart& part : parts) {
      wrapper.AppendStmt(
          mir::ExprStmt{
              .expr = wrapper.exprs.Add(BuildStoreExpr(
                  process.Owner().Unit(), wrapper, part.target, part.value,
                  std::nullopt, part.type))});
    }
  }

  const mir::BlockId wrapper_scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

// Whether this call statement is a suspension point (LRM 13.3). A callee whose
// completion the caller awaits states that in its call's type, so that is read
// from the type rather than re-derived -- the type is the single carrier of a
// call protocol, and reading it here is what keeps it so. The visit answers
// what a type cannot: a callee that parks the process through a runtime entry
// instead of completing, and one whose enable lowers to something other than a
// call. It is exhaustive over the callee kinds, so a kind that becomes
// suspendable forces a decision here rather than silently defaulting to
// non-suspending.
auto CallStatementSuspends(
    ProcessLowerer& process, const hir::CallExpr& call, mir::TypeId call_type)
    -> bool {
  if (process.Owner().Unit().types.Get(call_type).Is<mir::CoroutineType>()) {
    return true;
  }
  return std::visit(
      Overloaded{
          [](const hir::SystemSubroutineRef& sys) {
            return support::LookupSystemSubroutine(sys.id).suspends;
          },
          // An intra-unit task enable and a cross-unit one (LRM 26.3) both
          // complete as coroutines, which the call's type already answered.
          [](const hir::StructuralSubroutineRef&) { return false; },
          [](const hir::ExternalUnitSubroutineRef&) { return false; },
          // A task an interface publishes (LRM 25.7) completes as a coroutine
          // on the same terms, and so does one a hierarchical name reaches
          // through no promise at all (LRM 23.6).
          [](const hir::ExternalUnitMethodRef&) { return false; },
          [](const hir::OpaqueUnitMethodRef&) { return false; },
          // A foreign task import (LRM 35.5.2) completes as a coroutine too,
          // which the call's type already answered.
          [](const hir::ForeignImportRef&) { return false; },
          // A class method that is a task (LRM 8.6, 13.3) completes as a
          // coroutine, which the call's type already answered.
          [](const hir::MethodCallRef&) { return false; },
          [](const hir::StaticMethodCallRef&) { return false; },
          // A built-in method mostly computes a value against a library type
          // and never yields; the ones that park the caller until something
          // else settles (LRM 9.7 `await`) say so on their own declaration.
          [](const hir::BuiltinMethodRef& b) {
            return support::RuntimeEntryOf(b.method).parks_the_caller;
          },
          // An enumerated type method (LRM 6.19.5) is answered from the member
          // table, either as a constant or by a synthesized non-task callable.
          [](const hir::EnumMethodRef&) { return false; },
          // A sampled value function reads state a clocking event's ticks have
          // already settled (LRM 16.9.3), so it waits for nothing.
          [](const hir::PastValueRef&) { return false; },
          [](const hir::ValueChangeRef&) { return false; },
      },
      call.callee);
}

// The statement-position lowering a system subroutine needs when its effect
// cannot be expressed as a bare value: a file write whose formatted output is
// bound to an output argument, and the `$sformat` / `$swrite` family whose
// result lands in an output variable. Nullopt for every family that lowers as
// an ordinary expression. Exhaustive over the semantic families, so a new one
// forces a decision about whether it has a statement form.
//
// The label is borrowed rather than taken: this may decline, and a caller that
// had handed its label over would carry on with an emptied one and silently
// drop the name a `disable` resolves against (LRM 9.6.2). Only the paths that
// commit to a statement copy it.
auto LowerSystemSubroutineCallStmtForm(
    ProcessLowerer& process, WalkFrame frame,
    const std::optional<std::string>& label, const hir::CallExpr& call,
    const hir::SystemSubroutineRef& ref,
    std::optional<hir::ExprId> assign_target)
    -> std::optional<diag::Result<mir::Stmt>> {
  const auto& desc = support::LookupSystemSubroutine(ref.id);
  return std::visit(
      Overloaded{
          [](const support::FileIOSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [&](const support::SFormatSystemSubroutineInfo& sformat)
              -> std::optional<diag::Result<mir::Stmt>> {
            // The statement form writes into the call's own output variable
            // (`$sformat` / `$swrite`) or discards the text (`$sformatf`). A
            // call feeding an assignment target is necessarily the valued
            // `$sformatf`, whose result the assignment consumes, so it stays an
            // ordinary expression there.
            if (assign_target.has_value()) return std::nullopt;
            return LowerSFormatSystemSubroutineCallStmt(
                process, frame, label, call, sformat);
          },
          [](const support::PrintSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::TerminationSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::DiagnosticSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::ScanSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::TimeSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::TimeFormatSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::PrintTimescaleSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::PlusargsSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::BitVectorSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::HostCommandSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::RandomSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::DistributionSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            // The seed writeback is sequenced inside the call's own expression
            // lowering, so a statement-position call needs nothing extra.
            return std::nullopt;
          },
          [](const support::SampledValueSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            // Reading a sampled value settles nothing outside the value it
            // answers with, so a statement-position call has no form of its own
            // and lowers as the expression it is.
            return std::nullopt;
          },
          [](const support::ValueChangeSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [](const support::PastValueSystemSubroutineInfo&)
              -> std::optional<diag::Result<mir::Stmt>> {
            return std::nullopt;
          },
          [&](const support::MemFileSystemSubroutineInfo& mem_file)
              -> std::optional<diag::Result<mir::Stmt>> {
            // A void task (LRM 21.4 / 21.5): its only form is a statement, so
            // it never feeds an assignment target. The lowering branches on
            // load vs dump.
            return LowerMemFileSystemSubroutineCallStmt(
                process, frame, label, call, mem_file);
          },
      },
      desc.semantic);
}

}  // namespace

auto LowerExprStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ExprStmt& e) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  auto& block = *frame.current_block;

  // LRM 11.4.12 LHS destructuring: detect AssignExpr-with-ConcatExpr-LHS
  // and dispatch to the snapshot+distribute desugar.
  const hir::Expr& inner = hir_proc.exprs.Get(e.expr);
  if (const auto* assign = std::get_if<hir::AssignExpr>(&inner.data)) {
    const hir::Expr& lhs = hir_proc.exprs.Get(assign->lhs);
    if (const auto* concat = std::get_if<hir::ConcatExpr>(&lhs.data)) {
      if (assign->compound_op.has_value()) {
        throw InternalError(
            "LowerExprStmt: compound assignment with concatenation lvalue "
            "is not a legal SV form (LRM A.6.2 grammar)");
      }
      return LowerDestructuringAssign(
          process, frame, std::move(label), *assign, *concat, inner.span);
    }
    // LRM 11.4.14.3: the same grammatical position, filled from a stream of
    // bits rather than from a value of the target's own shape.
    if (const auto* stream = std::get_if<hir::StreamingConcatExpr>(&lhs.data)) {
      if (assign->compound_op.has_value()) {
        throw InternalError(
            "LowerExprStmt: compound assignment with a streaming lvalue is "
            "not a legal SV form (LRM A.6.2 grammar)");
      }
      return LowerStreamingUnpackAssign(
          process, frame, std::move(label), *assign, *stream, inner.span);
    }
  }

  // A call statement. A suspending callee ($finish, a task) is awaited here,
  // and what the await produces is what the awaitable carries: a callee that
  // completes as a coroutine hands over its completion payload, one that parks
  // the process through a runtime entry hands over nothing. Either way the
  // statement is the discard.
  if (const auto* call = std::get_if<hir::CallExpr>(&inner.data)) {
    const mir::TypeId inner_type = process.Owner().TranslateType(inner.type);
    if (const auto* sys_ref =
            std::get_if<hir::SystemSubroutineRef>(&call->callee)) {
      if (auto stmt = LowerSystemSubroutineCallStmtForm(
              process, frame, label, *call, *sys_ref, std::nullopt)) {
        return *std::move(stmt);
      }
    }
    if (const auto* import_ref =
            std::get_if<hir::ForeignImportRef>(&call->callee)) {
      if (auto stmt = LowerForeignImportCallStmtForm(
              process, frame, label, *call, *import_ref)) {
        return *std::move(stmt);
      }
    }
    if (auto stmt = LowerSubroutineCallStmtForm(
            process, frame, label, *call, inner_type)) {
      return *std::move(stmt);
    }
    auto call_or = process.LowerExpr(inner, frame);
    if (!call_or) return std::unexpected(std::move(call_or.error()));
    const mir::ExprId call_id = block.exprs.Add(*std::move(call_or));
    const mir::TypeId call_type = block.exprs.Get(call_id).type;
    if (CallStatementSuspends(process, *call, call_type)) {
      const mir::TypePool& types = process.Owner().Unit().types;
      const mir::Type& called = types.Get(call_type);
      const mir::ExprId await_id = block.exprs.Add(
          mir::Expr{
              .data = mir::AwaitExpr{.awaitable = call_id},
              .type = called.Is<mir::CoroutineType>()
                          ? called.Get<mir::CoroutineType>().payload
                          : process.Owner().Unit().builtins.void_type});
      return mir::Stmt{
          .label = std::move(label), .data = mir::ExprStmt{.expr = await_id}};
    }
    return mir::Stmt{
        .label = std::move(label), .data = mir::ExprStmt{.expr = call_id}};
  }
  if (const auto* assign = std::get_if<hir::AssignExpr>(&inner.data)) {
    if (!assign->compound_op.has_value() &&
        std::holds_alternative<hir::ImmediateEffect>(assign->timing)) {
      // Peek through an implicit conversion wrapper that slang inserts when
      // the call's return type does not match the LHS type bit-for-bit.
      const hir::Expr* call_carrier = &hir_proc.exprs.Get(assign->rhs);
      if (const auto* conv =
              std::get_if<hir::ConversionExpr>(&call_carrier->data)) {
        call_carrier = &hir_proc.exprs.Get(conv->operand);
      }
      if (const auto* call = std::get_if<hir::CallExpr>(&call_carrier->data)) {
        if (const auto* sys_ref =
                std::get_if<hir::SystemSubroutineRef>(&call->callee)) {
          if (auto stmt = LowerSystemSubroutineCallStmtForm(
                  process, frame, label, *call, *sys_ref, assign->lhs)) {
            return *std::move(stmt);
          }
        }
      }
    }
  }

  auto expr_or = process.LowerExpr(hir_proc.exprs.Get(e.expr), frame);
  if (!expr_or) {
    return std::unexpected(std::move(expr_or.error()));
  }
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ExprStmt{.expr = block.exprs.Add(*std::move(expr_or))}};
}

}  // namespace lyra::lowering::hir_to_mir
