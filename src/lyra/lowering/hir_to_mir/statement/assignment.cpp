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
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/stmt.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/file_io.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/mem_file.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/sformat.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/imported_runtime_class.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 11.4.12 LHS destructuring desugar. Triggered when an ExprStmt wraps an
// AssignExpr whose LHS is a ConcatExpr -- the only context in which
// destructuring is grammatically legal. Emits a block that snapshots the RHS
// into a single packed temp then distributes per-part slices to each LHS
// operand. For NBA (`kind == kNonBlocking`), each per-part assignment goes
// through the NBA closure machinery.
auto LowerDestructuringAssign(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::AssignExpr& assign, const hir::ConcatExpr& lhs_concat)
    -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  std::vector<std::uint64_t> part_widths;
  part_widths.reserve(lhs_concat.operands.size());
  bool any_four_state = false;
  std::uint64_t total_width = 0;
  const auto& hir_types = process.Owner().Hir().types;
  const auto& mir_types = process.Owner().Unit().types;
  for (const hir::ExprId op_id : lhs_concat.operands) {
    const hir::Expr& op = hir_proc.exprs.Get(op_id);
    if (!hir_types.Get(op.type).IsBitVector()) {
      throw InternalError(
          "LowerDestructuringAssign: destructuring operand is not "
          "a packed integral type");
    }
    // Width and 4-state-ness are properties of the flat MIR vector; read them
    // off it directly rather than recompute from the structural HIR type.
    const auto& packed = mir_types.Get(process.Owner().TranslateType(op.type))
                             .AsIntegralPacked();
    const std::uint64_t w = packed.BitWidth();
    part_widths.push_back(w);
    total_width += w;
    any_four_state = any_four_state || packed.IsFourState();
  }
  if (total_width == 0) {
    throw InternalError(
        "LowerDestructuringAssign: destructuring total width must be positive");
  }

  const mir::TypeId temp_type = process.Owner().Unit().types.Intern(
      mir::PackedArrayType{
          .atom = any_four_state ? mir::BitAtom::kLogic : mir::BitAtom::kBit,
          .signedness = mir::Signedness::kUnsigned,
          .dims = {mir::PackedRange{
              .left = static_cast<std::int64_t>(total_width) - 1, .right = 0}},
          .form = mir::PackedArrayForm::kExplicit});

  const mir::ExprId temp_default_init = wrapper.exprs.Add(
      BuildDefaultValueExpr(process.Owner(), wrapper_frame, temp_type));
  const mir::LocalId snapshot_var = wrapper_frame.bindings->DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_destruct_rhs", .type = temp_type});
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
    const mir::ExprId part_lhs_id = wrapper.exprs.Add(*std::move(part_lhs_or));

    const mir::ExprId offset_id = wrapper.exprs.Add(
        mir::MakeIntLiteral(
            process.Owner().Unit().builtins.int_type,
            static_cast<std::int64_t>(offset)));
    const mir::ExprId count_id = wrapper.exprs.Add(
        mir::MakeIntLiteral(
            process.Owner().Unit().builtins.int_type,
            static_cast<std::int64_t>(w)));
    // `temp[offset +: w]`: a raw indexed-up part-select (`value::SliceForm`
    // `kIndexedUp` == 1); the snapshot value resolves the bit window itself.
    const mir::ExprId form_id = wrapper.exprs.Add(
        mir::MakeIntLiteral(process.Owner().Unit().builtins.int_type, 1));
    const mir::ExprId temp_ref =
        wrapper.exprs.Add(mir::MakeLocalRefExpr(snapshot_var, temp_type));
    const mir::TypeId slice_type = process.Owner().Unit().types.Intern(
        mir::PackedArrayType{
            .atom = any_four_state ? mir::BitAtom::kLogic : mir::BitAtom::kBit,
            .signedness = mir::Signedness::kUnsigned,
            .dims = {mir::PackedRange{
                .left = static_cast<std::int64_t>(w) - 1, .right = 0}},
            .form = mir::PackedArrayForm::kExplicit});
    const mir::ExprId shape_id = BuildPackedShapePrototype(
        wrapper,
        process.Owner().Unit().types.Get(slice_type).AsIntegralPacked(),
        slice_type);
    const mir::ExprId raw_slice_id = wrapper.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = support::BuiltinFn::kSlice},
                    .arguments =
                        {temp_ref, offset_id, count_id, form_id, shape_id}},
            .type = slice_type});
    const mir::ExprId slice_id = wrapper.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kToOwned},
                    .arguments = {raw_slice_id}},
            .type = slice_type});
    mir::ExprId rhs_for_part = slice_id;
    if (part_mir_type != slice_type) {
      rhs_for_part = wrapper.exprs.Add(BuildValueConversion(
          process.Owner().Unit(), wrapper, slice_id, part_mir_type));
    }

    mir::ExprId per_part_expr_id{};
    if (assign.kind == hir::AssignKind::kBlocking) {
      const mir::Expr part_assign_expr = BuildStoreExpr(
          process.Owner().Unit(), wrapper, part_lhs_id, rhs_for_part,
          std::nullopt, part_mir_type);
      per_part_expr_id = wrapper.exprs.Add(part_assign_expr);
    } else {
      mir::Expr closure_expr = BuildNbaSubmitClosureExpr(
          process.Owner(), wrapper_frame, part_lhs_id, rhs_for_part,
          part_mir_type);
      const mir::ExprId closure_id = wrapper.exprs.Add(std::move(closure_expr));
      const mir::ExprId runtime_id =
          wrapper.exprs.Add(BuildCurrentRuntimeCallExpr(process.Owner()));
      per_part_expr_id = wrapper.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kSubmitNba},
                      .arguments = {runtime_id, closure_id}},
              .type = process.Owner().Unit().builtins.void_type});
    }
    wrapper.AppendStmt(mir::ExprStmt{.expr = per_part_expr_id});
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
  if (process.Owner().Unit().types.IsCoroutine(call_type)) {
    return true;
  }
  return std::visit(
      Overloaded{
          [](const hir::SystemSubroutineRef& sys) {
            return support::LookupSystemSubroutine(sys.id).suspends;
          },
          [](const hir::ImportedMethodRef& m) {
            return support::ImportedRuntimeMethodSuspends(m.method);
          },
          // An intra-unit task enable and a cross-unit one (LRM 26.3) both
          // complete as coroutines, which the call's type already answered.
          [](const hir::StructuralSubroutineRef&) { return false; },
          [](const hir::ExternalUnitSubroutineRef&) { return false; },
          // A foreign task import (LRM 35.5.2) completes as a coroutine too,
          // which the call's type already answered.
          [](const hir::ForeignImportRef&) { return false; },
          // A class method is always a function today -- a task declared in a
          // class body is rejected at AST-to-HIR -- so no receiver-bearing call
          // yields. When a class task method becomes lowerable, its enable
          // completes as a coroutine and the type answers it too.
          [](const hir::MethodCallRef&) { return false; },
          [](const hir::StaticMethodCallRef&) { return false; },
          // A built-in method (LRM 6.16 / 7.9 / 7.12 / 15.5) computes a value
          // against a library type and never yields.
          [](const hir::BuiltinMethodRef&) { return false; },
      },
      call.callee);
}

// The statement-position lowering a system subroutine needs when its effect
// cannot be expressed as a bare value: a file write whose formatted output is
// bound to an output argument, and the `$sformat` / `$swrite` family whose
// result lands in an output variable. Nullopt for every family that lowers as
// an ordinary expression. Exhaustive over the semantic families, so a new one
// forces a decision about whether it has a statement form.
auto LowerSystemSubroutineCallStmtForm(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const hir::SystemSubroutineRef& ref,
    std::optional<hir::ExprId> assign_target, mir::TypeId result_type)
    -> std::optional<diag::Result<mir::Stmt>> {
  const auto& desc = support::LookupSystemSubroutine(ref.id);
  return std::visit(
      Overloaded{
          [&](const support::FileIOSystemSubroutineInfo& file_io)
              -> std::optional<diag::Result<mir::Stmt>> {
            if (!support::IsFileOutputArgBuiltinFn(file_io.builtin_fn)) {
              return std::nullopt;
            }
            return LowerFileIOSystemSubroutineCallStmt(
                process, frame, std::move(label), call, file_io, assign_target,
                result_type);
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
                process, frame, std::move(label), call, sformat);
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
          [&](const support::MemFileSystemSubroutineInfo& mem_file)
              -> std::optional<diag::Result<mir::Stmt>> {
            // A void task (LRM 21.4 / 21.5): its only form is a statement, so
            // it never feeds an assignment target. The lowering branches on
            // load vs dump.
            return LowerMemFileSystemSubroutineCallStmt(
                process, frame, std::move(label), call, mem_file);
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
          process, frame, std::move(label), *assign, *concat);
    }
  }

  // A call statement. A suspending callee ($finish, a task) is awaited here,
  // and what the await produces is what the awaitable carries: a callee that
  // completes as a coroutine hands over its completion payload, one that parks
  // the process through a runtime entry hands over nothing. Either way the
  // statement is the discard.
  if (const auto* call = std::get_if<hir::CallExpr>(&inner.data)) {
    if (const auto* sys_ref =
            std::get_if<hir::SystemSubroutineRef>(&call->callee)) {
      if (auto stmt = LowerSystemSubroutineCallStmtForm(
              process, frame, std::move(label), *call, *sys_ref, std::nullopt,
              process.Owner().TranslateType(inner.type))) {
        return *std::move(stmt);
      }
    }
    auto call_or = process.LowerExpr(inner, frame);
    if (!call_or) return std::unexpected(std::move(call_or.error()));
    const mir::ExprId call_id = block.exprs.Add(*std::move(call_or));
    const mir::TypeId call_type = block.exprs.Get(call_id).type;
    if (CallStatementSuspends(process, *call, call_type)) {
      const mir::TypeInterner& types = process.Owner().Unit().types;
      const mir::ExprId await_id = block.exprs.Add(
          mir::Expr{
              .data = mir::AwaitExpr{.awaitable = call_id},
              .type = types.IsCoroutine(call_type)
                          ? types.CoroutinePayload(call_type)
                          : process.Owner().Unit().builtins.void_type});
      return mir::Stmt{
          .label = std::move(label), .data = mir::ExprStmt{.expr = await_id}};
    }
    return mir::Stmt{
        .label = std::move(label), .data = mir::ExprStmt{.expr = call_id}};
  }
  if (const auto* assign = std::get_if<hir::AssignExpr>(&inner.data)) {
    if (!assign->compound_op.has_value() &&
        assign->kind == hir::AssignKind::kBlocking) {
      // Peek through an implicit conversion wrapper that slang inserts when
      // the call's return type does not match the LHS type bit-for-bit.
      hir::ExprId rhs_id = assign->rhs;
      const hir::Expr* rhs = &hir_proc.exprs.Get(rhs_id);
      const hir::Expr* call_carrier = rhs;
      std::optional<hir::TypeId> conv_target_type = std::nullopt;
      if (const auto* conv = std::get_if<hir::ConversionExpr>(&rhs->data)) {
        rhs_id = conv->operand;
        call_carrier = &hir_proc.exprs.Get(rhs_id);
        conv_target_type = rhs->type;
      }
      if (const auto* call = std::get_if<hir::CallExpr>(&call_carrier->data)) {
        if (const auto* sys_ref =
                std::get_if<hir::SystemSubroutineRef>(&call->callee)) {
          const mir::TypeId result_type = process.Owner().TranslateType(
              conv_target_type.has_value() ? *conv_target_type
                                           : call_carrier->type);
          if (auto stmt = LowerSystemSubroutineCallStmtForm(
                  process, frame, std::move(label), *call, *sys_ref,
                  assign->lhs, result_type)) {
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
