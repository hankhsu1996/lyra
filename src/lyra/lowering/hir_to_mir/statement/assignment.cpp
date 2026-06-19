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
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/copy_out_desugar.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/file_io.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/sformat.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/runtime_submit.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
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
  mir::ProceduralScope wrapper;
  const WalkFrame wrapper_frame = frame.WithProceduralScope(&wrapper).Deeper();

  std::vector<std::uint64_t> part_widths;
  part_widths.reserve(lhs_concat.operands.size());
  bool any_four_state = false;
  std::uint64_t total_width = 0;
  for (const hir::ExprId op_id : lhs_concat.operands) {
    const hir::Expr& op = hir_proc.exprs.at(op_id.value);
    const hir::Type& op_ty = process.Module().Hir().GetType(op.type);
    if (!op_ty.IsPackedArray()) {
      throw InternalError(
          "LowerDestructuringAssign: destructuring operand is not "
          "a packed integral type");
    }
    const auto& packed = op_ty.AsPackedArray();
    const std::uint64_t w = packed.BitWidth();
    part_widths.push_back(w);
    total_width += w;
    any_four_state = any_four_state || packed.IsFourState();
  }
  if (total_width == 0) {
    throw InternalError(
        "LowerDestructuringAssign: destructuring total width must be positive");
  }

  const mir::TypeId temp_type = process.Module().Unit().AddType(
      mir::TypeData{mir::PackedArrayType{
          .atom = any_four_state ? mir::BitAtom::kLogic : mir::BitAtom::kBit,
          .signedness = mir::Signedness::kUnsigned,
          .dims = {mir::PackedRange{
              .left = static_cast<std::int64_t>(total_width) - 1, .right = 0}},
          .form = mir::PackedArrayForm::kExplicit}});

  const mir::ExprId temp_default_init = wrapper.AddExpr(
      BuildDefaultValueExpr(process.Module(), wrapper_frame, temp_type));
  const mir::ProceduralVarRef snapshot_ref = wrapper.AppendLocal(
      mir::ProceduralVarDecl{.name = "_lyra_destruct_rhs", .type = temp_type},
      temp_default_init);

  // RHS is evaluated once; the snapshot temp is what gets distributed,
  // which is what makes `{a, b} = {b, a}` swap correctly.
  auto rhs_or =
      process.LowerExpr(hir_proc.exprs.at(assign.rhs.value), wrapper_frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  mir::ExprId rhs_id = wrapper.AddExpr(*std::move(rhs_or));
  if (wrapper.GetExpr(rhs_id).type != temp_type) {
    rhs_id = wrapper.AddExpr(
        mir::Expr{
            .data =
                mir::ConversionExpr{
                    .operand = rhs_id, .kind = mir::ConversionKind::kImplicit},
            .type = temp_type});
  }

  const mir::ExprId temp_assign_target =
      wrapper.AddExpr(mir::Expr{.data = snapshot_ref, .type = temp_type});
  const mir::ExprId temp_assign_id = wrapper.AddExpr(
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
        hir_proc.exprs.at(lhs_concat.operands[i].value), wrapper_frame);
    if (!part_lhs_or) {
      return std::unexpected(std::move(part_lhs_or.error()));
    }
    const mir::TypeId part_mir_type = process.Module().TranslateType(
        hir_proc.exprs.at(lhs_concat.operands[i].value).type);
    const mir::ExprId part_lhs_id = wrapper.AddExpr(*std::move(part_lhs_or));

    const mir::ExprId offset_id = wrapper.AddExpr(
        mir::MakeInt32Literal(
            process.Module().Unit().builtins.int32,
            static_cast<std::int64_t>(offset)));
    const mir::ExprId count_id = wrapper.AddExpr(
        mir::MakeInt32Literal(
            process.Module().Unit().builtins.int32,
            static_cast<std::int64_t>(w)));
    const mir::ExprId temp_ref =
        wrapper.AddExpr(mir::Expr{.data = snapshot_ref, .type = temp_type});
    const mir::TypeId slice_type = process.Module().Unit().AddType(
        mir::TypeData{mir::PackedArrayType{
            .atom = any_four_state ? mir::BitAtom::kLogic : mir::BitAtom::kBit,
            .signedness = mir::Signedness::kUnsigned,
            .dims = {mir::PackedRange{
                .left = static_cast<std::int64_t>(w) - 1, .right = 0}},
            .form = mir::PackedArrayForm::kExplicit}});
    const mir::ExprId raw_slice_id = wrapper.AddExpr(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::BuiltinMethodCallee{
                            .method =
                                mir::ArrayMethodInfo{
                                    .kind = mir::ArrayMethodKind::kSlice}},
                    .arguments = {temp_ref, offset_id, count_id}},
            .type = slice_type});
    const mir::ExprId slice_id = wrapper.AddExpr(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::BuiltinMethodCallee{
                            .method =
                                mir::ArrayMethodInfo{
                                    .kind = mir::ArrayMethodKind::kToOwned}},
                    .arguments = {raw_slice_id}},
            .type = slice_type});
    mir::ExprId rhs_for_part = slice_id;
    if (part_mir_type != slice_type) {
      rhs_for_part = wrapper.AddExpr(
          mir::Expr{
              .data =
                  mir::ConversionExpr{
                      .operand = slice_id,
                      .kind = mir::ConversionKind::kImplicit},
              .type = part_mir_type});
    }

    mir::ExprId per_part_expr_id{};
    if (assign.kind == hir::AssignKind::kBlocking) {
      const mir::ExprId services_id =
          wrapper.AddExpr(BuildServicesCallExpr(process, wrapper_frame));
      const mir::Expr part_assign_expr = BuildObservableAssignExpr(
          process.Module().Unit(), wrapper, services_id, part_lhs_id,
          rhs_for_part, std::nullopt, part_mir_type,
          process.Module().Unit().builtins.void_type);
      per_part_expr_id = wrapper.AddExpr(part_assign_expr);
    } else {
      mir::Expr closure_expr = BuildNbaSubmitClosureExpr(
          process.Module(), wrapper_frame, part_lhs_id, rhs_for_part,
          part_mir_type);
      const mir::ExprId closure_id = wrapper.AddExpr(std::move(closure_expr));
      per_part_expr_id = wrapper.AddExpr(
          mir::Expr{
              .data =
                  mir::RuntimeCallExpr{
                      .call = mir::RuntimeSubmitNbaCall{.closure = closure_id}},
              .type = process.Module().Unit().builtins.void_type});
    }
    wrapper.AppendStmt(mir::ExprStmt{.expr = per_part_expr_id});
  }

  const mir::ProceduralScopeId wrapper_scope_id =
      frame.current_procedural_scope->AddChildScope(std::move(wrapper));

  return mir::Stmt{
      .label = std::move(label),
      .data = mir::BlockStmt{.scope = wrapper_scope_id}};
}

// A user subroutine whose formals include a non-`input` direction needs the
// call desugared so the writebacks become explicit MIR statements; returns
// its HIR declaration, or nullptr for a value-only call (system / builtin
// callee, or an all-`input` user callee).
auto SubroutineWithWritebacks(
    const StructuralScopeLowerer& scope, const hir::CallExpr& call)
    -> const hir::StructuralSubroutineDecl* {
  const auto* ref = std::get_if<hir::StructuralSubroutineRef>(&call.callee);
  if (ref == nullptr) {
    return nullptr;
  }
  const hir::StructuralSubroutineDecl& decl =
      scope.LookupHirSubroutine(ref->hops, ref->subroutine);
  for (const auto& param : decl.params) {
    if (hir::RequiresWriteback(param.direction)) {
      return &decl;
    }
  }
  return nullptr;
}

// LRM 13.5 output / inout argument passing. A subroutine call whose formals
// take values out is desugared into a block: a fresh local per writeback
// formal (default-initialized for `output`, copy-in initialized for
// `inout`), the call with those locals bound to the formals, then a
// copy-out assignment of each local back to its actual lvalue.
//
// `assign_target` is the lvalue for a `lhs = f(...)` statement, or nullopt
// for a bare call statement (void function or discarded result).
auto LowerSubroutineCallWithWritebacks(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const hir::StructuralSubroutineRef& callee_ref,
    const hir::StructuralSubroutineDecl& decl,
    std::optional<hir::ExprId> assign_target, mir::TypeId result_type)
    -> diag::Result<mir::Stmt> {
  if (call.arguments.size() != decl.params.size()) {
    throw InternalError(
        "LowerSubroutineCallWithWritebacks: argument / formal count mismatch");
  }

  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::ProceduralScope wrapper;
  const WalkFrame wrapper_frame = frame.WithProceduralScope(&wrapper).Deeper();

  // The callee body takes its instance handle as `arguments[0]` (mir.md
  // invariant 11); the SV actuals (with output / inout temps) follow.
  std::vector<mir::ExprId> call_args;
  call_args.reserve(call.arguments.size() + 1);
  call_args.push_back(wrapper.AddExpr(BuildSelfRefExpr(
      wrapper_frame, process.Module().Unit().builtins.self_pointer)));
  std::vector<OutputArgSlot> writebacks;

  for (std::size_t i = 0; i < call.arguments.size(); ++i) {
    const hir::ParamDirection dir = decl.params[i].direction;
    if (!call.arguments[i].has_value()) {
      throw InternalError(
          "LowerSubroutineCallWithWritebacks: user-call positional arg "
          "unexpectedly elided");
    }
    const hir::Expr& hir_arg = hir_proc.exprs.at(call.arguments[i]->value);

    if (!hir::RequiresWriteback(dir)) {
      // A ref / const-ref formal aliases the actual's cell -- pass the cell
      // (or a `ScopedMutation` for selector / range chains on an observable
      // cell) so the body's `Ref<T>` binds the underlying storage.
      const bool is_ref = dir == hir::ParamDirection::kRef ||
                          dir == hir::ParamDirection::kConstRef;
      if (is_ref) {
        auto arg_or = process.LowerLhsExpr(hir_arg, wrapper_frame);
        if (!arg_or) return std::unexpected(std::move(arg_or.error()));
        mir::ExprId actual_id = wrapper.AddExpr(*std::move(arg_or));
        const mir::ExprId root_id = FindLhsRootId(wrapper, actual_id);
        const bool root_is_cell = mir::IsObservableCellType(
            process.Module().Unit().GetType(wrapper.GetExpr(root_id).type));
        if (root_is_cell && root_id != actual_id) {
          const mir::ExprId services_id =
              wrapper.AddExpr(BuildServicesCallExpr(process, wrapper_frame));
          actual_id = RewriteLhsRootWithMutate(
              process.Module().Unit(), wrapper, actual_id, services_id);
        }
        call_args.push_back(actual_id);
        continue;
      }
      auto arg_or = process.LowerExpr(hir_arg, wrapper_frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      call_args.push_back(wrapper.AddExpr(*std::move(arg_or)));
      continue;
    }

    const mir::TypeId formal_type = process.Module().TranslateType(
        decl.body.GetProceduralVar(decl.params[i].var).type);
    // The actual is the writeback target: lower it as a cell-typed lvalue
    // so an observable destination's writeback fires `Var<T>::Set`.
    auto target_or = process.LowerLhsExpr(hir_arg, wrapper_frame);
    if (!target_or) return std::unexpected(std::move(target_or.error()));
    const mir::ExprId actual_id = wrapper.AddExpr(*std::move(target_or));

    mir::ExprId init{};
    if (dir == hir::ParamDirection::kInOut) {
      auto value_or = process.LowerExpr(hir_arg, wrapper_frame);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      init = wrapper.AddExpr(*std::move(value_or));
    } else {
      init = wrapper.AddExpr(
          BuildDefaultValueExpr(process.Module(), wrapper_frame, formal_type));
    }
    const mir::ProceduralVarRef temp = wrapper.AppendLocal(
        mir::ProceduralVarDecl{
            .name = "_lyra_arg" + std::to_string(i), .type = formal_type},
        init);
    call_args.push_back(
        wrapper.AddExpr(mir::Expr{.data = temp, .type = formal_type}));
    writebacks.push_back(
        {.actual = actual_id, .temp = temp, .type = formal_type});
  }

  mir::Expr call_expr{
      .data =
          mir::CallExpr{
              .callee = process.Scope().TranslateStructuralSubroutine(
                  callee_ref.hops, callee_ref.subroutine),
              .arguments = std::move(call_args)},
      .type = result_type};

  std::optional<mir::ExprId> assign_target_id = std::nullopt;
  if (assign_target.has_value()) {
    auto lhs_or = process.LowerLhsExpr(
        hir_proc.exprs.at(assign_target->value), wrapper_frame);
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    assign_target_id = wrapper.AddExpr(*std::move(lhs_or));
  }

  const mir::ExprId services_id =
      wrapper.AddExpr(BuildServicesCallExpr(process, wrapper_frame));
  return BuildCopyOutBlock(
      process.Module().Unit(), services_id, frame, std::move(wrapper),
      std::move(label), result_type, std::move(call_expr),
      decl.kind == hir::SubroutineKind::kTask, assign_target_id, writebacks);
}

}  // namespace

auto LowerExprStmt(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::ExprStmt& e) -> diag::Result<mir::Stmt> {
  const hir::ProceduralBody& hir_proc = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;

  // LRM 11.4.12 LHS destructuring: detect AssignExpr-with-ConcatExpr-LHS
  // and dispatch to the snapshot+distribute desugar.
  const hir::Expr& inner = hir_proc.exprs.at(e.expr.value);
  if (const auto* assign = std::get_if<hir::AssignExpr>(&inner.data)) {
    const hir::Expr& lhs = hir_proc.exprs.at(assign->lhs.value);
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

  // A call statement. Resolving the callee here -- once -- decides both the
  // output-arg desugar shape (LRM 13.5) and whether the call is a suspension
  // point. A suspending callee ($finish, a task) lowers to `AwaitStmt`; LRM
  // 13.4 keeps every suspending call void and statement-positioned, so a bare
  // call is the only place suspension arises.
  if (const auto* call = std::get_if<hir::CallExpr>(&inner.data)) {
    if (const auto* decl = SubroutineWithWritebacks(process.Scope(), *call)) {
      return LowerSubroutineCallWithWritebacks(
          process, frame, std::move(label), *call,
          std::get<hir::StructuralSubroutineRef>(call->callee), *decl,
          std::nullopt, process.Module().TranslateType(inner.type));
    }
    bool suspends = false;
    if (const auto* sys_ref =
            std::get_if<hir::SystemSubroutineRef>(&call->callee)) {
      const auto& desc = support::LookupSystemSubroutine(sys_ref->id);
      // System-subroutine tasks with an output arg ($fgets / $fread / $ferror)
      // ride the copy-out shape but build a RuntimeFileXxxCall MIR node instead
      // of a user-function CallExpr.
      if (const auto* file_info = support::GetFileIOInfo(desc)) {
        if (support::FileIOHasOutputArg(file_info->kind)) {
          return LowerFileIOSystemSubroutineCallStmt(
              process, frame, std::move(label), *call, desc.id, *file_info,
              std::nullopt, process.Module().TranslateType(inner.type));
        }
      }
      if (const auto* sformat_info = support::GetSFormatInfo(desc)) {
        return LowerSFormatSystemSubroutineCallStmt(
            process, frame, std::move(label), inner.span, *call, desc.id,
            desc.name, *sformat_info);
      }
      suspends = desc.suspends;
    } else if (
        const auto* struct_ref =
            std::get_if<hir::StructuralSubroutineRef>(&call->callee)) {
      suspends =
          process.Scope()
              .LookupHirSubroutine(struct_ref->hops, struct_ref->subroutine)
              .kind == hir::SubroutineKind::kTask;
    }
    auto call_or = process.LowerExpr(inner, frame);
    if (!call_or) return std::unexpected(std::move(call_or.error()));
    const mir::ExprId call_id = proc_scope.AddExpr(*std::move(call_or));
    if (suspends) {
      return mir::Stmt{
          .label = std::move(label),
          .data = mir::AwaitStmt{.awaitable = call_id}};
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
      const hir::Expr* rhs = &hir_proc.exprs.at(rhs_id.value);
      const hir::Expr* call_carrier = rhs;
      std::optional<hir::TypeId> conv_target_type = std::nullopt;
      if (const auto* conv = std::get_if<hir::ConversionExpr>(&rhs->data)) {
        rhs_id = conv->operand;
        call_carrier = &hir_proc.exprs.at(rhs_id.value);
        conv_target_type = rhs->type;
      }
      if (const auto* call = std::get_if<hir::CallExpr>(&call_carrier->data)) {
        if (const auto* decl =
                SubroutineWithWritebacks(process.Scope(), *call)) {
          if (!conv_target_type.has_value()) {
            return LowerSubroutineCallWithWritebacks(
                process, frame, std::move(label), *call,
                std::get<hir::StructuralSubroutineRef>(call->callee), *decl,
                assign->lhs,
                process.Module().TranslateType(call_carrier->type));
          }
        }
        if (const auto* sys_ref =
                std::get_if<hir::SystemSubroutineRef>(&call->callee)) {
          const auto& desc = support::LookupSystemSubroutine(sys_ref->id);
          const mir::TypeId result_type = process.Module().TranslateType(
              conv_target_type.has_value() ? *conv_target_type
                                           : call_carrier->type);
          if (const auto* file_info = support::GetFileIOInfo(desc)) {
            if (support::FileIOHasOutputArg(file_info->kind)) {
              return LowerFileIOSystemSubroutineCallStmt(
                  process, frame, std::move(label), *call, desc.id, *file_info,
                  assign->lhs, result_type);
            }
          }
        }
      }
    }
  }

  auto expr_or = process.LowerExpr(hir_proc.exprs.at(e.expr.value), frame);
  if (!expr_or) {
    return std::unexpected(std::move(expr_or.error()));
  }
  return mir::Stmt{
      .label = std::move(label),
      .data = mir::ExprStmt{.expr = proc_scope.AddExpr(*std::move(expr_or))}};
}

}  // namespace lyra::lowering::hir_to_mir
