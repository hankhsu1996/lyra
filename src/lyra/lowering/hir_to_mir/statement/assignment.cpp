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
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/completion_payload.hpp"
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
    const mir::ExprId raw_slice_id = wrapper.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee = mir::Direct{.target = support::BuiltinFn::kSlice},
                    .arguments = {temp_ref, offset_id, count_id, form_id}},
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
      const mir::ExprId services_id = wrapper.exprs.Add(
          BuildServicesCallExpr(process.Owner(), wrapper_frame));
      const mir::Expr part_assign_expr = BuildObservableAssignExpr(
          process.Owner().Unit(), wrapper, services_id, part_lhs_id,
          rhs_for_part, std::nullopt, part_mir_type,
          process.Owner().Unit().builtins.void_type);
      per_part_expr_id = wrapper.exprs.Add(part_assign_expr);
    } else {
      mir::Expr closure_expr = BuildNbaSubmitClosureExpr(
          process.Owner(), wrapper_frame, part_lhs_id, rhs_for_part,
          part_mir_type);
      const mir::ExprId closure_id = wrapper.exprs.Add(std::move(closure_expr));
      const mir::ExprId services_id = wrapper.exprs.Add(
          BuildServicesCallExpr(process.Owner(), wrapper_frame));
      per_part_expr_id = wrapper.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kSubmitNba},
                      .arguments = {services_id, closure_id}},
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

// A user subroutine whose formals include a non-`input` direction needs the
// call desugared so the writebacks become explicit MIR statements; returns
// its HIR declaration, or nullptr for a value-only call (system / builtin
// callee, or an all-`input` user callee).
auto SubroutineWithWritebacks(
    const StructuralScopeLowerer& lowerer, const hir::CallExpr& call)
    -> const hir::SubroutineDecl* {
  const auto* ref = std::get_if<hir::StructuralSubroutineRef>(&call.callee);
  if (ref == nullptr) {
    return nullptr;
  }
  const hir::SubroutineDecl& decl =
      lowerer.LookupHirSubroutine(ref->hops, ref->subroutine);
  for (const auto& param : decl.params) {
    if (hir::RequiresWriteback(param.direction)) {
      return &decl;
    }
  }
  return nullptr;
}

// One value the completion payload carries back to a caller place: the actual
// lvalue to write and which payload component supplies it.
struct CompletionWriteback {
  mir::ExprId place{};
  std::size_t component_index = 0;
  mir::TypeId type{};
};

// LRM 13.5 output / inout argument passing. The callable's `output` / `inout`
// values ride its completion payload: the call passes only the `input` values,
// the `ref` aliases, and an `inout`'s incoming value; the payload's components
// are then written back to the actual places after completion. A task awaits
// the payload; a function's result is the payload directly. The whole thing
// desugars into a block so the writebacks are explicit statements.
//
// `assign_target` is the lvalue for a `lhs = f(...)` statement -- it receives
// the function-return component -- or nullopt for a bare call.
auto LowerSubroutineCallWithWritebacks(
    ProcessLowerer& process, WalkFrame frame, std::optional<std::string> label,
    const hir::CallExpr& call, const hir::StructuralSubroutineRef& callee_ref,
    const hir::SubroutineDecl& decl, std::optional<hir::ExprId> assign_target)
    -> diag::Result<mir::Stmt> {
  if (call.arguments.size() != decl.params.size()) {
    throw InternalError(
        "LowerSubroutineCallWithWritebacks: argument / formal count mismatch");
  }

  UnitLowerer& unit_lowerer = process.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::Block wrapper;
  const WalkFrame wrapper_frame = frame.WithBlock(&wrapper);

  // The completion layout the callee body and this call site share.
  const std::vector<mir::TypeId> components =
      CompletionComponentTypes(unit_lowerer, decl);
  const mir::TypeId payload_type = NormalizeCompletionPayload(unit, components);
  const bool is_task = decl.kind == hir::SubroutineKind::kTask;
  const mir::TypeId call_result_type =
      is_task ? unit.types.CoroutineOf(payload_type) : payload_type;

  std::vector<mir::ExprId> call_args;
  call_args.reserve(call.arguments.size() + 1);
  call_args.push_back(wrapper.exprs.Add(MakeSelfRefExpr(
      wrapper_frame, wrapper_frame.current_class->self_pointer_type)));

  std::vector<CompletionWriteback> writebacks;
  std::size_t next_component = 0;

  // A non-void function's return is payload component 0; `lhs = f(...)` writes
  // it back, a discarded result skips it.
  if (decl.result_var.has_value()) {
    if (assign_target.has_value()) {
      auto lhs_or = process.LowerLhsExpr(
          hir_proc.exprs.Get(*assign_target), wrapper_frame);
      if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
      writebacks.push_back(
          {.place = wrapper.exprs.Add(*std::move(lhs_or)),
           .component_index = 0,
           .type = components.front()});
    }
    next_component = 1;
  }

  for (std::size_t i = 0; i < call.arguments.size(); ++i) {
    const hir::ParamDirection dir = decl.params[i].direction;
    if (!call.arguments[i].has_value()) {
      throw InternalError(
          "LowerSubroutineCallWithWritebacks: user-call positional arg "
          "unexpectedly elided");
    }
    const hir::Expr& hir_arg = hir_proc.exprs.Get(*call.arguments[i]);
    const mir::TypeId formal_type = unit_lowerer.TranslateType(
        decl.body.procedural_vars.Get(decl.params[i].var).type);

    // An `output` passes no argument; an `inout` passes its incoming value.
    // Both bind the actual place for a post-completion writeback.
    if (dir == hir::ParamDirection::kOutput ||
        dir == hir::ParamDirection::kInOut) {
      auto place_or = process.LowerLhsExpr(hir_arg, wrapper_frame);
      if (!place_or) return std::unexpected(std::move(place_or.error()));
      const mir::ExprId place = wrapper.exprs.Add(*std::move(place_or));
      if (dir == hir::ParamDirection::kInOut) {
        auto value_or = process.LowerExpr(hir_arg, wrapper_frame);
        if (!value_or) return std::unexpected(std::move(value_or.error()));
        call_args.push_back(wrapper.exprs.Add(*std::move(value_or)));
      }
      writebacks.push_back(
          {.place = place,
           .component_index = next_component++,
           .type = formal_type});
      continue;
    }

    // A `ref` / const-ref formal aliases the actual's cell -- pass the cell
    // (or a `ScopedMutation` for selector / range chains on an observable
    // cell) so the body's `Ref<T>` binds the underlying storage.
    const bool is_ref = dir == hir::ParamDirection::kRef ||
                        dir == hir::ParamDirection::kConstRef;
    if (is_ref) {
      auto arg_or = process.LowerLhsExpr(hir_arg, wrapper_frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      mir::ExprId actual_id = wrapper.exprs.Add(*std::move(arg_or));
      const mir::ExprId root_id = FindLhsRootId(unit, wrapper, actual_id);
      const bool root_is_cell = mir::IsObservableCellType(
          unit.types.Get(wrapper.exprs.Get(root_id).type));
      if (root_is_cell && root_id != actual_id) {
        const mir::ExprId services_id = wrapper.exprs.Add(
            BuildServicesCallExpr(unit_lowerer, wrapper_frame));
        actual_id =
            RewriteLhsRootWithMutate(unit, wrapper, actual_id, services_id);
      }
      call_args.push_back(BuildReferenceArg(
          unit, wrapper, actual_id, wrapper.exprs.Get(actual_id).type));
      continue;
    }

    auto arg_or = process.LowerExpr(hir_arg, wrapper_frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    call_args.push_back(wrapper.exprs.Add(*std::move(arg_or)));
  }

  const mir::ExprId call_id = wrapper.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = process.EnclosingScopeLowerer()
                                .TranslateStructuralSubroutine(
                                    callee_ref.hops, callee_ref.subroutine),
                  .arguments = std::move(call_args)},
          .type = call_result_type});

  // The completion value is the awaited payload for a task, the call result for
  // a function; bind it to a local that every writeback projects from (and that
  // the backend reuses as the task's completion slot).
  const mir::ExprId completion_value =
      is_task ? wrapper.exprs.Add(
                    mir::Expr{
                        .data = mir::AwaitExpr{.awaitable = call_id},
                        .type = payload_type})
              : call_id;
  const mir::LocalId completion = wrapper_frame.bindings->DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_completion", .type = payload_type});
  wrapper.AppendStmt(
      mir::LocalDeclStmt{.target = completion, .init = completion_value});

  const mir::ExprId services_id =
      wrapper.exprs.Add(BuildServicesCallExpr(unit_lowerer, wrapper_frame));
  const mir::TypeId void_type = unit.builtins.void_type;
  for (const CompletionWriteback& wb : writebacks) {
    // A single-component payload is the bare value; otherwise project the
    // component out of the tuple.
    mir::ExprId value_id{};
    if (components.size() == 1) {
      value_id = wrapper.exprs.Add(mir::MakeLocalRefExpr(completion, wb.type));
    } else {
      const mir::ExprId tuple_read =
          wrapper.exprs.Add(mir::MakeLocalRefExpr(completion, payload_type));
      value_id = wrapper.exprs.Add(
          mir::Expr{
              .data =
                  mir::TupleGetExpr{
                      .tuple = tuple_read, .index = wb.component_index},
              .type = wb.type});
    }
    const mir::Expr assign_expr = BuildObservableAssignExpr(
        unit, wrapper, services_id, wb.place, value_id, std::nullopt, wb.type,
        void_type);
    wrapper.AppendStmt(mir::ExprStmt{.expr = wrapper.exprs.Add(assign_expr)});
  }

  const mir::BlockId scope_id =
      frame.current_block->child_scopes.Add(std::move(wrapper));
  return mir::Stmt{
      .label = std::move(label), .data = mir::BlockStmt{.scope = scope_id}};
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

  // A call statement. Resolving the callee here -- once -- decides both the
  // output-arg desugar shape (LRM 13.5) and whether the call is a suspension
  // point. A suspending callee ($finish, a task) lowers to an awaited
  // expression whose completion yields nothing here (a bare call with no
  // outputs), so the await result is void and the enclosing statement discards
  // it.
  if (const auto* call = std::get_if<hir::CallExpr>(&inner.data)) {
    if (const auto* decl =
            SubroutineWithWritebacks(process.EnclosingScopeLowerer(), *call)) {
      return LowerSubroutineCallWithWritebacks(
          process, frame, std::move(label), *call,
          std::get<hir::StructuralSubroutineRef>(call->callee), *decl,
          std::nullopt);
    }
    bool suspends = false;
    if (const auto* sys_ref =
            std::get_if<hir::SystemSubroutineRef>(&call->callee)) {
      const auto& desc = support::LookupSystemSubroutine(sys_ref->id);
      if (const auto* file_info = support::GetFileIOInfo(desc)) {
        if (support::IsFileOutputArgBuiltinFn(file_info->builtin_fn)) {
          return LowerFileIOSystemSubroutineCallStmt(
              process, frame, std::move(label), *call, *file_info, std::nullopt,
              process.Owner().TranslateType(inner.type));
        }
      }
      if (const auto* sformat_info = support::GetSFormatInfo(desc)) {
        return LowerSFormatSystemSubroutineCallStmt(
            process, frame, std::move(label), *call, *sformat_info);
      }
      suspends = desc.suspends;
    } else if (
        const auto* struct_ref =
            std::get_if<hir::StructuralSubroutineRef>(&call->callee)) {
      suspends =
          process.EnclosingScopeLowerer()
              .LookupHirSubroutine(struct_ref->hops, struct_ref->subroutine)
              .kind == hir::SubroutineKind::kTask;
    } else if (
        const auto* imported =
            std::get_if<hir::ImportedMethodRef>(&call->callee)) {
      suspends = support::ImportedRuntimeMethodSuspends(imported->method);
    }
    auto call_or = process.LowerExpr(inner, frame);
    if (!call_or) return std::unexpected(std::move(call_or.error()));
    const mir::ExprId call_id = block.exprs.Add(*std::move(call_or));
    if (suspends) {
      const mir::ExprId await_id = block.exprs.Add(
          mir::Expr{
              .data = mir::AwaitExpr{.awaitable = call_id},
              .type = process.Owner().Unit().builtins.void_type});
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
        if (const auto* decl = SubroutineWithWritebacks(
                process.EnclosingScopeLowerer(), *call)) {
          if (!conv_target_type.has_value()) {
            return LowerSubroutineCallWithWritebacks(
                process, frame, std::move(label), *call,
                std::get<hir::StructuralSubroutineRef>(call->callee), *decl,
                assign->lhs);
          }
        }
        if (const auto* sys_ref =
                std::get_if<hir::SystemSubroutineRef>(&call->callee)) {
          const auto& desc = support::LookupSystemSubroutine(sys_ref->id);
          const mir::TypeId result_type = process.Owner().TranslateType(
              conv_target_type.has_value() ? *conv_target_type
                                           : call_carrier->type);
          if (const auto* file_info = support::GetFileIOInfo(desc)) {
            if (support::IsFileOutputArgBuiltinFn(file_info->builtin_fn)) {
              return LowerFileIOSystemSubroutineCallStmt(
                  process, frame, std::move(label), *call, *file_info,
                  assign->lhs, result_type);
            }
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
