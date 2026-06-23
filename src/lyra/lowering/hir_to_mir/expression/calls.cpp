#include "lyra/lowering/hir_to_mir/expression/calls.hpp"

#include <expected>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/control.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/file_io.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/print.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/scan.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/sformat.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/time.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/timescale.hpp"
#include "lyra/lowering/hir_to_mir/lhs_observable.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/block_hops.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// If the lowered LHS chain `lhs_id` is rooted in an observable cell, wrap
// the root with `Deref(Mutate(cell, services))` so the consumer operates on
// a `ScopedMutation` snapshot whose destructor commits via `Var::Set`.
// Returns `lhs_id` unchanged when no cell is at the root. Used by both the
// mutating-method receiver path and the ref-formal actual path; the latter
// gates its call on `root_id != lhs_id` because a bare observable cell
// should bind `Ref<T>` directly to the underlying `Var<T>&`, not via a
// snapshot.
auto MaybeWrapObservableLhsWithMutate(
    ProcessLowerer& process, WalkFrame frame, mir::Block& block,
    mir::ExprId lhs_id) -> mir::ExprId {
  const mir::ExprId root_id = FindLhsRootId(block, lhs_id);
  const bool root_is_cell = mir::IsObservableCellType(
      process.Module().Unit().GetType(block.exprs.Get(root_id).type));
  if (!root_is_cell) return lhs_id;
  const mir::ExprId services_id =
      block.exprs.Add(BuildServicesCallExpr(process, frame));
  return RewriteLhsRootWithMutate(
      process.Module().Unit(), block, lhs_id, services_id);
}

// LRM 7.9.4 -- 7.9.7 associative traversal (`m.first(idx)` / `last` / `next` /
// `prev`). The method writes the visited key into `idx` and returns SV int
// 1 / 0. The call sits in expression position (the canonical
// `do ... while (m.next(idx))` idiom) yet must run a statement sequence -- read
// the probe, query, then write the index back so its LRM 4.3 update event
// fires -- so it lowers to an immediately-invoked closure. The query is a pure
// value member that mutates a plain body-local probe; the event-firing lives in
// the ordinary observable write-back assignment, not in the query.
auto LowerAssociativeTraversal(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& c,
    support::BuiltinFn fn, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  if (!c.arguments[1].has_value()) {
    throw InternalError(
        "LowerAssociativeTraversal: index argument unexpectedly elided");
  }
  const auto& module = process.Module();
  const auto& hir_proc = process.HirBody();
  const auto recv_hir = *c.arguments[0];
  const auto idx_hir = *c.arguments[1];
  const mir::TypeId key_type =
      module.TranslateType(hir_proc.exprs.Get(idx_hir).type);
  const mir::TypeId void_t = module.Unit().builtins.void_type;

  ClosureBuilder closure(process.Module().Unit(), frame);
  mir::Block& body = closure.Body();
  const WalkFrame& closure_frame = closure.Frame();

  // probe = idx -- snapshot the current index value into a plain local.
  auto idx_read_or =
      process.LowerExpr(hir_proc.exprs.Get(idx_hir), closure_frame);
  if (!idx_read_or) return std::unexpected(std::move(idx_read_or.error()));
  const mir::LocalRef probe_ref = body.AppendLocal(
      mir::LocalDecl{.name = "_lyra_trav_probe", .type = key_type},
      body.exprs.Add(*std::move(idx_read_or)));

  // found = (map).<kind>(probe) -- pure query: mutates probe, yields 1/0.
  auto map_read_or =
      process.LowerExpr(hir_proc.exprs.Get(recv_hir), closure_frame);
  if (!map_read_or) return std::unexpected(std::move(map_read_or.error()));
  const mir::ExprId map_read_id = body.exprs.Add(*std::move(map_read_or));
  const mir::ExprId probe_read_id =
      body.exprs.Add(mir::Expr{.data = probe_ref, .type = key_type});
  const mir::ExprId query_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::BuiltinFnCallee{.id = fn},
                  .arguments = {map_read_id, probe_read_id}},
          .type = result_type});
  const mir::LocalRef found_ref = body.AppendLocal(
      mir::LocalDecl{.name = "_lyra_trav_found", .type = result_type},
      query_id);

  // idx = probe -- observable write-back fires the LRM 4.3 update event.
  auto idx_lhs_or =
      process.LowerLhsExpr(hir_proc.exprs.Get(idx_hir), closure_frame);
  if (!idx_lhs_or) return std::unexpected(std::move(idx_lhs_or.error()));
  const mir::ExprId idx_lhs_id = body.exprs.Add(*std::move(idx_lhs_or));
  const mir::ExprId probe_writeback_id =
      body.exprs.Add(mir::Expr{.data = probe_ref, .type = key_type});
  const mir::ExprId services_id =
      body.exprs.Add(BuildServicesCallExpr(process, closure_frame));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      module.Unit(), body, services_id, idx_lhs_id, probe_writeback_id,
      std::nullopt, key_type, void_t);
  body.AppendStmt(mir::ExprStmt{.expr = body.exprs.Add(assign_expr)});

  const mir::ExprId found_id =
      body.exprs.Add(mir::Expr{.data = found_ref, .type = result_type});
  return BuildClosureCallExpr(*frame.current_block, closure.Build(found_id));
}

// Translates a HIR builtin-method ref to its MIR `Callee`. The identifier
// is the flat `support::BuiltinFn`; the only decision here is whether the
// id names a type-namespace-qualified static call (e.g. `MyEnum::first()`)
// or an instance call.
auto MakeBuiltinMirCallee(
    const ModuleLowerer& module, const hir::BuiltinMethodRef& b,
    hir::TypeId hir_dispatch_type) -> mir::Callee {
  if (support::IsStaticBuiltinFn(b.method)) {
    return mir::BuiltinStaticCallee{
        .id = b.method, .type_qual = module.TranslateType(hir_dispatch_type)};
  }
  return mir::BuiltinFnCallee{.id = b.method};
}

// The LRM 7.12 family shares one closure shape across every unpacked-array
// receiver; only the element type differs, and each such HIR type exposes it
// as `element_type`.
auto ArrayMethodReceiverElementType(const hir::Type& ty)
    -> std::optional<hir::TypeId> {
  if (const auto* ua = std::get_if<hir::UnpackedArrayType>(&ty.data)) {
    return ua->element_type;
  }
  if (const auto* da = std::get_if<hir::DynamicArrayType>(&ty.data)) {
    return da->element_type;
  }
  if (const auto* q = std::get_if<hir::QueueType>(&ty.data)) {
    return q->element_type;
  }
  if (const auto* aa = std::get_if<hir::AssociativeArrayType>(&ty.data)) {
    return aa->element_type;
  }
  return std::nullopt;
}

// The canonical-default prototype type for a value-producing LRM 7.12 method
// (decision: array-manipulation-entry-stream): a locator / map result is a
// container, so its element type is the prototype; a reduction result is the
// scalar itself. The producer supplies the prototype because the result shape
// (an index locator's key, a map's chosen element, an empty reduction's zero)
// is not always recoverable from the receiver.
auto ArrayMethodResultProtoType(
    const ModuleLowerer& module, mir::TypeId result_type) -> mir::TypeId {
  return std::visit(
      Overloaded{
          [](const mir::UnpackedArrayType& t) { return t.element_type; },
          [](const mir::DynamicArrayType& t) { return t.element_type; },
          [](const mir::QueueType& t) { return t.element_type; },
          [](const mir::AssociativeArrayType& t) { return t.element_type; },
          [result_type](const auto&) { return result_type; },
      },
      module.Unit().GetType(result_type).data);
}

// LRM 7.12.1 / 7.12.2 / 7.12.3 with-clause closure synthesis. The iterator and
// index are the closure's two parameters (LRM 7.12.4): the iterator HIR id
// remaps to the `item` parameter and the index parameter resolves the
// `IteratorIndexRef` callee. The body is a normal expression lowered through
// `process.LowerExpr`. When the source has no `with` clause LRM 7.12.1 defines
// the default as `with (item)`; this synthesises the identity closure (body
// returns the iterator binding) so MIR always carries the closure argument and
// downstream consumers see one uniform shape.
auto BuildArrayMethodClosure(
    ProcessLowerer& process, WalkFrame frame, hir::TypeId hir_receiver_type,
    const hir::WithClause* with_clause) -> diag::Result<mir::Expr> {
  const auto& module = process.Module();
  const auto& hir_process = process.HirBody();
  const auto& hir_recv_ty = module.Hir().GetType(hir_receiver_type);
  const auto element_type = ArrayMethodReceiverElementType(hir_recv_ty);
  if (!element_type.has_value()) {
    throw InternalError(
        "BuildArrayMethodClosure: receiver is not an unpacked-array type");
  }
  const mir::TypeId item_type = module.TranslateType(*element_type);
  // LRM 7.12.4 `item.index`: the ordinal position for a sequence container, the
  // key for an associative receiver.
  mir::TypeId index_type = module.Unit().builtins.int32;
  if (const auto* assoc =
          std::get_if<hir::AssociativeArrayType>(&hir_recv_ty.data);
      assoc != nullptr && assoc->key_type.has_value()) {
    index_type = module.TranslateType(*assoc->key_type);
  }
  const std::string iterator_name =
      with_clause != nullptr
          ? hir_process.procedural_vars.Get(with_clause->iterator).name
          : std::string{"item"};

  ClosureBuilder closure(process.Module().Unit(), frame);
  const mir::LocalId item_binding = closure.AddParam(iterator_name, item_type);
  const mir::LocalId index_binding = closure.AddParam("index", index_type);
  mir::Block& body = closure.Body();

  mir::ExprId body_return_value{};
  if (with_clause != nullptr) {
    process.MapProceduralVar(
        with_clause->iterator,
        AutomaticVarBinding{
            .declaration_procedural_depth = closure.Frame().block_depth,
            .var = item_binding});
    // `item.index` (LRM 7.12.4) resolves to the index parameter -- a
    // with-clause-specific role, so the body lowers through a frame carrying
    // that binding rather than the builder knowing about it.
    auto body_expr_or = process.LowerExpr(
        hir_process.exprs.Get(with_clause->expr),
        closure.Frame().WithIndexBinding(index_binding));
    if (!body_expr_or) return std::unexpected(std::move(body_expr_or.error()));
    body_return_value = body.exprs.Add(*std::move(body_expr_or));
  } else {
    body_return_value = body.exprs.Add(
        mir::Expr{
            .data =
                mir::LocalRef{
                    .hops = mir::BlockHops{.value = 0}, .var = item_binding},
            .type = item_type});
  }
  return closure.Build(body_return_value);
}

// Fans out a system-subroutine call to the per-family handler under
// `expression/system/*.cpp`. The visitor is exhaustive over
// `support::SystemSubroutineSemantic`; new arms force a compile-time
// update here.
auto LowerSystemSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& call,
    const hir::SystemSubroutineRef& ref, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  const auto& desc = support::LookupSystemSubroutine(ref.id);
  return std::visit(
      Overloaded{
          [&](const support::PrintSystemSubroutineInfo& print)
              -> diag::Result<mir::Expr> {
            return LowerPrintSystemSubroutineCall(
                process, frame, call, print, span);
          },
          [&](const support::TerminationSystemSubroutineInfo& term)
              -> diag::Result<mir::Expr> {
            return LowerFinishSystemSubroutineCall(
                process, frame, call, desc.id, desc.name, term, span);
          },
          [&](const support::DiagnosticSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            return LowerDiagnosticSystemSubroutineCall(
                process, frame, call, desc.id, span);
          },
          [&](const support::FileIOSystemSubroutineInfo& file_io)
              -> diag::Result<mir::Expr> {
            return LowerFileIOSystemSubroutineCall(
                process, frame, call, desc.id, desc.name, file_io, span);
          },
          [&](const support::ScanSystemSubroutineInfo& scan_info)
              -> diag::Result<mir::Expr> {
            return LowerScanSystemSubroutineCall(
                process, frame, call, scan_info, span);
          },
          [&](const support::SFormatSystemSubroutineInfo& sformat)
              -> diag::Result<mir::Expr> {
            return LowerSFormatSystemSubroutineCall(
                process, frame, call, sformat, span);
          },
          [&](const support::TimeSystemSubroutineInfo& time_info)
              -> diag::Result<mir::Expr> {
            return LowerTimeSystemSubroutineCall(
                process, frame, desc.id, time_info);
          },
          [&](const support::TimeFormatSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            return LowerTimeFormatSystemSubroutineCall(
                process, frame, call, desc.id, span);
          },
          [&](const support::PrintTimescaleSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            return LowerPrintTimescaleSystemSubroutineCall(
                process, frame, desc.id);
          },
      },
      desc.semantic);
}

// LRM 13.5 user-subroutine call (function or task). A call with output /
// inout actuals is desugared to copy-in-copy-out at statement position;
// reaching this lowering means the call is a nested expression operand,
// where the copy-out statement has nowhere to be sequenced. ref / const
// ref alias the actual and copy nothing back (LRM 13.5.2), so they fall
// through to the value-only lowering. The callee body receives its
// instance handle as `arguments[0]` (mir.md invariant 11); SV actuals
// follow.
auto LowerStructuralSubroutineCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& c,
    const hir::StructuralSubroutineRef& usr, diag::SourceSpan span,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& lowerer = process.Owner();
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;
  const hir::StructuralSubroutineDecl& decl =
      lowerer.LookupHirSubroutine(usr.hops, usr.subroutine);
  for (const auto& param : decl.params) {
    if (hir::RequiresWriteback(param.direction)) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedSubroutineArgument,
          "a call with output / inout arguments is only supported in "
          "statement position, not as a nested expression");
    }
  }
  std::vector<mir::ExprId> args;
  args.reserve(c.arguments.size() + 1);
  args.push_back(block.exprs.Add(
      MakeSelfRefExpr(frame, frame.current_class->self_pointer_type)));
  for (std::size_t i = 0; i < c.arguments.size(); ++i) {
    if (!c.arguments[i].has_value()) {
      throw InternalError("user-function call argument unexpectedly elided");
    }
    // A ref / const-ref formal aliases the actual's cell. The body's
    // `Ref<T>` either binds the wrapped cell directly (bare observable
    // actual -- `Ref<T>` to `Var<T>&`) or holds a `ScopedMutation`
    // snapshot (selector / range on an observable actual). The bare-cell
    // case skips the mutate wrap, since the helper would otherwise
    // detach the body from the underlying cell.
    const bool is_ref_formal =
        i < decl.params.size() &&
        (decl.params[i].direction == hir::ParamDirection::kRef ||
         decl.params[i].direction == hir::ParamDirection::kConstRef);
    mir::ExprId actual_id{};
    if (is_ref_formal) {
      auto arg_or =
          process.LowerLhsExpr(hir_process.exprs.Get(*c.arguments[i]), frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      actual_id = block.exprs.Add(*std::move(arg_or));
      const mir::ExprId root_id = FindLhsRootId(block, actual_id);
      if (root_id != actual_id) {
        actual_id =
            MaybeWrapObservableLhsWithMutate(process, frame, block, actual_id);
      }
      args.push_back(BuildReferenceArg(
          process.Module().Unit(), block, actual_id,
          block.exprs.Get(actual_id).type));
    } else {
      auto arg_or =
          process.LowerExpr(hir_process.exprs.Get(*c.arguments[i]), frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      args.push_back(block.exprs.Add(*std::move(arg_or)));
    }
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = lowerer.TranslateStructuralSubroutine(
                  usr.hops, usr.subroutine),
              .arguments = std::move(args)},
      .type = result_type};
}

// Built-in method dispatch (LRM 6.16 / 6.19.5 / 7.9 / 7.10 / 7.12 / 15.5).
// AST -> HIR puts a type-bearing expression at `c.arguments[0]`: for an
// instance call it is the receiver itself, for a type-namespace static
// call (`MyEnum::first()`) it is a discardable bearer whose type supplies
// the static callee's `type_qual`. Either way, the for-loop below skips
// index 0 and starts the real user-argument scan at index 1.
auto LowerBuiltinMethodCall(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& c,
    const hir::BuiltinMethodRef& b, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  if (c.arguments.empty()) {
    throw InternalError(
        "BuiltinMethodRef call has no receiver / type-bearer argument");
  }
  if (!c.arguments.front().has_value()) {
    throw InternalError(
        "BuiltinMethodRef receiver / type-bearer unexpectedly elided");
  }
  // LRM 7.9.4 -- 7.9.7 traversal lowers to an immediately-invoked closure:
  // it writes the index back through an observable assign and runs in
  // expression position. The other associative methods are plain member
  // calls handled by the generic path below.
  if (support::IsAssociativeTraversalFn(b.method)) {
    return LowerAssociativeTraversal(process, frame, c, b.method, result_type);
  }
  const auto& module = process.Module();
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;
  const hir::TypeId hir_dispatch_type =
      hir_process.exprs.Get(*c.arguments.front()).type;
  std::vector<mir::ExprId> args;
  args.reserve(c.arguments.size() + 1);

  // Translate the callee up front so the same trait (`mir::IsMutatingCallee`)
  // drives both lowering and backend rendering.
  const mir::Callee mir_callee =
      MakeBuiltinMirCallee(module, b, hir_dispatch_type);

  // A static call has no value receiver -- `args[0]` is the discardable
  // type-bearer, the type-namespace qualifier rides on the callee. An
  // instance call lowers `args[0]` as the receiver, routing through
  // `Var<T>::Mutate` when the method mutates and the LHS roots in an
  // observable cell (so the method body operates on a `ScopedMutation`
  // snapshot whose destructor commits via `Var::Set`); non-mutating
  // methods consume a value, so the default value path (which auto-wraps
  // in `Get`) applies.
  const bool has_receiver =
      !std::holds_alternative<mir::BuiltinStaticCallee>(mir_callee);
  if (has_receiver) {
    const bool method_mutates = mir::IsMutatingCallee(mir_callee);
    mir::ExprId receiver_id{};
    if (method_mutates) {
      auto recv_or = process.LowerLhsExpr(
          hir_process.exprs.Get(*c.arguments.front()), frame);
      if (!recv_or) return std::unexpected(std::move(recv_or.error()));
      receiver_id = block.exprs.Add(*std::move(recv_or));
      receiver_id =
          MaybeWrapObservableLhsWithMutate(process, frame, block, receiver_id);
    } else {
      auto recv_or =
          process.LowerExpr(hir_process.exprs.Get(*c.arguments.front()), frame);
      if (!recv_or) return std::unexpected(std::move(recv_or.error()));
      receiver_id = block.exprs.Add(*std::move(recv_or));
    }
    args.push_back(receiver_id);
  }

  // Skip args[0] -- it was either pushed above as the lowered receiver
  // (instance call) or discarded as the type-bearer (static call).
  for (std::size_t i = 1; i < c.arguments.size(); ++i) {
    if (!c.arguments[i].has_value()) {
      throw InternalError("builtin-method call argument unexpectedly elided");
    }
    auto arg_or =
        process.LowerExpr(hir_process.exprs.Get(*c.arguments[i]), frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    args.push_back(block.exprs.Add(*std::move(arg_or)));
  }

  // LRM 7.12.1: reduction / ordering / locator array methods take a
  // closure. The user's `with` clause is used if present; otherwise LRM
  // defines the default as `with (item)`, which HIR-to-MIR synthesises so
  // MIR always carries the closure argument and downstream consumers see
  // one uniform shape per kind.
  if (support::ArrayMethodTakesClosure(b.method)) {
    auto closure_or = BuildArrayMethodClosure(
        process, frame, hir_dispatch_type,
        c.with_clause.has_value() ? &*c.with_clause : nullptr);
    if (!closure_or) return std::unexpected(std::move(closure_or.error()));
    args.push_back(block.exprs.Add(*std::move(closure_or)));
    // LRM 7.12 reduction / locator / map: the producer supplies the result's
    // canonical default, since the result shape (an index locator's key, a
    // map's chosen element, an empty reduction's zero) is not recoverable from
    // the receiver (decision: array-manipulation-entry-stream). The ordering
    // family produces no value and takes none.
    if (support::ArrayMethodProducesValue(b.method)) {
      const mir::TypeId proto_type =
          ArrayMethodResultProtoType(module, result_type);
      args.push_back(
          block.exprs.Add(BuildDefaultValueExpr(module, frame, proto_type)));
    }
  } else if (c.with_clause.has_value()) {
    throw InternalError(
        "BuiltinMethodRef with-clause on a method kind that does not "
        "accept a with-clause (LRM 7.12.1 family only)");
  }

  // LRM 15.5.3: `e.triggered` reads the triggered flag out of
  // RuntimeServices. The engine handle is a real trailing argument,
  // threaded the same way every runtime effect threads it
  // (docs/decisions/runtime-effects-as-generic-calls.md), not a backend-
  // fabricated one. (`-> e` is the only producer of the trigger kind and
  // lowers through LowerEventTriggerStmt; `await` takes no services.)
  if (b.method == support::BuiltinFn::kTriggered) {
    args.push_back(block.exprs.Add(BuildServicesCallExpr(process, frame)));
  }

  return mir::Expr{
      .data = mir::CallExpr{.callee = mir_callee, .arguments = std::move(args)},
      .type = result_type};
}

// LRM 7.12.4 `item.index` reads the second parameter of the enclosing
// array-method with-clause closure (LRM 7.12 closures pass both `item`
// and `index` to each invocation). The binding is installed by
// `BuildArrayMethodClosure`; reaching here without it is an upstream
// lowering bug.
auto LowerIteratorIndexCall(WalkFrame frame, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  if (!frame.active_index_binding.has_value()) {
    throw InternalError(
        "LowerIteratorIndexCall: IteratorIndexRef outside an array-method "
        "`with` body (LRM 7.12.4)");
  }
  return mir::Expr{
      .data =
          mir::LocalRef{
              .hops = mir::BlockHops{.value = 0},
              .var = *frame.active_index_binding},
      .type = result_type};
}

}  // namespace

auto LowerHirCallExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& c,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::SystemSubroutineRef& sys) -> diag::Result<mir::Expr> {
            return LowerSystemSubroutineCall(process, frame, c, sys, span);
          },
          [&](const hir::StructuralSubroutineRef& usr)
              -> diag::Result<mir::Expr> {
            return LowerStructuralSubroutineCall(
                process, frame, c, usr, span, result_type);
          },
          [&](const hir::BuiltinMethodRef& b) -> diag::Result<mir::Expr> {
            return LowerBuiltinMethodCall(process, frame, c, b, result_type);
          },
          [&](const hir::IteratorIndexRef&) -> diag::Result<mir::Expr> {
            return LowerIteratorIndexCall(frame, result_type);
          },
      },
      c.callee);
}

}  // namespace lyra::lowering::hir_to_mir
