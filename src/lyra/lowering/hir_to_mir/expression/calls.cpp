#include "lyra/lowering/hir_to_mir/expression/calls.hpp"

#include <algorithm>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/with_clause_id.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/control.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/file_io.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/plusargs.hpp"
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
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enclosing_hops.hpp"
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
template <ExprLowerer Lowerer>
auto MaybeWrapObservableLhsWithMutate(
    Lowerer& lowerer, WalkFrame frame, mir::Block& block, mir::ExprId lhs_id)
    -> mir::ExprId {
  const mir::ExprId root_id =
      FindLhsRootId(lowerer.Module().Unit(), block, lhs_id);
  const bool root_is_cell = mir::IsObservableCellType(
      lowerer.Module().Unit().types.Get(block.exprs.Get(root_id).type));
  if (!root_is_cell) return lhs_id;
  const mir::ExprId services_id =
      block.exprs.Add(BuildServicesCallExpr(lowerer.Module(), frame));
  return RewriteLhsRootWithMutate(
      lowerer.Module().Unit(), block, lhs_id, services_id);
}

// LRM 7.9.4 -- 7.9.7 associative traversal (`m.first(idx)` / `last` / `next` /
// `prev`). The method writes the visited key into `idx` and returns SV int
// 1 / 0. The call sits in expression position (the canonical
// `do ... while (m.next(idx))` idiom) yet must run a statement sequence -- read
// the probe, query, then write the index back so its LRM 4.3 update event
// fires -- so it lowers to an immediately-invoked closure. The query is a pure
// value member that mutates a plain body-local probe; the event-firing lives in
// the ordinary observable write-back assignment, not in the query.
template <ExprLowerer Lowerer>
auto LowerAssociativeTraversal(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    support::BuiltinFn fn, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  if (!c.arguments[1].has_value()) {
    throw InternalError(
        "LowerAssociativeTraversal: index argument unexpectedly elided");
  }
  const auto& module = lowerer.Module();
  const auto& hir_exprs = lowerer.HirExprs();
  const auto recv_hir = *c.arguments[0];
  const auto idx_hir = *c.arguments[1];
  const mir::TypeId key_type =
      module.TranslateType(hir_exprs.Get(idx_hir).type);
  const mir::TypeId void_t = module.Unit().builtins.void_type;

  ClosureBuilder closure(lowerer.Module().Unit(), frame);
  mir::Block& body = closure.Body();
  const WalkFrame& closure_frame = closure.Frame();

  // probe = idx -- snapshot the current index value into a plain local.
  auto idx_read_or = lowerer.LowerExpr(hir_exprs.Get(idx_hir), closure_frame);
  if (!idx_read_or) return std::unexpected(std::move(idx_read_or.error()));
  const mir::LocalId probe_var = closure.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_trav_probe", .type = key_type});
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = probe_var,
          .init = body.exprs.Add(*std::move(idx_read_or))});

  // found = (map).<kind>(probe) -- pure query: mutates probe, yields 1/0.
  auto map_read_or = lowerer.LowerExpr(hir_exprs.Get(recv_hir), closure_frame);
  if (!map_read_or) return std::unexpected(std::move(map_read_or.error()));
  const mir::ExprId map_read_id = body.exprs.Add(*std::move(map_read_or));
  const mir::ExprId probe_read_id =
      body.exprs.Add(mir::MakeLocalRefExpr(probe_var, key_type));
  const mir::ExprId query_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = fn},
                  .arguments = {map_read_id, probe_read_id}},
          .type = result_type});
  const mir::LocalId found_var = closure.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_trav_found", .type = result_type});
  body.AppendStmt(mir::LocalDeclStmt{.target = found_var, .init = query_id});

  // idx = probe -- observable write-back fires the LRM 4.3 update event.
  auto idx_lhs_or = lowerer.LowerLhsExpr(hir_exprs.Get(idx_hir), closure_frame);
  if (!idx_lhs_or) return std::unexpected(std::move(idx_lhs_or.error()));
  const mir::ExprId idx_lhs_id = body.exprs.Add(*std::move(idx_lhs_or));
  const mir::ExprId probe_writeback_id =
      body.exprs.Add(mir::MakeLocalRefExpr(probe_var, key_type));
  const mir::ExprId services_id =
      body.exprs.Add(BuildServicesCallExpr(lowerer.Module(), closure_frame));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      module.Unit(), body, services_id, idx_lhs_id, probe_writeback_id,
      std::nullopt, key_type, void_t);
  body.AppendStmt(mir::ExprStmt{.expr = body.exprs.Add(assign_expr)});

  const mir::ExprId found_id =
      body.exprs.Add(mir::MakeLocalRefExpr(found_var, result_type));
  return BuildClosureCallExpr(
      lowerer.Module().Unit(), *frame.current_block, closure.Build(found_id));
}

// Translates a HIR builtin-method ref to its MIR `Callee`. The identifier
// is the flat `support::BuiltinFn`; the only decision here is whether the
// id names a type-namespace-qualified static call (e.g. `MyEnum::first()`)
// or an instance call.
auto MakeBuiltinMirCallee(
    const ModuleLowerer& module, const hir::BuiltinMethodRef& b,
    hir::TypeId hir_dispatch_type) -> mir::Callee {
  if (support::IsStaticBuiltinFn(b.method)) {
    return mir::Direct{
        .target = b.method,
        .qualification = mir::TypeQualifier{
            .type = module.TranslateType(hir_dispatch_type)}};
  }
  return mir::Direct{.target = b.method};
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
      module.Unit().types.Get(result_type).data);
}

// LRM 7.12.1 / 7.12.2 / 7.12.3 with-clause closure synthesis. The element and
// index are the closure's two parameters (LRM 7.12.4); an `IterationBindingRef`
// in the body resolves to one of them by the clause identity registered here.
// The body is a normal expression lowered through `lowerer.LowerExpr`. When the
// source has no `with` clause LRM 7.12.1 defines the default as `with (item)`;
// this synthesises the identity closure (body returns the element parameter) so
// MIR always carries the closure argument and downstream consumers see one
// uniform shape.
template <ExprLowerer Lowerer>
auto BuildArrayMethodClosure(
    Lowerer& lowerer, WalkFrame frame, hir::TypeId hir_receiver_type,
    const hir::WithClause* with_clause) -> diag::Result<mir::Expr> {
  const auto& module = lowerer.Module();
  const auto& hir_exprs = lowerer.HirExprs();
  const auto& hir_recv_ty = module.Hir().types.Get(hir_receiver_type);
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
      assoc != nullptr) {
    index_type = module.TranslateType(assoc->key_type);
  }
  const std::string iterator_name =
      with_clause != nullptr ? with_clause->element_name : std::string{"item"};

  ClosureBuilder closure(lowerer.Module().Unit(), frame);
  mir::Block& body = closure.Body();

  mir::ExprId body_return_value{};
  if (with_clause != nullptr) {
    // `item` and `item.index` (LRM 7.12.4) are this clause's two per-invocation
    // parameters, declared under the clause's iterator origins so the body
    // resolves each reference by identity and captures it -- through the same
    // forwarding machinery as any binding -- when it crosses a closure boundary
    // (a clause nested in the body still reaches this outer iterator).
    closure.AddParam(
        BindingOriginId::Iterator(
            with_clause->id.value,
            static_cast<std::uint32_t>(hir::IterationBindingRole::kElement)),
        iterator_name, item_type);
    closure.AddParam(
        BindingOriginId::Iterator(
            with_clause->id.value,
            static_cast<std::uint32_t>(hir::IterationBindingRole::kIndex)),
        "index", index_type);
    auto body_expr_or =
        lowerer.LowerExpr(hir_exprs.Get(with_clause->expr), closure.Frame());
    if (!body_expr_or) return std::unexpected(std::move(body_expr_or.error()));
    body_return_value = body.exprs.Add(*std::move(body_expr_or));
  } else {
    // A built-in reduction (no with-clause) returns the bare element; its two
    // parameters carry no cross-body identity -- nothing can capture them.
    const mir::LocalId item_binding =
        closure.AddParamAnonymous(iterator_name, item_type);
    closure.AddParamAnonymous("index", index_type);
    body_return_value =
        body.exprs.Add(mir::MakeLocalRefExpr(item_binding, item_type));
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
            return LowerPrintSystemSubroutineCall(process, frame, call, print);
          },
          [&](const support::TerminationSystemSubroutineInfo& term)
              -> diag::Result<mir::Expr> {
            return LowerFinishSystemSubroutineCall(
                process, frame, call, desc.name, term, span);
          },
          [&](const support::DiagnosticSystemSubroutineInfo& diag_info)
              -> diag::Result<mir::Expr> {
            return LowerDiagnosticSystemSubroutineCall(
                process, frame, call, diag_info, span);
          },
          [&](const support::FileIOSystemSubroutineInfo& file_io)
              -> diag::Result<mir::Expr> {
            return LowerFileIOSystemSubroutineCall(
                process, frame, call, desc.name, file_io, span);
          },
          [&](const support::ScanSystemSubroutineInfo& scan_info)
              -> diag::Result<mir::Expr> {
            return LowerScanSystemSubroutineCall(
                process, frame, call, scan_info, span);
          },
          [&](const support::SFormatSystemSubroutineInfo& sformat)
              -> diag::Result<mir::Expr> {
            return LowerSFormatSystemSubroutineCall(
                process, frame, call, sformat);
          },
          [&](const support::TimeSystemSubroutineInfo& time_info)
              -> diag::Result<mir::Expr> {
            return LowerTimeSystemSubroutineCall(process, frame, time_info);
          },
          [&](const support::TimeFormatSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            return LowerTimeFormatSystemSubroutineCall(
                process, frame, call, span);
          },
          [&](const support::PrintTimescaleSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            return LowerPrintTimescaleSystemSubroutineCall(process, frame);
          },
          [&](const support::PlusargsSystemSubroutineInfo& plusargs)
              -> diag::Result<mir::Expr> {
            return LowerPlusargsSystemSubroutineCall(
                process, frame, call, plusargs);
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
// instance handle as `arguments[0]`; SV actuals follow.
template <ExprLowerer Lowerer>
auto LowerStructuralSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::StructuralSubroutineRef& usr, diag::SourceSpan span,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const hir::SubroutineDecl& decl =
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
  // The callee is reached through the object that owns it: at hops 0 the
  // current body's `self`, above that an enclosing scope's object navigated
  // through `Parent()`. The same receiver path a structural member access
  // takes, so the callee's class is named by this receiver's type.
  args.push_back(BuildEnclosingScopeReceiver(
      frame, lowerer.Module().Unit(),
      mir::EnclosingHops{.value = usr.hops.value}));
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
      auto arg_or = lowerer.LowerLhsExpr(hir_exprs.Get(*c.arguments[i]), frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      actual_id = block.exprs.Add(*std::move(arg_or));
      const mir::ExprId root_id =
          FindLhsRootId(lowerer.Module().Unit(), block, actual_id);
      if (root_id != actual_id) {
        actual_id =
            MaybeWrapObservableLhsWithMutate(lowerer, frame, block, actual_id);
      }
      args.push_back(BuildReferenceArg(
          lowerer.Module().Unit(), block, actual_id,
          block.exprs.Get(actual_id).type));
    } else {
      auto arg_or = lowerer.LowerExpr(hir_exprs.Get(*c.arguments[i]), frame);
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
template <ExprLowerer Lowerer>
auto LowerBuiltinMethodCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
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
    return LowerAssociativeTraversal(lowerer, frame, c, b.method, result_type);
  }
  const auto& module = lowerer.Module();
  const auto& hir_exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const hir::TypeId hir_dispatch_type =
      hir_exprs.Get(*c.arguments.front()).type;
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
  const auto* direct = std::get_if<mir::Direct>(&mir_callee);
  const bool has_receiver =
      direct != nullptr && !direct->qualification.has_value();
  if (has_receiver) {
    const bool method_mutates = mir::IsMutatingCallee(mir_callee);
    mir::ExprId receiver_id{};
    if (method_mutates) {
      auto recv_or =
          lowerer.LowerLhsExpr(hir_exprs.Get(*c.arguments.front()), frame);
      if (!recv_or) return std::unexpected(std::move(recv_or.error()));
      receiver_id = block.exprs.Add(*std::move(recv_or));
      receiver_id =
          MaybeWrapObservableLhsWithMutate(lowerer, frame, block, receiver_id);
    } else {
      auto recv_or =
          lowerer.LowerExpr(hir_exprs.Get(*c.arguments.front()), frame);
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
    auto arg_or = lowerer.LowerExpr(hir_exprs.Get(*c.arguments[i]), frame);
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
        lowerer, frame, hir_dispatch_type,
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
  // RuntimeServices. The engine handle is a real trailing argument, threaded
  // the same way every runtime effect threads it -- not a backend-fabricated
  // one. (`-> e` is the only producer of the trigger kind and lowers through
  // the event-trigger stmt path; `await` takes no services.)
  if (b.method == support::BuiltinFn::kTriggered) {
    args.push_back(
        block.exprs.Add(BuildServicesCallExpr(lowerer.Module(), frame)));
  }

  return mir::Expr{
      .data = mir::CallExpr{.callee = mir_callee, .arguments = std::move(args)},
      .type = result_type};
}

// An instance-method call (LRM 8.6): the receiver handle is the first argument,
// followed by the method's value arguments. The callee names the class method
// directly; the receiver's class layout, not the call site, decides how the
// backend dispatches.
template <ExprLowerer Lowerer>
auto LowerMethodCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::MethodCallRef& m, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> args;
  args.reserve(c.arguments.size() + 1);

  // The receiver evaluates to the managed handle; the method body operates on
  // a borrowed pointer to the object (LRM 8.6). Reaching the object from the
  // handle and taking its address yields that borrowed receiver -- the body
  // does not own or root the object, the caller's handle does.
  auto receiver_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(m.receiver), frame);
  if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
  auto& types = lowerer.Module().Unit().types;
  const mir::TypeId object_type =
      std::get<mir::ManagedRefType>(types.Get(receiver_or->type).data).pointee;
  const mir::ExprId handle_id = block.exprs.Add(*std::move(receiver_or));
  const mir::ExprId object_id = block.exprs.Add(
      mir::Expr{
          .data = mir::DerefExpr{.pointer = handle_id}, .type = object_type});
  const mir::TypeId self_pointer_type =
      types.PointerTo(object_type, mir::PointerOwnership::kBorrowed);
  args.push_back(
      block.exprs.Add(mir::MakeAddressOfExpr(object_id, self_pointer_type)));

  for (const auto& arg : c.arguments) {
    if (!arg.has_value()) {
      throw InternalError("LowerMethodCall: method-call argument elided");
    }
    auto arg_or = lowerer.LowerExpr(lowerer.HirExprs().Get(*arg), frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    args.push_back(block.exprs.Add(*std::move(arg_or)));
  }

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = mir::MethodId{.value = m.method_index},
                      .qualification = std::nullopt},
              .arguments = std::move(args)},
      .type = result_type};
}

// SV value -> foreign ABI carrier (LRM 35.5.6). An integral value yields its
// host int64, narrowed to the carrier's C width by a host cast; a real yields
// its native floating value; a string borrows a NUL-terminated C string that
// stays valid for the call. Reuses the ordinary value accessors; the boundary
// conversion is a plain expression, not a DPI-specific primitive. Feeds both an
// input argument (crossed by value) and the copy-in of an inout / output
// argument (seeding its boundary temp).
auto MarshalSvToCarrier(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId sv_id,
    const support::DpiCarrier& carrier_desc) -> mir::ExprId {
  const auto* scalar = std::get_if<support::ScalarCarrier>(&carrier_desc);
  if (scalar == nullptr) {
    throw InternalError(
        "MarshalSvToCarrier: a canonical-vector carrier is not yet "
        "implemented");
  }
  const mir::TypeId carrier = unit.types.Intern(
      mir::TypeData{mir::DpiCarrierType{.carrier = carrier_desc}});
  switch (scalar->abi) {
    case support::DpiScalarAbi::kBitScalar:
    case support::DpiScalarAbi::kByte:
    case support::DpiScalarAbi::kShortInt:
    case support::DpiScalarAbi::kInt:
    case support::DpiScalarAbi::kLongInt: {
      const mir::ExprId host_int = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kToInt64},
                      .arguments = {sv_id}},
              .type = unit.builtins.int32});
      return block.exprs.Add(
          mir::Expr{
              .data = mir::CastExpr{.operand = host_int}, .type = carrier});
    }
    case support::DpiScalarAbi::kReal:
      return block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kRealValue},
                      .arguments = {sv_id}},
              .type = carrier});
    case support::DpiScalarAbi::kString:
      return block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kStringCStr},
                      .arguments = {sv_id}},
              .type = carrier});
    case support::DpiScalarAbi::kChandle:
      return block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{
                              .target = support::BuiltinFn::kChandlePtr},
                      .arguments = {sv_id}},
              .type = carrier});
    case support::DpiScalarAbi::kLogicScalar:
      return block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kToSvLogic},
                      .arguments = {sv_id}},
              .type = carrier});
    case support::DpiScalarAbi::kVoid:
      throw InternalError(
          "MarshalSvToCarrier: void is not an argument carrier");
  }
  throw InternalError("MarshalSvToCarrier: unknown DpiScalarAbi");
}

// Foreign ABI carrier -> SV value into a declared SV type's canonical shape. An
// integral carrier is landed into the type's representation by the packed
// factory (the prototype carries that shape, so width / signedness / state
// domain follow the declared type); a real / string / chandle carrier
// constructs the SV value directly. Feeds both a function's marshaled return
// and the copy-back of an output / inout argument into its actual.
auto MarshalCarrierToSv(
    ModuleLowerer& module, WalkFrame frame, mir::ExprId call_id,
    const support::DpiCarrier& carrier_desc, mir::TypeId result_type)
    -> mir::Expr {
  const auto* scalar = std::get_if<support::ScalarCarrier>(&carrier_desc);
  if (scalar == nullptr) {
    throw InternalError(
        "MarshalCarrierToSv: a canonical-vector carrier is not yet "
        "implemented");
  }
  mir::Block& block = *frame.current_block;
  switch (scalar->abi) {
    case support::DpiScalarAbi::kBitScalar:
    case support::DpiScalarAbi::kByte:
    case support::DpiScalarAbi::kShortInt:
    case support::DpiScalarAbi::kInt:
    case support::DpiScalarAbi::kLongInt: {
      const mir::ExprId prototype =
          block.exprs.Add(BuildDefaultValueExpr(module, frame, result_type));
      return mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kFromInt,
                          .qualification =
                              mir::TypeQualifier{.type = result_type}},
                  .arguments = {call_id, prototype}},
          .type = result_type};
    }
    case support::DpiScalarAbi::kReal:
    case support::DpiScalarAbi::kString:
    case support::DpiScalarAbi::kChandle:
      return mir::Expr{
          .data =
              mir::CallExpr{.callee = mir::Construct{}, .arguments = {call_id}},
          .type = result_type};
    case support::DpiScalarAbi::kLogicScalar: {
      const mir::ExprId prototype =
          block.exprs.Add(BuildDefaultValueExpr(module, frame, result_type));
      return mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kFromSvLogic},
                  .arguments = {call_id, prototype}},
          .type = result_type};
    }
    case support::DpiScalarAbi::kVoid:
      throw InternalError("MarshalCarrierToSv: void has no marshal-out");
  }
  throw InternalError("MarshalCarrierToSv: unknown DpiScalarAbi");
}

// A canonical-vector actual crosses by pointer to a boundary buffer, regardless
// of direction -- even an input, since the C ABI is `const svLogicVecVal*`. The
// buffer is a `DpiBitBuffer` / `DpiLogicBuffer` value, so it needs a boundary
// temp and the sequenced closure path.
[[nodiscard]] auto NeedsBoundaryTemp(const mir::ForeignParam& p) -> bool {
  return support::DpiDirectionWritesBack(p.direction) ||
         std::holds_alternative<support::VectorCarrier>(p.carrier);
}

// The read-back builtin for a canonical-vector carrier: 4-state reads both
// planes, 2-state the value plane only.
[[nodiscard]] auto VectorReadBuiltin(const support::VectorCarrier& v)
    -> support::BuiltinFn {
  return v.four_state ? support::BuiltinFn::kReadCanonicalLogicVec
                      : support::BuiltinFn::kReadCanonicalBitVec;
}

// The writable canonical chunk pointer of a boundary buffer, `(buf).Data()`. It
// feeds both the foreign call (which writes through it) and the copy-back read.
// The result type is a borrowed pointer for bookkeeping only: value emission
// renders the `Data()` call and passes it as an argument, never spelling the
// pointer type itself.
auto BuildBufferDataCall(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId buffer_ref,
    mir::TypeId carrier_type) -> mir::ExprId {
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = support::BuiltinFn::kDpiBufferData},
                  .arguments = {buffer_ref}},
          .type = unit.types.PointerTo(
              carrier_type, mir::PointerOwnership::kBorrowed)});
}

// The all-input import call: every actual crosses by value, so the call is a
// plain expression -- no statement sequencing, no boundary temps. Each actual
// is marshaled to its ABI carrier, the foreign symbol is called over the
// carriers, and a non-void result is marshaled back to the declared SV type.
template <ExprLowerer Lowerer>
auto LowerForeignImportInputsOnly(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const mir::StaticCallableDecl& decl, mir::StaticCallableTarget target,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& module = lowerer.Module();
  auto& unit = module.Unit();
  auto& block = *frame.current_block;
  const auto& hir_exprs = lowerer.HirExprs();

  std::vector<mir::ExprId> carrier_args;
  carrier_args.reserve(c.arguments.size());
  for (std::size_t i = 0; i < c.arguments.size(); ++i) {
    if (!c.arguments[i].has_value()) {
      throw InternalError("DPI-C import call argument unexpectedly elided");
    }
    auto sv_or = lowerer.LowerExpr(hir_exprs.Get(*c.arguments[i]), frame);
    if (!sv_or) return std::unexpected(std::move(sv_or.error()));
    const mir::ExprId sv_id = block.exprs.Add(*std::move(sv_or));
    carrier_args.push_back(
        MarshalSvToCarrier(unit, block, sv_id, decl.params[i].carrier));
  }

  const bool is_void = decl.ret_abi == support::DpiScalarAbi::kVoid;
  const mir::TypeId call_type =
      is_void ? unit.builtins.void_type
              : unit.types.Intern(
                    mir::TypeData{mir::DpiCarrierType{
                        .carrier = support::ScalarCarrier{decl.ret_abi}}});
  mir::Expr foreign_call{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = target},
              .arguments = std::move(carrier_args)},
      .type = call_type};
  if (is_void) {
    return foreign_call;
  }
  const mir::ExprId call_id = block.exprs.Add(std::move(foreign_call));
  return MarshalCarrierToSv(
      module, frame, call_id, support::ScalarCarrier{decl.ret_abi},
      result_type);
}

// The general import call: at least one actual needs a boundary temp -- an
// output / inout of any carrier, or a canonical vector of any direction (a
// vector crosses by pointer even as an input). The call sequences copy-in, the
// by-pointer foreign call, copy-back into each writeback actual's cell, and the
// marshaled return -- a statement sequence yielding a value, so it lowers to an
// immediately-invoked closure, uniform for void / valued and statement /
// expression position. A by-value scalar input still crosses by value with no
// temp.
template <ExprLowerer Lowerer>
auto LowerForeignImportSequenced(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const mir::StaticCallableDecl& decl, mir::StaticCallableTarget target,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& module = lowerer.Module();
  auto& unit = module.Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  const mir::TypeId void_t = unit.builtins.void_type;

  ClosureBuilder closure(unit, frame);
  mir::Block& body = closure.Body();
  const WalkFrame& cframe = closure.Frame();

  // One output / inout argument to copy back after the call.
  struct Writeback {
    mir::LocalId temp{};
    mir::TypeId carrier_type{};
    hir::ExprId actual{};
    support::DpiCarrier carrier{};
    mir::TypeId sv_type{};
  };
  std::vector<Writeback> writebacks;

  std::vector<mir::ExprId> call_args;
  call_args.reserve(c.arguments.size());
  for (std::size_t i = 0; i < c.arguments.size(); ++i) {
    if (!c.arguments[i].has_value()) {
      throw InternalError("DPI-C import call argument unexpectedly elided");
    }
    const hir::ExprId actual = *c.arguments[i];
    const support::DpiCarrier& carrier = decl.params[i].carrier;
    const support::DpiDirection direction = decl.params[i].direction;
    const bool writes_back = support::DpiDirectionWritesBack(direction);
    const auto* vector = std::get_if<support::VectorCarrier>(&carrier);

    // A by-value scalar input crosses by value: no boundary temp, no
    // sequencing.
    if (vector == nullptr && !writes_back) {
      auto sv_or = lowerer.LowerExpr(hir_exprs.Get(actual), cframe);
      if (!sv_or) return std::unexpected(std::move(sv_or.error()));
      const mir::ExprId sv_id = body.exprs.Add(*std::move(sv_or));
      call_args.push_back(MarshalSvToCarrier(unit, body, sv_id, carrier));
      continue;
    }

    // Every other actual seeds a boundary temp from the actual's current value.
    // inout requires that initial value (LRM 35.5.1.2); for output the initial
    // carrier value is implementation-defined, so seeding from the actual is a
    // legal, uniform choice; a vector input seeds the same way. The foreign
    // side writes through the pointer (for writeback directions); the copy-back
    // below lands the result back in the actual's cell.
    const mir::TypeId carrier_type = unit.types.Intern(
        mir::TypeData{mir::DpiCarrierType{.carrier = carrier}});
    const mir::LocalId temp = closure.Bindings().DeclareAnonymous(
        mir::LocalDecl{
            .name = "_lyra_dpi_arg" + std::to_string(i), .type = carrier_type});
    auto seed_or = lowerer.LowerExpr(hir_exprs.Get(actual), cframe);
    if (!seed_or) return std::unexpected(std::move(seed_or.error()));
    const mir::ExprId seed_sv = body.exprs.Add(*std::move(seed_or));

    if (vector != nullptr) {
      // A canonical vector's boundary temp is a buffer value constructed from
      // the seed (its constructor fills it). The foreign call and the copy-back
      // read reach the chunks through its writable data pointer.
      body.AppendStmt(
          mir::LocalDeclStmt{
              .target = temp,
              .init = body.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee = mir::Construct{},
                              .arguments = {seed_sv}},
                      .type = carrier_type})});
      const mir::ExprId buffer_ref =
          body.exprs.Add(mir::MakeLocalRefExpr(temp, carrier_type));
      call_args.push_back(
          BuildBufferDataCall(unit, body, buffer_ref, carrier_type));
    } else {
      // A scalar output / inout's boundary temp is the by-value carrier, seeded
      // by the marshal-in rvalue and passed by pointer.
      body.AppendStmt(
          mir::LocalDeclStmt{
              .target = temp,
              .init = MarshalSvToCarrier(unit, body, seed_sv, carrier)});
      const mir::TypeId ptr_type =
          unit.types.PointerTo(carrier_type, mir::PointerOwnership::kBorrowed);
      const mir::ExprId temp_ref =
          body.exprs.Add(mir::MakeLocalRefExpr(temp, carrier_type));
      call_args.push_back(
          body.exprs.Add(mir::MakeAddressOfExpr(temp_ref, ptr_type)));
    }

    if (writes_back) {
      writebacks.push_back(
          Writeback{
              .temp = temp,
              .carrier_type = carrier_type,
              .actual = actual,
              .carrier = carrier,
              .sv_type = decl.params[i].sv_type});
    }
  }

  const bool is_void = decl.ret_abi == support::DpiScalarAbi::kVoid;
  const mir::TypeId call_type =
      is_void ? void_t
              : unit.types.Intern(
                    mir::TypeData{mir::DpiCarrierType{
                        .carrier = support::ScalarCarrier{decl.ret_abi}}});
  mir::Expr foreign_call{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = target},
              .arguments = std::move(call_args)},
      .type = call_type};

  // A valued call captures its carrier result in a temp so the copy-backs run
  // before it is marshaled and returned; a void call is a bare statement.
  std::optional<mir::LocalId> ret_temp;
  if (!is_void) {
    ret_temp = closure.Bindings().DeclareAnonymous(
        mir::LocalDecl{.name = "_lyra_dpi_ret", .type = call_type});
    body.AppendStmt(
        mir::LocalDeclStmt{
            .target = *ret_temp,
            .init = body.exprs.Add(std::move(foreign_call))});
  } else {
    body.AppendStmt(
        mir::ExprStmt{.expr = body.exprs.Add(std::move(foreign_call))});
  }

  for (const Writeback& wb : writebacks) {
    auto lhs_or = lowerer.LowerLhsExpr(hir_exprs.Get(wb.actual), cframe);
    if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
    const mir::ExprId lhs_id = body.exprs.Add(*std::move(lhs_or));
    const mir::ExprId temp_ref =
        body.exprs.Add(mir::MakeLocalRefExpr(wb.temp, wb.carrier_type));

    // A scalar carrier marshals its by-value temp back; a vector carrier reads
    // its buffer's chunks back through the buffer's data pointer.
    mir::ExprId rhs_id{};
    if (const auto* vector = std::get_if<support::VectorCarrier>(&wb.carrier)) {
      const mir::ExprId data =
          BuildBufferDataCall(unit, body, temp_ref, wb.carrier_type);
      const mir::ExprId prototype =
          body.exprs.Add(BuildDefaultValueExpr(module, cframe, wb.sv_type));
      rhs_id = body.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = VectorReadBuiltin(*vector)},
                      .arguments = {data, prototype}},
              .type = wb.sv_type});
    } else {
      rhs_id = body.exprs.Add(
          MarshalCarrierToSv(module, cframe, temp_ref, wb.carrier, wb.sv_type));
    }
    const mir::ExprId services_id =
        body.exprs.Add(BuildServicesCallExpr(module, cframe));
    const mir::Expr assign = BuildObservableAssignExpr(
        unit, body, services_id, lhs_id, rhs_id, std::nullopt, wb.sv_type,
        void_t);
    body.AppendStmt(mir::ExprStmt{.expr = body.exprs.Add(assign)});
  }

  if (is_void) {
    return BuildClosureCallExpr(
        unit, *frame.current_block, closure.BuildVoid());
  }
  const mir::ExprId ret_ref =
      body.exprs.Add(mir::MakeLocalRefExpr(*ret_temp, call_type));
  const mir::ExprId result_id = body.exprs.Add(MarshalCarrierToSv(
      module, cframe, ret_ref, support::ScalarCarrier{decl.ret_abi},
      result_type));
  return BuildClosureCallExpr(
      unit, *frame.current_block, closure.Build(result_id));
}

// The compilation-unit identity of the class `hops` enclosing levels up. A
// receiver-less callable names its owner in the call target, so that owner must
// be a unit-stable class identity rather than a position in the lowering walk;
// a class carries its own identity in its self-pointer type, so recover it from
// there.
auto EnclosingClassIdAtHops(
    const WalkFrame& frame, const mir::CompilationUnit& unit,
    mir::EnclosingHops hops) -> mir::ClassId {
  const mir::TypeId self_ptr =
      frame.EnclosingClassAtHops(hops).self_pointer_type;
  const auto& ptr = std::get<mir::PointerType>(unit.types.Get(self_ptr).data);
  return std::get<mir::ObjectType>(unit.types.Get(ptr.pointee).data).class_id;
}

// LRM 35.4 import call: an ordinary call to the external static callable,
// wrapped in boundary marshaling. The carriers and directions are read from the
// callable's own declaration, resolved once at its declaration lowering. A call
// whose every actual is a by-value scalar input is a plain expression; a call
// with any actual that needs a boundary temp (an output / inout, or a canonical
// vector of any direction) sequences through a closure.
template <ExprLowerer Lowerer>
auto LowerForeignImportCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ForeignImportRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  // An import is a receiver-less associated callable of the scope that declares
  // it. `hops` is how far out that scope sits from the caller -- a declaration
  // lookup distance, resolved here and then gone; the callable has no body and
  // no receiver, so nothing is reached at run time and the call names its owner
  // directly.
  const mir::CompilationUnit& unit = lowerer.Module().Unit();
  const mir::EnclosingHops hops{.value = ref.hops.value};
  const mir::StaticCallableTarget target{
      .owner = EnclosingClassIdAtHops(frame, unit, hops),
      .slot = mir::StaticCallableId{ref.id.value}};
  const mir::StaticCallableDecl& decl =
      frame.EnclosingClassAtHops(hops).static_callables.Get(target.slot);

  const bool needs_temp = std::ranges::any_of(decl.params, NeedsBoundaryTemp);
  if (needs_temp) {
    return LowerForeignImportSequenced(
        lowerer, frame, c, decl, target, result_type);
  }
  return LowerForeignImportInputsOnly(
      lowerer, frame, c, decl, target, result_type);
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerHirCallExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::SystemSubroutineRef& sys) -> diag::Result<mir::Expr> {
            // System subroutines (the print / scan / file families plus the
            // value functions) are lowered through statement-flavored handlers
            // bound to a process body; a continuous-assign RHS reaching one is
            // a value system function ($time and the like), which is the only
            // form legal in expression position there but not yet wired on the
            // structural path.
            if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
              return LowerSystemSubroutineCall(lowerer, frame, c, sys, span);
            } else {
              return diag::Fail(
                  span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
                  "a system subroutine call is not yet supported in a "
                  "continuous assignment");
            }
          },
          [&](const hir::StructuralSubroutineRef& usr)
              -> diag::Result<mir::Expr> {
            return LowerStructuralSubroutineCall(
                lowerer, frame, c, usr, span, result_type);
          },
          [&](const hir::MethodCallRef& m) -> diag::Result<mir::Expr> {
            return LowerMethodCall(lowerer, frame, c, m, result_type);
          },
          [&](const hir::BuiltinMethodRef& b) -> diag::Result<mir::Expr> {
            return LowerBuiltinMethodCall(lowerer, frame, c, b, result_type);
          },
          [&](const hir::ForeignImportRef& imp) -> diag::Result<mir::Expr> {
            return LowerForeignImportCall(lowerer, frame, c, imp, result_type);
          },
      },
      c.callee);
}

template auto LowerHirCallExpr(
    ProcessLowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr>;
template auto LowerHirCallExpr(
    const StructuralScopeLowerer& lowerer, WalkFrame frame,
    const hir::CallExpr& c, diag::SourceSpan span, mir::TypeId result_type)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
