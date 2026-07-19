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
#include "lyra/hir/foreign_export.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/with_clause_id.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/completion_payload.hpp"
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
#include "lyra/mir/foreign_export_wrapper.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/imported_runtime_class.hpp"
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
      FindLhsRootId(lowerer.Owner().Unit(), block, lhs_id);
  const bool root_is_cell = mir::IsObservableCellType(
      lowerer.Owner().Unit().types.Get(block.exprs.Get(root_id).type));
  if (!root_is_cell) return lhs_id;
  const mir::ExprId services_id =
      block.exprs.Add(BuildServicesCallExpr(lowerer.Owner(), frame));
  return RewriteLhsRootWithMutate(
      lowerer.Owner().Unit(), block, lhs_id, services_id);
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
  const auto& unit_lowerer = lowerer.Owner();
  const auto& hir_exprs = lowerer.HirExprs();
  const auto recv_hir = *c.arguments[0];
  const auto idx_hir = *c.arguments[1];
  const mir::TypeId key_type =
      unit_lowerer.TranslateType(hir_exprs.Get(idx_hir).type);
  const mir::TypeId void_t = unit_lowerer.Unit().builtins.void_type;

  ClosureBuilder closure(lowerer.Owner().Unit(), frame);
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
      body.exprs.Add(BuildServicesCallExpr(lowerer.Owner(), closure_frame));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      unit_lowerer.Unit(), body, services_id, idx_lhs_id, probe_writeback_id,
      std::nullopt, key_type, void_t);
  body.AppendStmt(mir::ExprStmt{.expr = body.exprs.Add(assign_expr)});

  const mir::ExprId found_id =
      body.exprs.Add(mir::MakeLocalRefExpr(found_var, result_type));
  return BuildClosureCallExpr(
      lowerer.Owner().Unit(), *frame.current_block, closure.Build(found_id));
}

// Translates a HIR builtin-method ref to its MIR `Callee`. The identifier
// is the flat `support::BuiltinFn`; the only decision here is whether the
// id names a type-namespace-qualified static call (e.g. `MyEnum::first()`)
// or an instance call.
auto MakeBuiltinMirCallee(
    const UnitLowerer& unit_lowerer, const hir::BuiltinMethodRef& b,
    hir::TypeId hir_dispatch_type) -> mir::Callee {
  if (support::IsStaticBuiltinFn(b.method)) {
    return mir::Direct{
        .target = b.method,
        .qualification = mir::TypeQualifier{
            .type = unit_lowerer.TranslateType(hir_dispatch_type)}};
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

// The canonical-default prototype type for an entry whose result shape the
// receiver does not determine: a container result contributes its element type
// as the prototype; a scalar result is its own prototype.
auto ResultPrototypeType(
    const UnitLowerer& unit_lowerer, mir::TypeId result_type) -> mir::TypeId {
  return std::visit(
      Overloaded{
          [](const mir::UnpackedArrayType& t) { return t.element_type; },
          [](const mir::DynamicArrayType& t) { return t.element_type; },
          [](const mir::QueueType& t) { return t.element_type; },
          [](const mir::AssociativeArrayType& t) { return t.element_type; },
          [result_type](const auto&) { return result_type; },
      },
      unit_lowerer.Unit().types.Get(result_type).data);
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
  const auto& unit_lowerer = lowerer.Owner();
  const auto& hir_exprs = lowerer.HirExprs();
  const auto& hir_recv_ty = unit_lowerer.Hir().types.Get(hir_receiver_type);
  const auto element_type = ArrayMethodReceiverElementType(hir_recv_ty);
  if (!element_type.has_value()) {
    throw InternalError(
        "BuildArrayMethodClosure: receiver is not an unpacked-array type");
  }
  const mir::TypeId item_type = unit_lowerer.TranslateType(*element_type);
  // LRM 7.12.4 `item.index`: the ordinal position for a sequence container, the
  // key for an associative receiver.
  mir::TypeId index_type = unit_lowerer.Unit().builtins.int_type;
  if (const auto* assoc =
          std::get_if<hir::AssociativeArrayType>(&hir_recv_ty.data);
      assoc != nullptr) {
    index_type = unit_lowerer.TranslateType(assoc->key_type);
  }
  const std::string iterator_name =
      with_clause != nullptr ? with_clause->element_name : std::string{"item"};

  ClosureBuilder closure(lowerer.Owner().Unit(), frame);
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
      frame, lowerer.Owner().Unit(),
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
          FindLhsRootId(lowerer.Owner().Unit(), block, actual_id);
      if (root_id != actual_id) {
        actual_id =
            MaybeWrapObservableLhsWithMutate(lowerer, frame, block, actual_id);
      }
      args.push_back(BuildReferenceArg(
          lowerer.Owner().Unit(), block, actual_id,
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
  const auto& unit_lowerer = lowerer.Owner();
  const auto& hir_exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const hir::TypeId hir_dispatch_type =
      hir_exprs.Get(*c.arguments.front()).type;
  std::vector<mir::ExprId> args;
  args.reserve(c.arguments.size() + 1);

  // Translate the callee up front so the same trait (`mir::IsMutatingCallee`)
  // drives both lowering and backend rendering.
  const mir::Callee mir_callee =
      MakeBuiltinMirCallee(unit_lowerer, b, hir_dispatch_type);

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
  } else if (c.with_clause.has_value()) {
    throw InternalError(
        "BuiltinMethodRef with-clause on a method kind that does not "
        "accept a with-clause (LRM 7.12.1 family only)");
  }

  // The producer supplies the result's canonical default whenever the receiver
  // does not determine the result's shape -- an LRM 7.12 index locator's key, a
  // map's chosen element, an empty reduction's zero, or the index an empty
  // associative dimension reports (LRM 20.7).
  if (support::BuiltinFnTakesResultPrototype(b.method)) {
    const mir::TypeId proto_type =
        ResultPrototypeType(unit_lowerer, result_type);
    args.push_back(block.exprs.Add(
        BuildDefaultValueExpr(unit_lowerer, frame, proto_type)));
  }

  // LRM 15.5.3: `e.triggered` reads the triggered flag out of
  // RuntimeServices. The engine handle is a real trailing argument, threaded
  // the same way every runtime effect threads it -- not a backend-fabricated
  // one. (`-> e` is the only producer of the trigger kind and lowers through
  // the event-trigger stmt path; `await` takes no services.)
  if (b.method == support::BuiltinFn::kTriggered) {
    args.push_back(
        block.exprs.Add(BuildServicesCallExpr(lowerer.Owner(), frame)));
  }

  return mir::Expr{
      .data = mir::CallExpr{.callee = mir_callee, .arguments = std::move(args)},
      .type = result_type};
}

// Reads the slot's canonical (owner, id) off a method's stated
// `virtual_dispatch`. Every participating method already stores this pair --
// an introducer names itself, an intra-unit override was populated with the
// canonical id at class lowering -- so this is a one-arm dispatch, never a
// chain walk.
auto CanonicalIntraUnitSlot(
    mir::ClassId self_owner, mir::CallableId self_slot,
    const mir::VirtualDispatchRole& role)
    -> std::pair<mir::ClassId, mir::CallableId> {
  return std::visit(
      Overloaded{
          [&](const mir::IntroducesVirtualSlot&)
              -> std::pair<mir::ClassId, mir::CallableId> {
            return {self_owner, self_slot};
          },
          [](const mir::OverridesIntraUnitSlot& s)
              -> std::pair<mir::ClassId, mir::CallableId> {
            return {s.slot_owner, s.slot_id};
          },
          [](const mir::OverridesExternalSlot&)
              -> std::pair<mir::ClassId, mir::CallableId> {
            throw InternalError(
                "CanonicalIntraUnitSlot: expected an intra-unit slot, but the "
                "method's role names a cross-unit slot introducer");
          }},
      role);
}

// An instance-method call (LRM 8.6): the receiver evaluates to the managed
// handle, and the borrowed pointer to the object reaches the method body as
// its `self`. A direct call passes that pointer as `arguments[0]` and names
// the concrete method arena entry; a virtual call carries it as
// `Virtual::receiver` and names the slot's canonical identity so the
// receiver's dynamic type -- not the call site -- picks the implementation
// (LRM 8.20).
template <ExprLowerer Lowerer>
auto LowerMethodCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::MethodCallRef& m, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  auto& types = lowerer.Owner().Unit().types;

  // The receiver pointer origin depends only on whether the source supplied
  // an explicit handle. A `HandleReceiver` evaluates the handle expression
  // then derefs the managed wrapper to reach the object; both
  // `ImplicitSelfReceiver` and `SuperReceiver` read the enclosing method's
  // own self binding, which is already a borrowed pointer to the object.
  // The super arm shares the self read here because it differs only in
  // dispatch semantics, decided below.
  mir::ExprId receiver_ptr{};
  if (const auto* handle = std::get_if<hir::HandleReceiver>(&m.receiver)) {
    auto handle_or =
        lowerer.LowerExpr(lowerer.HirExprs().Get(handle->expr), frame);
    if (!handle_or) return std::unexpected(std::move(handle_or.error()));
    const mir::TypeId object_type =
        std::get<mir::ManagedRefType>(types.Get(handle_or->type).data).pointee;
    const mir::ExprId handle_id = block.exprs.Add(*std::move(handle_or));
    const mir::ExprId object_id = block.exprs.Add(
        mir::Expr{
            .data = mir::DerefExpr{.pointer = handle_id}, .type = object_type});
    const mir::TypeId self_pointer_type =
        types.PointerTo(object_type, mir::PointerOwnership::kBorrowed);
    receiver_ptr =
        block.exprs.Add(mir::MakeAddressOfExpr(object_id, self_pointer_type));
  } else {
    receiver_ptr = block.exprs.Add(
        MakeSelfRefExpr(frame, frame.current_class->self_pointer_type));
  }

  std::vector<mir::ExprId> user_args;
  user_args.reserve(c.arguments.size());
  for (const auto& arg : c.arguments) {
    if (!arg.has_value()) {
      throw InternalError("LowerMethodCall: method-call argument elided");
    }
    auto arg_or = lowerer.LowerExpr(lowerer.HirExprs().Get(*arg), frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    user_args.push_back(block.exprs.Add(*std::move(arg_or)));
  }

  // Cross-unit instance method: no local shape store to query for the
  // callee's dispatch role, so this site always lowers to a direct call
  // naming the method by (unit, class, method) name. Virtual resolution
  // rides on the target-language include of the declaring unit's header --
  // the callee's own virtual machinery dispatches at the receiver's runtime
  // class -- and a super qualifier crosses the boundary the same way, as
  // an owner-scoped call the backend renders when the qualification arm
  // is set.
  if (const auto* ext_target =
          std::get_if<hir::ExternalClassMethodTarget>(&m.target)) {
    std::vector<mir::ExprId> direct_args;
    direct_args.reserve(user_args.size() + 1);
    direct_args.push_back(receiver_ptr);
    for (const mir::ExprId a : user_args) direct_args.push_back(a);
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = lowerer.Owner().MakeExternalMethodTarget(
                            *ext_target),
                        .qualification = std::nullopt},
                .arguments = std::move(direct_args)},
        .type = result_type};
  }

  // The method's declaring class is stated on the HIR node; the receiver's
  // runtime class is only used to type the self parameter (which pairs with
  // the target's own arena entry through C++ member lookup). For a super
  // call HIR already sets the target to the base's identity.
  const auto& local_target = std::get<hir::LocalClassMethodTarget>(m.target);
  const mir::ClassId owner_class =
      lowerer.Owner().TranslateClass(local_target.owner);
  const mir::CallableId method_slot{local_target.method.value};

  // A super call (LRM 8.15) is a dispatch fact independent of the callee's
  // virtual role: the source demands the base's implementation regardless
  // of the receiver's dynamic type, so it lowers as `Direct` on the base's
  // method. The backend's uniform owner-qualified render skips dynamic
  // dispatch mechanically. Every other receiver form defers to the
  // callee's own dispatch role: `Virtual` when the target participates in
  // a slot, `Direct` otherwise. Cross-class dispatch role is queried
  // through the shape store, not the unit's class registry: while any peer
  // body is lowering the registry is one-way `Define`, so a `Get` there
  // would leak lowering order into the reading site.
  const bool through_super =
      std::holds_alternative<hir::SuperReceiver>(m.receiver);
  const auto& method_sig = lowerer.Owner()
                               .GetClassShape(owner_class)
                               .callable_signatures.Get(method_slot);
  if (!through_super && method_sig.virtual_dispatch.has_value()) {
    const auto [canonical_owner, canonical_slot] = CanonicalIntraUnitSlot(
        owner_class, method_slot, *method_sig.virtual_dispatch);
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Virtual{
                        .receiver = receiver_ptr,
                        .owner_class = canonical_owner,
                        .slot = canonical_slot},
                .arguments = std::move(user_args)},
        .type = result_type};
  }

  std::vector<mir::ExprId> direct_args;
  direct_args.reserve(user_args.size() + 1);
  direct_args.push_back(receiver_ptr);
  for (const mir::ExprId a : user_args) direct_args.push_back(a);
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target =
                          mir::CallableTarget{
                              .owner = owner_class, .slot = method_slot},
                      .qualification = std::nullopt},
              .arguments = std::move(direct_args)},
      .type = result_type};
}

// Lowers a static class method call (LRM 8.10). The signature has no `self`
// so the MIR call carries exactly the user's arguments; the callee is a
// direct owner-qualified target with no dispatch role.
template <ExprLowerer Lowerer>
auto LowerStaticMethodCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::StaticMethodCallRef& m, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const auto* local_target =
      std::get_if<hir::LocalClassMethodTarget>(&m.target);
  if (local_target == nullptr) {
    // Cross-unit static method: no receiver, rendered by the backend as the
    // free qualified `unit::Class::method(args)` after the declaring unit's
    // header is included.
    const auto& ext_target = std::get<hir::ExternalClassMethodTarget>(m.target);
    std::vector<mir::ExprId> args;
    args.reserve(c.arguments.size());
    for (const auto& arg : c.arguments) {
      if (!arg.has_value()) {
        throw InternalError("LowerStaticMethodCall: argument elided");
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
                        .target = lowerer.Owner().MakeExternalMethodTarget(
                            ext_target),
                        .qualification = std::nullopt},
                .arguments = std::move(args)},
        .type = result_type};
  }
  const mir::ClassId owner_class =
      lowerer.Owner().TranslateClass(local_target->owner);
  const mir::CallableId method_slot{local_target->method.value};

  std::vector<mir::ExprId> args;
  args.reserve(c.arguments.size());
  for (const auto& arg : c.arguments) {
    if (!arg.has_value()) {
      throw InternalError("LowerStaticMethodCall: argument elided");
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
                      .target =
                          mir::CallableTarget{
                              .owner = owner_class, .slot = method_slot},
                      .qualification = std::nullopt},
              .arguments = std::move(args)},
      .type = result_type};
}

// The type a value has once it is marshaled to a DPI-C carrier (LRM 35.5.6,
// Table H.1). The carrier classifies the *formal*'s ABI; the value that crosses
// is an ordinary machine value, so it is typed as one -- an integer of the
// declared C width, a machine float, a borrowed C string, a raw pointer. A
// packed vector crosses in a canonical chunk buffer, which is a runtime library
// value. No type exists solely to mark a value as being at the boundary.
auto CarrierTypeId(
    mir::CompilationUnit& unit, const support::DpiCarrier& carrier)
    -> mir::TypeId {
  if (const auto* vec = std::get_if<support::VectorCarrier>(&carrier)) {
    return unit.types.Intern(
        mir::TypeData{mir::RuntimeLibraryType{
            .kind = vec->four_state ? mir::RuntimeLibraryKind::kDpiLogicBuffer
                                    : mir::RuntimeLibraryKind::kDpiBitBuffer}});
  }
  const auto machine_int = [&](std::uint32_t bits, mir::Signedness sign) {
    return unit.types.Intern(
        mir::TypeData{
            mir::MachineIntType{.bit_width = bits, .signedness = sign}});
  };
  switch (std::get<support::ScalarCarrier>(carrier).abi) {
    case support::DpiScalarAbi::kBitScalar:
    case support::DpiScalarAbi::kLogicScalar:
      return machine_int(8, mir::Signedness::kUnsigned);
    case support::DpiScalarAbi::kByte:
      return machine_int(8, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kShortInt:
      return machine_int(16, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kInt:
      return machine_int(32, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kLongInt:
      return machine_int(64, mir::Signedness::kSigned);
    case support::DpiScalarAbi::kReal:
      return unit.types.Intern(
          mir::TypeData{mir::MachineFloatType{.bit_width = 64}});
    case support::DpiScalarAbi::kString:
      return unit.types.Intern(mir::TypeData{mir::MachineCStringType{}});
    case support::DpiScalarAbi::kChandle:
      return unit.types.PointerTo(
          unit.builtins.void_type, mir::PointerOwnership::kBorrowed);
    case support::DpiScalarAbi::kVoid:
      return unit.builtins.void_type;
  }
  throw InternalError("CarrierTypeId: unknown DPI-C scalar ABI");
}

// SV value -> foreign ABI carrier (LRM 35.5.6). An integral value yields its
// widest machine integer, narrowed to the carrier's C width by a machine cast;
// a real yields its native floating value; a string borrows a NUL-terminated C
// string that stays valid for the call. Reuses the ordinary value accessors;
// the boundary conversion is a plain expression, not a DPI-specific primitive.
// Feeds both an input argument (crossed by value) and the copy-in of an inout /
// output argument (seeding its boundary temp).
auto MarshalSvToCarrier(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId sv_id,
    const support::DpiCarrier& carrier_desc) -> mir::ExprId {
  const auto* scalar = std::get_if<support::ScalarCarrier>(&carrier_desc);
  if (scalar == nullptr) {
    throw InternalError(
        "MarshalSvToCarrier: a canonical-vector carrier is not yet "
        "implemented");
  }
  const mir::TypeId carrier = CarrierTypeId(unit, carrier_desc);
  switch (scalar->abi) {
    case support::DpiScalarAbi::kBitScalar:
    case support::DpiScalarAbi::kByte:
    case support::DpiScalarAbi::kShortInt:
    case support::DpiScalarAbi::kInt:
    case support::DpiScalarAbi::kLongInt: {
      const mir::ExprId machine_int = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kToInt64},
                      .arguments = {sv_id}},
              .type = unit.builtins.machine_int64});
      return block.exprs.Add(
          mir::Expr{
              .data = mir::IntCastExpr{.operand = machine_int},
              .type = carrier});
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
    UnitLowerer& unit_lowerer, WalkFrame frame, mir::ExprId call_id,
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
      // The carrier is the formal's declared C width; the packed factory takes
      // the widest machine integer, so widening here keeps one runtime entry
      // serving every carrier width instead of one per width.
      const mir::ExprId machine_int = block.exprs.Add(
          mir::Expr{
              .data = mir::IntCastExpr{.operand = call_id},
              .type = unit_lowerer.Unit().builtins.machine_int64});
      const mir::ExprId prototype = block.exprs.Add(
          BuildDefaultValueExpr(unit_lowerer, frame, result_type));
      return mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kFromInt,
                          .qualification =
                              mir::TypeQualifier{.type = result_type}},
                  .arguments = {machine_int, prototype}},
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
      const mir::ExprId prototype = block.exprs.Add(
          BuildDefaultValueExpr(unit_lowerer, frame, result_type));
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

// The all-input function import call: every actual crosses by value, so the
// call is a plain expression -- no statement sequencing, no boundary temps.
// Each actual is marshaled to its ABI carrier, the foreign symbol is called
// over the carriers, and a non-void result is marshaled back to the declared SV
// type. A task never reaches here; its await needs a coroutine, so it always
// sequences.
template <ExprLowerer Lowerer>
auto LowerForeignImportInputsOnly(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const mir::ExternalCallable& decl, mir::CallableTarget target,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
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
              : CarrierTypeId(unit, support::ScalarCarrier{decl.ret_abi});
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
      unit_lowerer, frame, call_id, support::ScalarCarrier{decl.ret_abi},
      result_type);
}

// Populates `closure`'s body with the DPI import boundary: each actual
// marshaled to its carrier -- a by-value scalar input crossing by value, a
// writeback direction or a canonical vector of any direction seeded into a
// boundary temp passed by pointer -- then the foreign call, then the copy-back
// of each writeback into its actual's cell. A valued call captures its carrier
// result in a temp, returned so the caller marshals it after the copy-backs; a
// void call (including a task, whose foreign symbol's disable-ack `int` is
// discarded) is a bare statement and returns no temp. Shared by the function
// and task import lowerings, which differ only in how they finish the closure.
template <ExprLowerer Lowerer>
auto PopulateForeignImportBoundary(
    Lowerer& lowerer, ClosureBuilder& closure, const hir::CallExpr& c,
    const mir::ExternalCallable& decl, mir::CallableTarget target)
    -> diag::Result<std::optional<mir::LocalId>> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  const mir::TypeId void_t = unit.builtins.void_type;

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
    const mir::TypeId carrier_type = CarrierTypeId(unit, carrier);
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
              : CarrierTypeId(unit, support::ScalarCarrier{decl.ret_abi});
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
      const mir::ExprId prototype = body.exprs.Add(
          BuildDefaultValueExpr(unit_lowerer, cframe, wb.sv_type));
      rhs_id = body.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = VectorReadBuiltin(*vector)},
                      .arguments = {data, prototype}},
              .type = wb.sv_type});
    } else {
      rhs_id = body.exprs.Add(MarshalCarrierToSv(
          unit_lowerer, cframe, temp_ref, wb.carrier, wb.sv_type));
    }
    const mir::ExprId services_id =
        body.exprs.Add(BuildServicesCallExpr(unit_lowerer, cframe));
    const mir::Expr assign = BuildObservableAssignExpr(
        unit, body, services_id, lhs_id, rhs_id, std::nullopt, wb.sv_type,
        void_t);
    body.AppendStmt(mir::ExprStmt{.expr = body.exprs.Add(assign)});
  }

  return ret_temp;
}

// The general function import call: at least one actual needs a boundary temp
// -- an output / inout of any carrier, or a canonical vector of any direction
// (a vector crosses by pointer even as an input). The boundary is a statement
// sequence yielding a value, so it lowers to an immediately-invoked closure,
// uniform for void / valued and statement / expression position. A by-value
// scalar input still crosses by value with no temp.
template <ExprLowerer Lowerer>
auto LowerForeignImportSequenced(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const mir::ExternalCallable& decl, mir::CallableTarget target,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();

  ClosureBuilder closure(unit, frame);
  auto ret_temp =
      PopulateForeignImportBoundary(lowerer, closure, c, decl, target);
  if (!ret_temp) return std::unexpected(std::move(ret_temp.error()));

  if (!ret_temp->has_value()) {
    return BuildClosureCallExpr(
        unit, *frame.current_block, closure.BuildVoid());
  }
  mir::Block& body = closure.Body();
  const WalkFrame& cframe = closure.Frame();
  const mir::TypeId call_type =
      CarrierTypeId(unit, support::ScalarCarrier{decl.ret_abi});
  const mir::ExprId ret_ref =
      body.exprs.Add(mir::MakeLocalRefExpr(**ret_temp, call_type));
  const mir::ExprId result_id = body.exprs.Add(MarshalCarrierToSv(
      unit_lowerer, cframe, ret_ref, support::ScalarCarrier{decl.ret_abi},
      result_type));
  return BuildClosureCallExpr(
      unit, *frame.current_block, closure.Build(result_id));
}

// The task import call (LRM 35.5.2): the same boundary as a function, but a
// task has no SV return, so the closure finishes as a coroutine -- the
// awaitable the caller drives, the same call protocol as a native task enable
// (LRM 35.8), uniform whether or not the foreign side consumes time. The
// boundary always sequences through the closure, even all-input, because the
// await needs a coroutine to drive. The coroutine closure is returned directly,
// not called: it renders self-invoking to a `Coroutine`, and the statement
// lowering awaits it.
template <ExprLowerer Lowerer>
auto LowerForeignImportTask(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const mir::ExternalCallable& decl, mir::CallableTarget target)
    -> diag::Result<mir::Expr> {
  auto& unit = lowerer.Owner().Unit();
  ClosureBuilder closure(unit, frame);
  auto ret_temp =
      PopulateForeignImportBoundary(lowerer, closure, c, decl, target);
  if (!ret_temp) return std::unexpected(std::move(ret_temp.error()));
  return closure.BuildCoroutine();
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
// callable's own declaration, resolved once at its declaration lowering. A task
// call is a coroutine the caller awaits, so it always sequences through a
// closure; a function call whose every actual is a by-value scalar input is a
// plain expression, and one with any actual that needs a boundary temp (an
// output / inout, or a canonical vector of any direction) sequences through a
// closure.
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
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::EnclosingHops hops{.value = ref.hops.value};
  const mir::CallableTarget target{
      .owner = EnclosingClassIdAtHops(frame, unit, hops),
      .slot = mir::CallableId{ref.id.value}};
  const mir::CallableDecl& callable =
      frame.EnclosingClassAtHops(hops).callables.Get(target.slot);
  // A DPI import always resolves to the external implementation form; its ABI
  // projection drives the marshaling below.
  const auto& decl = std::get<mir::ExternalCallable>(callable.impl);

  if (decl.is_task) {
    return LowerForeignImportTask(lowerer, frame, c, decl, target);
  }
  const bool needs_temp = std::ranges::any_of(decl.params, NeedsBoundaryTemp);
  if (needs_temp) {
    return LowerForeignImportSequenced(
        lowerer, frame, c, decl, target, result_type);
  }
  return LowerForeignImportInputsOnly(
      lowerer, frame, c, decl, target, result_type);
}

// A call to a package subroutine (LRM 26.3): a receiver-less callable of
// another compilation unit, reached by name. No receiver is prepended, and the
// arguments are input-only (the AST-to-HIR producer rejects output / inout /
// ref formals), so each actual lowers as a plain value.
template <ExprLowerer Lowerer>
auto LowerExternalUnitSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ExternalUnitSubroutineRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> args;
  args.reserve(c.arguments.size());
  for (const auto& argument : c.arguments) {
    if (!argument.has_value()) {
      throw InternalError("package call argument unexpectedly elided");
    }
    auto arg_or = lowerer.LowerExpr(hir_exprs.Get(*argument), frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    args.push_back(block.exprs.Add(*std::move(arg_or)));
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target =
                          lowerer.Owner().MakeExternalCallableTarget(ref)},
              .arguments = std::move(args)},
      .type = result_type};
}

}  // namespace

// A call to a method the runtime library provides for an imported class (LRM
// 9.7 `process`) lowers to a direct call on the library symbol. An instance
// method passes its receiver handle as the leading argument; whether the
// engine services handle follows is a per-method fact. The receiver is passed
// as the managed handle itself, not a borrowed object pointer -- the runtime
// reads the process identity from the handle. A suspending method (`await`) is
// wrapped in an await by the statement lowering, the same as a task enable.
template <ExprLowerer Lowerer>
auto LowerImportedMethodCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ImportedMethodRef& m, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> args;
  args.reserve(c.arguments.size() + 1);

  if (m.receiver.has_value()) {
    auto receiver_or =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*m.receiver), frame);
    if (!receiver_or) return std::unexpected(std::move(receiver_or.error()));
    args.push_back(block.exprs.Add(*std::move(receiver_or)));
  }
  // A static method (no receiver) threads the services handle as its leading
  // argument; an instance method threads it after the receiver when the method
  // needs the engine -- to schedule, or to identify the calling process.
  if (support::ImportedRuntimeMethodTakesServices(m.method)) {
    args.push_back(
        block.exprs.Add(BuildServicesCallExpr(lowerer.Owner(), frame)));
  }

  for (const auto& arg : c.arguments) {
    if (!arg.has_value()) {
      throw InternalError("LowerImportedMethodCall: argument elided");
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
                      .target =
                          mir::ImportedRuntimeCallTarget{.method = m.method},
                      .qualification = std::nullopt},
              .arguments = std::move(args)},
      .type = result_type};
}

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
          [&](const hir::StaticMethodCallRef& s) -> diag::Result<mir::Expr> {
            return LowerStaticMethodCall(lowerer, frame, c, s, result_type);
          },
          [&](const hir::BuiltinMethodRef& b) -> diag::Result<mir::Expr> {
            return LowerBuiltinMethodCall(lowerer, frame, c, b, result_type);
          },
          [&](const hir::ForeignImportRef& imp) -> diag::Result<mir::Expr> {
            return LowerForeignImportCall(lowerer, frame, c, imp, result_type);
          },
          [&](const hir::ImportedMethodRef& im) -> diag::Result<mir::Expr> {
            return LowerImportedMethodCall(lowerer, frame, c, im, result_type);
          },
          [&](const hir::ExternalUnitSubroutineRef& ext)
              -> diag::Result<mir::Expr> {
            return LowerExternalUnitSubroutineCall(
                lowerer, frame, c, ext, result_type);
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

// The MIR type of one foreign wrapper parameter, the ABI carrier realized as a
// concrete type so a backend spells it through ordinary type mapping. A scalar
// `input` is its by-value carrier; a scalar `output` / `inout` is a borrowed
// pointer to it. A packed vector is a borrowed pointer to its canonical chunk,
// read-only for an `input` the wrapper only reads (rendering `const
// svBitVecVal*` through the pointer's mutability) and mutable for a writeback
// direction.
auto ExportParamType(
    mir::CompilationUnit& unit, const support::DpiCarrier& carrier,
    support::DpiDirection direction) -> mir::TypeId {
  if (const auto* vec = std::get_if<support::VectorCarrier>(&carrier)) {
    const mir::TypeId chunk = unit.types.Intern(
        mir::TypeData{mir::RuntimeLibraryType{
            .kind = vec->four_state ? mir::RuntimeLibraryKind::kDpiLogicChunk
                                    : mir::RuntimeLibraryKind::kDpiBitChunk}});
    const mir::Mutability mutability =
        direction == support::DpiDirection::kInput ? mir::Mutability::kReadOnly
                                                   : mir::Mutability::kMutable;
    return unit.types.PointerTo(
        chunk, mir::PointerOwnership::kBorrowed, mutability);
  }
  const mir::TypeId carrier_type = CarrierTypeId(unit, carrier);
  if (support::DpiDirectionWritesBack(direction)) {
    return unit.types.PointerTo(carrier_type, mir::PointerOwnership::kBorrowed);
  }
  return carrier_type;
}

// The builtin that writes an SV value out into a foreign-owned canonical
// buffer, the write direction that pairs with the vector read builtin.
[[nodiscard]] auto VectorWriteBuiltin(const support::VectorCarrier& v)
    -> support::BuiltinFn {
  return v.four_state ? support::BuiltinFn::kWriteCanonicalLogicVec
                      : support::BuiltinFn::kWriteCanonicalBitVec;
}

auto SynthesizeForeignExportWrapper(
    UnitLowerer& module, const WalkFrame& ctor_frame, mir::ClassId class_id,
    mir::CallableId method_id, const hir::ForeignExportDecl& export_decl,
    std::string instance_name) -> mir::ForeignExportWrapper {
  mir::CompilationUnit& unit = module.Unit();
  const mir::Class& mir_class = *ctor_frame.current_class;
  const mir::TypeId self_ptr = mir_class.self_pointer_type;
  const mir::TypeId void_type = unit.builtins.void_type;

  mir::CallableCode code;
  CallableBindings bindings(unit, code);
  mir::Block& body = code.body;

  // The receiver the exported method runs against is recovered by the backend's
  // wrapper shell from the running design, not passed by the foreign caller
  // (LRM 35.5.3), so it is a body local rather than a parameter.
  const mir::LocalId self_local = bindings.Declare(
      BindingOriginId::Receiver(),
      mir::LocalDecl{.name = "self", .type = self_ptr});

  const WalkFrame body_frame =
      ctor_frame.WithBlock(&body).WithBindings(&bindings);

  // One foreign parameter per SV formal, in declaration order (the C ABI
  // order). The parameter's MIR type is the ABI carrier realized concretely, so
  // nothing downstream re-derives the C signature from the direction / carrier.
  std::vector<mir::LocalId> params;
  params.reserve(export_decl.params.size());
  for (std::size_t i = 0; i < export_decl.params.size(); ++i) {
    const hir::DpiParamAbi& p = export_decl.params[i];
    params.push_back(bindings.DeclareAnonymous(
        mir::LocalDecl{
            .name = "arg" + std::to_string(i),
            .type = ExportParamType(unit, p.carrier, p.direction)}));
  }

  const auto param_ref = [&](std::size_t i) -> mir::ExprId {
    return body.exprs.Add(
        mir::MakeLocalRefExpr(params[i], code.locals.Get(params[i]).type));
  };

  // Marshal each `input` / `inout` argument to an explicit SV-typed temporary
  // before the call, so the read of an `inout`'s incoming value is sequenced
  // ahead of the copy-back that later overwrites it, and multiple arguments do
  // not alias inside one nested call expression. An `output` formal is not a
  // method parameter -- it rides the completion payload (LRM 13.5). A vector
  // reads its SV value from the incoming canonical buffer; a scalar `input`
  // crosses by value; a scalar `inout` reads through its pointer.
  std::vector<mir::ExprId> call_args;
  call_args.push_back(
      body.exprs.Add(mir::MakeLocalRefExpr(self_local, self_ptr)));
  for (std::size_t i = 0; i < export_decl.params.size(); ++i) {
    const hir::DpiParamAbi& p = export_decl.params[i];
    if (p.direction == support::DpiDirection::kOutput) {
      continue;
    }
    const mir::TypeId sv_type = module.TranslateType(p.sv_type);
    mir::ExprId sv_init{};
    if (const auto* vec = std::get_if<support::VectorCarrier>(&p.carrier)) {
      const mir::ExprId prototype =
          body.exprs.Add(BuildDefaultValueExpr(module, body_frame, sv_type));
      sv_init = body.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee = mir::Direct{.target = VectorReadBuiltin(*vec)},
                      .arguments = {param_ref(i), prototype}},
              .type = sv_type});
    } else {
      const mir::ExprId carrier =
          p.direction == support::DpiDirection::kInout
              ? body.exprs.Add(
                    mir::Expr{
                        .data = mir::DerefExpr{.pointer = param_ref(i)},
                        .type = CarrierTypeId(unit, p.carrier)})
              : param_ref(i);
      sv_init = body.exprs.Add(
          MarshalCarrierToSv(module, body_frame, carrier, p.carrier, sv_type));
    }
    const mir::LocalId sv_in = bindings.DeclareAnonymous(
        mir::LocalDecl{.name = "in" + std::to_string(i), .type = sv_type});
    body.AppendStmt(mir::LocalDeclStmt{.target = sv_in, .init = sv_init});
    call_args.push_back(body.exprs.Add(mir::MakeLocalRefExpr(sv_in, sv_type)));
  }

  const mir::TypeId payload_type =
      std::get<mir::InternalCallable>(mir_class.callables.Get(method_id).impl)
          .code.result_type;
  const mir::ExprId method_call = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target =
                              mir::CallableTarget{
                                  .owner = class_id, .slot = method_id}},
                  .arguments = std::move(call_args)},
          .type = payload_type});

  // The completion payload's components, in the callee's payload order: the
  // function return (when non-void) first, then each `output` / `inout` formal
  // in declaration order. Built in that exact order so each component's index
  // lines up with the payload the callee returns. `param_index` is unused for
  // the return component.
  struct Component {
    mir::TypeId sv_type;
    bool is_return;
    std::size_t param_index;
  };
  std::vector<Component> components;
  const bool has_return = export_decl.ret_abi != support::DpiScalarAbi::kVoid;
  if (has_return) {
    components.push_back(
        Component{
            .sv_type = module.TranslateType(export_decl.ret_sv_type),
            .is_return = true,
            .param_index = 0});
  }
  for (std::size_t i = 0; i < export_decl.params.size(); ++i) {
    if (support::DpiDirectionWritesBack(export_decl.params[i].direction)) {
      components.push_back(
          Component{
              .sv_type = module.TranslateType(export_decl.params[i].sv_type),
              .is_return = false,
              .param_index = i});
    }
  }

  // Bind the completion value to a local every component projects out of; the
  // projection encoding (bare value vs tuple) lives in one shared place. An
  // empty payload has nothing to bind, so the call is a bare statement.
  std::optional<mir::LocalId> completion;
  if (!components.empty()) {
    completion = bindings.DeclareAnonymous(
        mir::LocalDecl{.name = "_lyra_completion", .type = payload_type});
    body.AppendStmt(
        mir::LocalDeclStmt{.target = *completion, .init = method_call});
  } else {
    body.AppendStmt(mir::ExprStmt{.expr = method_call});
  }
  const auto component_value = [&](std::size_t k) -> mir::ExprId {
    return ProjectCompletionComponent(
        body, *completion, payload_type, components.size(), k,
        components[k].sv_type);
  };

  // Copy each `output` / `inout` component back through its foreign pointer: a
  // scalar stores its marshaled carrier through the pointer; a vector reshapes
  // the SV value into the foreign-owned canonical buffer.
  for (std::size_t k = 0; k < components.size(); ++k) {
    const Component& c = components[k];
    if (c.is_return) {
      continue;
    }
    const hir::DpiParamAbi& p = export_decl.params[c.param_index];
    const mir::ExprId value = component_value(k);
    if (const auto* vec = std::get_if<support::VectorCarrier>(&p.carrier)) {
      body.AppendStmt(
          mir::ExprStmt{
              .expr = body.exprs.Add(
                  mir::Expr{
                      .data =
                          mir::CallExpr{
                              .callee =
                                  mir::Direct{
                                      .target = VectorWriteBuiltin(*vec)},
                              .arguments = {param_ref(c.param_index), value}},
                      .type = void_type})});
      continue;
    }
    const mir::TypeId carrier_type = CarrierTypeId(unit, p.carrier);
    const mir::ExprId carrier =
        MarshalSvToCarrier(unit, body, value, p.carrier);
    const mir::ExprId place = body.exprs.Add(
        mir::Expr{
            .data = mir::DerefExpr{.pointer = param_ref(c.param_index)},
            .type = carrier_type});
    body.AppendStmt(
        mir::ExprStmt{
            .expr = body.exprs.Add(
                mir::MakeAssignExpr(place, carrier, void_type))});
  }

  if (has_return) {
    const mir::ExprId ret_carrier = MarshalSvToCarrier(
        unit, body, component_value(0),
        support::ScalarCarrier{export_decl.ret_abi});
    body.AppendStmt(mir::ReturnStmt{.value = ret_carrier});
  } else {
    body.AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  }

  code.params = std::move(params);
  code.result_type =
      has_return
          ? CarrierTypeId(unit, support::ScalarCarrier{export_decl.ret_abi})
          : void_type;

  return mir::ForeignExportWrapper{
      .foreign_name = export_decl.foreign_name,
      .instance_name = std::move(instance_name),
      .code = std::move(code),
      .self_local = self_local};
}

}  // namespace lyra::lowering::hir_to_mir
