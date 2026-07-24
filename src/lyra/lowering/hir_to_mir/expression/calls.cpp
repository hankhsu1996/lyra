#include "lyra/lowering/hir_to_mir/expression/calls.hpp"

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
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
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
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/lowering/hir_to_mir/writeback_call.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/enclosing_hops.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/imported_runtime_class.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// If the lowered LHS chain `lhs_id` is rooted in an observable cell, wrap
// the root with `Deref(Mutate(cell, runtime))` so the consumer operates on
// a `ScopedMutation` snapshot whose destructor commits via `Var::Set`.
// Returns `lhs_id` unchanged when no cell is at the root. Used by both the
// mutating-method receiver path and the ref-formal actual path; the latter
// gates its call on `root_id != lhs_id` because a bare observable cell
// should bind `Ref<T>` directly to the underlying `Var<T>&`, not via a
// snapshot.
template <ExprLowerer Lowerer>
auto MaybeWrapObservableLhsWithMutate(
    Lowerer& lowerer, mir::Block& block, mir::ExprId lhs_id) -> mir::ExprId {
  const mir::ExprId root_id =
      FindLhsRootId(lowerer.Owner().Unit(), block, lhs_id);
  const bool root_is_cell = mir::IsObservableCellType(
      lowerer.Owner().Unit().types.Get(block.exprs.Get(root_id).type));
  if (!root_is_cell) return lhs_id;
  const mir::ExprId runtime_id =
      block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  return RewriteLhsRootWithMutate(
      lowerer.Owner().Unit(), block, lhs_id, runtime_id);
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
  const mir::ExprId runtime_id =
      body.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner()));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      unit_lowerer.Unit(), body, runtime_id, idx_lhs_id, probe_writeback_id,
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
// A structural context admits only a pure value query -- one that reads state
// and sequences nothing. Every other family is an effect that needs a process
// body, so it has no structural lowering.
auto RejectStructuralEffect(diag::SourceSpan span) -> diag::Result<mir::Expr> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
      "this system subroutine is not yet supported in a continuous assignment; "
      "only a value query is legal there");
}

// Fans out a system-subroutine call to the per-family handler. The visit is
// exhaustive over `support::SystemSubroutineSemantic`, so a new family forces
// an arm here and a decision about both contexts. A family that is a pure value
// query (LRM 20.3 time, LRM 21.6 plusargs, LRM 21.3.3 `$sformatf`) needs no
// process body and its one handler serves both pass classes; a family that is
// an effect is procedural-only.
template <ExprLowerer Lowerer>
auto LowerSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const hir::SystemSubroutineRef& ref, diag::SourceSpan span)
    -> diag::Result<mir::Expr> {
  constexpr bool kProcedural = std::same_as<Lowerer, ProcessLowerer>;
  const auto& desc = support::LookupSystemSubroutine(ref.id);
  return std::visit(
      Overloaded{
          [&](const support::PrintSystemSubroutineInfo& print)
              -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerPrintSystemSubroutineCall(
                  lowerer, frame, call, print);
            } else {
              return RejectStructuralEffect(span);
            }
          },
          [&](const support::TerminationSystemSubroutineInfo& term)
              -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerFinishSystemSubroutineCall(
                  lowerer, frame, call, desc.name, term, span);
            } else {
              return RejectStructuralEffect(span);
            }
          },
          [&](const support::DiagnosticSystemSubroutineInfo& diag_info)
              -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerDiagnosticSystemSubroutineCall(
                  lowerer, frame, call, diag_info, span);
            } else {
              return RejectStructuralEffect(span);
            }
          },
          [&](const support::FileIOSystemSubroutineInfo& file_io)
              -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerFileIOSystemSubroutineCall(
                  lowerer, frame, call, desc.name, file_io, span);
            } else {
              return RejectStructuralEffect(span);
            }
          },
          [&](const support::ScanSystemSubroutineInfo& scan_info)
              -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerScanSystemSubroutineCall(
                  lowerer, frame, call, scan_info, span);
            } else {
              return RejectStructuralEffect(span);
            }
          },
          [&](const support::SFormatSystemSubroutineInfo& sformat)
              -> diag::Result<mir::Expr> {
            return LowerSFormatSystemSubroutineCall(
                lowerer, frame, call, sformat);
          },
          [&](const support::TimeSystemSubroutineInfo& time_info)
              -> diag::Result<mir::Expr> {
            return LowerTimeSystemSubroutineCall(lowerer, frame, time_info);
          },
          [&](const support::TimeFormatSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerTimeFormatSystemSubroutineCall(
                  lowerer, frame, call, span);
            } else {
              return RejectStructuralEffect(span);
            }
          },
          [&](const support::PrintTimescaleSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerPrintTimescaleSystemSubroutineCall(lowerer, frame);
            } else {
              return RejectStructuralEffect(span);
            }
          },
          [&](const support::PlusargsSystemSubroutineInfo& plusargs)
              -> diag::Result<mir::Expr> {
            return LowerPlusargsSystemSubroutineCall(
                lowerer, frame, call, plusargs);
          },
          [&](const support::ReadMemSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            // A void task (LRM 21.4) has no value, so the frontend rejects it
            // in any value position; a statement-position call is intercepted
            // by the statement-form dispatch and never falls through here.
            // Reaching this arm is therefore a frontend / lowering invariant
            // violation, not an unsupported source form.
            throw InternalError(
                "$readmemh / $readmemb reached expression lowering; a void "
                "task only lowers through the statement-form dispatch");
          },
      },
      desc.semantic);
}

// LRM 13.5 intra-unit subroutine call in value position: the callee takes only
// `input` values and `ref` / const-ref aliases here. A `ref` / const-ref formal
// aliases the actual's cell (LRM 13.5.2); the body's `Ref<T>` binds it. An
// output / inout call is intercepted before this dispatch and lowered through
// the completion-payload writeback path, so it never reaches here. The callee
// body receives its instance handle as `arguments[0]`; SV actuals follow.
template <ExprLowerer Lowerer>
auto LowerStructuralSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::StructuralSubroutineRef& usr, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const hir::SubroutineDecl& decl =
      lowerer.LookupHirSubroutine(usr.hops, usr.subroutine);
  if constexpr (!std::same_as<Lowerer, ProcessLowerer>) {
    // An output / inout formal only reaches value lowering in a structural
    // context; the procedural path intercepts it as a completion-payload
    // closure. The frontend rejects an output / inout call outside procedural
    // code (LRM 13.4), so a structural one is a compiler-invariant violation
    // rather than a user-diagnosable form.
    for (const auto& param : decl.params) {
      if (hir::RequiresWriteback(param.direction)) {
        throw InternalError(
            "structural subroutine call carries an output / inout argument; "
            "the "
            "frontend rejects these outside procedural code (LRM 13.4)");
      }
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
        actual_id = MaybeWrapObservableLhsWithMutate(lowerer, block, actual_id);
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
          MaybeWrapObservableLhsWithMutate(lowerer, block, receiver_id);
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
  // RuntimeEffects. The runtime handle is a real trailing argument, threaded
  // the same way every runtime effect threads it -- not a backend-fabricated
  // one. (`-> e` is the only producer of the trigger kind and lowers through
  // the event-trigger stmt path; `await` takes no runtime handle.)
  if (b.method == support::BuiltinFn::kTriggered) {
    args.push_back(
        block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner())));
  }

  return mir::Expr{
      .data = mir::CallExpr{.callee = mir_callee, .arguments = std::move(args)},
      .type = result_type};
}

// Reads the virtual slot a method participates in off its stated
// `virtual_dispatch`. Every participating method already names its slot -- an
// introducer names its own (owner, id), an intra-unit override was populated
// with the canonical intra-unit id at class lowering, and a cross-unit
// override names the introducing (unit, class, method) triple -- so this is a
// one-arm dispatch, never a chain walk. A method whose slot was introduced in
// another compilation unit resolves to an `ExternalVirtualSlot`, reached
// through the declaring unit's header.
auto CanonicalVirtualSlot(
    mir::ClassId self_owner, mir::CallableId self_slot,
    const mir::VirtualDispatchRole& role) -> mir::VirtualSlot {
  return std::visit(
      Overloaded{
          [&](const mir::IntroducesVirtualSlot&) -> mir::VirtualSlot {
            return mir::LocalVirtualSlot{
                .owner_class = self_owner, .slot = self_slot};
          },
          [](const mir::OverridesIntraUnitSlot& s) -> mir::VirtualSlot {
            return mir::LocalVirtualSlot{
                .owner_class = s.slot_owner, .slot = s.slot_id};
          },
          [](const mir::OverridesExternalSlot& e) -> mir::VirtualSlot {
            return mir::ExternalVirtualSlot{
                .unit_name = e.unit_name,
                .class_name = e.class_name,
                .method_name = e.method_name};
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
  // callee's dispatch role, so the HIR target's `is_virtual` fact -- read
  // from the callee's frontend view when the target was minted -- decides
  // between virtual dispatch (LRM 8.20) and static dispatch. A super
  // qualifier always demands the base's implementation regardless of the
  // callee's virtuality, so it lowers as a direct owner-qualified call
  // even when the target is virtual.
  if (const auto* ext_target =
          std::get_if<hir::ExternalClassMethodTarget>(&m.target)) {
    const bool through_super_ext =
        std::holds_alternative<hir::SuperReceiver>(m.receiver);
    if (!through_super_ext && ext_target->is_virtual) {
      return mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Virtual{
                          .receiver = receiver_ptr,
                          .slot = lowerer.Owner().MakeExternalVirtualSlot(
                              *ext_target)},
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
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Virtual{
                        .receiver = receiver_ptr,
                        .slot = CanonicalVirtualSlot(
                            owner_class, method_slot,
                            *method_sig.virtual_dispatch)},
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

// A call to a package or `$unit` subroutine (LRM 26.3 / 3.12.1) in value
// position: a receiver-less callable of another compilation unit, reached by
// name. It takes no receiver but does take the engine services as its leading
// argument -- the receiver-less peer of `self`, so the callee body can wake a
// variable's subscribers or suspend. A ref / const-ref formal aliases the
// actual's cell exactly as an intra-unit call does. An output / inout call is
// intercepted before this dispatch and lowered through the completion-payload
// writeback path, so it never reaches here.
template <ExprLowerer Lowerer>
auto LowerExternalUnitSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::ExternalUnitSubroutineRef& ref, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  if constexpr (!std::same_as<Lowerer, ProcessLowerer>) {
    // As with an intra-unit call, an output / inout formal only reaches value
    // lowering in a structural context; the procedural path intercepts it. The
    // frontend rejects such a call outside procedural code (LRM 13.4).
    for (const auto& param : ref.params) {
      if (hir::RequiresWriteback(param.direction)) {
        throw InternalError(
            "cross-unit subroutine call carries an output / inout argument; "
            "the "
            "frontend rejects these outside procedural code (LRM 13.4)");
      }
    }
  }
  std::vector<mir::ExprId> args;
  args.reserve(c.arguments.size() + 1);
  // The caller supplies services from its own ambient handle: an instance
  // body's `self`, or an enclosing package callable's own services parameter.
  args.push_back(block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner())));
  for (std::size_t i = 0; i < c.arguments.size(); ++i) {
    if (!c.arguments[i].has_value()) {
      throw InternalError("package call argument unexpectedly elided");
    }
    const bool is_ref_formal =
        i < ref.params.size() &&
        (ref.params[i].direction == hir::ParamDirection::kRef ||
         ref.params[i].direction == hir::ParamDirection::kConstRef);
    if (is_ref_formal) {
      auto arg_or = lowerer.LowerLhsExpr(hir_exprs.Get(*c.arguments[i]), frame);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      mir::ExprId actual_id = block.exprs.Add(*std::move(arg_or));
      const mir::ExprId root_id =
          FindLhsRootId(lowerer.Owner().Unit(), block, actual_id);
      if (root_id != actual_id) {
        actual_id = MaybeWrapObservableLhsWithMutate(lowerer, block, actual_id);
      }
      args.push_back(BuildReferenceArg(
          lowerer.Owner().Unit(), block, actual_id,
          block.exprs.Get(actual_id).type));
      continue;
    }
    auto arg_or = lowerer.LowerExpr(hir_exprs.Get(*c.arguments[i]), frame);
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
// runtime handle follows is a per-method fact. The receiver is passed
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
  // A static method (no receiver) threads the runtime handle as its leading
  // argument; an instance method threads it after the receiver when the method
  // needs the engine -- to schedule, or to identify the calling process.
  if (support::ImportedRuntimeMethodTakesServices(m.method)) {
    args.push_back(
        block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner())));
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
  // A call whose callee has an output / inout formal carries its writebacks
  // through a completion payload; in value position it lowers to an
  // immediately-invoked closure that writes them back and yields the function
  // result (LRM 13.4). Procedural-only -- the frontend rejects an output /
  // inout / ref call outside procedural code (LRM 13.4), so no such call
  // reaches a structural HIR -- so the interception is guarded here, and the
  // per-callee dispatch below sees only input / ref arguments.
  if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
    if (auto plan = PlanWritebackCall(lowerer, c, result_type)) {
      return LowerWritebackCallExpr(lowerer, frame, c, *plan);
    }
  }
  return std::visit(
      Overloaded{
          [&](const hir::SystemSubroutineRef& sys) -> diag::Result<mir::Expr> {
            return LowerSystemSubroutineCall(lowerer, frame, c, sys, span);
          },
          [&](const hir::StructuralSubroutineRef& usr)
              -> diag::Result<mir::Expr> {
            return LowerStructuralSubroutineCall(
                lowerer, frame, c, usr, result_type);
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

}  // namespace lyra::lowering::hir_to_mir
