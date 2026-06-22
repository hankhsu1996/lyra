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
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// True iff the LRM 7.12 method takes a `with`-clause closure as its second
// argument. The other LRM 7.5 / 7.10 array entries (`size`, `delete`,
// `reverse`) take no closure.
auto ArrayMethodTakesClosure(support::BuiltinFn fn) -> bool {
  switch (fn) {
    case support::BuiltinFn::kSort:
    case support::BuiltinFn::kRsort:
    case support::BuiltinFn::kSum:
    case support::BuiltinFn::kProduct:
    case support::BuiltinFn::kAnd:
    case support::BuiltinFn::kOr:
    case support::BuiltinFn::kXor:
    case support::BuiltinFn::kFind:
    case support::BuiltinFn::kFindIndex:
    case support::BuiltinFn::kFindFirst:
    case support::BuiltinFn::kFindFirstIndex:
    case support::BuiltinFn::kFindLast:
    case support::BuiltinFn::kFindLastIndex:
    case support::BuiltinFn::kMin:
    case support::BuiltinFn::kMax:
    case support::BuiltinFn::kUnique:
    case support::BuiltinFn::kUniqueIndex:
    case support::BuiltinFn::kMap:
      return true;
    default:
      return false;
  }
}

// True iff `fn` is an associative-array traversal entry. LRM 7.9.4 -- 7.9.7
// lower to an immediately-invoked closure (mutates the index argument and
// runs the write-back inline).
auto IsAssociativeTraversalFn(support::BuiltinFn fn) -> bool {
  return fn == support::BuiltinFn::kAssocFirst ||
         fn == support::BuiltinFn::kAssocLast ||
         fn == support::BuiltinFn::kAssocNext ||
         fn == support::BuiltinFn::kAssocPrev;
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
  const auto recv_hir = c.arguments[0]->value;
  const auto idx_hir = c.arguments[1]->value;
  const mir::TypeId key_type =
      module.TranslateType(hir_proc.exprs.at(idx_hir).type);
  const mir::TypeId void_t = module.Unit().builtins.void_type;

  ClosureBuilder closure(process.Module().Unit(), frame);
  mir::Block& body = closure.Body();
  const WalkFrame& closure_frame = closure.Frame();

  // probe = idx -- snapshot the current index value into a plain local.
  auto idx_read_or =
      process.LowerExpr(hir_proc.exprs.at(idx_hir), closure_frame);
  if (!idx_read_or) return std::unexpected(std::move(idx_read_or.error()));
  const mir::LocalRef probe_ref = body.AppendLocal(
      mir::LocalDecl{.name = "_lyra_trav_probe", .type = key_type},
      body.AddExpr(*std::move(idx_read_or)));

  // found = (map).<kind>(probe) -- pure query: mutates probe, yields 1/0.
  auto map_read_or =
      process.LowerExpr(hir_proc.exprs.at(recv_hir), closure_frame);
  if (!map_read_or) return std::unexpected(std::move(map_read_or.error()));
  const mir::ExprId map_read_id = body.AddExpr(*std::move(map_read_or));
  const mir::ExprId probe_read_id =
      body.AddExpr(mir::Expr{.data = probe_ref, .type = key_type});
  const mir::ExprId query_id = body.AddExpr(
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
      process.LowerLhsExpr(hir_proc.exprs.at(idx_hir), closure_frame);
  if (!idx_lhs_or) return std::unexpected(std::move(idx_lhs_or.error()));
  const mir::ExprId idx_lhs_id = body.AddExpr(*std::move(idx_lhs_or));
  const mir::ExprId probe_writeback_id =
      body.AddExpr(mir::Expr{.data = probe_ref, .type = key_type});
  const mir::ExprId services_id =
      body.AddExpr(BuildServicesCallExpr(process, closure_frame));
  const mir::Expr assign_expr = BuildObservableAssignExpr(
      module.Unit(), body, services_id, idx_lhs_id, probe_writeback_id,
      std::nullopt, key_type, void_t);
  body.AppendStmt(mir::ExprStmt{.expr = body.AddExpr(assign_expr)});

  const mir::ExprId found_id =
      body.AddExpr(mir::Expr{.data = found_ref, .type = result_type});
  return BuildClosureCallExpr(*frame.current_block, closure.Build(found_id));
}

// Translates a HIR builtin-method ref to its MIR `Callee`. The identifier
// is the flat `support::BuiltinFn`; the only decision here is whether the
// id names a type-namespace-qualified static call (e.g. `MyEnum::first()`)
// or an instance call. `IteratorMethodKind::kIndex` is rewritten upstream
// to a `LocalRef` and never reaches this translation.
auto BuildBuiltinMirCallee(
    const ModuleLowerer& module, const hir::BuiltinMethodRef& b,
    hir::TypeId hir_receiver_type) -> mir::Callee {
  return std::visit(
      Overloaded{
          [&](support::BuiltinFn fn) -> mir::Callee {
            if (support::IsStaticBuiltinFn(fn)) {
              return mir::BuiltinStaticCallee{
                  .id = fn,
                  .type_qual = module.TranslateType(hir_receiver_type)};
            }
            return mir::BuiltinFnCallee{.id = fn};
          },
          [](hir::IteratorMethodKind) -> mir::Callee {
            // LRM 7.12.4 `item.index` is rewritten to a `LocalRef` on the
            // enclosing array-method closure's index binding before reaching
            // the generic dispatch (see `LowerHirCallExprProc`), so the
            // builtin-method translation is never invoked with it.
            throw InternalError(
                "BuildBuiltinMirCallee: IteratorMethodKind should have "
                "been rewritten to a LocalRef");
          },
      },
      b.method);
}

auto LowerUserCallee(
    const ClassLowerer& lowerer, const hir::StructuralSubroutineRef& u)
    -> mir::Callee {
  return lowerer.TranslateStructuralSubroutine(u.hops, u.subroutine);
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
  return std::nullopt;
}

// The element type of `map`'s result container (LRM 7.12.5), used to build its
// shield.
auto ArrayContainerElementType(const mir::Type& ty) -> mir::TypeId {
  return std::visit(
      [](const auto& t) -> mir::TypeId {
        using TyT = std::decay_t<decltype(t)>;
        if constexpr (
            std::same_as<TyT, mir::UnpackedArrayType> ||
            std::same_as<TyT, mir::DynamicArrayType> ||
            std::same_as<TyT, mir::QueueType>) {
          return t.element_type;
        } else {
          throw InternalError(
              "ArrayContainerElementType: map result is not an unpacked-array "
              "container type");
        }
      },
      ty.data);
}

// LRM 7.12.1 / 7.12.2 / 7.12.3 with-clause closure synthesis. The iterator and
// index are the closure's two parameters (LRM 7.12.4): the iterator HIR id
// remaps to the `item` parameter and the index parameter resolves the `kIndex`
// reference. The body is a normal expression lowered through
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
  const mir::TypeId index_type = module.Unit().builtins.int32;
  const std::string iterator_name =
      with_clause != nullptr
          ? hir_process.procedural_vars.at(with_clause->iterator.value).name
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
        hir_process.exprs.at(with_clause->expr.value),
        closure.Frame().WithIndexBinding(index_binding));
    if (!body_expr_or) return std::unexpected(std::move(body_expr_or.error()));
    body_return_value = body.AddExpr(*std::move(body_expr_or));
  } else {
    body_return_value = body.AddExpr(
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
                process, frame, call, desc.id, print, span);
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
                process, frame, call, desc.id, sformat, span);
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

}  // namespace

auto LowerHirCallExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::CallExpr& c,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& module = process.Module();
  const auto& lowerer = process.Owner();
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;
  return std::visit(
      Overloaded{
          [&](const hir::SystemSubroutineRef& sys) -> diag::Result<mir::Expr> {
            return LowerSystemSubroutineCall(process, frame, c, sys, span);
          },
          [&](const hir::StructuralSubroutineRef& usr)
              -> diag::Result<mir::Expr> {
            // LRM 13.5: a call with output / inout actuals is desugared to
            // copy-in-copy-out at statement position. Reaching it here means
            // the call is a nested expression operand, where the copy-out
            // statement has nowhere to be sequenced. ref / const ref alias
            // the actual and copy nothing back (LRM 13.5.2), so they are
            // fine as operands and fall through to the value-only lowering.
            const hir::StructuralSubroutineDecl& decl =
                lowerer.LookupHirSubroutine(usr.hops, usr.subroutine);
            for (const auto& param : decl.params) {
              if (hir::RequiresWriteback(param.direction)) {
                return diag::Unsupported(
                    span, diag::DiagCode::kUnsupportedSubroutineArgument,
                    "a call with output / inout arguments is only supported "
                    "in statement position, not as a nested expression",
                    diag::UnsupportedCategory::kFeature);
              }
            }
            // The callee body takes its instance handle as `arguments[0]`
            // (mir.md invariant 11); the SV actuals follow.
            std::vector<mir::ExprId> args;
            args.reserve(c.arguments.size() + 1);
            args.push_back(block.AddExpr(BuildSelfRefExpr(
                frame, frame.current_class->self_pointer_type)));
            for (std::size_t i = 0; i < c.arguments.size(); ++i) {
              if (!c.arguments[i].has_value()) {
                throw InternalError(
                    "user-function call argument unexpectedly elided");
              }
              // A ref / const-ref formal aliases the actual's cell. The
              // body's `Ref<T>` either binds the wrapped cell directly
              // (bare observable actual) or holds a `ScopedMutation` snapshot
              // (selector / range on an observable actual) -- both routed by
              // the LHS-form lowering plus the observable mutate rewrite.
              const bool is_ref_formal =
                  i < decl.params.size() &&
                  (decl.params[i].direction == hir::ParamDirection::kRef ||
                   decl.params[i].direction == hir::ParamDirection::kConstRef);
              mir::ExprId actual_id{};
              if (is_ref_formal) {
                auto arg_or = process.LowerLhsExpr(
                    hir_process.exprs.at(c.arguments[i]->value), frame);
                if (!arg_or) {
                  return std::unexpected(std::move(arg_or.error()));
                }
                actual_id = block.AddExpr(*std::move(arg_or));
                const mir::ExprId root_id = FindLhsRootId(block, actual_id);
                const bool root_is_cell = mir::IsObservableCellType(
                    module.Unit().GetType(block.GetExpr(root_id).type));
                if (root_is_cell && root_id != actual_id) {
                  // Selector / range on an observable cell -- the body sees
                  // a `Ref<T>` bound to a `ScopedMutation` snapshot of the
                  // selected element. Bare cells stay as-is so the body's
                  // `Ref<T>` binds the underlying `Var<T>` directly.
                  const mir::ExprId services_id =
                      block.AddExpr(BuildServicesCallExpr(process, frame));
                  actual_id = RewriteLhsRootWithMutate(
                      module.Unit(), block, actual_id, services_id);
                }
              } else {
                auto arg_or = process.LowerExpr(
                    hir_process.exprs.at(c.arguments[i]->value), frame);
                if (!arg_or) {
                  return std::unexpected(std::move(arg_or.error()));
                }
                actual_id = block.AddExpr(*std::move(arg_or));
              }
              if (is_ref_formal) {
                args.push_back(BuildReferenceArg(
                    process.Module().Unit(), block, actual_id,
                    block.GetExpr(actual_id).type));
              } else {
                args.push_back(actual_id);
              }
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = LowerUserCallee(lowerer, usr),
                        .arguments = std::move(args)},
                .type = result_type};
          },
          [&](const hir::BuiltinMethodRef& b) -> diag::Result<mir::Expr> {
            // LRM 7.12.4 `item.index` -> the closure's `index` parameter
            // binding. Reaches here from a with-clause body and the binding
            // was installed by `BuildArrayMethodClosure`.
            if (std::holds_alternative<hir::IteratorMethodKind>(b.method)) {
              const auto index_binding = frame.active_index_binding;
              if (!index_binding.has_value()) {
                throw InternalError(
                    "LowerHirCallExprProc: IteratorMethodKind outside an "
                    "array-method `with` body (LRM 7.12.4)");
              }
              return mir::Expr{
                  .data =
                      mir::LocalRef{
                          .hops = mir::BlockHops{.value = 0},
                          .var = *index_binding},
                  .type = result_type};
            }
            if (c.arguments.empty()) {
              throw InternalError(
                  "BuiltinMethodRef call has no receiver argument");
            }
            if (!c.arguments.front().has_value()) {
              throw InternalError(
                  "BuiltinMethodRef receiver unexpectedly elided");
            }
            // LRM 7.9.4 -- 7.9.7 traversal lowers to an immediately-invoked
            // closure: it writes the index back through an observable assign
            // and runs in expression position. The other associative methods
            // are plain member calls handled by the generic path below.
            if (const auto* fn = std::get_if<support::BuiltinFn>(&b.method);
                fn != nullptr && IsAssociativeTraversalFn(*fn)) {
              return LowerAssociativeTraversal(
                  process, frame, c, *fn, result_type);
            }
            const hir::TypeId hir_receiver_type =
                hir_process.exprs.at(c.arguments.front()->value).type;
            std::vector<mir::ExprId> args;
            args.reserve(c.arguments.size() + 1);

            // Translate to the MIR callee up front so the same trait
            // (`mir::IsMutatingCallee`) drives both lowering and backend
            // rendering.
            const mir::Callee mir_callee =
                BuildBuiltinMirCallee(module, b, hir_receiver_type);

            // Lower the receiver. A mutating method against an observable
            // cell routes through `Var<T>::Mutate` -- the receiver is the
            // bare cell expression wrapped in
            // `DerefExpr(CallExpr(ObservableMethod{kMutate}, [cell,
            // services]))` so the method body operates on the snapshot and the
            // destructor commits via `Var::Set`. A non-mutating method consumes
            // the value, so the default `LowerExpr` path (which auto-wraps in
            // `Get`) is correct.
            const bool method_mutates = mir::IsMutatingCallee(mir_callee);
            // A static call has no value receiver -- `args[0]` is a normal
            // user argument, the type-namespace qualifier rides on the
            // callee.
            const bool has_receiver =
                !std::holds_alternative<mir::BuiltinStaticCallee>(mir_callee);
            if (has_receiver) {
              mir::ExprId receiver_id{};
              if (method_mutates) {
                auto recv_or = process.LowerLhsExpr(
                    hir_process.exprs.at(c.arguments.front()->value), frame);
                if (!recv_or) {
                  return std::unexpected(std::move(recv_or.error()));
                }
                receiver_id = block.AddExpr(*std::move(recv_or));
                const mir::ExprId root_id = FindLhsRootId(block, receiver_id);
                const bool root_is_cell = mir::IsObservableCellType(
                    module.Unit().GetType(block.GetExpr(root_id).type));
                if (root_is_cell) {
                  const mir::ExprId services_id =
                      block.AddExpr(BuildServicesCallExpr(process, frame));
                  receiver_id = RewriteLhsRootWithMutate(
                      module.Unit(), block, receiver_id, services_id);
                }
              } else {
                auto recv_or = process.LowerExpr(
                    hir_process.exprs.at(c.arguments.front()->value), frame);
                if (!recv_or) {
                  return std::unexpected(std::move(recv_or.error()));
                }
                receiver_id = block.AddExpr(*std::move(recv_or));
              }
              args.push_back(receiver_id);
            }

            for (std::size_t i = 1; i < c.arguments.size(); ++i) {
              if (!c.arguments[i].has_value()) {
                throw InternalError(
                    "builtin-method call argument unexpectedly elided");
              }
              auto arg_or = process.LowerExpr(
                  hir_process.exprs.at(c.arguments[i]->value), frame);
              if (!arg_or) {
                return std::unexpected(std::move(arg_or.error()));
              }
              args.push_back(block.AddExpr(*std::move(arg_or)));
            }
            // LRM 7.12.1: reduction / ordering / locator array methods take a
            // closure. The user's `with` clause is used if present; otherwise
            // LRM defines the default as `with (item)`, which HIR-to-MIR
            // synthesises so MIR always carries the closure argument and
            // downstream consumers see one uniform shape per kind.
            const auto* fn = std::get_if<support::BuiltinFn>(&b.method);
            if (fn != nullptr && ArrayMethodTakesClosure(*fn)) {
              auto closure_or = BuildArrayMethodClosure(
                  process, frame, hir_receiver_type,
                  c.with_clause.has_value() ? &*c.with_clause : nullptr);
              if (!closure_or) {
                return std::unexpected(std::move(closure_or.error()));
              }
              args.push_back(block.AddExpr(*std::move(closure_or)));
              // LRM 7.12.5: `map`'s result element type may differ from the
              // receiver's, so its shield is supplied here as that type's
              // canonical default -- the runtime shape is not recoverable from
              // the C++ type alone.
              if (*fn == support::BuiltinFn::kMap) {
                const mir::TypeId result_elem = ArrayContainerElementType(
                    module.Unit().GetType(result_type));
                args.push_back(block.AddExpr(
                    BuildDefaultValueExpr(module, frame, result_elem)));
              }
            } else if (c.with_clause.has_value()) {
              throw InternalError(
                  "BuiltinMethodRef with-clause on a method kind that does "
                  "not accept a with-clause (LRM 7.12.1 family only)");
            }
            // LRM 15.5.3: `e.triggered` reads the triggered flag out of
            // RuntimeServices. The engine handle is a real trailing argument,
            // threaded the same way every runtime effect threads it
            // (docs/decisions/runtime-effects-as-generic-calls.md), not a
            // backend-fabricated one. (`-> e` is the only producer of the
            // trigger kind and lowers through LowerEventTriggerStmt; `await`
            // takes no services.)
            if (fn != nullptr && *fn == support::BuiltinFn::kTriggered) {
              args.push_back(
                  block.AddExpr(BuildServicesCallExpr(process, frame)));
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = mir_callee, .arguments = std::move(args)},
                .type = result_type};
          },
      },
      c.callee);
}

}  // namespace lyra::lowering::hir_to_mir
