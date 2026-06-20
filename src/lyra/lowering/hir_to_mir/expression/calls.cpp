#include "lyra/lowering/hir_to_mir/expression/calls.hpp"

#include <expected>
#include <memory>
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
#include "lyra/lowering/hir_to_mir/capture_sink.hpp"
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
#include "lyra/lowering/hir_to_mir/procedural_depth.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/services_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto LowerEnumMethodKind(hir::EnumMethodKind k) -> mir::EnumMethodKind {
  switch (k) {
    case hir::EnumMethodKind::kFirst:
      return mir::EnumMethodKind::kFirst;
    case hir::EnumMethodKind::kLast:
      return mir::EnumMethodKind::kLast;
    case hir::EnumMethodKind::kNum:
      return mir::EnumMethodKind::kNum;
    case hir::EnumMethodKind::kNext:
      return mir::EnumMethodKind::kNext;
    case hir::EnumMethodKind::kPrev:
      return mir::EnumMethodKind::kPrev;
    case hir::EnumMethodKind::kName:
      return mir::EnumMethodKind::kName;
  }
  throw InternalError("LowerEnumMethodKind: unknown hir::EnumMethodKind");
}

auto LowerStringMethodKind(hir::StringMethodKind k) -> mir::StringMethodKind {
  switch (k) {
    case hir::StringMethodKind::kLen:
      return mir::StringMethodKind::kLen;
    case hir::StringMethodKind::kGetc:
      return mir::StringMethodKind::kGetc;
    case hir::StringMethodKind::kPutc:
      return mir::StringMethodKind::kPutc;
    case hir::StringMethodKind::kToupper:
      return mir::StringMethodKind::kToupper;
    case hir::StringMethodKind::kTolower:
      return mir::StringMethodKind::kTolower;
    case hir::StringMethodKind::kCompare:
      return mir::StringMethodKind::kCompare;
    case hir::StringMethodKind::kIcompare:
      return mir::StringMethodKind::kIcompare;
    case hir::StringMethodKind::kSubstr:
      return mir::StringMethodKind::kSubstr;
    case hir::StringMethodKind::kAtoi:
      return mir::StringMethodKind::kAtoi;
    case hir::StringMethodKind::kAtohex:
      return mir::StringMethodKind::kAtohex;
    case hir::StringMethodKind::kAtooct:
      return mir::StringMethodKind::kAtooct;
    case hir::StringMethodKind::kAtobin:
      return mir::StringMethodKind::kAtobin;
    case hir::StringMethodKind::kAtoreal:
      return mir::StringMethodKind::kAtoreal;
    case hir::StringMethodKind::kItoa:
      return mir::StringMethodKind::kItoa;
    case hir::StringMethodKind::kHextoa:
      return mir::StringMethodKind::kHextoa;
    case hir::StringMethodKind::kOcttoa:
      return mir::StringMethodKind::kOcttoa;
    case hir::StringMethodKind::kBintoa:
      return mir::StringMethodKind::kBintoa;
    case hir::StringMethodKind::kRealtoa:
      return mir::StringMethodKind::kRealtoa;
  }
  throw InternalError("LowerStringMethodKind: unknown hir::StringMethodKind");
}

auto LowerEventMethodKind(hir::EventMethodKind k) -> mir::EventMethodKind {
  switch (k) {
    case hir::EventMethodKind::kTrigger:
      return mir::EventMethodKind::kTrigger;
    case hir::EventMethodKind::kAwait:
      return mir::EventMethodKind::kAwait;
    case hir::EventMethodKind::kTriggered:
      return mir::EventMethodKind::kTriggered;
  }
  throw InternalError("LowerEventMethodKind: unknown hir::EventMethodKind");
}

auto ArrayMethodTakesClosure(hir::ArrayMethodKind k) -> bool {
  switch (k) {
    case hir::ArrayMethodKind::kSize:
    case hir::ArrayMethodKind::kDelete:
    case hir::ArrayMethodKind::kReverse:
      return false;
    case hir::ArrayMethodKind::kSort:
    case hir::ArrayMethodKind::kRsort:
    case hir::ArrayMethodKind::kSum:
    case hir::ArrayMethodKind::kProduct:
    case hir::ArrayMethodKind::kAnd:
    case hir::ArrayMethodKind::kOr:
    case hir::ArrayMethodKind::kXor:
    case hir::ArrayMethodKind::kFind:
    case hir::ArrayMethodKind::kFindIndex:
    case hir::ArrayMethodKind::kFindFirst:
    case hir::ArrayMethodKind::kFindFirstIndex:
    case hir::ArrayMethodKind::kFindLast:
    case hir::ArrayMethodKind::kFindLastIndex:
    case hir::ArrayMethodKind::kMin:
    case hir::ArrayMethodKind::kMax:
    case hir::ArrayMethodKind::kUnique:
    case hir::ArrayMethodKind::kUniqueIndex:
    case hir::ArrayMethodKind::kMap:
      return true;
  }
  throw InternalError("ArrayMethodTakesClosure: unknown hir::ArrayMethodKind");
}

auto LowerArrayMethodKind(hir::ArrayMethodKind k) -> mir::ArrayMethodKind {
  switch (k) {
    case hir::ArrayMethodKind::kSize:
      return mir::ArrayMethodKind::kSize;
    case hir::ArrayMethodKind::kDelete:
      return mir::ArrayMethodKind::kDelete;
    case hir::ArrayMethodKind::kReverse:
      return mir::ArrayMethodKind::kReverse;
    case hir::ArrayMethodKind::kSort:
      return mir::ArrayMethodKind::kSort;
    case hir::ArrayMethodKind::kRsort:
      return mir::ArrayMethodKind::kRsort;
    case hir::ArrayMethodKind::kSum:
      return mir::ArrayMethodKind::kSum;
    case hir::ArrayMethodKind::kProduct:
      return mir::ArrayMethodKind::kProduct;
    case hir::ArrayMethodKind::kAnd:
      return mir::ArrayMethodKind::kAnd;
    case hir::ArrayMethodKind::kOr:
      return mir::ArrayMethodKind::kOr;
    case hir::ArrayMethodKind::kXor:
      return mir::ArrayMethodKind::kXor;
    case hir::ArrayMethodKind::kFind:
      return mir::ArrayMethodKind::kFind;
    case hir::ArrayMethodKind::kFindIndex:
      return mir::ArrayMethodKind::kFindIndex;
    case hir::ArrayMethodKind::kFindFirst:
      return mir::ArrayMethodKind::kFindFirst;
    case hir::ArrayMethodKind::kFindFirstIndex:
      return mir::ArrayMethodKind::kFindFirstIndex;
    case hir::ArrayMethodKind::kFindLast:
      return mir::ArrayMethodKind::kFindLast;
    case hir::ArrayMethodKind::kFindLastIndex:
      return mir::ArrayMethodKind::kFindLastIndex;
    case hir::ArrayMethodKind::kMin:
      return mir::ArrayMethodKind::kMin;
    case hir::ArrayMethodKind::kMax:
      return mir::ArrayMethodKind::kMax;
    case hir::ArrayMethodKind::kUnique:
      return mir::ArrayMethodKind::kUnique;
    case hir::ArrayMethodKind::kUniqueIndex:
      return mir::ArrayMethodKind::kUniqueIndex;
    case hir::ArrayMethodKind::kMap:
      return mir::ArrayMethodKind::kMap;
  }
  throw InternalError("LowerArrayMethodKind: unknown hir::ArrayMethodKind");
}

auto LowerQueueMethodKind(hir::QueueMethodKind k) -> mir::QueueMethodKind {
  switch (k) {
    case hir::QueueMethodKind::kSize:
      return mir::QueueMethodKind::kSize;
    case hir::QueueMethodKind::kInsert:
      return mir::QueueMethodKind::kInsert;
    case hir::QueueMethodKind::kDelete:
      return mir::QueueMethodKind::kDelete;
    case hir::QueueMethodKind::kPopFront:
      return mir::QueueMethodKind::kPopFront;
    case hir::QueueMethodKind::kPopBack:
      return mir::QueueMethodKind::kPopBack;
    case hir::QueueMethodKind::kPushFront:
      return mir::QueueMethodKind::kPushFront;
    case hir::QueueMethodKind::kPushBack:
      return mir::QueueMethodKind::kPushBack;
    case hir::QueueMethodKind::kElementAt:
      return mir::QueueMethodKind::kElementAt;
    case hir::QueueMethodKind::kWriteRef:
      return mir::QueueMethodKind::kWriteRef;
    case hir::QueueMethodKind::kSlice:
      return mir::QueueMethodKind::kSlice;
  }
  throw InternalError("LowerQueueMethodKind: unknown hir::QueueMethodKind");
}

auto LowerAssociativeMethodKind(hir::AssociativeMethodKind k)
    -> mir::AssociativeMethodKind {
  switch (k) {
    case hir::AssociativeMethodKind::kNum:
      return mir::AssociativeMethodKind::kNum;
    case hir::AssociativeMethodKind::kSize:
      return mir::AssociativeMethodKind::kSize;
    case hir::AssociativeMethodKind::kExists:
      return mir::AssociativeMethodKind::kExists;
    case hir::AssociativeMethodKind::kDelete:
      return mir::AssociativeMethodKind::kDelete;
    case hir::AssociativeMethodKind::kFirst:
      return mir::AssociativeMethodKind::kFirst;
    case hir::AssociativeMethodKind::kLast:
      return mir::AssociativeMethodKind::kLast;
    case hir::AssociativeMethodKind::kNext:
      return mir::AssociativeMethodKind::kNext;
    case hir::AssociativeMethodKind::kPrev:
      return mir::AssociativeMethodKind::kPrev;
  }
  throw InternalError(
      "LowerAssociativeMethodKind: unknown hir::AssociativeMethodKind");
}

auto LowerIteratorMethodKind(hir::IteratorMethodKind k)
    -> mir::IteratorMethodKind {
  switch (k) {
    case hir::IteratorMethodKind::kIndex:
      return mir::IteratorMethodKind::kIndex;
  }
  throw InternalError(
      "LowerIteratorMethodKind: unknown hir::IteratorMethodKind");
}

// Translates a HIR builtin-method ref to its MIR `BuiltinMethodCallee`. The
// receiver type is the original HIR type of `c.arguments[0]` so the enum-
// method case can carry the enum type id MIR needs.
auto BuildMirBuiltinMethodCallee(
    const ModuleLowerer& module, const hir::BuiltinMethodRef& b,
    hir::TypeId hir_receiver_type) -> mir::BuiltinMethodCallee {
  return std::visit(
      Overloaded{
          [&](hir::EnumMethodKind k) -> mir::BuiltinMethodCallee {
            return {
                .method = mir::EnumMethodInfo{
                    .enum_type = module.TranslateType(hir_receiver_type),
                    .kind = LowerEnumMethodKind(k)}};
          },
          [](hir::StringMethodKind k) -> mir::BuiltinMethodCallee {
            return {
                .method =
                    mir::StringMethodInfo{.kind = LowerStringMethodKind(k)}};
          },
          [](hir::EventMethodKind k) -> mir::BuiltinMethodCallee {
            return {
                .method =
                    mir::EventMethodInfo{.kind = LowerEventMethodKind(k)}};
          },
          [](hir::ArrayMethodKind k) -> mir::BuiltinMethodCallee {
            return {
                .method =
                    mir::ArrayMethodInfo{.kind = LowerArrayMethodKind(k)}};
          },
          [](hir::QueueMethodKind k) -> mir::BuiltinMethodCallee {
            return {
                .method =
                    mir::QueueMethodInfo{.kind = LowerQueueMethodKind(k)}};
          },
          [](hir::AssociativeMethodKind k) -> mir::BuiltinMethodCallee {
            return {
                .method = mir::AssociativeMethodInfo{
                    .kind = LowerAssociativeMethodKind(k)}};
          },
          [](hir::IteratorMethodKind k) -> mir::BuiltinMethodCallee {
            return {
                .method = mir::IteratorMethodInfo{
                    .kind = LowerIteratorMethodKind(k)}};
          },
      },
      b.method);
}

auto LowerUserCallee(
    const StructuralScopeLowerer& scope, const hir::StructuralSubroutineRef& u)
    -> mir::Callee {
  return scope.TranslateStructuralSubroutine(u.hops, u.subroutine);
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

// LRM 7.12.1 / 7.12.2 / 7.12.3 with-clause closure synthesis. The body is a
// normal expression lowered through `process.LowerExpr`; only the
// closure-specific leaf behaviour lives elsewhere -- captures via
// `CaptureSink`, iterator and index resolution via pre-allocated body
// procedural-var bindings remapped for the iterator HIR id and tracked on
// the walk frame for the kIndex call. When the source has no `with` clause
// LRM 7.12.1 defines the default as `with (item)`; this synthesises the
// identity closure (body returns the iterator binding) so MIR always carries
// the closure argument and downstream consumers see one uniform shape.
auto BuildArrayMethodClosure(
    ProcessLowerer& process, WalkFrame frame, hir::TypeId hir_receiver_type,
    const hir::WithClause* with_clause) -> diag::Result<mir::Expr> {
  const auto& module = process.Module();
  const auto& hir_process = process.HirBody();
  auto& outer_scope = *frame.current_procedural_scope;
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

  const mir::TypeId self_ptr_type = module.Unit().builtins.self_pointer;
  mir::ProceduralScope body_scope;
  const WalkFrame body_frame = frame.WithProceduralScope(&body_scope).Deeper();
  const ProceduralDepth body_depth = body_frame.procedural_depth;

  const mir::ProceduralVarId self_binding = body_scope.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "self", .type = self_ptr_type});
  const mir::ExprId outer_self_read =
      outer_scope.AddExpr(BuildSelfRefExpr(frame, self_ptr_type));
  const mir::ProceduralVarId item_binding = body_scope.AddProceduralVar(
      mir::ProceduralVarDecl{.name = iterator_name, .type = item_type});
  const mir::ProceduralVarId index_binding = body_scope.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "index", .type = index_type});
  if (with_clause != nullptr) {
    process.MapProceduralVar(
        with_clause->iterator,
        AutomaticVarBinding{
            .declaration_procedural_depth = body_depth, .var = item_binding});
  }

  CaptureSink sink{
      body_depth, body_scope, outer_scope, process.Module().Unit()};
  // A with-clause body is a synchronous predicate / projection closure -- it
  // never suspends; its trailing `return` renders as a plain `return`.
  const WalkFrame closure_frame = body_frame.WithClosure(&sink)
                                      .WithIndexBinding(index_binding)
                                      .WithSelfBinding(self_binding, body_depth)
                                      .WithCoroutineBody(false);

  mir::ExprId body_return_value{};
  mir::TypeId return_type{};
  if (with_clause != nullptr) {
    auto body_expr_or = process.LowerExpr(
        hir_process.exprs.at(with_clause->expr.value), closure_frame);
    if (!body_expr_or) return std::unexpected(std::move(body_expr_or.error()));
    return_type = body_expr_or->type;
    body_return_value = body_scope.AddExpr(*std::move(body_expr_or));
  } else {
    return_type = item_type;
    body_return_value = body_scope.AddExpr(
        mir::Expr{
            .data =
                mir::ProceduralVarRef{
                    .hops = mir::ProceduralHops{.value = 0},
                    .var = item_binding},
            .type = item_type});
  }
  body_scope.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ReturnStmt{.value = body_return_value}});

  std::vector<mir::Capture> captures;
  captures.push_back(
      mir::Capture{.value = outer_self_read, .binding = self_binding});
  for (const CaptureRequest& request : sink.TakeRequests()) {
    captures.push_back(
        mir::Capture{.value = request.source, .binding = request.binding});
  }
  mir::ClosureExpr closure;
  closure.captures = std::move(captures);
  closure.params.push_back(mir::Parameter{.binding = item_binding});
  closure.params.push_back(mir::Parameter{.binding = index_binding});
  closure.body = std::make_unique<mir::ProceduralScope>(std::move(body_scope));
  return mir::Expr{.data = std::move(closure), .type = return_type};
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
  const auto& scope = process.Scope();
  const auto& hir_process = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
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
                scope.LookupHirSubroutine(usr.hops, usr.subroutine);
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
            args.push_back(proc_scope.AddExpr(
                BuildSelfRefExpr(frame, module.Unit().builtins.self_pointer)));
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
                actual_id = proc_scope.AddExpr(*std::move(arg_or));
                const mir::ExprId root_id =
                    FindLhsRootId(proc_scope, actual_id);
                const bool root_is_cell = mir::IsObservableCellType(
                    module.Unit().GetType(proc_scope.GetExpr(root_id).type));
                if (root_is_cell && root_id != actual_id) {
                  // Selector / range on an observable cell -- the body sees
                  // a `Ref<T>` bound to a `ScopedMutation` snapshot of the
                  // selected element. Bare cells stay as-is so the body's
                  // `Ref<T>` binds the underlying `Var<T>` directly.
                  const mir::ExprId services_id =
                      proc_scope.AddExpr(BuildServicesCallExpr(process, frame));
                  actual_id = RewriteLhsRootWithMutate(
                      module.Unit(), proc_scope, actual_id, services_id);
                }
              } else {
                auto arg_or = process.LowerExpr(
                    hir_process.exprs.at(c.arguments[i]->value), frame);
                if (!arg_or) {
                  return std::unexpected(std::move(arg_or.error()));
                }
                actual_id = proc_scope.AddExpr(*std::move(arg_or));
              }
              if (is_ref_formal) {
                args.push_back(BuildReferenceArg(
                    process.Module().Unit(), proc_scope, actual_id,
                    proc_scope.GetExpr(actual_id).type));
              } else {
                args.push_back(actual_id);
              }
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = LowerUserCallee(scope, usr),
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
                      mir::ProceduralVarRef{
                          .hops = mir::ProceduralHops{.value = 0},
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
            const hir::TypeId hir_receiver_type =
                hir_process.exprs.at(c.arguments.front()->value).type;
            std::vector<mir::ExprId> args;
            args.reserve(c.arguments.size() + 1);

            // Translate to the MIR callee up front so the same trait
            // (`mir::IsMutatingBuiltinMethod`) drives both lowering and
            // backend rendering.
            const mir::BuiltinMethodCallee mir_callee =
                BuildMirBuiltinMethodCallee(module, b, hir_receiver_type);

            // Lower the receiver. A mutating method against an observable
            // cell routes through `Var<T>::Mutate` -- the receiver is the
            // bare cell expression wrapped in
            // `DerefExpr(CallExpr(ObservableMethod{kMutate}, [cell,
            // services]))` so the method body operates on the snapshot and the
            // destructor commits via `Var::Set`. A non-mutating method consumes
            // the value, so the default `LowerExpr` path (which auto-wraps in
            // `Get`) is correct.
            const bool method_mutates =
                mir::IsMutatingBuiltinMethod(mir_callee);
            mir::ExprId receiver_id{};
            if (method_mutates) {
              auto recv_or = process.LowerLhsExpr(
                  hir_process.exprs.at(c.arguments.front()->value), frame);
              if (!recv_or) {
                return std::unexpected(std::move(recv_or.error()));
              }
              receiver_id = proc_scope.AddExpr(*std::move(recv_or));
              const mir::ExprId root_id =
                  FindLhsRootId(proc_scope, receiver_id);
              const bool root_is_cell = mir::IsObservableCellType(
                  module.Unit().GetType(proc_scope.GetExpr(root_id).type));
              if (root_is_cell) {
                const mir::ExprId services_id =
                    proc_scope.AddExpr(BuildServicesCallExpr(process, frame));
                receiver_id = RewriteLhsRootWithMutate(
                    module.Unit(), proc_scope, receiver_id, services_id);
              }
            } else {
              auto recv_or = process.LowerExpr(
                  hir_process.exprs.at(c.arguments.front()->value), frame);
              if (!recv_or) {
                return std::unexpected(std::move(recv_or.error()));
              }
              receiver_id = proc_scope.AddExpr(*std::move(recv_or));
            }
            args.push_back(receiver_id);

            // LRM 7.9.4 -- 7.9.7 associative-array traversal methods
            // (`first` / `last` / `next` / `prev`) take their index argument
            // as a `ref` so the visited key writes through to the actual --
            // lower it cell-rooted like any other ref formal.
            const auto* assoc_kind =
                std::get_if<hir::AssociativeMethodKind>(&b.method);
            const bool index_is_ref =
                assoc_kind != nullptr &&
                (*assoc_kind == hir::AssociativeMethodKind::kFirst ||
                 *assoc_kind == hir::AssociativeMethodKind::kLast ||
                 *assoc_kind == hir::AssociativeMethodKind::kNext ||
                 *assoc_kind == hir::AssociativeMethodKind::kPrev);
            for (std::size_t i = 1; i < c.arguments.size(); ++i) {
              if (!c.arguments[i].has_value()) {
                throw InternalError(
                    "builtin-method call argument unexpectedly elided");
              }
              const bool lower_as_lhs = index_is_ref && i == 1;
              auto arg_or =
                  lower_as_lhs
                      ? process.LowerLhsExpr(
                            hir_process.exprs.at(c.arguments[i]->value), frame)
                      : process.LowerExpr(
                            hir_process.exprs.at(c.arguments[i]->value), frame);
              if (!arg_or) {
                return std::unexpected(std::move(arg_or.error()));
              }
              args.push_back(proc_scope.AddExpr(*std::move(arg_or)));
            }
            // LRM 7.12.1: reduction / ordering / locator array methods take a
            // closure. The user's `with` clause is used if present; otherwise
            // LRM defines the default as `with (item)`, which HIR-to-MIR
            // synthesises so MIR always carries the closure argument and
            // downstream consumers see one uniform shape per kind.
            if (const auto* ak = std::get_if<hir::ArrayMethodKind>(&b.method);
                ak != nullptr && ArrayMethodTakesClosure(*ak)) {
              auto closure_or = BuildArrayMethodClosure(
                  process, frame, hir_receiver_type,
                  c.with_clause.has_value() ? &*c.with_clause : nullptr);
              if (!closure_or) {
                return std::unexpected(std::move(closure_or.error()));
              }
              args.push_back(proc_scope.AddExpr(*std::move(closure_or)));
              // LRM 7.12.5: `map`'s result element type may differ from the
              // receiver's, so its shield is supplied here as that type's
              // canonical default -- the runtime shape is not recoverable from
              // the C++ type alone.
              if (*ak == hir::ArrayMethodKind::kMap) {
                const mir::TypeId result_elem = ArrayContainerElementType(
                    module.Unit().GetType(result_type));
                args.push_back(proc_scope.AddExpr(
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
            if (const auto* ev = std::get_if<hir::EventMethodKind>(&b.method)) {
              if (*ev == hir::EventMethodKind::kTriggered) {
                args.push_back(
                    proc_scope.AddExpr(BuildServicesCallExpr(process, frame)));
              }
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
