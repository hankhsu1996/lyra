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
#include "lyra/lowering/hir_to_mir/expression/system/control.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/file_io.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/print.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/scan.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/sformat.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/timescale.hpp"
#include "lyra/lowering/hir_to_mir/procedural_depth.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/closure.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/procedural_hops.hpp"
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
  }
  throw InternalError("LowerArrayMethodKind: unknown hir::ArrayMethodKind");
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

auto LowerUserCallee(
    const StructuralScopeLowerer& scope, const hir::StructuralSubroutineRef& u)
    -> mir::Callee {
  return scope.TranslateStructuralSubroutine(u.hops, u.subroutine);
}

// LRM 7.12.2 / 7.12.3 with-clause closure synthesis. The body is a normal
// expression lowered through `process.LowerExpr`; only the closure-specific
// leaf behaviour lives elsewhere -- captures via `CaptureSink`, iterator and
// index resolution via pre-allocated body procedural-var bindings remapped
// for the iterator HIR id and tracked on the walk frame for the kIndex call.
auto BuildArrayMethodClosure(
    ProcessLowerer& process, WalkFrame frame, hir::TypeId hir_receiver_type,
    const hir::WithClause& with_clause) -> diag::Result<mir::Expr> {
  const auto& module = process.Module();
  const auto& hir_process = process.HirBody();
  auto& outer_scope = *frame.current_procedural_scope;
  const auto& hir_recv_ty = module.Hir().GetType(hir_receiver_type);
  const auto* hir_da = std::get_if<hir::DynamicArrayType>(&hir_recv_ty.data);
  if (hir_da == nullptr) {
    throw InternalError(
        "BuildArrayMethodClosure: receiver is not a dynamic-array type");
  }
  const mir::TypeId item_type = module.TranslateType(hir_da->element_type);
  const mir::TypeId index_type = process.Module().Unit().builtins.int32;
  const auto& iterator_decl =
      hir_process.procedural_vars.at(with_clause.iterator.value);

  const mir::TypeId self_ptr_type = module.Unit().builtins.self_pointer;
  mir::ProceduralScope body_scope;
  const WalkFrame body_frame = frame.WithProceduralScope(&body_scope).Deeper();
  const ProceduralDepth body_depth = body_frame.procedural_depth;

  const mir::ProceduralVarId self_binding = body_scope.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "self", .type = self_ptr_type});
  const mir::ExprId outer_self_read = outer_scope.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = frame.procedural_depth - ProceduralDepth{},
                  .var = *frame.self_binding},
          .type = self_ptr_type});
  const mir::ProceduralVarId item_binding = body_scope.AddProceduralVar(
      mir::ProceduralVarDecl{.name = iterator_decl.name, .type = item_type});
  const mir::ProceduralVarId index_binding = body_scope.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "index", .type = index_type});
  process.MapProceduralVar(
      with_clause.iterator,
      ProceduralVarBinding{
          .declaration_procedural_depth = body_depth, .var = item_binding});

  CaptureSink sink{body_depth, body_scope, outer_scope};
  const WalkFrame closure_frame = body_frame.WithClosure(&sink)
                                      .WithIndexBinding(index_binding)
                                      .WithSelfBinding(self_binding);

  auto body_expr_or = process.LowerExpr(
      hir_process.exprs.at(with_clause.expr.value), closure_frame);

  if (!body_expr_or) return std::unexpected(std::move(body_expr_or.error()));

  const mir::ExprId body_return_value =
      body_scope.AddExpr(*std::move(body_expr_or));
  body_scope.AppendStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ReturnStmt{.value = body_return_value}});

  std::vector<mir::Capture> captures;
  captures.emplace_back(
      mir::ByValueCapture{.value = outer_self_read, .binding = self_binding});
  for (const CaptureRequest& request : sink.TakeRequests()) {
    captures.emplace_back(
        mir::ByReferenceCapture{
            .target = request.source, .binding = request.binding});
  }
  mir::ClosureExpr closure;
  closure.captures = std::move(captures);
  closure.params.push_back(mir::Parameter{.binding = item_binding});
  closure.params.push_back(mir::Parameter{.binding = index_binding});
  closure.body = std::make_unique<mir::ProceduralScope>(std::move(body_scope));
  return mir::Expr{
      .data = std::move(closure), .type = module.Unit().builtins.void_type};
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
                process, call, desc.name, term, span);
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
                process, frame, call, sformat, span);
          },
          [&](const support::TimeSystemSubroutineInfo& time_info)
              -> diag::Result<mir::Expr> {
            mir::TypeId result_type = process.Module().Unit().builtins.time;
            switch (time_info.kind) {
              case support::TimeKind::kTime:
                result_type = process.Module().Unit().builtins.time;
                break;
              case support::TimeKind::kStime:
                result_type = process.Module().Unit().builtins.int32;
                break;
              case support::TimeKind::kRealtime:
                result_type = process.Module().Unit().builtins.realtime;
                break;
            }
            return mir::Expr{
                .data =
                    mir::RuntimeCallExpr{
                        .call = mir::RuntimeTimeCall{.kind = time_info.kind}},
                .type = result_type};
          },
          [&](const support::TimeFormatSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            return LowerTimeFormatSystemSubroutineCall(
                process, frame, call, span);
          },
          [&](const support::PrintTimescaleSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            return LowerPrintTimescaleSystemSubroutineCall(process);
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
            std::vector<mir::ExprId> args;
            args.reserve(c.arguments.size());
            for (const auto& arg : c.arguments) {
              if (!arg.has_value()) {
                throw InternalError(
                    "user-function call argument unexpectedly elided");
              }
              auto arg_or =
                  process.LowerExpr(hir_process.exprs.at(arg->value), frame);
              if (!arg_or) return std::unexpected(std::move(arg_or.error()));
              args.push_back(proc_scope.AddExpr(*std::move(arg_or)));
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
            for (const auto& arg : c.arguments) {
              if (!arg.has_value()) {
                throw InternalError(
                    "builtin-method call argument unexpectedly elided");
              }
              auto arg_or =
                  process.LowerExpr(hir_process.exprs.at(arg->value), frame);
              if (!arg_or) {
                return std::unexpected(std::move(arg_or.error()));
              }
              args.push_back(proc_scope.AddExpr(*std::move(arg_or)));
            }
            if (c.with_clause.has_value()) {
              auto closure_or = BuildArrayMethodClosure(
                  process, frame, hir_receiver_type, *c.with_clause);
              if (!closure_or) {
                return std::unexpected(std::move(closure_or.error()));
              }
              args.push_back(proc_scope.AddExpr(*std::move(closure_or)));
            }
            auto callee = std::visit(
                Overloaded{
                    [&](hir::EnumMethodKind k) -> mir::BuiltinMethodCallee {
                      return {
                          .method = mir::EnumMethodInfo{
                              .enum_type =
                                  module.TranslateType(hir_receiver_type),
                              .kind = LowerEnumMethodKind(k)}};
                    },
                    [&](hir::StringMethodKind k) -> mir::BuiltinMethodCallee {
                      return {
                          .method = mir::StringMethodInfo{
                              .kind = LowerStringMethodKind(k)}};
                    },
                    [&](hir::EventMethodKind k) -> mir::BuiltinMethodCallee {
                      return {
                          .method = mir::EventMethodInfo{
                              .kind = LowerEventMethodKind(k)}};
                    },
                    [&](hir::ArrayMethodKind k) -> mir::BuiltinMethodCallee {
                      return {
                          .method = mir::ArrayMethodInfo{
                              .kind = LowerArrayMethodKind(k)}};
                    },
                    [&](hir::IteratorMethodKind k) -> mir::BuiltinMethodCallee {
                      return {
                          .method = mir::IteratorMethodInfo{
                              .kind = LowerIteratorMethodKind(k)}};
                    },
                },
                b.method);
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = std::move(callee),
                        .arguments = std::move(args)},
                .type = result_type};
          },
      },
      c.callee);
}

}  // namespace lyra::lowering::hir_to_mir
