#include "lyra/lowering/hir_to_mir/expression/calls.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/with_clause_id.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/expression/enum_method.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/real_conversion.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/bit_vector.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/control.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/file_io.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/host_command.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/plusargs.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/print.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/random.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/sampled_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/scan.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/sformat.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/time.hpp"
#include "lyra/lowering/hir_to_mir/expression/system/timescale.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/sampled_history.hpp"
#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Where a traversal's completion components sit: the SV int the method answers
// with, then the index it visited (LRM 7.9.4 -- 7.9.7).
constexpr base::ComponentIndex kTraversalFound{0};
constexpr base::ComponentIndex kTraversalVisitedIndex{1};

// LRM 7.9.4 -- 7.9.7 associative traversal (`m.first(idx)` / `last` / `next` /
// `prev`). The query answers with the SV int 1 / 0 and the index it visited,
// so it completes with the two of them and the index reaches the variable the
// source named through that variable's own write -- which is what fires its
// LRM 4.3 update event. Binding the completion and writing the index back are
// statements while the call sits in expression position (the canonical
// `do ... while (m.next(idx))` idiom), so they are the steps of one block
// expression.
template <ExprLowerer Lowerer>
auto LowerAssociativeTraversal(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::BuiltinMethodRef& b, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  // The index whose neighbour is asked for (LRM 7.9.4); the array it is asked
  // of is the object the entry acts on.
  const std::vector<hir::ExprId> operands = RequiredOperands(c, 1);
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  const auto& hir_exprs = lowerer.HirExprs();
  const hir::ExprId recv_hir = ObjectActedOn(b);
  const hir::ExprId idx_hir = operands[0];
  const mir::TypeId key_type =
      unit_lowerer.TranslateType(hir_exprs.Get(idx_hir).type);

  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const WalkFrame& step_frame = steps.Frame();

  auto map_read_or = lowerer.LowerExpr(hir_exprs.Get(recv_hir), step_frame);
  if (!map_read_or) return std::unexpected(std::move(map_read_or.error()));
  const mir::ExprId map_read_id = body.exprs.Add(*std::move(map_read_or));
  // `first` / `last` ignore the index's current value and `next` / `prev` read
  // it as the search bound, so every form takes it as an ordinary operand.
  auto idx_read_or = lowerer.LowerExpr(hir_exprs.Get(idx_hir), step_frame);
  if (!idx_read_or) return std::unexpected(std::move(idx_read_or.error()));
  const mir::ExprId idx_read_id = body.exprs.Add(*std::move(idx_read_or));

  const mir::TypeId payload_type =
      CompletionPayloadType(unit, {result_type, key_type});
  const mir::ExprId query_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{.target = b.method, .receiver = map_read_id},
                  .arguments = {idx_read_id}},
          .type = payload_type});
  const mir::LocalId completion =
      steps.Bindings().DeclareAnonymous(payload_type);
  body.AppendStmt(mir::LocalDeclStmt{.target = completion, .init = query_id});

  auto idx_lhs_or = lowerer.LowerLhsExpr(hir_exprs.Get(idx_hir), step_frame);
  if (!idx_lhs_or) return std::unexpected(std::move(idx_lhs_or.error()));
  const mir::ExprId visited_id = ProjectCompletionComponent(
      body, completion, payload_type, kTraversalVisitedIndex, key_type);
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(BuildStoreExpr(
              unit, body, *idx_lhs_or, visited_id, std::nullopt, key_type))});

  return steps.Build(ProjectCompletionComponent(
      body, completion, payload_type, kTraversalFound, result_type));
}

// The canonical-default prototype type for an entry whose result shape the
// receiver does not determine: a container result contributes its element type
// as the prototype; a scalar result is its own prototype.
auto ResultPrototypeType(
    const UnitLowerer& unit_lowerer, mir::TypeId result_type) -> mir::TypeId {
  return ContainerElementType(unit_lowerer.Unit(), result_type)
      .value_or(result_type);
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
  const hir::Type& hir_recv_ty =
      unit_lowerer.Hir().types.Get(hir_receiver_type);
  // The LRM 7.12 family shares one closure shape across every unpacked-array
  // receiver; only the element type differs.
  const auto element_type = ContainerElementType(hir_recv_ty);
  if (!element_type.has_value()) {
    throw InternalError(
        "BuildArrayMethodClosure: receiver is not an unpacked-array type");
  }
  const mir::TypeId item_type = unit_lowerer.TranslateType(*element_type);
  // LRM 7.12.4 `item.index`: the ordinal position for a sequence container, the
  // key for an associative receiver.
  mir::TypeId index_type = unit_lowerer.Unit().builtins.int_type;
  if (const auto* assoc = hir_recv_ty.As<hir::AssociativeArrayType>();
      assoc != nullptr) {
    index_type = unit_lowerer.TranslateType(assoc->key_type);
  }
  ClosureBuilder closure(lowerer.Owner().Unit(), frame);
  mir::Block& body = closure.Body();

  mir::ExprId body_return_value{};
  if (with_clause != nullptr) {
    // `item` and `item.index` (LRM 7.12.4) are this clause's two per-invocation
    // parameters, declared under the clause's iterator origins so the body
    // resolves each reference by identity and captures it -- through the same
    // forwarding machinery as any binding -- when it crosses a closure boundary
    // (a clause nested in the body still reaches this outer iterator).
    closure.AddNamedParam(
        BindingOriginId::Iterator(
            with_clause->id.value,
            static_cast<std::uint32_t>(hir::IterationBindingRole::kElement)),
        with_clause->element_name, item_type);
    closure.AddParam(
        BindingOriginId::Iterator(
            with_clause->id.value,
            static_cast<std::uint32_t>(hir::IterationBindingRole::kIndex)),
        index_type);
    auto body_expr_or =
        lowerer.LowerExpr(hir_exprs.Get(with_clause->expr), closure.Frame());
    if (!body_expr_or) return std::unexpected(std::move(body_expr_or.error()));
    body_return_value = body.exprs.Add(*std::move(body_expr_or));
  } else {
    // A built-in reduction (no with-clause) returns the bare element. The
    // source wrote no iteration variable for it, so neither parameter answers
    // to an identifier, and neither carries a cross-body identity because
    // nothing can capture them.
    const mir::LocalId item_binding = closure.AddParamAnonymous(item_type);
    closure.AddParamAnonymous(index_type);
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
              return LowerTerminationSystemSubroutineCall(
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
          [&](const support::BitVectorSystemSubroutineInfo& bit_vector)
              -> diag::Result<mir::Expr> {
            return LowerBitVectorSystemSubroutineCall(
                lowerer, frame, call, bit_vector, span);
          },
          [&](const support::HostCommandSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerHostCommandSystemSubroutineCall(lowerer, frame, call);
            } else {
              return RejectStructuralEffect(span);
            }
          },
          [&](const support::RandomSystemSubroutineInfo& random_info)
              -> diag::Result<mir::Expr> {
            return LowerRandomSystemSubroutineCall(
                lowerer, frame, call, random_info);
          },
          [&](const support::DistributionSystemSubroutineInfo& distribution)
              -> diag::Result<mir::Expr> {
            return LowerDistributionSystemSubroutineCall(
                lowerer, frame, call, distribution);
          },
          [&](const support::SampledValueSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            // Reads state and sequences nothing, so it is legal wherever a
            // value is and one handler serves both contexts.
            return LowerSampledValueSystemSubroutineCall(lowerer, frame, call);
          },
          [](const support::ValueChangeSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerSystemSubroutineCall: a call answering across the ticks "
                "of a clocking event names the history it answers from, so it "
                "is planned before this dispatch");
          },
          [](const support::PastValueSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerSystemSubroutineCall: a call answering across the ticks "
                "of a clocking event names the history it answers from, so it "
                "is planned before this dispatch");
          },
          [&](const support::MemFileSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            // A void task (LRM 21.4 / 21.5) has no value, so the frontend
            // rejects it in any value position; a statement-position call is
            // intercepted by the statement-form dispatch and never falls
            // through here. Reaching this arm is therefore a frontend /
            // lowering invariant violation, not an unsupported source form.
            throw InternalError(
                "$readmem / $writemem reached expression lowering; a void "
                "task only lowers through the statement-form dispatch");
          },
      },
      desc.semantic);
}

// Built-in method dispatch (LRM 6.16 / 7.9 / 7.10 / 7.12 / 9.7 / 15.5). The
// call states the object the entry acts on, so every operand here is one the
// callee takes.
template <ExprLowerer Lowerer>
auto LowerBuiltinMethodCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::BuiltinMethodRef& b, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const support::RuntimeEntry entry = support::RuntimeEntryOf(b.method);
  // LRM 7.9.4 -- 7.9.7 traversal answers with two values and has to place one
  // of them, so it is not the plain member call the generic path below builds
  // for every other associative method.
  if (entry.writes_the_index_back) {
    return LowerAssociativeTraversal(lowerer, frame, c, b, result_type);
  }
  // LRM 20.5 conversions answer in a machine integer, which the destination's
  // declared representation then has to land, so each is a pair of steps
  // rather than the single call the generic path builds.
  if (b.method == support::BuiltinFn::kTruncate ||
      b.method == support::BuiltinFn::kToBits ||
      b.method == support::BuiltinFn::kFromBits) {
    return LowerRealConversionCall(lowerer, frame, c, b, result_type);
  }
  const auto& unit_lowerer = lowerer.Owner();
  const auto& hir_exprs = lowerer.HirExprs();

  auto& block = *frame.current_block;
  // A method that changes the object it acts on takes the place that object
  // stands in, so the change lands where the source named it; one that does not
  // consumes a value.
  std::optional<mir::ExprId> receiver;
  if (b.receiver.has_value()) {
    const hir::Expr& object = hir_exprs.Get(*b.receiver);
    if (entry.mutates_receiver) {
      auto recv_or = lowerer.LowerLhsExpr(object, frame);
      if (!recv_or) return std::unexpected(std::move(recv_or.error()));
      receiver = TargetPlace(lowerer.Owner().Unit(), block, *recv_or);
    } else {
      auto recv_or = lowerer.LowerExpr(object, frame);
      if (!recv_or) return std::unexpected(std::move(recv_or.error()));
      receiver = block.exprs.Add(*std::move(recv_or));
    }
  }
  const mir::Direct mir_callee{.target = b.method, .receiver = receiver};

  std::vector<mir::ExprId> args;
  args.reserve(c.arguments.size() + 1);

  // An entry reaching the engine takes it as its leading argument, the way
  // every runtime effect does.
  if (entry.takes_the_runtime_handle) {
    args.push_back(
        block.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner())));
  }

  for (const hir::ExprId operand : RequiredOperands(c)) {
    auto arg_or = lowerer.LowerExpr(hir_exprs.Get(operand), frame);
    if (!arg_or) return std::unexpected(std::move(arg_or.error()));
    args.push_back(block.exprs.Add(*std::move(arg_or)));
  }

  // LRM 7.12.1: reduction / ordering / locator array methods take a
  // closure. The user's `with` clause is used if present; otherwise LRM
  // defines the default as `with (item)`, which HIR-to-MIR synthesises so
  // MIR always carries the closure argument and downstream consumers see
  // one uniform shape per kind.
  if (entry.takes_closure) {
    auto closure_or = BuildArrayMethodClosure(
        lowerer, frame, hir_exprs.Get(ObjectActedOn(b)).type,
        c.with_clause.has_value() ? &*c.with_clause : nullptr);
    if (!closure_or) return std::unexpected(std::move(closure_or.error()));
    args.push_back(block.exprs.Add(*std::move(closure_or)));
  } else if (c.with_clause.has_value()) {
    throw InternalError(
        "BuiltinMethodRef with-clause on a method kind that does not "
        "accept a with-clause (LRM 7.12.1 family only)");
  }

  // A prototype the entry takes is never written at the source, so the producer
  // is what supplies it, at the result type's own canonical default. It trails
  // the operands the entry itself takes, which is where appending places it.
  if (entry.result_prototype_operand.has_value()) {
    const mir::TypeId proto_type =
        ResultPrototypeType(unit_lowerer, result_type);
    args.push_back(block.exprs.Add(
        BuildDefaultValueExpr(unit_lowerer.Unit(), block, proto_type)));
  }

  return mir::Expr{
      .data = mir::CallExpr{.callee = mir_callee, .arguments = std::move(args)},
      .type = result_type};
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerHirCallExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  // A user subroutine call carries its completion through a payload the caller
  // projects its result and its writebacks out of (LRM 13.4, 13.5). The
  // dispatch below covers the callees that are not user subroutines and have
  // their own boundary.
  if (auto lowered = LowerSubroutineCall(lowerer, frame, c, result_type)) {
    return *std::move(lowered);
  }
  return std::visit(
      Overloaded{
          [&](const hir::SystemSubroutineRef& sys) -> diag::Result<mir::Expr> {
            return LowerSystemSubroutineCall(lowerer, frame, c, sys, span);
          },
          [](const hir::StructuralSubroutineRef&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerHirCallExpr: a user subroutine call is planned before "
                "this dispatch");
          },
          [](const hir::MethodCallRef&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerHirCallExpr: a class method call is planned before this "
                "dispatch");
          },
          [](const hir::StaticMethodCallRef&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerHirCallExpr: a class method call is planned before this "
                "dispatch");
          },
          [&](const hir::BuiltinMethodRef& b) -> diag::Result<mir::Expr> {
            return LowerBuiltinMethodCall(lowerer, frame, c, b, result_type);
          },
          [&](const hir::EnumMethodRef& e) -> diag::Result<mir::Expr> {
            return LowerEnumMethod(lowerer, frame, c, e, result_type);
          },
          [&](const hir::PastValueRef& p) -> diag::Result<mir::Expr> {
            return LowerPastValueCall(lowerer, frame, p);
          },
          [&](const hir::ValueChangeRef& v) -> diag::Result<mir::Expr> {
            return LowerValueChangeCall(
                lowerer, frame, c, v, result_type, span);
          },
          [&](const hir::ForeignImportRef& imp) -> diag::Result<mir::Expr> {
            return LowerForeignImportCall(lowerer, frame, c, imp, result_type);
          },
          [](const hir::ExternalUnitSubroutineRef&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerHirCallExpr: a cross-unit subroutine call is planned "
                "before this dispatch");
          },
          [](const hir::ExternalUnitMethodRef&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerHirCallExpr: a call on another unit's object is planned "
                "before this dispatch");
          },
          [](const hir::OpaqueUnitMethodRef&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerHirCallExpr: a call on another unit's object is planned "
                "before this dispatch");
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
