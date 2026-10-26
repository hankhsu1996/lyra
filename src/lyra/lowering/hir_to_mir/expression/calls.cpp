#include "lyra/lowering/hir_to_mir/expression/calls.hpp"

#include <cstddef>
#include <expected>
#include <format>
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
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/covergroup_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/array_iterator_method.hpp"
#include "lyra/lowering/hir_to_mir/expression/dpi_call.hpp"
#include "lyra/lowering/hir_to_mir/expression/enum_method.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/real_conversion.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
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
#include "lyra/lowering/hir_to_mir/expression/system/value_dump.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/sampled_history.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/sensitivity_wait.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/subroutine_call.hpp"
#include "lyra/lowering/hir_to_mir/value_operations.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/storage_part.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/system_subroutine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Where a traversal's completion components sit: the SV int the method answers
// with, then the index it visited (LRM 7.9.4 -- 7.9.7).
constexpr base::ComponentIndex kTraversalFound{0};
constexpr base::ComponentIndex kTraversalVisitedIndex{1};

// LRM 7.9.4 -- 7.9.7 associative traversal (`m.first(idx)` / `last` / `next` /
// `prev`), with the array and the index it is asked of lowered as the first of
// its steps.
template <ExprLowerer Lowerer>
auto LowerAssociativeTraversal(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& c,
    const hir::BuiltinMethodRef& b, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  // The index whose neighbour is asked for (LRM 7.9.4); the array it is asked
  // of is the object the entry acts on.
  const std::vector<hir::ExprId> operands = RequiredOperands(c, 1);
  auto& unit_lowerer = lowerer.Owner();
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
  auto idx_lhs_or = lowerer.LowerLhsExpr(hir_exprs.Get(idx_hir), step_frame);
  if (!idx_lhs_or) return std::unexpected(std::move(idx_lhs_or.error()));
  return BuildAssociativeTraversal(
      unit_lowerer, steps, b.method, map_read_id, *std::move(idx_lhs_or),
      key_type, result_type);
}

// A structural context admits only a pure value query -- one that reads state
// and sequences nothing. Every other family is an effect that needs a process
// body, so it has no structural lowering.
auto RejectStructuralEffect(diag::SourceSpan span) -> diag::Result<mir::Expr> {
  return diag::Fail(
      span, diag::DiagCode::kUnsupportedStructuralExpressionForm,
      "this system subroutine is not yet supported in a continuous assignment");
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
                  lowerer, frame, call, term, span);
            } else {
              return RejectStructuralEffect(span);
            }
          },
          [&](const support::ProgramControlSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerProgramControlSystemSubroutineCall(
                  lowerer, *frame.current_block, span);
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
          [&](const support::ValueDumpSystemSubroutineInfo& dump)
              -> diag::Result<mir::Expr> {
            if constexpr (kProcedural) {
              return LowerValueDumpSystemSubroutineCall(
                  lowerer, frame, call, dump, span);
            } else {
              return RejectStructuralEffect(span);
            }
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
          [](const support::CoverageDatabaseSystemSubroutineInfo&)
              -> diag::Result<mir::Expr> {
            throw InternalError(
                "a coverage database function is the coverage library's, "
                "called before system subroutines are dispatched -- please "
                "report this as a bug");
          },
      },
      desc.semantic);
}

// An operand as the entry reads it. One read as a position is an ordinal the
// source wrote in a type of its own (LRM 6.16.2, 7.10.2), so it is brought to
// the position type here, where it is written; every other operand is stated
// as it stands.
auto StatedAsRead(
    mir::CompilationUnit& unit, mir::Block& block,
    support::OperandReading reading, mir::ExprId operand) -> mir::ExprId {
  switch (reading) {
    case support::OperandReading::kPosition:
      return BuildOrdinalPosition(unit, block, operand);
    case support::OperandReading::kHeld:
    case support::OperandReading::kBits:
    case support::OperandReading::kNumber:
    case support::OperandReading::kTyped:
    case support::OperandReading::kKey:
    case support::OperandReading::kMachine:
      return operand;
  }
  throw InternalError("StatedAsRead: unknown operand reading");
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
  mir::CompilationUnit& unit = lowerer.Owner().Unit();

  // The entry reads its operands in one order: the object it acts on where it
  // has one, the engine where it reaches it, the report of the evaluation
  // calling it where it states what it reaches there, then these.
  const std::size_t leading = (b.receiver.has_value() ? 1 : 0) +
                              (entry.takes_the_runtime_handle ? 1 : 0);
  const auto lower_operands =
      [&](WalkFrame at) -> diag::Result<std::vector<mir::ExprId>> {
    mir::Block& into = *at.current_block;
    std::vector<mir::ExprId> args;
    args.reserve(c.arguments.size() + 1);
    if (entry.takes_the_read_report) {
      args.push_back(BuildReportHandedOn(unit, at));
    }
    for (const hir::ExprId operand : RequiredOperands(c)) {
      auto arg_or = lowerer.LowerExpr(hir_exprs.Get(operand), at);
      if (!arg_or) return std::unexpected(std::move(arg_or.error()));
      mir::ExprId lowered = into.exprs.Add(*std::move(arg_or));
      // An entry acting on an object it is handed as its first operand is
      // handed the value reaching that object, read where a variable holds
      // it, as an access's receiver is.
      if (entry.reaches_an_object && !b.receiver.has_value() && args.empty()) {
        lowered = into.AsValue(unit.types, lowered);
      }
      args.push_back(StatedAsRead(
          unit, into, entry.operands.At(leading + args.size()), lowered));
    }
    return args;
  };
  const auto call_on = [&](mir::Block& into,
                           std::optional<mir::ExprId> receiver,
                           std::vector<mir::ExprId> args) -> mir::Expr {
    // A question asked of a whole value is asked as every such question is:
    // through the type's own body where it has one -- a structure's or a
    // union's method, the body a unit states for an array -- and of the
    // library otherwise.
    if (receiver.has_value() && IsWholeValueOperation(b.method)) {
      return into.exprs.Get(BuildValueOperation(
          unit, into, b.method, receiver, std::move(args), result_type));
    }
    return BuildBuiltinMethodCall(
        unit_lowerer, into, b.method, receiver, std::move(args), result_type);
  };

  auto& block = *frame.current_block;
  // A method that changes the object it acts on takes the place that object
  // stands in, so the change lands where the source named it; one that does not
  // is entered on the object where it lies, and reads it there if it takes it
  // by value. An object lying where another member of an untagged union may
  // have written is changed in a copy read as its type holds a value, which is
  // then written back, since the method reads what it changes (LRM 7.3.2).
  std::optional<mir::ExprId> receiver;
  std::optional<AccessPath> written_back;
  if (b.receiver.has_value()) {
    const hir::Expr& object = hir_exprs.Get(*b.receiver);
    if (entry.mutates_receiver) {
      auto recv_or = lowerer.LowerLhsExpr(object, frame);
      if (!recv_or) return std::unexpected(std::move(recv_or.error()));
      if (ReachesOverlaidStorage(*recv_or)) {
        ReadThenWritten changed =
            ReadThenWrite(lowerer.Owner(), frame, *std::move(recv_or));
        const mir::TypeId type = block.exprs.Get(changed.incoming).type;
        receiver =
            block.LocalPlace(DeclareLocal(frame, changed.incoming), type);
        written_back = std::move(changed.place);
      } else {
        receiver = PathPlace(unit, block, *recv_or);
      }
    } else if (IsTagCheckedRead(lowerer, frame, object)) {
      // An object reached through a member a tag guards is entered where it
      // lies among the steps that check the tag (LRM 11.9), after every
      // operand of the call, so nothing else the expression evaluates runs
      // between a check and the call it guards.
      return LowerTagCheckedUse(
          lowerer, frame, object,
          [&](BlockBuilder& steps,
              const AccessPath& path) -> diag::Result<mir::ExprId> {
            mir::Block& body = steps.Body();
            auto args = lower_operands(steps.Frame());
            if (!args) return std::unexpected(std::move(args.error()));
            const mir::ExprId object_place = PathAsPlace(unit, body, path);
            return body.exprs.Add(call_on(
                body, mir::ReadIfOverlaid(unit, body, object_place),
                *std::move(args)));
          });
    } else {
      auto recv_or = lowerer.LowerAsPlace(object, frame);
      if (!recv_or) return std::unexpected(std::move(recv_or.error()));
      receiver = mir::ReadIfOverlaid(
          unit, block, block.exprs.Add(*std::move(recv_or)));
    }
  }
  auto args = lower_operands(frame);
  if (!args) return std::unexpected(std::move(args.error()));
  mir::Expr call = call_on(block, receiver, *std::move(args));
  if (!written_back.has_value()) {
    return call;
  }
  // What an untagged union may hold changes only through a method answering
  // nothing -- an array's ordering methods (LRM 7.3, 7.12.2) -- so the write
  // back is all the call stands for.
  const mir::Type& answered = unit.types.Get(result_type);
  if (!answered.Is<mir::VoidType>()) {
    throw InternalError(
        "LowerBuiltinMethodCall: a method changing overlaid storage answers a "
        "value");
  }
  block.AppendStmt(mir::ExprStmt{.expr = block.exprs.Add(std::move(call))});
  return BuildStoreExpr(unit, block, *written_back, block.Read(*receiver));
}

// A call of a hook a class has without declaring it, where no class of its
// lineage declares one either or the class is an interface class -- the calls
// the library runs, where a method of the source runs no other (the plan of a
// method call answers those). An interface class's hook runs what the object's
// own class runs, the reading Lyra takes where LRM 8.26.9 calls it virtual and
// 18.6.2 calls a class's not; a class's runs the built-in one, which does
// nothing (LRM 18.6.2).
template <ExprLowerer Lowerer>
auto LowerLibraryHookCall(
    Lowerer& lowerer, WalkFrame frame, const hir::MethodCallRef& ref,
    const hir::BuiltinHookCallee& hook, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const bool runs_objects_hook =
      unit_lowerer.Hir().classes.IsInterfaceClass(hook.cls);
  const support::BuiltinFn fn = [&] {
    switch (hook.hook) {
      case hir::RandomizationHook::kPreRandomize:
        return runs_objects_hook ? support::BuiltinFn::kObjectPreRandomize
                                 : support::BuiltinFn::kBuiltinPreRandomize;
      case hir::RandomizationHook::kPostRandomize:
        return runs_objects_hook ? support::BuiltinFn::kObjectPostRandomize
                                 : support::BuiltinFn::kBuiltinPostRandomize;
    }
    throw InternalError("LowerLibraryHookCall: unknown RandomizationHook");
  }();
  auto object = std::visit(
      Overloaded{
          [&](const hir::HandleReceiver& handle) -> diag::Result<mir::ExprId> {
            auto lowered =
                lowerer.LowerExpr(lowerer.HirExprs().Get(handle.expr), frame);
            if (!lowered) return std::unexpected(std::move(lowered.error()));
            return block.AsValue(
                unit.types, block.exprs.Add(*std::move(lowered)));
          },
          [&](const hir::SelfReceiver&) -> diag::Result<mir::ExprId> {
            return block.exprs.Add(BuildThisHandle(frame));
          },
          [&](const hir::SuperReceiver&) -> diag::Result<mir::ExprId> {
            return block.exprs.Add(BuildThisHandle(frame));
          }},
      ref.receiver);
  if (!object) return std::unexpected(std::move(object.error()));
  return BuildBuiltinMethodCall(
      unit_lowerer, block, fn, std::nullopt,
      {StatedAsRead(
          unit, block, support::RuntimeEntryOf(fn).operands.At(0), *object)},
      result_type);
}

}  // namespace

auto BuildBuiltinMethodCall(
    const UnitLowerer& unit_lowerer, mir::Block& block,
    support::BuiltinFn method, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result_type) -> mir::Expr {
  const support::RuntimeEntry entry = support::RuntimeEntryOf(method);

  // The entry's operands after the object it acts on, in the order it takes
  // them. The engine rides immediately after that object, and first where
  // there is none.
  std::vector<mir::ExprId> arguments;
  arguments.reserve(operands.size() + 2);
  if (entry.takes_the_runtime_handle) {
    arguments.push_back(
        block.exprs.Add(BuildCurrentRuntimeCallExpr(unit_lowerer)));
  }
  arguments.insert(arguments.end(), operands.begin(), operands.end());

  // An entry answering a wait answers the wait its caller then stops at, so
  // that is what the call is, whatever the source-level call it came from
  // yields. The source type would say `void` for a task like LRM 9.7's
  // `await`, and nothing could be stopped at.
  const mir::TypeId answered =
      entry.answers_a_wait ? unit_lowerer.Unit().builtins.wait : result_type;
  return mir::MakeBuiltinCall(
      unit_lowerer.Unit(), block, method, receiver, std::move(arguments),
      answered);
}

auto BuildAssociativeTraversal(
    UnitLowerer& unit_lowerer, BlockBuilder& steps, support::BuiltinFn method,
    mir::ExprId array, AccessPath index, mir::TypeId key_type,
    mir::TypeId result_type) -> mir::Expr {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& body = steps.Body();
  // `first` / `last` ignore the index's current value and `next` / `prev` read
  // it as the search bound, so every form takes it as an ordinary operand. The
  // index is then written with the one visited, so it is read and written
  // through one path.
  const ReadThenWritten visited_into =
      ReadThenWrite(unit_lowerer, steps.Frame(), std::move(index));

  const mir::TypeId payload_type =
      CompletionPayloadType(unit, {result_type, key_type});
  const mir::ExprId query_id = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = method, .receiver = array},
                  .arguments = {visited_into.incoming}},
          .type = payload_type});
  const mir::LocalId completion =
      steps.Bindings().DeclareAnonymous(payload_type);
  body.AppendStmt(mir::LocalDeclStmt{.target = completion, .init = query_id});

  const mir::ExprId visited_id = ProjectCompletionComponent(
      body, completion, payload_type, kTraversalVisitedIndex, key_type);
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              BuildStoreExpr(unit, body, visited_into.place, visited_id))});

  return steps.Build(ProjectCompletionComponent(
      body, completion, payload_type, kTraversalFound, result_type));
}

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
            // A coverage database function is the coverage library's function
            // of the same name (LRM 19.9), called as a user function is.
            if (const auto* database =
                    std::get_if<support::CoverageDatabaseSystemSubroutineInfo>(
                        &support::LookupSystemSubroutine(sys.id).semantic)) {
              return LowerCoverageDatabaseCall(
                  lowerer, frame, database->call, c);
            }
            auto lowered =
                LowerSystemSubroutineCall(lowerer, frame, c, sys, span);
            // Every operator around the call was typed against the type the
            // front end gave it, so a call stated at another type hands them
            // an operand they were not typed for.
            if (lowered && lowered->type != result_type) {
              throw InternalError(
                  std::format(
                      "LowerHirCallExpr: the system subroutine {} is lowered "
                      "at another type than the call has -- please report "
                      "this as a bug",
                      support::LookupSystemSubroutine(sys.id).name));
            }
            return lowered;
          },
          [](const hir::StructuralSubroutineRef&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerHirCallExpr: a user subroutine call is planned before "
                "this dispatch");
          },
          // A method call the source's methods do not answer is a hook the
          // library runs; every other is planned before this dispatch.
          [&](const hir::MethodCallRef& ref) -> diag::Result<mir::Expr> {
            const auto* hook = std::get_if<hir::BuiltinHookCallee>(&ref.callee);
            if (hook == nullptr) {
              throw InternalError(
                  "LowerHirCallExpr: a class method call is planned before "
                  "this dispatch");
            }
            return LowerLibraryHookCall(
                lowerer, frame, ref, *hook, result_type);
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
          [&](const hir::ArrayIteratorMethodRef& a) -> diag::Result<mir::Expr> {
            return LowerArrayIteratorMethod(lowerer, frame, c, a, result_type);
          },
          [&](const hir::PastValueRef& p) -> diag::Result<mir::Expr> {
            return LowerPastValueCall(lowerer, frame, c, p);
          },
          [&](const hir::ValueChangeRef& v) -> diag::Result<mir::Expr> {
            return LowerValueChangeCall(
                lowerer, frame, c, v, result_type, span);
          },
          [&](const hir::ForeignImportRef& imp) -> diag::Result<mir::Expr> {
            return LowerForeignImportCall(lowerer, frame, c, imp, result_type);
          },
          [](const hir::NamespaceSubroutineRef&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerHirCallExpr: a package subroutine call is planned "
                "before this dispatch");
          },
          [](const hir::PublishedScopeMethodRef&) -> diag::Result<mir::Expr> {
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
