#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"

#include <array>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/inc_dec_op.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Whether a deferred update may hold a reference to what this target names
// until the update region runs (LRM 10.4.2). The storage a place descends from
// decides it: a member of an object, a type-associated cell, and another
// unit's namespace variable all outlive the stretch that submits the update,
// while a procedural local's does not. Every descent step is transparent -- it
// reaches part of the same storage -- so the answer is the root's.
//
// Every form a target can take answers for itself. A form that is not a place
// cannot be one, so reaching the fallback means the target lowering produced
// something that is not an assignment target at all.
auto TargetOutlivesDeferredUpdate(const mir::Block& block, mir::ExprId expr_id)
    -> bool {
  const auto& expr = block.exprs.Get(expr_id);
  const auto not_a_target = []() -> bool {
    throw InternalError(
        "TargetOutlivesDeferredUpdate: the assignment target is not a place; "
        "the target lowering should have produced one -- please report this "
        "as a bug");
  };
  return std::visit(
      Overloaded{
          // A field is storage of its own, reached through a pointer, so it
          // outlives the update whatever holds that pointer.
          [](const mir::FieldAccessExpr&) { return true; },
          // A name reaches storage of one of two durations: a body's own
          // binding, which goes away when the stretch that holds it returns,
          // and everything a compilation unit declares once -- a
          // type-associated cell, a namespace variable, a constant -- which
          // the whole program shares and so outlives any stretch.
          [](const mir::ReferenceExpr& r) {
            return std::visit(
                Overloaded{
                    [](const mir::LocalRef&) { return false; },
                    [](const mir::DefinitionRef&) { return true; },
                    [](const mir::StaticPropertyRef&) { return true; },
                    [](const mir::EnumTableRef&) { return true; },
                    [](const mir::IntegralConstantRef&) { return true; },
                    [](const mir::StaticVariableRef&) { return true; },
                    [](const mir::ExternalUnitVariableRef&) { return true; },
                    [](const mir::ExternalStaticPropertyRef&) { return true; },
                    [](const mir::ClassConstantRef&) { return true; },
                    [](const mir::FunctionRef&) { return true; },
                },
                r.target);
          },
          // A cell reached through a pointer -- one a route stored, or a port
          // bound to another object's cell -- is structural when that pointer
          // is, so recurse through it; a `ref` formal, whose pointer roots at
          // a local, stays non-structural.
          [&](const mir::DerefExpr& d) {
            return TargetOutlivesDeferredUpdate(block, d.pointer);
          },
          // A call in target position names a place through the object it
          // dispatches on.
          [&](const mir::CallExpr& c) {
            const std::optional<mir::ExprId> receiver =
                mir::CalleeReceiver(c.callee);
            return receiver.has_value() &&
                   TargetOutlivesDeferredUpdate(block, *receiver);
          },
          // A form that names no storage: a value composed, read, converted,
          // chosen, awaited, or written somewhere else. The target lowering
          // answers a left-hand side with a place, so meeting one of these
          // means it produced something that is not an assignment target.
          [&](const mir::StringLiteral&) { return not_a_target(); },
          [&](const mir::NullLiteral&) { return not_a_target(); },
          [&](const mir::MachineBoolLiteral&) { return not_a_target(); },
          [&](const mir::MachineIntLiteral&) { return not_a_target(); },
          [&](const mir::MachineFloatLiteral&) { return not_a_target(); },
          [&](const mir::UnaryExpr&) { return not_a_target(); },
          [&](const mir::BinaryExpr&) { return not_a_target(); },
          [&](const mir::CastExpr&) { return not_a_target(); },
          [&](const mir::DynamicCastExpr&) { return not_a_target(); },
          [&](const mir::ConditionalExpr&) { return not_a_target(); },
          [&](const mir::BlockExpr&) { return not_a_target(); },
          [&](const mir::AssignExpr&) { return not_a_target(); },
          [&](const mir::AddressOfExpr&) { return not_a_target(); },
          [&](const mir::MoveExpr&) { return not_a_target(); },
          [&](const mir::ClosureExpr&) { return not_a_target(); },
          [&](const mir::CompositeExpr&) { return not_a_target(); },
          [&](const mir::AwaitExpr&) { return not_a_target(); },
          [&](const mir::WaitExpr&) { return not_a_target(); },
          [&](const mir::VectorGetExpr&) { return not_a_target(); },
      },
      expr.data);
}

// The owner of a target as the body reaches it, evaluated once at submit time
// and captured (LRM 10.4.2). A place is captured as a reference to it. A
// property of an object is captured as its object, so the write the body makes
// is opened on that object then and is the one that tells it.
auto FrozenOwner(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, const PathOwner& owner) -> PathOwner {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& outer_block = *outer_frame.current_block;
  const auto captured = [&](mir::ExprId id) {
    return SnapshotIntoClosure(unit_lowerer, outer_frame, closure, id);
  };
  return std::visit(
      Overloaded{
          [&](mir::ExprId place) -> PathOwner {
            return captured(BuildReferenceArg(
                unit, outer_block, place, outer_block.exprs.Get(place).type));
          },
          [&](const ObjectProperty& property) -> PathOwner {
            return ObjectProperty{
                .object = captured(property.object),
                .property = property.property,
                .type = property.type};
          }},
      owner);
}

// Rebuilds a target's descent onto its frozen owner. The descent is restated
// over the capture with every coordinate snapshotted by value, so the body
// writes the part the statement named at submit time (LRM 10.4.2).
auto FreezeTarget(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, const AccessPath& target) -> AccessPath {
  AccessPath frozen = target;
  frozen.owner = FrozenOwner(unit_lowerer, outer_frame, closure, target.owner);
  for (DescentStep& step : frozen.descent) {
    ForEachOperand(step, [&](mir::ExprId& coordinate) {
      coordinate =
          SnapshotIntoClosure(unit_lowerer, outer_frame, closure, coordinate);
    });
  }
  return frozen;
}

// What a deferred update writes, and where it writes it, once both are frozen
// into a closure's environment: the navigation to the place's owner is
// evaluated where the statement is reached and captured as a reference, the
// descent above it is restated over that capture with its coordinates
// snapshotted, and the value is snapshotted. LRM 10.4.2 settles both there,
// however much later the update runs.
auto FreezeShareInto(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, const Share& share_in_outer) -> Share {
  return Share{
      .place = FreezeTarget(
          unit_lowerer, outer_frame, closure, share_in_outer.place),
      .value = SnapshotIntoClosure(
          unit_lowerer, outer_frame, closure, share_in_outer.value)};
}

// The update runs after the stretch that reached the statement returns, and it
// holds a reference to the target's storage until then, so that storage has to
// outlive the stretch. LRM 10.4.2 makes the case that fails this illegal -- "It
// shall be illegal to make nonblocking assignments to automatic variables" --
// and the front end rejects it, so this stands behind that rather than in front
// of it: reaching it means a target was lowered to storage the source did not
// name.
auto CheckTargetOutlivesUpdate(
    const mir::Block& block, const PathOwner& target_in_outer,
    diag::SourceSpan span) -> diag::Result<void> {
  // A property is storage its object holds, which outlives the update whatever
  // reaches the object, as a field reached through a pointer does.
  const bool outlives = std::visit(
      Overloaded{
          [&](mir::ExprId place) {
            return TargetOutlivesDeferredUpdate(block, place);
          },
          [](const ObjectProperty&) { return true; }},
      target_in_outer);
  if (!outlives) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "a nonblocking assignment names storage that does not outlive the "
        "statement that reached it (LRM 10.4.2)");
  }
  return {};
}

// The stores one nonblocking assignment makes, as one update due later. The
// source wrote one statement, so however many places it writes they are frozen
// together and due at one placement, which is what makes a control on the
// assignment read once and land every share in the same slot (LRM 9.4.5,
// 10.4.2).
auto BuildDeferredStores(
    ProcessLowerer& process, WalkFrame frame, diag::SourceSpan span,
    const std::optional<hir::DelayOrEventControl>& control,
    std::span<const Share> shares) -> diag::Result<mir::Expr> {
  for (const Share& share : shares) {
    auto outlives = CheckTargetOutlivesUpdate(
        *frame.current_block, share.place.owner, span);
    if (!outlives) return std::unexpected(std::move(outlives.error()));
  }
  return BuildDeferredEffect(
      process, frame, control,
      [&](ClosureBuilder& closure) -> diag::Result<std::vector<Share>> {
        std::vector<Share> frozen;
        frozen.reserve(shares.size());
        for (const Share& share : shares) {
          frozen.push_back(
              FreezeShareInto(process.Owner(), frame, closure, share));
        }
        return frozen;
      },
      [&](mir::Block& body, const std::vector<Share>& frozen) {
        for (const Share& share : frozen) {
          body.AppendStmt(
              mir::ExprStmt{
                  .expr = body.exprs.Add(BuildStoreExpr(
                      process.Owner().Unit(), body, share.place,
                      share.value))});
        }
      });
}

// The operator an increment or a decrement applies to what its target holds,
// with one as the other operand (LRM 11.4.2).
auto StepOperator(hir::IncDecOp op) -> hir::BinaryOp {
  switch (op) {
    case hir::IncDecOp::kPreInc:
    case hir::IncDecOp::kPostInc:
      return hir::BinaryOp::kAdd;
    case hir::IncDecOp::kPreDec:
    case hir::IncDecOp::kPostDec:
      return hir::BinaryOp::kSub;
  }
  throw InternalError("StepOperator: unknown increment or decrement operator");
}

auto One(const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type)
    -> mir::ExprId {
  return ConvertToType(unit, block, BuildIntLiteral(unit, block, 1), type);
}

// What an assignment operator computes from what its target held and its
// operand (LRM 11.4.1): the operator carried out at the type its operands fix
// between them (LRM 11.6.1 Table 11-21, 11.8.1). What the target held is an
// operand of that operator, so it is taken to that type as a context-determined
// operand is (LRM 11.8.2). The answer is of that type, and assigning it is
// what brings it to the target's.
auto AppliedTo(
    mir::CompilationUnit& unit, mir::Block& block,
    const hir::CompoundAssignOperator& compound, mir::TypeId applied_at,
    mir::ExprId held, mir::ExprId operand) -> mir::ExprId {
  return block.exprs.Add(BuildMirBinaryExpr(
      unit, block, compound.op,
      ConvertToPropagatedType(unit, block, held, applied_at), operand,
      applied_at));
}

// Where an assignment writes and what it stores there: the right-hand side
// into the target, or, for a compound one, the operator applied to what the
// target held (LRM 11.4.1), with the target settled once for the read and the
// write.
struct AssignedStore {
  AccessPath target;
  mir::ExprId stored;
};

auto StoreOf(
    UnitLowerer& unit_lowerer, const WalkFrame& frame,
    const hir::AssignExpr& assign, AccessPath target, mir::ExprId rhs)
    -> AssignedStore {
  if (!assign.compound.has_value()) {
    return AssignedStore{.target = std::move(target), .stored = rhs};
  }
  ReadThenWritten settled =
      ReadThenWrite(unit_lowerer, frame, std::move(target));
  const mir::ExprId applied = AppliedTo(
      unit_lowerer.Unit(), *frame.current_block, *assign.compound,
      unit_lowerer.TranslateType(assign.compound->applied_at), settled.incoming,
      rhs);
  return AssignedStore{.target = std::move(settled.place), .stored = applied};
}

// An increment or a decrement of a join, as steps of `frame`'s block: the
// members settled and read together, the step applied at `type`, and each
// member given its share of the result (LRM 11.4.2, 11.4.12). Answers what the
// members held before and after.
struct SteppedJoin {
  mir::ExprId before;
  mir::ExprId after;
};

template <ExprLowerer Lowerer>
auto StepJoin(
    Lowerer& lowerer, const WalkFrame& frame, const hir::IncDecExpr& inc,
    const hir::Expr& target, std::optional<mir::TypeId> type)
    -> diag::Result<SteppedJoin> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  auto lvalue = LowerLvalue(lowerer, target, frame);
  if (!lvalue) return std::unexpected(std::move(lvalue.error()));
  auto settled = ReadThenWrite(lowerer.Owner(), frame, *std::move(lvalue));
  if (!settled) return std::unexpected(std::move(settled.error()));
  const mir::TypeId stepped_at = type.value_or(settled->lvalue.type);
  const mir::ExprId before = EvaluatedOnce(
      frame, ConvertToType(unit, block, settled->incoming, stepped_at));
  const mir::ExprId after = EvaluatedOnce(
      frame, block.exprs.Add(BuildMirBinaryExpr(
                 unit, block, StepOperator(inc.op), before,
                 One(unit, block, stepped_at), stepped_at)));
  auto stored = AppendStores(lowerer.Owner(), frame, settled->lvalue, after);
  if (!stored) return std::unexpected(std::move(stored.error()));
  return SteppedJoin{.before = before, .after = after};
}

}  // namespace

template <ExprLowerer Lowerer>
auto AssignToJoin(
    Lowerer& lowerer, const WalkFrame& frame, const hir::AssignExpr& assign,
    const hir::Expr& lhs, diag::SourceSpan span) -> diag::Result<mir::ExprId> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const auto* deferred = std::get_if<hir::NonBlockingEffect>(&assign.timing);

  auto rhs_or = lowerer.LowerExpr(lowerer.HirExprs().Get(assign.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  mir::ExprId stored = block.exprs.Add(*std::move(rhs_or));
  auto lvalue_or = LowerLvalue(lowerer, lhs, frame);
  if (!lvalue_or) return std::unexpected(std::move(lvalue_or.error()));
  Lvalue lvalue = *std::move(lvalue_or);

  // An assignment operator applies to what the members hold together, with
  // every member settled once for the read and the write (LRM 11.4.1).
  if (assign.compound.has_value()) {
    auto settled = ReadThenWrite(unit_lowerer, frame, std::move(lvalue));
    if (!settled) return std::unexpected(std::move(settled.error()));
    lvalue = std::move(settled->lvalue);
    stored = AppliedTo(
        unit, block, *assign.compound,
        unit_lowerer.TranslateType(assign.compound->applied_at),
        settled->incoming, stored);
  }

  // What is stored is evaluated once, before any member is written, which is
  // what makes `{a, b} = {b, a}` swap, and it is the assignment's value (LRM
  // 11.3.6).
  const mir::ExprId value = EvaluatedOnce(frame, stored);
  if (deferred == nullptr) {
    auto written = AppendStores(unit_lowerer, frame, lvalue, value);
    if (!written) return std::unexpected(std::move(written.error()));
    return value;
  }
  // Only a procedure has a later region to place a write in (LRM 10.4), and
  // there every member is frozen where the statement is reached (LRM 10.4.2).
  auto shares = Shares(unit_lowerer, frame, lvalue, value);
  if (!shares) return std::unexpected(std::move(shares.error()));
  if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
    auto update =
        BuildDeferredStores(lowerer, frame, span, deferred->control, *shares);
    if (!update) return std::unexpected(std::move(update.error()));
    block.AppendStmt(
        mir::ExprStmt{.expr = block.exprs.Add(*std::move(update))});
    return value;
  } else {
    throw InternalError(
        "AssignToJoin: a nonblocking assignment outside a procedure, which "
        "the front end refuses (LRM 10.4.2)");
  }
}

template <ExprLowerer Lowerer>
auto LowerHirAssignWrite(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  if (a.compound.has_value() &&
      std::holds_alternative<hir::NonBlockingEffect>(a.timing)) {
    throw InternalError(
        "LowerHirAssignWrite: compound assignment with non-blocking timing "
        "is not a legal SV form (LRM A.6.2 grammar)");
  }
  const hir::Expr& lhs = lowerer.HirExprs().Get(a.lhs);
  // A write to one place is the store itself. A write to a join takes several
  // steps, so it is a sequence of them standing where the expression does.
  if (IsJoin(lhs)) {
    BlockBuilder steps(frame);
    auto value = AssignToJoin(lowerer, steps.Frame(), a, lhs, span);
    if (!value) return std::unexpected(std::move(value.error()));
    return steps.Build(*value);
  }
  auto& block = *frame.current_block;

  auto rhs_or = lowerer.LowerExpr(lowerer.HirExprs().Get(a.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = block.exprs.Add(*std::move(rhs_or));
  auto lhs_or = lowerer.LowerLhsExpr(lhs, frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));

  // The store brings what is stored to the target's type.
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  auto [target, stored] =
      StoreOf(lowerer.Owner(), frame, a, *std::move(lhs_or), rhs_id);
  // What a write to the target is belongs to the store, and what belongs here
  // is when it takes place. Only a procedure has a later region to place one in
  // (LRM 10.4); a write a construction performs takes place where the
  // construction reaches it, so there is nothing to choose.
  if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
    if (const auto* deferred = std::get_if<hir::NonBlockingEffect>(&a.timing)) {
      const std::array shares{
          Share{.place = std::move(target), .value = stored}};
      return BuildDeferredStores(
          lowerer, frame, span, deferred->control, shares);
    }
  }
  return BuildStoreExpr(unit, block, target, stored);
}

template <ExprLowerer Lowerer>
auto LowerHirAssignExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  if (!std::holds_alternative<hir::ImmediateEffect>(a.timing)) {
    throw InternalError(
        "LowerHirAssignExpr: a nonblocking assignment is a statement and has "
        "no value (LRM 10.4.2) -- please report this as a bug");
  }
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const hir::Expr& lhs = lowerer.HirExprs().Get(a.lhs);
  if (IsJoin(lhs)) {
    auto value = AssignToJoin(lowerer, steps.Frame(), a, lhs, span);
    if (!value) return std::unexpected(std::move(value.error()));
    return steps.Build(ConvertToType(unit, body, *value, result_type));
  }

  auto rhs_or = lowerer.LowerExpr(lowerer.HirExprs().Get(a.rhs), steps.Frame());
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = body.exprs.Add(*std::move(rhs_or));
  auto target_or = lowerer.LowerLhsExpr(lhs, steps.Frame());
  if (!target_or) return std::unexpected(std::move(target_or.error()));

  // What the assignment stores is its value (LRM 11.3.6), so that is computed
  // once, held, and both written and yielded; the target read again after the
  // write would answer differently where the write is dropped.
  const auto [target, stored] =
      StoreOf(lowerer.Owner(), steps.Frame(), a, *std::move(target_or), rhs_id);
  const mir::LocalId value = steps.DeclareLocal(
      result_type, ConvertToType(unit, body, stored, result_type));
  const auto read_value = [&] {
    return body.exprs.Add(mir::MakeLocalRefExpr(value, result_type));
  };
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              BuildStoreExpr(unit, body, target, read_value()))});
  return steps.Build(read_value());
}

template <ExprLowerer Lowerer>
auto LowerHirIncDecWrite(
    Lowerer& lowerer, WalkFrame frame, const hir::IncDecExpr& inc)
    -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const hir::Expr& target = lowerer.HirExprs().Get(inc.target);
  if (IsJoin(target)) {
    BlockBuilder steps(frame);
    auto stepped = StepJoin(lowerer, steps.Frame(), inc, target, std::nullopt);
    if (!stepped) return std::unexpected(std::move(stepped.error()));
    return steps.Build(stepped->after);
  }
  auto target_or = lowerer.LowerLhsExpr(target, frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  // The target is settled once for the read and the write (LRM 11.4.2).
  const ReadThenWritten settled =
      ReadThenWrite(lowerer.Owner(), frame, *std::move(target_or));
  const mir::TypeId type = block.exprs.Get(settled.incoming).type;
  return BuildStoreExpr(
      unit, block, settled.place,
      block.exprs.Add(BuildMirBinaryExpr(
          unit, block, StepOperator(inc.op), settled.incoming,
          One(unit, block, type), type)));
}

template <ExprLowerer Lowerer>
auto LowerHirIncDecExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const bool yields_before =
      inc.op == hir::IncDecOp::kPostInc || inc.op == hir::IncDecOp::kPostDec;
  const hir::Expr& target = lowerer.HirExprs().Get(inc.target);
  if (IsJoin(target)) {
    auto stepped = StepJoin(lowerer, steps.Frame(), inc, target, result_type);
    if (!stepped) return std::unexpected(std::move(stepped.error()));
    return steps.Build(yields_before ? stepped->before : stepped->after);
  }
  auto target_or = lowerer.LowerLhsExpr(target, steps.Frame());
  if (!target_or) return std::unexpected(std::move(target_or.error()));

  // The value before the step and the value after it are both held, since a
  // postfix form yields the first and the write stores the second (LRM
  // 11.4.2), and the target is settled once for the read and the write.
  const ReadThenWritten settled =
      ReadThenWrite(lowerer.Owner(), steps.Frame(), *std::move(target_or));
  const mir::LocalId before = steps.DeclareLocal(
      result_type, ConvertToType(unit, body, settled.incoming, result_type));
  const auto read = [&](mir::LocalId local) {
    return body.exprs.Add(mir::MakeLocalRefExpr(local, result_type));
  };
  const mir::LocalId after = steps.DeclareLocal(
      result_type, body.exprs.Add(BuildMirBinaryExpr(
                       unit, body, StepOperator(inc.op), read(before),
                       One(unit, body, result_type), result_type)));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(
              BuildStoreExpr(unit, body, settled.place, read(after)))});
  return steps.Build(read(yields_before ? before : after));
}

template auto AssignToJoin(
    ProcessLowerer&, const WalkFrame&, const hir::AssignExpr&, const hir::Expr&,
    diag::SourceSpan) -> diag::Result<mir::ExprId>;
template auto AssignToJoin(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::AssignExpr&,
    const hir::Expr&, diag::SourceSpan) -> diag::Result<mir::ExprId>;
template auto LowerHirAssignWrite(
    ProcessLowerer&, WalkFrame, const hir::AssignExpr&, diag::SourceSpan)
    -> diag::Result<mir::Expr>;
template auto LowerHirAssignWrite(
    const StructuralScopeLowerer&, WalkFrame, const hir::AssignExpr&,
    diag::SourceSpan) -> diag::Result<mir::Expr>;
template auto LowerHirAssignExpr(
    ProcessLowerer&, WalkFrame, const hir::AssignExpr&, diag::SourceSpan,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssignExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::AssignExpr&,
    diag::SourceSpan, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirIncDecWrite(
    ProcessLowerer&, WalkFrame, const hir::IncDecExpr&)
    -> diag::Result<mir::Expr>;
template auto LowerHirIncDecWrite(
    const StructuralScopeLowerer&, WalkFrame, const hir::IncDecExpr&)
    -> diag::Result<mir::Expr>;
template auto LowerHirIncDecExpr(
    ProcessLowerer&, WalkFrame, const hir::IncDecExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirIncDecExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::IncDecExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
