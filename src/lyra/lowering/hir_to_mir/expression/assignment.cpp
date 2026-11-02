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
#include "lyra/lowering/hir_to_mir/lvalue.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The owner of a target as the body reaches it, evaluated once at submit time
// and captured (LRM 10.4.2). A place is captured as a reference to it. A
// property of an object is captured as its object, so the write the body makes
// is opened on that object then and is the one that tells it.
auto FrozenOwner(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, const PathOwner& owner) -> PathOwner {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder outer_block = outer_frame.Builder();
  const auto captured = [&](mir::ExprId id) {
    return SnapshotValueIntoClosure(unit_lowerer, outer_frame, closure, id);
  };
  return std::visit(
      Overloaded{
          [&](mir::ExprId place) -> PathOwner {
            return SnapshotPlaceIntoClosure(
                unit_lowerer, outer_frame, closure,
                BuildReferenceArg(
                    unit, outer_block, place, outer_block.Get(place).type));
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
      coordinate = SnapshotValueIntoClosure(
          unit_lowerer, outer_frame, closure, coordinate);
    });
  }
  return frozen;
}

// What a deferred update writes, and where it writes it, once both are frozen
// into a closure's environment: the value is snapshotted, then the navigation
// to the place's owner is evaluated where the statement is reached and
// captured as a reference, and the descent above it is restated over that
// capture with its coordinates snapshotted. LRM 10.4.2 settles both there,
// however much later the update runs, in an order it leaves open, and the
// value runs first as it does for every other write.
auto FreezeShareInto(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, const Share& share_in_outer) -> Share {
  const mir::ExprId value = SnapshotValueIntoClosure(
      unit_lowerer, outer_frame, closure, share_in_outer.value);
  return Share{
      .place = FreezeTarget(
          unit_lowerer, outer_frame, closure, share_in_outer.place),
      .value = value};
}

// The stores one nonblocking assignment makes, as one update due later. The
// source wrote one statement, so however many places it writes they are frozen
// together and due at one placement, which is what makes a control on the
// assignment read once and land every share in the same slot (LRM 9.4.5,
// 10.4.2).
auto BuildDeferredStores(
    ProcessLowerer& process, WalkFrame frame,
    const std::optional<hir::DelayOrEventControl>& control,
    std::span<const Share> shares) -> diag::Result<mir::Expr> {
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
      [&](mir::Builder body, const std::vector<Share>& frozen) {
        for (const Share& share : frozen) {
          body.AppendStmt(
              mir::ExprStmt{
                  .expr = body.Add(BuildStoreExpr(
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

auto One(const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type)
    -> mir::ExprId {
  return ConvertToType(unit, block, mir::BuildIntLiteral(unit, block, 1), type);
}

// What an assignment operator computes from what its target held and its
// operand (LRM 11.4.1): the operator carried out at the type its operands fix
// between them (LRM 11.6.1 Table 11-21, 11.8.1). What the target held is an
// operand of that operator, so it is taken to that type as a context-determined
// operand is (LRM 11.8.2). The answer is of that type, and assigning it is
// what brings it to the target's.
auto AppliedTo(
    mir::CompilationUnit& unit, mir::Builder block,
    const hir::CompoundAssignOperator& compound, mir::TypeId applied_at,
    mir::ExprId held, mir::ExprId operand) -> mir::ExprId {
  return block.Add(BuildMirBinaryExpr(
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
      unit_lowerer.Unit(), frame.Builder(), *assign.compound,
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
  mir::Builder block = frame.Builder();
  auto lvalue = LowerLvalue(lowerer, target, frame);
  if (!lvalue) return std::unexpected(std::move(lvalue.error()));
  auto settled = ReadThenWrite(lowerer.Owner(), frame, *std::move(lvalue));
  if (!settled) return std::unexpected(std::move(settled.error()));
  const mir::TypeId stepped_at = type.value_or(settled->lvalue.type);
  const mir::ExprId before = EvaluatedOnce(
      frame, ConvertToType(unit, block, settled->incoming, stepped_at));
  const mir::ExprId after = EvaluatedOnce(
      frame, block.Add(BuildMirBinaryExpr(
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
    const hir::Expr& lhs) -> diag::Result<mir::ExprId> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Builder block = frame.Builder();
  const auto* deferred = std::get_if<hir::NonBlockingEffect>(&assign.timing);

  auto rhs_or = lowerer.LowerExpr(lowerer.HirExprs().Get(assign.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  mir::ExprId stored = block.Add(*std::move(rhs_or));
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
        BuildDeferredStores(lowerer, frame, deferred->control, *shares);
    if (!update) return std::unexpected(std::move(update.error()));
    block.AppendStmt(mir::ExprStmt{.expr = block.Add(*std::move(update))});
    return value;
  } else {
    throw InternalError(
        "AssignToJoin: a nonblocking assignment outside a procedure, which "
        "the front end refuses (LRM 10.4.2)");
  }
}

template <ExprLowerer Lowerer>
auto LowerHirAssignWrite(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignExpr& a)
    -> diag::Result<mir::Expr> {
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
    auto value = AssignToJoin(lowerer, steps.Frame(), a, lhs);
    if (!value) return std::unexpected(std::move(value.error()));
    return steps.Build(*value);
  }
  mir::Builder block = frame.Builder();

  auto rhs_or = lowerer.LowerExpr(lowerer.HirExprs().Get(a.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = block.Add(*std::move(rhs_or));
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
      return BuildDeferredStores(lowerer, frame, deferred->control, shares);
    }
  }
  return BuildStoreExpr(unit, block, target, stored);
}

template <ExprLowerer Lowerer>
auto LowerHirAssignExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignExpr& a,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  if (!std::holds_alternative<hir::ImmediateEffect>(a.timing)) {
    throw InternalError(
        "LowerHirAssignExpr: a nonblocking assignment is a statement and has "
        "no value (LRM 10.4.2) -- please report this as a bug");
  }
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  BlockBuilder steps(frame);
  mir::Builder body = steps.Body();
  const hir::Expr& lhs = lowerer.HirExprs().Get(a.lhs);
  if (IsJoin(lhs)) {
    auto value = AssignToJoin(lowerer, steps.Frame(), a, lhs);
    if (!value) return std::unexpected(std::move(value.error()));
    return steps.Build(ConvertToType(unit, body, *value, result_type));
  }

  auto rhs_or = lowerer.LowerExpr(lowerer.HirExprs().Get(a.rhs), steps.Frame());
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = body.Add(*std::move(rhs_or));
  auto target_or = lowerer.LowerLhsExpr(lhs, steps.Frame());
  if (!target_or) return std::unexpected(std::move(target_or.error()));

  // What the assignment stores is its value (LRM 11.3.6), so that is computed
  // once, held, and both written and yielded; the target read again after the
  // write would answer differently where the write is dropped.
  const auto [target, stored] =
      StoreOf(lowerer.Owner(), steps.Frame(), a, *std::move(target_or), rhs_id);
  const mir::LocalId value = steps.DeclareLocal(
      result_type, ConvertToType(unit, body, stored, result_type));
  const auto read_value = [&] { return body.ReadLocal(value, result_type); };
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.Add(BuildStoreExpr(unit, body, target, read_value()))});
  return steps.Build(read_value());
}

template <ExprLowerer Lowerer>
auto LowerHirIncDecWrite(
    Lowerer& lowerer, WalkFrame frame, const hir::IncDecExpr& inc)
    -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Builder block = frame.Builder();
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
  const mir::TypeId type = block.Get(settled.incoming).type;
  return BuildStoreExpr(
      unit, block, settled.place,
      block.Add(BuildMirBinaryExpr(
          unit, block, StepOperator(inc.op), settled.incoming,
          One(unit, block, type), type)));
}

template <ExprLowerer Lowerer>
auto LowerHirIncDecExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  BlockBuilder steps(frame);
  mir::Builder body = steps.Body();
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
    return body.ReadLocal(local, result_type);
  };
  const mir::ExprId lhs = read(before);
  const mir::ExprId rhs = One(unit, body, result_type);
  const mir::LocalId after = steps.DeclareLocal(
      result_type,
      body.Add(BuildMirBinaryExpr(
          unit, body, StepOperator(inc.op), lhs, rhs, result_type)));
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.Add(
              BuildStoreExpr(unit, body, settled.place, read(after)))});
  return steps.Build(read(yields_before ? before : after));
}

template auto AssignToJoin(
    ProcessLowerer&, const WalkFrame&, const hir::AssignExpr&, const hir::Expr&)
    -> diag::Result<mir::ExprId>;
template auto AssignToJoin(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::AssignExpr&,
    const hir::Expr&) -> diag::Result<mir::ExprId>;
template auto LowerHirAssignWrite(
    ProcessLowerer&, WalkFrame, const hir::AssignExpr&)
    -> diag::Result<mir::Expr>;
template auto LowerHirAssignWrite(
    const StructuralScopeLowerer&, WalkFrame, const hir::AssignExpr&)
    -> diag::Result<mir::Expr>;
template auto LowerHirAssignExpr(
    ProcessLowerer&, WalkFrame, const hir::AssignExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirAssignExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::AssignExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
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
