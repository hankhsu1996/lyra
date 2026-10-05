#include "lyra/lowering/hir_to_mir/expression/assignment.hpp"

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
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
#include "lyra/hir/procedural_body.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/deferred_effect.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_builders.hpp"

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
          // type-associated cell, a namespace variable, a generated descriptor
          // -- which the whole program shares and so outlives any stretch.
          [](const mir::ReferenceExpr& r) {
            return std::visit(
                Overloaded{
                    [](const mir::LocalRef&) { return false; },
                    [](const mir::DefinitionRef&) { return true; },
                    [](const mir::StaticPropertyRef&) { return true; },
                    [](const mir::TypeDescriptorRef&) { return true; },
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
          // dispatches on. A join stands for the destructuring LHS it came
          // from, which writes each operand through that operand's own root, so
          // the whole outlives the update exactly when every operand does.
          [&](const mir::CallExpr& c) {
            const std::optional<mir::ExprId> receiver =
                mir::CalleeReceiver(c.callee);
            if (!receiver.has_value()) {
              return false;
            }
            if (!TargetOutlivesDeferredUpdate(block, *receiver)) {
              return false;
            }
            if (mir::DirectBuiltinFn(c) != support::BuiltinFn::kConcat) {
              return true;
            }
            return std::ranges::all_of(c.arguments, [&](mir::ExprId op) {
              return TargetOutlivesDeferredUpdate(block, op);
            });
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
    for (mir::ExprId& coordinate : step.operands) {
      coordinate =
          SnapshotIntoClosure(unit_lowerer, outer_frame, closure, coordinate);
    }
  }
  return frozen;
}

// What a deferred update writes, and where it writes it, once both are frozen
// into a closure's environment: the navigation to the target's owner is
// evaluated where the statement is reached and captured as a reference, the
// descent above it is restated over that capture with its coordinates
// snapshotted, and each operand is snapshotted. LRM 10.4.2 settles both there,
// however much later the update runs.
struct FrozenAssignment {
  AccessPath target;
  std::vector<mir::ExprId> operands;
};

auto FreezeAssignmentInto(
    UnitLowerer& unit_lowerer, const WalkFrame& outer_frame,
    ClosureBuilder& closure, const AccessPath& target_in_outer,
    std::span<const mir::ExprId> operands_in_outer) -> FrozenAssignment {
  FrozenAssignment frozen{
      .target =
          FreezeTarget(unit_lowerer, outer_frame, closure, target_in_outer),
      .operands = {}};
  frozen.operands.reserve(operands_in_outer.size());
  for (const mir::ExprId op : operands_in_outer) {
    frozen.operands.push_back(
        SnapshotIntoClosure(unit_lowerer, outer_frame, closure, op));
  }
  return frozen;
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

// Applies a target's write where the statement is reached, or as an update due
// later. `effect_fn(block, target, operands)`
// builds the write into `block`, the same node for either timing, so a target
// says nothing about when its write happens (LRM 10.4).
template <typename EffectFn>
auto ApplyAssignEffect(
    ProcessLowerer& process, WalkFrame frame, const hir::EffectTiming& timing,
    diag::SourceSpan span, const AccessPath& target_in_outer,
    std::span<const mir::ExprId> operands_in_outer, EffectFn effect_fn)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const auto* deferred = std::get_if<hir::NonBlockingEffect>(&timing);
  if (deferred == nullptr) {
    return effect_fn(block, target_in_outer, operands_in_outer);
  }
  auto outlives = CheckTargetOutlivesUpdate(block, target_in_outer.owner, span);
  if (!outlives) return std::unexpected(std::move(outlives.error()));
  return BuildDeferredEffect(
      process, frame, deferred->control,
      [&](ClosureBuilder& closure) -> diag::Result<FrozenAssignment> {
        return FreezeAssignmentInto(
            process.Owner(), frame, closure, target_in_outer,
            operands_in_outer);
      },
      [&](mir::Block& body, const FrozenAssignment& frozen) {
        body.AppendStmt(
            mir::ExprStmt{
                .expr = body.exprs.Add(effect_fn(
                    body, frozen.target,
                    std::span<const mir::ExprId>(frozen.operands)))});
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

}  // namespace

auto Destructure(
    ProcessLowerer& process, const WalkFrame& frame,
    const hir::AssignExpr& assign, const hir::ConcatExpr& lhs_concat,
    diag::SourceSpan span) -> diag::Result<mir::ExprId> {
  if (assign.compound_op.has_value()) {
    throw InternalError(
        "Destructure: a compound assignment to a concatenation is not a legal "
        "SV form (LRM A.6.2 grammar)");
  }
  const hir::ProceduralBody& hir_proc = process.HirBody();
  mir::CompilationUnit& unit = process.Owner().Unit();
  mir::Block& block = *frame.current_block;

  std::vector<std::uint64_t> part_widths;
  part_widths.reserve(lhs_concat.operands.size());
  mir::IntegralStateKind state_kind = mir::IntegralStateKind::kTwoState;
  std::uint64_t total_width = 0;
  for (const hir::ExprId op_id : lhs_concat.operands) {
    const hir::Expr& op = hir_proc.exprs.Get(op_id);
    if (!process.Owner().Hir().types.Get(op.type).IsIntegral()) {
      throw InternalError(
          "Destructure: a destructuring operand is not an integral type");
    }
    // Width and state domain are properties of the operand's MIR type, which
    // is what the bound value is sliced against.
    const auto& packed =
        unit.types.Get(process.Owner().TranslateType(op.type)).PackedShape();
    const std::uint64_t w = packed.BitWidth();
    part_widths.push_back(w);
    total_width += w;
    if (packed.state_kind == mir::IntegralStateKind::kFourState) {
      state_kind = mir::IntegralStateKind::kFourState;
    }
  }
  if (total_width == 0) {
    throw InternalError("Destructure: the total width must be positive");
  }

  // The right-hand side is evaluated once and the bound value is what gets
  // distributed, which is what makes `{a, b} = {b, a}` swap.
  const mir::TypeId bound_type =
      mir::PackedVectorOf(unit.types, total_width, state_kind);
  auto rhs_or = process.LowerExpr(hir_proc.exprs.Get(assign.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::LocalId bound = frame.bindings->DeclareAnonymous(bound_type);
  block.AppendStmt(
      mir::LocalDeclStmt{
          .target = bound,
          .init = ConvertToType(
              unit, block, block.exprs.Add(*std::move(rhs_or)), bound_type)});
  const auto read_bound = [&] {
    return block.exprs.Add(mir::MakeLocalRefExpr(bound, bound_type));
  };

  // MSB-first: operands[0] takes the high bits, operands.back() the low ones.
  std::vector<DestructuredPart> parts;
  parts.reserve(lhs_concat.operands.size());
  std::uint64_t offset = total_width;
  for (std::size_t i = 0; i < lhs_concat.operands.size(); ++i) {
    const std::uint64_t w = part_widths[i];
    offset -= w;
    const hir::Expr& part = hir_proc.exprs.Get(lhs_concat.operands[i]);
    auto part_lhs_or = process.LowerLhsExpr(part, frame);
    if (!part_lhs_or) {
      return std::unexpected(std::move(part_lhs_or.error()));
    }
    const mir::TypeId part_type = process.Owner().TranslateType(part.type);
    const mir::ExprId share = block.exprs.Add(BuildPackedBitsRead(
        process.Owner(), block, read_bound(), offset, w,
        mir::PackedVectorOf(unit.types, w, state_kind)));
    parts.push_back(
        DestructuredPart{
            .target = *std::move(part_lhs_or),
            .value = ConvertToType(unit, block, share, part_type)});
  }

  if (const auto* deferred =
          std::get_if<hir::NonBlockingEffect>(&assign.timing)) {
    auto effect_or = BuildDestructuredDeferredAssign(
        process, frame, span, deferred->control, parts);
    if (!effect_or) return std::unexpected(std::move(effect_or.error()));
    block.AppendStmt(
        mir::ExprStmt{.expr = block.exprs.Add(*std::move(effect_or))});
  } else {
    for (const DestructuredPart& part : parts) {
      block.AppendStmt(
          mir::ExprStmt{
              .expr = block.exprs.Add(
                  BuildStoreExpr(unit, block, part.target, part.value))});
    }
  }
  return read_bound();
}

template <ExprLowerer Lowerer>
auto LowerHirAssignWrite(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignExpr& a,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  if (a.compound_op.has_value() &&
      std::holds_alternative<hir::NonBlockingEffect>(a.timing)) {
    throw InternalError(
        "LowerHirAssignWrite: compound assignment with non-blocking timing "
        "is not a legal SV form (LRM A.6.2 grammar)");
  }
  if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
    const hir::Expr& lhs = lowerer.HirExprs().Get(a.lhs);
    if (const auto* concat = std::get_if<hir::ConcatExpr>(&lhs.data)) {
      BlockBuilder steps(frame);
      auto bound = Destructure(lowerer, steps.Frame(), a, *concat, span);
      if (!bound) return std::unexpected(std::move(bound.error()));
      return steps.Build(*bound);
    }
  }
  auto& block = *frame.current_block;

  auto rhs_or = lowerer.LowerExpr(lowerer.HirExprs().Get(a.rhs), frame);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = block.exprs.Add(*std::move(rhs_or));
  auto lhs_or = lowerer.LowerLhsExpr(lowerer.HirExprs().Get(a.lhs), frame);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));

  const std::optional<CompoundOperation> compound_op =
      a.compound_op.has_value()
          ? std::optional{LowerCompoundOperation(*a.compound_op)}
          : std::nullopt;
  const std::array<mir::ExprId, 1> operands{rhs_id};
  const auto store = [&](mir::Block& blk, const AccessPath& target,
                         std::span<const mir::ExprId> ops) -> mir::Expr {
    return BuildStoreExpr(
        lowerer.Owner().Unit(), blk, target, ops[0], compound_op);
  };
  // What a write to the target is belongs to the store built above, and what
  // belongs here is when it takes place. Only a procedure has a later region
  // to place one in (LRM 10.4); a write a construction performs takes place
  // where the construction reaches it, so there is nothing to choose.
  if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
    return ApplyAssignEffect(
        lowerer, frame, a.timing, span, *lhs_or, operands, store);
  } else {
    return store(block, *lhs_or, operands);
  }
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
  if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
    if (const auto* concat = std::get_if<hir::ConcatExpr>(&lhs.data)) {
      auto value_or = Destructure(lowerer, steps.Frame(), a, *concat, span);
      if (!value_or) return std::unexpected(std::move(value_or.error()));
      return steps.Build(*value_or);
    }
  }

  auto rhs_or = lowerer.LowerExpr(lowerer.HirExprs().Get(a.rhs), steps.Frame());
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = body.exprs.Add(*std::move(rhs_or));
  auto target_or = lowerer.LowerLhsExpr(lhs, steps.Frame());
  if (!target_or) return std::unexpected(std::move(target_or.error()));

  // What the assignment stores is its value (LRM 11.3.6), so that is computed
  // once, held, and both written and yielded; the target read again after the
  // write would answer differently where the write is dropped. A compound one
  // stores the operator applied to what the target held (LRM 11.4.1), with the
  // target settled once for the read and the write.
  AccessPath target = *std::move(target_or);
  mir::ExprId stored = rhs_id;
  if (a.compound_op.has_value()) {
    ReadThenWritten settled =
        ReadThenWrite(lowerer.Owner(), steps.Frame(), std::move(target));
    target = std::move(settled.place);
    stored = body.exprs.Add(BuildMirBinaryExpr(
        unit, body, *a.compound_op, settled.incoming, rhs_id, result_type));
  }
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
  auto target_or =
      lowerer.LowerLhsExpr(lowerer.HirExprs().Get(inc.target), frame);
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  const mir::TypeId type = PathValueType(unit, block, *target_or);
  return BuildStoreExpr(
      unit, block, *target_or, One(unit, block, type),
      LowerCompoundOperation(StepOperator(inc.op)));
}

template <ExprLowerer Lowerer>
auto LowerHirIncDecExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  auto target_or =
      lowerer.LowerLhsExpr(lowerer.HirExprs().Get(inc.target), steps.Frame());
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
  const bool yields_before =
      inc.op == hir::IncDecOp::kPostInc || inc.op == hir::IncDecOp::kPostDec;
  return steps.Build(read(yields_before ? before : after));
}

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

auto BuildDestructuredDeferredAssign(
    ProcessLowerer& process, WalkFrame frame, diag::SourceSpan span,
    const std::optional<hir::DelayOrEventControl>& control,
    std::span<const DestructuredPart> parts) -> diag::Result<mir::Expr> {
  for (const DestructuredPart& part : parts) {
    auto outlives = CheckTargetOutlivesUpdate(
        *frame.current_block, part.target.owner, span);
    if (!outlives) return std::unexpected(std::move(outlives.error()));
  }
  return BuildDeferredEffect(
      process, frame, control,
      [&](ClosureBuilder& closure)
          -> diag::Result<std::vector<FrozenAssignment>> {
        std::vector<FrozenAssignment> frozen;
        frozen.reserve(parts.size());
        for (const DestructuredPart& part : parts) {
          const std::array<mir::ExprId, 1> operands{part.value};
          frozen.push_back(FreezeAssignmentInto(
              process.Owner(), frame, closure, part.target, operands));
        }
        return frozen;
      },
      [&](mir::Block& body, const std::vector<FrozenAssignment>& frozen) {
        for (const FrozenAssignment& part : frozen) {
          body.AppendStmt(
              mir::ExprStmt{
                  .expr = body.exprs.Add(BuildStoreExpr(
                      process.Owner().Unit(), body, part.target,
                      part.operands.front()))});
        }
      });
}

}  // namespace lyra::lowering::hir_to_mir
