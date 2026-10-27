#include "lyra/lowering/hir_to_mir/sampled_history.hpp"

#include <array>
#include <expected>
#include <optional>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/lowering/hir_to_mir/callable_bindings.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/self_ref.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// A value change function asks one of two questions (LRM 16.9.3). `$stable` and
// `$changed` ask whether the whole sampled value is the one the prior tick
// settled, under case equality so an x or a z compares as itself. `$rose` and
// `$fell` ask whether the least significant bit reached a particular value:
// it holds now and did not at the prior tick.
struct WholeValueComparison {
  hir::BinaryOp op;
};
struct BitReachedComparison {
  bool reached_one;
};
using ValueChangeComparison =
    std::variant<WholeValueComparison, BitReachedComparison>;

auto ComparisonOf(support::ValueChangeReading reading)
    -> ValueChangeComparison {
  switch (reading) {
    case support::ValueChangeReading::kUnchanged:
      return WholeValueComparison{.op = hir::BinaryOp::kCaseEquality};
    case support::ValueChangeReading::kChanged:
      return WholeValueComparison{.op = hir::BinaryOp::kCaseInequality};
    case support::ValueChangeReading::kRoseToOne:
      return BitReachedComparison{.reached_one = true};
    case support::ValueChangeReading::kFellToZero:
      return BitReachedComparison{.reached_one = false};
  }
  throw InternalError("ComparisonOf: unknown value change reading");
}

// The history entry a read names, as the call that answers with it. Both shapes
// reach one: `$past` the tick it names, and a value change function the most
// recent strictly prior tick, which is that same read at depth one (LRM
// 16.9.3).
template <ExprLowerer Lowerer>
auto BuildPriorTickRead(
    Lowerer& lowerer, const WalkFrame& frame, hir::SampledHistoryId id,
    mir::ExprId ticks_back) -> mir::Expr {
  // The history is a member of the scope declaring it, which a process reaches
  // through the scope enclosing it and a scope's own expression directly.
  const StructuralScopeLowerer& scope = [&]() -> const StructuralScopeLowerer& {
    if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
      return lowerer.EnclosingScopeLowerer();
    } else {
      return lowerer;
    }
  }();
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;
  const mir::ExprId history = BuildSampledHistoryExpr(block, frame, scope, id);
  return mir::MakeSampledHistoryAtCallExpr(
      history, ticks_back,
      unit.types.Get(block.exprs.Get(history).type)
          .Get<mir::SampledHistoryType>()
          .value);
}

}  // namespace

auto BuildSampledHistoryExpr(
    mir::Block& block, const WalkFrame& frame,
    const StructuralScopeLowerer& lowerer, hir::SampledHistoryId id)
    -> mir::ExprId {
  const mir::FieldId field = lowerer.TranslateSampledHistory(id);
  const mir::ExprId self = block.exprs.Add(MakeSelfRefExpr(frame));
  return block.exprs.Add(
      mir::MakeFieldAccessExpr(
          BuildObjectDeref(lowerer.Owner().Unit(), block, self),
          mir::ClassFieldTarget{.owner = frame.current_class.id, .slot = field},
          frame.current_class.cls->fields.Get(field).type));
}

auto LowerSampledHistorySampler(
    const StructuralScopeLowerer& lowerer, const WalkFrame& ctor_frame,
    hir::SampledHistoryId id, const hir::SampledHistoryDecl& history)
    -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const hir::StructuralScope& hir_scope = lowerer.HirScope();
  const mir::TypeId self_ptr_type =
      ctor_frame.current_class.cls->self_pointer_type;
  const mir::TypeId void_type = unit.builtins.void_type;

  mir::CallableCode code = mir::CallableCode::Defined();
  CallableBindings bindings(unit, code);
  const mir::LocalId self_id =
      bindings.Declare(ReceiverOrigin{}, self_ptr_type);

  mir::Block body_block;
  const WalkFrame body_frame =
      ctor_frame.WithBindings(&bindings).WithBlock(&body_block);

  // The tick comes first. A continuous assignment evaluates before its first
  // wait because it owes a value at time zero; a history owes none, because it
  // was installed full of the default sampled value the standard asks for until
  // a tick has happened (LRM 16.9.3).
  auto wait_or =
      BuildEventWaitStmt(lowerer, body_frame, body_block, history.clock);
  if (!wait_or) return std::unexpected(std::move(wait_or.error()));
  body_block.AppendStmt(*std::move(wait_or));

  // What the tick settles is the subject over its variables' sampled values
  // (LRM 16.5.1), read here rather than where the answer is used: a call in the
  // expression is called on the arguments this tick sampled, and calling it
  // again later would be a different call.
  auto settled_or = lowerer.LowerExpr(
      hir_scope.exprs.Get(history.subject),
      body_frame.WithReadsAsOf(ReadsAsOf::kPreponed));
  if (!settled_or) return std::unexpected(std::move(settled_or.error()));
  const mir::ExprId settled = body_block.exprs.Add(*std::move(settled_or));

  // Recording is held to the Postponed region of this same time step, so the
  // tick a reader in this step sees is never this one -- which is what makes
  // "strictly prior" hold by construction rather than by anyone's ordering.
  ClosureBuilder closure(unit, body_frame, support::ClosureKind::kFn);
  const mir::ExprId captured =
      SnapshotValueIntoClosure(lowerer.Owner(), body_frame, closure, settled);
  closure.Body().AppendStmt(
      mir::ExprStmt{
          .expr = closure.Body().exprs.Add(
              mir::MakeSampledHistoryPushCallExpr(
                  BuildSampledHistoryExpr(
                      closure.Body(), closure.Frame(), lowerer, id),
                  captured, void_type))});

  mir::Expr keeper = BuildRegionEffectsCallExpr(
      lowerer.Owner(), body_block, support::BuiltinFn::kPostponedEffects);
  const mir::ExprId kept = body_block.exprs.Add(BuildEmplacedClosure(
      lowerer.Owner().Unit(), body_frame, std::move(keeper),
      closure.BuildVoid()));
  body_block.AppendStmt(mir::ExprStmt{.expr = kept});

  const mir::BlockId body_scope_id =
      code.Body().child_scopes.Add(std::move(body_block));
  code.Body().AppendStmt(
      mir::ForStmt{
          .init = {},
          .condition = std::nullopt,
          .step = {},
          .scope = body_scope_id});
  code.Body().AppendStmt(mir::ReturnStmt{.value = std::nullopt});
  code.params = {self_id};
  code.result_type = unit.builtins.coroutine_void;
  return mir::CallableDecl{
      .code = std::move(code),
      .foreign = std::nullopt,
      .virtual_dispatch = std::nullopt,
      .defining_unit = std::nullopt};
}

template <ExprLowerer Lowerer>
auto LowerPastValueCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const hir::PastValueRef& ref) -> diag::Result<mir::Expr> {
  if (call.arguments.size() != 1 || !call.arguments.front().has_value()) {
    throw InternalError(
        "LowerPastValueCall: a prior-tick read states how far back it reaches "
        "and states nothing else");
  }
  auto back_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(*call.arguments.front()), frame);
  if (!back_or) return std::unexpected(std::move(back_or.error()));
  mir::Block& block = *frame.current_block;
  return BuildPriorTickRead(
      lowerer, frame, ref.history,
      BuildToInt64Call(
          lowerer.Owner().Unit(), block, block.exprs.Add(*std::move(back_or))));
}

template <ExprLowerer Lowerer>
auto LowerValueChangeCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const hir::ValueChangeRef& ref, mir::TypeId result_type,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  if (call.arguments.empty() || !call.arguments.front().has_value()) {
    throw InternalError(
        "LowerValueChangeCall: a value change function carries the expression "
        "whose change it reports");
  }
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Block& block = *frame.current_block;

  // A value change function names no distance: LRM 16.9.3 defines it against
  // the most recent strictly prior tick, so the one here is this compiler's own
  // rather than anything the source wrote.
  const mir::ExprId prior = block.exprs.Add(BuildPriorTickRead(
      lowerer, frame, ref.history,
      mir::BuildMachineIntLiteral(unit, block, 1)));
  const mir::TypeId value_type = block.exprs.Get(prior).type;

  // The current side is the sampled value of this time step, which is what
  // `$sampled` answers with and is read the same way (LRM 16.9.3, 16.5.1).
  auto current_or = lowerer.LowerExpr(
      lowerer.HirExprs().Get(*call.arguments.front()),
      frame.WithReadsAsOf(ReadsAsOf::kPreponed));
  if (!current_or) return std::unexpected(std::move(current_or.error()));
  const mir::ExprId current = block.exprs.Add(*std::move(current_or));

  return std::visit(
      Overloaded{
          [&](const WholeValueComparison& whole) -> diag::Result<mir::Expr> {
            return BuildMirBinaryExpr(
                unit, block, whole.op, current, prior, result_type);
          },
          [&](const BitReachedComparison& bit) -> diag::Result<mir::Expr> {
            const std::optional<mir::TypeId> bit_type =
                LeastSignificantBitType(unit, value_type);
            if (!bit_type.has_value()) {
              return diag::Fail(
                  span, diag::DiagCode::kUnsupportedExpressionForm,
                  "'{}' reports on the least significant bit, which a value "
                  "of this type does not have",
                  support::ValueChangeReadingName(ref.reading));
            }
            const mir::ExprId target = mir::BuildIntegralLiteral(
                unit, block, *bit_type,
                mir::IntegralConstant{
                    .value_words = {bit.reached_one ? 1ULL : 0ULL},
                    .state_words = {}});
            const mir::ExprId reached = block.exprs.Add(BuildMirBinaryExpr(
                unit, block, hir::BinaryOp::kCaseEquality,
                BuildLeastSignificantBit(unit, block, current, *bit_type),
                target, result_type));
            const mir::ExprId held_before = block.exprs.Add(BuildMirBinaryExpr(
                unit, block, hir::BinaryOp::kCaseInequality,
                BuildLeastSignificantBit(unit, block, prior, *bit_type), target,
                result_type));
            return block.exprs.Get(BuildMirLogicalAnd(
                unit, block, result_type, std::array{reached, held_before}));
          }},
      ComparisonOf(ref.reading));
}

template auto LowerPastValueCall(
    ProcessLowerer&, const WalkFrame&, const hir::CallExpr&,
    const hir::PastValueRef&) -> diag::Result<mir::Expr>;
template auto LowerPastValueCall(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::CallExpr&,
    const hir::PastValueRef&) -> diag::Result<mir::Expr>;
template auto LowerValueChangeCall(
    ProcessLowerer&, const WalkFrame&, const hir::CallExpr&,
    const hir::ValueChangeRef&, mir::TypeId, diag::SourceSpan)
    -> diag::Result<mir::Expr>;
template auto LowerValueChangeCall(
    const StructuralScopeLowerer&, const WalkFrame&, const hir::CallExpr&,
    const hir::ValueChangeRef&, mir::TypeId, diag::SourceSpan)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
