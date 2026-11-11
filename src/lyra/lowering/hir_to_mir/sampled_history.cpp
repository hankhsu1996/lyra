#include "lyra/lowering/hir_to_mir/sampled_history.hpp"

#include <algorithm>
#include <array>
#include <concepts>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/closure_builder.hpp"
#include "lyra/lowering/hir_to_mir/expression/operators.hpp"
#include "lyra/lowering/hir_to_mir/kept_piece.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/statement/timing.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/synthesized_body.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/expr_builders.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/library_decl.hpp"

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

// How far back the sampled value function `call` reaches: `$past`'s tick
// count, which settles before the program runs and reads nothing as of any
// tick, and the most recent strictly prior tick for a value change function
// (LRM 16.9.3).
template <ExprLowerer Lowerer>
auto LowerTicksBack(Lowerer& lowerer, const WalkFrame& frame, hir::ExprId call)
    -> diag::Result<mir::Expr> {
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto& as_call =
      std::get<hir::CallExpr>(lowerer.HirExprs().Get(call).data);
  if (std::holds_alternative<hir::ValueChangeRef>(as_call.callee)) {
    return mir::MakeIntegralLiteral(
        unit, unit.builtins.int_type,
        mir::IntegralConstant{.value_words = {1}, .state_words = {}});
  }
  return lowerer.LowerExpr(
      lowerer.HirExprs().Get(*as_call.arguments.at(1)),
      frame.WithReadsAsOf(ReadsAsOf::kNow).WithPriorTicks({}));
}

// The sampled value function among `lowerer`'s expressions that `call` is.
template <ExprLowerer Lowerer>
auto SampledCallIdOf(const Lowerer& lowerer, const hir::CallExpr& call)
    -> hir::ExprId {
  for (const hir::ExprId id : SampledValueCalls(lowerer.HirExprs())) {
    if (&std::get<hir::CallExpr>(lowerer.HirExprs().Get(id).data) == &call) {
      return id;
    }
  }
  throw InternalError(
      "SampledCallIdOf: a sampled value function is not among the expressions "
      "it is lowered with");
}

// What the history of `call` holds of `kept` at the tick the call names, as
// the place it lies in there, so a read of a part of it reaches only that part.
template <ExprLowerer Lowerer>
auto PriorTickRead(
    Lowerer& lowerer, const WalkFrame& frame, hir::ExprId call,
    const KeptPiece& kept) -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Builder block = frame.Builder();
  const mir::ExprId cell =
      block.Add(lowerer.Histories().CellOf(unit, frame, call, kept));
  auto back_or = LowerTicksBack(lowerer, frame, call);
  if (!back_or) return std::unexpected(std::move(back_or.error()));
  const mir::ExprId back =
      BuildToInt64Call(unit, block, block.Add(*std::move(back_or)));
  const mir::TypeId value = unit.types.Get(block.Get(cell).type)
                                .Get<mir::LibraryType>()
                                .TypeArg(support::LibraryParam::kType);
  const mir::TypeId reference = unit.types.Intern(
      mir::Type{mir::ReferenceType{
          .referent = value, .mutability = mir::Mutability::kReadOnly}});
  return mir::MakeDerefExpr(
      block.Add(mir::MakeSampledHistoryAtCallExpr(cell, back, reference)),
      value);
}

// What a history records of `kept` at a tick of its clock, and is filled with
// before the first: the piece's sampled value now, or, for a piece read
// through a sampled value function inside the operand, what that function's
// history holds of it at its own tick count.
template <ExprLowerer Lowerer>
auto KeptValue(Lowerer& lowerer, const WalkFrame& frame, const KeptPiece& kept)
    -> diag::Result<mir::ExprId> {
  mir::Builder block = frame.Builder();
  if (kept.through.empty()) {
    auto value_or = lowerer.LowerExpr(
        lowerer.HirExprs().Get(kept.piece),
        frame.WithReadsAsOf(ReadsAsOf::kPreponed).WithPriorTicks({}));
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    return block.Add(*std::move(value_or));
  }
  auto kept_or = PriorTickRead(
      lowerer, frame, kept.through.front(),
      KeptPiece{
          .piece = kept.piece,
          .through = {kept.through.begin() + 1, kept.through.end()}});
  if (!kept_or) return std::unexpected(std::move(kept_or.error()));
  return block.Add(
      block.AsValue(lowerer.Owner().Unit().types, *std::move(kept_or)));
}

// The process keeping one history. LRM 14.7 already describes the sampling
// agent as an always procedure -- one copy per instance of the scope declaring
// it -- and a history has to record every tick of its clock whether or not
// anything is reading it, so it is lowered as one.
template <ExprLowerer Lowerer>
auto LowerSampler(Lowerer& reader, const WalkFrame& ctor_frame, hir::ExprId id)
    -> diag::Result<mir::CallableDecl> {
  mir::CompilationUnit& unit = reader.Owner().Unit();
  const hir::Expr& call = reader.HirExprs().Get(id);
  const std::vector<KeptPiece> kept = reader.Histories().PiecesOf(id);
  // What runs it is the instance's: the scope enclosing a body, or the scope
  // itself among its own expressions.
  const StructuralScopeLowerer& scope = [&]() -> const StructuralScopeLowerer& {
    if constexpr (std::same_as<Lowerer, ProcessLowerer>) {
      return reader.EnclosingScopeLowerer();
    } else {
      return reader;
    }
  }();
  return ScopeExecution(
      scope, ctor_frame, call.span,
      [&](const WalkFrame& body_frame) -> diag::Result<void> {
        mir::Builder body_block = body_frame.Builder();
        // The tick comes first. A continuous assignment evaluates before its
        // first wait because it owes a value at time zero; a history owes
        // none, because it was installed full of the default sampled value
        // the standard asks for until a tick has happened (LRM 16.9.3).
        auto wait_or = BuildEventWaitStmt(
            reader, body_frame, body_block, *SampledClockOf(call));
        if (!wait_or) return std::unexpected(std::move(wait_or.error()));
        body_block.AppendStmt(*std::move(wait_or));

        // What the tick settles is taken here rather than where the answer is
        // used, and recorded in the Postponed region of this same time step,
        // so the tick a reader in this step sees is never this one -- which is
        // what makes "strictly prior" hold by construction rather than by
        // anyone's ordering.
        ClosureBuilder closure(unit, body_frame, support::ClosureKind::kFn);
        for (const KeptPiece& piece : kept) {
          auto settled_or = KeptValue(reader, body_frame, piece);
          if (!settled_or)
            return std::unexpected(std::move(settled_or.error()));
          const mir::ExprId captured = SnapshotValueIntoClosure(
              reader.Owner(), body_frame, closure, *settled_or);
          const mir::ExprId cell = closure.Body().Add(
              reader.Histories().CellOf(unit, closure.Frame(), id, piece));
          closure.Body().AppendStmt(
              mir::ExprStmt{
                  .expr = closure.Body().Add(
                      mir::MakeSampledHistoryPushCallExpr(
                          cell, captured, unit.builtins.void_type))});
        }
        mir::Expr keeper = BuildRegionEffectsCallExpr(
            reader.Owner(), body_block, support::BuiltinFn::kPostponedEffects);
        body_block.AppendStmt(
            mir::ExprStmt{
                .expr = body_block.Add(BuildEmplacedClosure(
                    unit, body_frame, std::move(keeper),
                    closure.BuildVoid()))});
        return {};
      });
}

// Fills each piece's history with what the piece is now, which where the
// design activates is its default sampled value (LRM 16.5.1): the answer the
// standard requires of a read reaching further back than the ticks that have
// happened, so a history that starts full has no empty case and keeps no
// count of them.
template <ExprLowerer Lowerer>
auto InstallHistory(
    Lowerer& reader, const WalkFrame& activate_frame, hir::ExprId id)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = reader.Owner().Unit();
  mir::Builder block = activate_frame.Builder();
  for (const KeptPiece& piece : reader.Histories().PiecesOf(id)) {
    auto value_or = KeptValue(reader, activate_frame, piece);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    auto depth_or = LowerTicksBack(reader, activate_frame, id);
    if (!depth_or) return std::unexpected(std::move(depth_or.error()));
    const mir::ExprId depth =
        BuildToInt64Call(unit, block, block.Add(*std::move(depth_or)));
    const mir::ExprId cell =
        block.Add(reader.Histories().CellOf(unit, activate_frame, id, piece));
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.Add(
                mir::MakeSampledHistoryInstallCallExpr(
                    cell, *value_or, depth, unit.builtins.void_type))});
  }
  return {};
}

// The prior ticks the operand of `call` is read through: the ones the call
// stands inside, then its own.
auto PriorTicksOfOperand(const WalkFrame& frame, hir::ExprId call)
    -> std::vector<hir::ExprId> {
  std::vector<hir::ExprId> through(
      frame.prior_ticks.begin(), frame.prior_ticks.end());
  through.push_back(call);
  return through;
}

}  // namespace

template <ExprLowerer Lowerer>
auto KeepSampledHistories(
    Lowerer& reader, mir::Class& mir_class, const WalkFrame& ctor_frame,
    const WalkFrame& activate_frame)
    -> diag::Result<std::vector<HistorySampler>> {
  std::vector<HistorySampler> samplers;
  // A history kept through another reads that one's, so each is filled after
  // the histories of the calls inside its operand, which the arena holds
  // before it.
  std::vector<hir::ExprId> filled;
  for (const hir::ExprId id : SampledValueCalls(reader.HirExprs())) {
    const std::vector<KeptPiece> kept = reader.Histories().PiecesOf(id);
    // An operand keeping no piece reads only what answers as it is now, at
    // every tick alike (LRM 16.5.1), so nothing has a tick to record and no
    // process runs for it.
    if (kept.empty()) continue;
    for (const KeptPiece& piece : kept) {
      for (const hir::ExprId inner : piece.through) {
        if (std::ranges::find(filled, inner) == filled.end()) {
          throw InternalError(
              "KeepSampledHistories: a history is read through one not yet "
              "filled -- please report this as a bug");
        }
      }
    }
    filled.push_back(id);
    auto sampler_or = LowerSampler(reader, ctor_frame, id);
    if (!sampler_or) return std::unexpected(std::move(sampler_or.error()));
    samplers.push_back(
        HistorySampler{
            .body = mir_class.callables.Add(*std::move(sampler_or)),
            .written_at = reader.HirExprs().Get(id).span});
    if (auto filled = InstallHistory(reader, activate_frame, id); !filled) {
      return std::unexpected(std::move(filled.error()));
    }
  }
  return samplers;
}

template <ExprLowerer Lowerer>
auto ReadOfKeptPiece(
    Lowerer& lowerer, const hir::Expr& expr, const WalkFrame& frame)
    -> std::optional<diag::Result<mir::Expr>> {
  if (frame.prior_ticks.empty()) return std::nullopt;
  const hir::ExprId outermost = frame.prior_ticks.front();
  const std::span<const hir::ExprId> rest = frame.prior_ticks.subspan(1);
  for (const KeptPiece& kept : lowerer.Histories().PiecesOf(outermost)) {
    if (&lowerer.HirExprs().Get(kept.piece) != &expr) continue;
    if (!std::ranges::equal(kept.through, rest)) continue;
    return PriorTickRead(lowerer, frame, outermost, kept);
  }
  return std::nullopt;
}

template <ExprLowerer Lowerer>
auto LowerPastValueCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const hir::PastValueRef&) -> diag::Result<mir::Expr> {
  if (call.arguments.size() != 2 || !call.arguments.front().has_value()) {
    throw InternalError(
        "LowerPastValueCall: a prior-tick read states its operand and how far "
        "back it reaches");
  }
  const std::vector<hir::ExprId> through =
      PriorTicksOfOperand(frame, SampledCallIdOf(lowerer, call));
  return lowerer.LowerExpr(
      lowerer.HirExprs().Get(*call.arguments.front()),
      frame.WithPriorTicks(through));
}

template <ExprLowerer Lowerer>
auto LowerValueChangeCall(
    Lowerer& lowerer, const WalkFrame& frame, const hir::CallExpr& call,
    const hir::ValueChangeRef& ref, mir::TypeId result_type,
    diag::SourceSpan span) -> diag::Result<mir::Expr> {
  if (call.arguments.size() != 1 || !call.arguments.front().has_value()) {
    throw InternalError(
        "LowerValueChangeCall: a value change function states the expression "
        "whose change it reports");
  }
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  mir::Builder block = frame.Builder();
  const hir::Expr& operand = lowerer.HirExprs().Get(*call.arguments.front());

  // Each side evaluates the operand here, as the sampled value of its own time
  // step: each makes the calls it reaches and reads each constant cast as it
  // is now (LRM 16.5.1). The current side is the sampled value of this time
  // step, which is what `$sampled` answers with and is read the same way (LRM
  // 16.9.3).
  auto current_or =
      lowerer.LowerExpr(operand, frame.WithReadsAsOf(ReadsAsOf::kPreponed));
  if (!current_or) return std::unexpected(std::move(current_or.error()));
  const mir::ExprId current = block.Add(*std::move(current_or));
  const std::vector<hir::ExprId> through =
      PriorTicksOfOperand(frame, SampledCallIdOf(lowerer, call));
  auto prior_or = lowerer.LowerExpr(operand, frame.WithPriorTicks(through));
  if (!prior_or) return std::unexpected(std::move(prior_or.error()));
  const mir::ExprId prior = block.Add(*std::move(prior_or));
  const mir::TypeId value_type = block.Get(prior).type;

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
            const mir::ExprId reached = block.Add(BuildMirBinaryExpr(
                unit, block, hir::BinaryOp::kCaseEquality,
                BuildLeastSignificantBit(unit, block, current, *bit_type),
                target, result_type));
            const mir::ExprId held_before = block.Add(BuildMirBinaryExpr(
                unit, block, hir::BinaryOp::kCaseInequality,
                BuildLeastSignificantBit(unit, block, prior, *bit_type), target,
                result_type));
            return block.Get(BuildMirLogicalAnd(
                unit, block, result_type, std::array{reached, held_before}));
          }},
      ComparisonOf(ref.reading));
}

template auto KeepSampledHistories(
    ProcessLowerer&, mir::Class&, const WalkFrame&, const WalkFrame&)
    -> diag::Result<std::vector<HistorySampler>>;
template auto KeepSampledHistories(
    const StructuralScopeLowerer&, mir::Class&, const WalkFrame&,
    const WalkFrame&) -> diag::Result<std::vector<HistorySampler>>;
template auto ReadOfKeptPiece(
    ProcessLowerer&, const hir::Expr&, const WalkFrame&)
    -> std::optional<diag::Result<mir::Expr>>;
template auto ReadOfKeptPiece(
    const StructuralScopeLowerer&, const hir::Expr&, const WalkFrame&)
    -> std::optional<diag::Result<mir::Expr>>;
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
