#include "lyra/lowering/hir_to_mir/expression/system/random.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <limits>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/arena.hpp"
#include "lyra/base/component_index.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/call_operands.hpp"
#include "lyra/lowering/hir_to_mir/callee_interface.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"  // IWYU pragma: keep
#include "lyra/lowering/hir_to_mir/runtime_call.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"  // IWYU pragma: keep
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// The runtime entry a process draw generates through (LRM 18.13.1 -- 18.13.2).
// `$urandom`'s optional seed re-seeds the generator before the draw, which is a
// separate entry rather than an optional operand, so the argument count is what
// tells the two apart. `$urandom_range` has one entry whichever of its bounds
// the call spells.
auto ProcessDrawEntry(support::RandomKind kind, std::size_t argument_count)
    -> support::BuiltinFn {
  switch (kind) {
    case support::RandomKind::kUrandom:
      return argument_count == 1 ? support::BuiltinFn::kUrandomSeeded
                                 : support::BuiltinFn::kUrandom;
    case support::RandomKind::kUrandomRange:
      return support::BuiltinFn::kUrandomRange;
  }
  throw InternalError("ProcessDrawEntry: unknown RandomKind");
}

// The variable a seed argument names (LRM 20.14.1: the seed "shall be an
// integral variable"). A seed whose declared type is not the one the argument
// position takes arrives read through a conversion; the variable the draw
// advances is what that conversion reads.
auto SeedVariable(
    const base::Arena<hir::Expr, hir::ExprId>& exprs, hir::ExprId seed)
    -> hir::ExprId {
  const auto* conversion =
      std::get_if<hir::ConversionExpr>(&exprs.Get(seed).data);
  return conversion == nullptr ? seed : conversion->operand;
}

// The runtime entry each LRM 20.14.2 distribution function generates through.
auto DistributionEntry(support::DistributionKind kind) -> support::BuiltinFn {
  switch (kind) {
    // LRM Annex N Table N.1 lists a seeded `$random` as the uniform draw over
    // the whole signed range, so it is that entry rather than one of its own;
    // what tells them apart is the bounds the call carries, not the algorithm.
    case support::DistributionKind::kRandom:
      return support::BuiltinFn::kDistUniform;
    case support::DistributionKind::kUniform:
      return support::BuiltinFn::kDistUniform;
    case support::DistributionKind::kNormal:
      return support::BuiltinFn::kDistNormal;
    case support::DistributionKind::kExponential:
      return support::BuiltinFn::kDistExponential;
    case support::DistributionKind::kPoisson:
      return support::BuiltinFn::kDistPoisson;
    case support::DistributionKind::kChiSquare:
      return support::BuiltinFn::kDistChiSquare;
    case support::DistributionKind::kT:
      return support::BuiltinFn::kDistT;
    case support::DistributionKind::kErlang:
      return support::BuiltinFn::kDistErlang;
  }
  throw InternalError("DistributionEntry: unknown DistributionKind");
}

// Where the completion's components sit: what the call answers with, then the
// seed the draw advanced (LRM 20.14.2).
constexpr base::ComponentIndex kDrawnValue{0};
constexpr base::ComponentIndex kAdvancedSeed{1};

}  // namespace

template <ExprLowerer Lowerer>
auto LowerRandomSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::RandomSystemSubroutineInfo& info)
    -> diag::Result<mir::Expr> {
  const std::vector<hir::ExprId> operands = RequiredOperands(call);
  auto& unit = lowerer.Owner().Unit();
  auto& body = *frame.current_block;

  std::vector<mir::ExprId> arguments;
  arguments.push_back(
      body.exprs.Add(BuildCurrentRuntimeCallExpr(lowerer.Owner())));
  for (const hir::ExprId operand : operands) {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(operand), frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    arguments.push_back(body.exprs.Add(*std::move(lowered)));
  }

  // An omitted low bound is the zero LRM 18.13.2 defines it to be, and the
  // runtime entry always takes both.
  if (info.kind == support::RandomKind::kUrandomRange && operands.size() == 1) {
    arguments.push_back(BuildIntLiteral(unit, body, 0));
  }

  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = ProcessDrawEntry(info.kind, operands.size())},
              .arguments = std::move(arguments)},
      .type = unit.builtins.int_unsigned};
}

template <ExprLowerer Lowerer>
auto LowerDistributionSystemSubroutineCall(
    Lowerer& lowerer, WalkFrame frame, const hir::CallExpr& call,
    const support::DistributionSystemSubroutineInfo& info)
    -> diag::Result<mir::Expr> {
  const std::vector<hir::ExprId> operands = RequiredOperands(call);
  const auto& hir_exprs = lowerer.HirExprs();
  auto& unit_lowerer = lowerer.Owner();
  auto& unit = unit_lowerer.Unit();
  const mir::TypeId int_type = unit.builtins.int_type;

  // LRM 20.14.1 makes `$random`'s seed optional, and a call that omits one
  // names no stream to advance, so it draws where LRM 18.13.1 does.
  if (operands.empty()) {
    auto& block = *frame.current_block;
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = mir::Direct{.target = support::BuiltinFn::kRandom},
                .arguments = {block.exprs.Add(
                    BuildCurrentRuntimeCallExpr(unit_lowerer))}},
        .type = int_type};
  }

  // Binding the completion, writing the seed back, and yielding the value are
  // statements, and a call sits in expression position, so they are the steps
  // of one block expression.
  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const WalkFrame& step_frame = steps.Frame();

  std::vector<mir::ExprId> arguments;
  arguments.reserve(operands.size());
  for (const hir::ExprId operand : operands) {
    auto lowered = lowerer.LowerExpr(hir_exprs.Get(operand), step_frame);
    if (!lowered) {
      return std::unexpected(std::move(lowered.error()));
    }
    const mir::ExprId raw = body.exprs.Add(*std::move(lowered));
    // Every argument is an integer value (LRM 20.14.2) whatever integral type
    // the design declared it, and the generator works in 32 signed bits.
    arguments.push_back(ConvertToType(unit, body, raw, int_type));
  }
  // LRM Annex N Table N.1: a seeded `$random` is the uniform draw bounded by
  // the whole signed range, which the source writes as one operand and the
  // entry takes as three.
  if (info.kind == support::DistributionKind::kRandom) {
    arguments.push_back(
        BuildIntLiteral(unit, body, std::numeric_limits<std::int32_t>::min()));
    arguments.push_back(
        BuildIntLiteral(unit, body, std::numeric_limits<std::int32_t>::max()));
  }

  const mir::TypeId payload_type =
      CompletionPayloadType(unit, {int_type, int_type});
  const mir::ExprId draw_call = body.exprs.Add(
      mir::Expr{
          .data =
              mir::CallExpr{
                  .callee = mir::Direct{.target = DistributionEntry(info.kind)},
                  .arguments = std::move(arguments)},
          .type = payload_type});
  const mir::LocalId completion = steps.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_draw", .type = payload_type});
  body.AppendStmt(mir::LocalDeclStmt{.target = completion, .init = draw_call});

  // LRM 20.14.1 / 20.14.2 make the seed an integral variable the draw advances,
  // so what the store writes back to is that variable. A seed declared as
  // something other than the entry's own operand type is read through a
  // conversion, and the variable is what the conversion reads -- the store then
  // lands the advanced value in the declared type, which is what the design
  // reads next.
  const hir::Expr& hir_seed =
      hir_exprs.Get(SeedVariable(hir_exprs, operands[0]));
  const mir::TypeId seed_type = unit_lowerer.TranslateType(hir_seed.type);
  auto seed_place_or = lowerer.LowerLhsExpr(hir_seed, step_frame);
  if (!seed_place_or) {
    return std::unexpected(std::move(seed_place_or.error()));
  }
  const mir::ExprId advanced = ConvertToType(
      unit, body,
      ProjectCompletionComponent(
          body, completion, payload_type, kAdvancedSeed, int_type),
      seed_type);
  body.AppendStmt(
      mir::ExprStmt{
          .expr = body.exprs.Add(BuildStoreExpr(
              unit, body, *seed_place_or, advanced, std::nullopt, seed_type))});

  return steps.Build(ProjectCompletionComponent(
      body, completion, payload_type, kDrawnValue, int_type));
}

template auto LowerRandomSystemSubroutineCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&,
    const support::RandomSystemSubroutineInfo&) -> diag::Result<mir::Expr>;
template auto LowerRandomSystemSubroutineCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    const support::RandomSystemSubroutineInfo&) -> diag::Result<mir::Expr>;

template auto LowerDistributionSystemSubroutineCall(
    ProcessLowerer&, WalkFrame, const hir::CallExpr&,
    const support::DistributionSystemSubroutineInfo&)
    -> diag::Result<mir::Expr>;
template auto LowerDistributionSystemSubroutineCall(
    const StructuralScopeLowerer&, WalkFrame, const hir::CallExpr&,
    const support::DistributionSystemSubroutineInfo&)
    -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
