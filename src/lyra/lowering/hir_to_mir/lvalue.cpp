#include "lyra/lowering/hir_to_mir/lvalue.hpp"

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
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/concatenation.hpp"
#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
#include "lyra/lowering/hir_to_mir/stream_target.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto Distribute(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    mir::ExprId value, const ShareSink& sink) -> diag::Result<void>;

// The bits of `value`, held once as a vector of the join's type, that each
// member takes, the first member the most significant ones (LRM 11.4.12), each
// brought to its member's type.
auto DistributePacked(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    const LvalueJoin& join, mir::ExprId value, const ShareSink& sink)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::IntegralType whole = unit.types.Get(lvalue.type).Integral();
  const mir::ExprId held = EvaluatedOnce(
      frame, ConvertToType(
                 unit, block, block.AsValue(unit.types, value), lvalue.type));
  std::uint64_t offset = whole.bit_width;
  for (const Lvalue& member : join.members) {
    const mir::Type& member_type = unit.types.Get(member.type);
    if (!member_type.IsIntegral()) {
      throw InternalError(
          "lvalue: a member of a concatenation is not an integral type, which "
          "the front end refuses (LRM 11.4.12)");
    }
    const std::uint64_t width = member_type.Integral().bit_width;
    offset -= width;
    const mir::ExprId bits = block.exprs.Add(BuildPackedBitsRead(
        unit_lowerer, block, held, offset,
        mir::PackedVectorOf(unit.types, width, whole.state_kind)));
    auto taken = Distribute(
        unit_lowerer, frame, member,
        ConvertToType(unit, block, bits, member.type), sink);
    if (!taken) return taken;
  }
  return {};
}

// The step from an aggregate of `whole` to its member `position`: a component
// of a product, or an element of a fixed-size array in the order its elements
// are numbered (LRM 10.9.1, 10.9.2). Nothing for an aggregate whose members
// are not counted by its type.
auto PatternMemberStep(
    mir::CompilationUnit& unit, mir::Block& block, mir::TypeId whole,
    std::size_t position) -> std::optional<DescentStep> {
  if (const auto components = mir::ProductElements(unit, whole)) {
    return DescentStep{
        .to =
            StepToComponent{
                .position =
                    base::ComponentIndex{static_cast<std::uint32_t>(position)}},
        .part_type = (*components)[position]};
  }
  if (const auto* array = unit.types.Get(whole).As<mir::UnpackedArrayType>()) {
    const mir::TypeId element = array->element_type;
    return DescentStep{
        .to =
            StepToElement{
                .position = BuildConstantPosition(
                    unit, block, static_cast<std::int64_t>(position))},
        .part_type = element};
  }
  return std::nullopt;
}

auto DistributeUnpacked(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    const LvalueJoin& join, mir::ExprId value, const ShareSink& sink)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  // Each member is taken where it lies in what is distributed; a value that
  // lies nowhere is held in a local first, as a place is needed to step into.
  mir::ExprId held = value;
  if (!mir::IsReadablePlace(unit.types, block.exprs.Get(value))) {
    const mir::LocalId local = DeclareLocal(frame, value);
    held = block.LocalPlace(local, block.exprs.Get(value).type);
  }
  for (std::size_t i = 0; i < join.members.size(); ++i) {
    const std::optional<DescentStep> step =
        PatternMemberStep(unit, block, lvalue.type, i);
    if (!step.has_value()) {
      // LRM 10.9.
      return diag::Fail(
          lvalue.span, diag::DiagCode::kUnsupportedExpressionForm,
          "an assignment pattern of this type is not yet supported as the "
          "target of a write");
    }
    const mir::ExprId member =
        block.exprs.Add(StepRead(unit, block, *step, held));
    auto taken = Distribute(unit_lowerer, frame, join.members[i], member, sink);
    if (!taken) return taken;
  }
  return {};
}

auto Distribute(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    mir::ExprId value, const ShareSink& sink) -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [&](const AccessPath& place) -> diag::Result<void> {
            sink(
                Share{
                    .place = place,
                    .value = frame.current_block->AsValue(
                        unit_lowerer.Unit().types, value)});
            return {};
          },
          [&](const LvalueJoin& join) -> diag::Result<void> {
            return std::visit(
                Overloaded{
                    [&](const PackedJoin&) {
                      return DistributePacked(
                          unit_lowerer, frame, lvalue, join, value, sink);
                    },
                    [&](const UnpackedJoin&) {
                      return DistributeUnpacked(
                          unit_lowerer, frame, lvalue, join, value, sink);
                    }},
                join.kind);
          },
          [&](const StreamTarget& stream) -> diag::Result<void> {
            return DistributeStreamTarget(
                unit_lowerer, frame, stream, value, lvalue.span, sink);
          }},
      lvalue.form);
}

}  // namespace

template <ExprLowerer L>
auto LowerLvalue(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<Lvalue> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const auto lower_join = [&](std::span<const hir::ExprId> members,
                              JoinKind kind,
                              mir::TypeId type) -> diag::Result<Lvalue> {
    LvalueJoin join{.members = {}, .kind = kind};
    join.members.reserve(members.size());
    for (const hir::ExprId member : members) {
      auto lowered =
          LowerLvalue(lowerer, lowerer.HirExprs().Get(member), frame);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      join.members.push_back(*std::move(lowered));
    }
    return Lvalue{
        .form = std::move(join),
        .type = type,
        .source_type = expr.type,
        .span = expr.span};
  };
  if (const auto* concat = std::get_if<hir::ConcatExpr>(&expr.data)) {
    return lower_join(
        concat->operands, PackedJoin{}, unit_lowerer.TranslateType(expr.type));
  }
  if (const auto* stream = std::get_if<hir::StreamingConcatExpr>(&expr.data)) {
    auto target = LowerStreamTarget(lowerer, *stream, frame);
    if (!target) return std::unexpected(std::move(target.error()));
    return Lvalue{
        .form = *std::move(target),
        .type = unit_lowerer.Unit().builtins.void_type,
        .source_type = expr.type,
        .span = expr.span};
  }
  if (const auto* pattern =
          std::get_if<hir::AssignmentPatternExpr>(&expr.data)) {
    const mir::TypeId type = unit_lowerer.TranslateType(expr.type);
    if (unit_lowerer.Unit().types.Get(type).IsIntegral()) {
      return lower_join(pattern->elements, PackedJoin{}, type);
    }
    return lower_join(pattern->elements, UnpackedJoin{}, type);
  }
  auto place = lowerer.LowerLhsExpr(expr, frame);
  if (!place) return std::unexpected(std::move(place.error()));
  return Lvalue{
      .form = *std::move(place),
      .type = unit_lowerer.TranslateType(expr.type),
      .source_type = expr.type,
      .span = expr.span};
}

template auto LowerLvalue(ProcessLowerer&, const hir::Expr&, WalkFrame)
    -> diag::Result<Lvalue>;
template auto LowerLvalue(
    const StructuralScopeLowerer&, const hir::Expr&, WalkFrame)
    -> diag::Result<Lvalue>;

auto IsJoin(const hir::Expr& expr) -> bool {
  return std::holds_alternative<hir::ConcatExpr>(expr.data) ||
         std::holds_alternative<hir::StreamingConcatExpr>(expr.data) ||
         std::holds_alternative<hir::AssignmentPatternExpr>(expr.data);
}

auto Shares(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    mir::ExprId value) -> diag::Result<std::vector<Share>> {
  std::vector<Share> shares;
  auto taken = Distribute(unit_lowerer, frame, lvalue, value, [&](Share share) {
    shares.push_back(std::move(share));
  });
  if (!taken) return std::unexpected(std::move(taken.error()));
  return shares;
}

auto AppendStores(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    mir::ExprId value) -> diag::Result<void> {
  mir::Block& block = *frame.current_block;
  const auto store = [&](const Share& share) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(BuildStoreExpr(
                unit_lowerer.Unit(), block, share.place, share.value))});
  };
  // One place alone has no other store to be ordered against and takes the
  // value as it was named.
  if (std::holds_alternative<AccessPath>(lvalue.form)) {
    return Distribute(unit_lowerer, frame, lvalue, value, store);
  }
  // What a join or a stream target distributes is held in a local of its own
  // before any place is
  // located, so a member written first changes nothing a later one is taken
  // from, even where the value was read out of a member's own storage (LRM
  // 10.9, 11.4.12).
  const mir::LocalId local = DeclareLocal(frame, value);
  const mir::ExprId held = block.LocalPlace(local, block.exprs.Get(value).type);
  Lvalue located = lvalue;
  ForEachPlace(located, [&](AccessPath& place) {
    place = Settled(unit_lowerer, frame, std::move(place));
  });
  return Distribute(unit_lowerer, frame, located, held, store);
}

auto ReadThenWrite(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, Lvalue lvalue)
    -> diag::Result<LvalueReadThenWritten> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  // What the members of a join hold together is the value the join stands
  // for: their bits end to end, or the aggregate they are the members of.
  const auto read_join = [&](LvalueJoin& join) -> diag::Result<mir::ExprId> {
    std::vector<mir::ExprId> incoming;
    incoming.reserve(join.members.size());
    for (Lvalue& member : join.members) {
      auto settled = ReadThenWrite(unit_lowerer, frame, std::move(member));
      if (!settled) return std::unexpected(std::move(settled.error()));
      member = std::move(settled->lvalue);
      incoming.push_back(settled->incoming);
    }
    return std::visit(
        Overloaded{
            [&](const PackedJoin&) -> mir::ExprId {
              return ConvertToType(
                  unit, block, BuildBitsJoin(unit, frame, incoming),
                  lvalue.type);
            },
            [&](const UnpackedJoin&) -> mir::ExprId {
              for (std::size_t i = 0; i < incoming.size(); ++i) {
                const std::optional<DescentStep> step =
                    PatternMemberStep(unit, block, lvalue.type, i);
                if (step.has_value()) {
                  incoming[i] =
                      ConvertToType(unit, block, incoming[i], step->part_type);
                }
              }
              return block.exprs.Add(BuildPositionalAggregate(
                  unit, frame, lvalue.type, std::move(incoming)));
            }},
        join.kind);
  };
  auto incoming = std::visit(
      Overloaded{
          [&](AccessPath& place) -> diag::Result<mir::ExprId> {
            ReadThenWritten settled =
                ReadThenWrite(unit_lowerer, frame, std::move(place));
            place = std::move(settled.place);
            return settled.incoming;
          },
          [&](LvalueJoin& join) -> diag::Result<mir::ExprId> {
            return read_join(join);
          },
          [](StreamTarget&) -> diag::Result<mir::ExprId> {
            throw InternalError(
                "lvalue: a streaming concatenation is read before it is "
                "written, which the front end refuses (LRM 11.4.14)");
          }},
      lvalue.form);
  if (!incoming) return std::unexpected(std::move(incoming.error()));
  return LvalueReadThenWritten{
      .lvalue = std::move(lvalue), .incoming = *incoming};
}

}  // namespace lyra::lowering::hir_to_mir
