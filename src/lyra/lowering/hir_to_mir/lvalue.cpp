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
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/packed_concat.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/select_position.hpp"
#include "lyra/lowering/hir_to_mir/snapshot_local.hpp"
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
    mir::ExprId value, std::vector<Share>& shares) -> diag::Result<void>;

// The bits of the held vector `held` each member takes, the first member the
// most significant ones (LRM 11.4.12), each brought to its member's type.
auto DistributePacked(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    const LvalueConcat& concat, mir::ExprId value, std::vector<Share>& shares)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  // By value: the pool's view does not survive the interning below.
  const mir::IntegralType whole = unit.types.Get(lvalue.type).Integral();
  const mir::ExprId held =
      EvaluatedOnce(frame, ConvertToType(unit, block, value, lvalue.type));
  std::uint64_t offset = whole.bit_width;
  for (const Lvalue& elem : concat.elems) {
    const mir::Type& elem_type = unit.types.Get(elem.type);
    if (!elem_type.IsIntegral()) {
      throw InternalError(
          "lvalue: a member of a concatenation is not an integral type, which "
          "the front end refuses (LRM 11.4.12)");
    }
    const std::uint64_t width = elem_type.Integral().bit_width;
    offset -= width;
    const mir::ExprId bits = block.exprs.Add(BuildPackedBitsRead(
        unit_lowerer, block, held, offset,
        mir::PackedVectorOf(unit.types, width, whole.state_kind)));
    auto taken = Distribute(
        unit_lowerer, frame, elem, ConvertToType(unit, block, bits, elem.type),
        shares);
    if (!taken) return taken;
  }
  return {};
}

// The bits `value` makes as a stream, re-ordered once and then cut among the
// members from the most significant end, each read back at its own type (LRM
// 11.4.14.3). Where the stream carries more bits than the members need, the
// surplus is at its least significant end and is dropped, which is why the
// usable bits are taken before the re-ordering rather than after.
auto DistributeStreamed(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    const LvalueConcat& concat, const StreamedJoin& streamed, mir::ExprId value,
    std::vector<Share>& shares) -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;

  // How wide each member's share is, and never its state domain: the bits come
  // from the source, so the stream's domain is the source's, and a member's
  // own is applied where its share is read back at its type.
  std::vector<std::uint64_t> widths;
  widths.reserve(concat.elems.size());
  std::uint64_t members_width = 0;
  for (const Lvalue& elem : concat.elems) {
    const std::optional<StreamShape> shape =
        FixedStreamShapeOf(unit, elem.type);
    if (!shape.has_value()) {
      return diag::Fail(
          elem.span, diag::DiagCode::kUnsupportedExpressionForm,
          "filling a value of this type from a stream of bits is not yet "
          "supported (LRM 11.4.14.3)");
    }
    widths.push_back(shape->width);
    members_width += shape->width;
  }

  auto source_or = BuildToBitstream(unit, block, value, lvalue.span);
  if (!source_or) return std::unexpected(std::move(source_or.error()));
  // By value: the pool's view does not survive the interning below.
  const mir::IntegralType source =
      unit.types.Get(block.exprs.Get(*source_or).type).Integral();
  if (source.bit_width < members_width) {
    throw InternalError(
        "lvalue: the front end refuses a source with fewer bits than a "
        "streaming target needs (LRM 11.4.14.3) -- please report this as a "
        "bug");
  }
  const mir::TypeId stream_type =
      mir::PackedVectorOf(unit.types, members_width, source.state_kind);
  const mir::ExprId held = EvaluatedOnce(
      frame, BuildReorderedStream(
                 unit, block,
                 block.exprs.Add(BuildPackedBitsRead(
                     unit_lowerer, block, *source_or,
                     source.bit_width - members_width, stream_type)),
                 streamed.block_bits));

  std::uint64_t offset = members_width;
  for (std::size_t i = 0; i < concat.elems.size(); ++i) {
    const Lvalue& elem = concat.elems[i];
    offset -= widths[i];
    const mir::ExprId segment = block.exprs.Add(BuildPackedBitsRead(
        unit_lowerer, block, held, offset,
        mir::PackedVectorOf(unit.types, widths[i], source.state_kind)));
    auto read_back =
        BuildFromBitstream(unit, block, segment, elem.type, elem.span);
    if (!read_back) return std::unexpected(std::move(read_back.error()));
    auto taken = Distribute(unit_lowerer, frame, elem, *read_back, shares);
    if (!taken) return taken;
  }
  return {};
}

// The step from an aggregate of `whole` to its member `position`: a component
// of a product, or an element of a fixed-size array in the order its elements
// are numbered (LRM 10.9.1, 10.9.2). Nothing for an aggregate whose members
// are not counted by its type.
auto MemberStep(
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
    const LvalueConcat& concat, mir::ExprId value, std::vector<Share>& shares)
    -> diag::Result<void> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  const mir::ExprId held = EvaluatedOnce(frame, value);
  for (std::size_t i = 0; i < concat.elems.size(); ++i) {
    const std::optional<DescentStep> step =
        MemberStep(unit, block, lvalue.type, i);
    if (!step.has_value()) {
      return diag::Fail(
          lvalue.span, diag::DiagCode::kUnsupportedExpressionForm,
          "an assignment pattern of this type is not yet supported as the "
          "target of a write (LRM 10.9)");
    }
    const mir::ExprId member =
        block.exprs.Add(StepRead(unit, block, *step, held));
    auto taken =
        Distribute(unit_lowerer, frame, concat.elems[i], member, shares);
    if (!taken) return taken;
  }
  return {};
}

auto Distribute(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    mir::ExprId value, std::vector<Share>& shares) -> diag::Result<void> {
  return std::visit(
      Overloaded{
          [&](const AccessPath& place) -> diag::Result<void> {
            shares.push_back(Share{.place = place, .value = value});
            return {};
          },
          [&](const LvalueConcat& concat) -> diag::Result<void> {
            return std::visit(
                Overloaded{
                    [&](const PackedJoin&) {
                      return DistributePacked(
                          unit_lowerer, frame, lvalue, concat, value, shares);
                    },
                    [&](const UnpackedJoin&) {
                      return DistributeUnpacked(
                          unit_lowerer, frame, lvalue, concat, value, shares);
                    },
                    [&](const StreamedJoin& streamed) {
                      return DistributeStreamed(
                          unit_lowerer, frame, lvalue, concat, streamed, value,
                          shares);
                    }},
                concat.kind);
          }},
      lvalue.form);
}

}  // namespace

template <ExprLowerer L>
auto LowerLvalue(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<Lvalue> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const auto join = [&](std::span<const hir::ExprId> members, JoinKind kind,
                        mir::TypeId type) -> diag::Result<Lvalue> {
    LvalueConcat concat{.elems = {}, .kind = kind};
    concat.elems.reserve(members.size());
    for (const hir::ExprId member : members) {
      auto elem = LowerLvalue(lowerer, lowerer.HirExprs().Get(member), frame);
      if (!elem) return std::unexpected(std::move(elem.error()));
      concat.elems.push_back(*std::move(elem));
    }
    return Lvalue{
        .form = std::move(concat),
        .type = type,
        .source_type = expr.type,
        .span = expr.span};
  };
  if (const auto* concat = std::get_if<hir::ConcatExpr>(&expr.data)) {
    return join(
        concat->operands, PackedJoin{}, unit_lowerer.TranslateType(expr.type));
  }
  // A stream as a target is no value of a type: what its members take is cut
  // from the stream the source makes, so it holds nothing to name a type for.
  if (const auto* stream = std::get_if<hir::StreamingConcatExpr>(&expr.data)) {
    return join(
        stream->operands, StreamedJoin{.block_bits = stream->block_bits},
        unit_lowerer.Unit().builtins.void_type);
  }
  if (const auto* pattern =
          std::get_if<hir::AssignmentPatternExpr>(&expr.data)) {
    const mir::TypeId type = unit_lowerer.TranslateType(expr.type);
    if (unit_lowerer.Unit().types.Get(type).IsIntegral()) {
      return join(pattern->elements, PackedJoin{}, type);
    }
    return join(pattern->elements, UnpackedJoin{}, type);
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
  auto taken = Distribute(unit_lowerer, frame, lvalue, value, shares);
  if (!taken) return std::unexpected(std::move(taken.error()));
  return shares;
}

auto AppendStores(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    mir::ExprId value) -> diag::Result<void> {
  mir::Block& block = *frame.current_block;
  // The value is held before the places are located, so what it reads is what
  // stood when the write began. One place alone has no other store to be
  // ordered against and is left as it was named.
  const mir::ExprId held = std::holds_alternative<LvalueConcat>(lvalue.form)
                               ? EvaluatedOnce(frame, value)
                               : value;
  Lvalue located = lvalue;
  if (std::holds_alternative<LvalueConcat>(located.form)) {
    ForEachPlace(located, [&](AccessPath& place) {
      place = Settled(unit_lowerer, frame, std::move(place));
    });
  }
  auto shares = Shares(unit_lowerer, frame, located, held);
  if (!shares) return std::unexpected(std::move(shares.error()));
  for (const Share& share : *shares) {
    block.AppendStmt(
        mir::ExprStmt{
            .expr = block.exprs.Add(BuildStoreExpr(
                unit_lowerer.Unit(), block, share.place, share.value))});
  }
  return {};
}

auto ReadThenWrite(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, Lvalue lvalue)
    -> diag::Result<LvalueReadThenWritten> {
  mir::CompilationUnit& unit = unit_lowerer.Unit();
  mir::Block& block = *frame.current_block;
  if (auto* place = std::get_if<AccessPath>(&lvalue.form)) {
    ReadThenWritten settled =
        ReadThenWrite(unit_lowerer, frame, std::move(*place));
    lvalue.form = std::move(settled.place);
    return LvalueReadThenWritten{
        .lvalue = std::move(lvalue), .incoming = settled.incoming};
  }
  auto& concat = std::get<LvalueConcat>(lvalue.form);
  std::vector<mir::ExprId> incoming;
  incoming.reserve(concat.elems.size());
  for (Lvalue& elem : concat.elems) {
    auto settled = ReadThenWrite(unit_lowerer, frame, std::move(elem));
    if (!settled) return std::unexpected(std::move(settled.error()));
    elem = std::move(settled->lvalue);
    incoming.push_back(settled->incoming);
  }
  // What the members hold together is the value the join stands for: their
  // bits end to end, or the aggregate they are the members of.
  const mir::ExprId joined = std::visit(
      Overloaded{
          [&](const PackedJoin&) -> mir::ExprId {
            return ConvertToType(
                unit, block, BuildPackedConcat(unit, block, incoming),
                lvalue.type);
          },
          [&](const UnpackedJoin&) -> mir::ExprId {
            for (std::size_t i = 0; i < incoming.size(); ++i) {
              const std::optional<DescentStep> member =
                  MemberStep(unit, block, lvalue.type, i);
              if (member.has_value()) {
                incoming[i] =
                    ConvertToType(unit, block, incoming[i], member->part_type);
              }
            }
            return block.exprs.Add(BuildPositionalAggregate(
                unit_lowerer, block, lvalue.source_type, lvalue.type,
                std::move(incoming)));
          },
          [](const StreamedJoin&) -> mir::ExprId {
            throw InternalError(
                "lvalue: a streaming concatenation is read before it is "
                "written, which the front end refuses (LRM 11.4.14)");
          }},
      concat.kind);
  return LvalueReadThenWritten{.lvalue = std::move(lvalue), .incoming = joined};
}

}  // namespace lyra::lowering::hir_to_mir
