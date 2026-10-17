#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/hir/expr.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/packed_concat.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 10.10: an unpacked concatenation part that contributes its elements in
// order rather than contributing itself as one element. The array being built
// is what settles it: a part of the element type is one element however
// array-shaped that type is, and any other container spreads. Where the
// element type is itself a container, both readings fit the part's own type
// and only the destination tells them apart.
auto ContributesItsElements(
    const mir::CompilationUnit& unit, mir::TypeId part, mir::TypeId element)
    -> bool {
  if (part == element) {
    return false;
  }
  return unit.types.Get(part).ContainerElementType().has_value();
}

// A container whose value is built from a positional list of its elements,
// which is every array-shaped one. An associative array holds elements of one
// type like the rest and is still not among them: LRM 7.9.11 builds it from
// key-value entries, so there is no list to lay down or to repeat.
//
// What an operand contributes and what a result can be assembled as are two
// questions whose answers coincide over these three kinds and whose LRM clauses
// are not the same, so neither one stands in for the other.
auto BuildsFromAnElementList(const mir::Type& ty) -> bool {
  return ty.Is<mir::UnpackedArrayType>() || ty.Is<mir::DynamicArrayType>() ||
         ty.Is<mir::QueueType>();
}

// Bits repeated as many times as fill `result_type`, an integral type (LRM
// 11.4.12.1).
auto BuildBitsReplication(
    const mir::CompilationUnit& unit, const mir::Block& block, mir::ExprId run,
    mir::TypeId result_type) -> mir::Expr {
  return MakeBuiltinCall(
      unit, block, support::BuiltinFn::kReplicateBits, run, {}, result_type);
}

// An unpacked array concatenation (LRM 10.10) as the chain of appends it folds
// to: the empty accumulator the destination declares, then each part appended
// left to right -- one element, or, where the part is itself a container, every
// element it holds in order. No entry composes a part list of arbitrary length,
// so the fold is here and each step is a two-operand call; the accumulator's
// own domain (a queue enforcing its bound, a dynamic array unbounded) selects
// the realization. A spread part crosses erased, its own container domain being
// no concern of the entry that appends its elements. The empty `{}` is the
// accumulator with no step folded onto it.
auto BuildUnpackedConcatChain(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId acc_type,
    const std::vector<mir::ExprId>& operand_ids) -> mir::Expr {
  const mir::TypeId element_type = RequiredContainerElementType(unit, acc_type);
  mir::Expr acc = BuildArrayConstructionCall(unit, block, acc_type, {});
  for (const mir::ExprId part : operand_ids) {
    const bool spread =
        ContributesItsElements(unit, block.exprs.Get(part).type, element_type);
    const mir::ExprId acc_id = block.exprs.Add(std::move(acc));
    acc = mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = spread
                                      ? support::BuiltinFn::kArrayConcatSpread
                                      : support::BuiltinFn::kArrayConcatElement,
                        .receiver = acc_id},
                .arguments = {part}},
        .type = acc_type};
  }
  return acc;
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerHirConcatExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ConcatExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> operand_ids;
  operand_ids.reserve(c.operands.size());
  for (const auto& id : c.operands) {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    operand_ids.push_back(block.exprs.Add(*std::move(lowered)));
  }
  // What the operator joins -- bits, characters, elements -- differs by operand
  // family, so which operation this is settles here, where the family is known,
  // rather than at each consumer.
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto& result_ty = unit.types.Get(result_type);
  // Characters join two at a time, because no entry takes an operand list of
  // arbitrary length; a source-level join of one is already the string it
  // names.
  if (result_ty.Is<mir::StringType>()) {
    if (operand_ids.size() == 1) {
      return block.exprs.Get(operand_ids.front());
    }
    const auto join = [&](mir::ExprId lhs, mir::ExprId rhs) {
      return mir::Expr{
          .data =
              mir::CallExpr{
                  .callee =
                      mir::Direct{
                          .target = support::BuiltinFn::kConcat,
                          .receiver = lhs},
                  .arguments = {rhs}},
          .type = result_type};
    };
    mir::ExprId lhs = operand_ids.front();
    for (std::size_t i = 1; i + 1 < operand_ids.size(); ++i) {
      lhs = block.exprs.Add(join(lhs, operand_ids[i]));
    }
    return join(lhs, operand_ids.back());
  }
  // A queue or a dynamic array is grown from the parts directly, the chain
  // building at the destination's own type -- which is also what decides what
  // each part contributes.
  if (result_ty.Is<mir::QueueType>() || result_ty.Is<mir::DynamicArrayType>()) {
    return BuildUnpackedConcatChain(unit, block, result_type, operand_ids);
  }
  // A fixed-size unpacked array whose parts are all single elements is the
  // assignment pattern it coincides with (LRM 10.10.1), built by position. One
  // carrying a spread has a run-time element count, so it grows a dynamic array
  // from the parts and then adopts it into the fixed target, which is an error
  // when the counts differ (LRM 10.10). The front end has already rejected a
  // spread-free mismatch, so only the spread form can reach the run-time check.
  if (result_ty.Is<mir::UnpackedArrayType>()) {
    const mir::TypeId element_type =
        RequiredContainerElementType(unit, result_type);
    const bool has_spread =
        std::ranges::any_of(operand_ids, [&](mir::ExprId part) {
          return ContributesItsElements(
              unit, block.exprs.Get(part).type, element_type);
        });
    if (!has_spread) {
      return BuildArrayConstructionCall(
          unit, block, result_type, std::move(operand_ids));
    }
    const mir::TypeId dyn_type = unit.types.Intern(
        mir::Type{mir::DynamicArrayType{.element_type = element_type}});
    const mir::ExprId dyn_id = block.exprs.Add(
        BuildUnpackedConcatChain(unit, block, dyn_type, operand_ids));
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kArrayConformSize},
                .arguments = {dyn_id}},
        .type = result_type};
  }
  return BuildValueConversion(
      unit, block, BuildPackedConcat(unit, block, operand_ids), result_type);
}

template <ExprLowerer Lowerer>
auto LowerHirStreamingConcatExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::StreamingConcatExpr& s,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  // LRM 11.4.14.1: each operand contributes its own bits, appended to the right
  // of what the operands before it contributed, so the first is most
  // significant -- which is the order the join already composes in.
  std::vector<mir::ExprId> runs;
  runs.reserve(s.operands.size());
  for (const auto& id : s.operands) {
    const hir::Expr& operand = lowerer.HirExprs().Get(id);
    auto lowered = lowerer.LowerAsPlace(operand, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    auto run_or = BuildToBitstream(
        unit, block, block.exprs.Add(*std::move(lowered)), operand.span);
    if (!run_or) return std::unexpected(std::move(run_or.error()));
    runs.push_back(block.AsValue(unit.types, *run_or));
  }
  const mir::ExprId stream = BuildReorderedStream(
      unit, block, BuildPackedConcat(unit, block, runs), s.block_bits);
  return BuildValueConversion(unit, block, stream, result_type);
}

template <ExprLowerer Lowerer>
auto LowerHirReplicationExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ReplicationExpr& r,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  auto concat_or = lowerer.LowerExpr(lowerer.HirExprs().Get(r.concat), frame);
  if (!concat_or) return std::unexpected(std::move(concat_or.error()));
  const mir::ExprId concat_id = block.exprs.Add(*std::move(concat_or));
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  if (unit.types.Get(result_type).IsIntegral()) {
    return BuildBitsReplication(unit, block, concat_id, result_type);
  }
  // Characters are repeated as many times as the multiplier evaluates to, which
  // for a string need not be a constant (LRM 11.4.12.2).
  auto count_or = lowerer.LowerExpr(lowerer.HirExprs().Get(r.count), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const mir::ExprId value_id = block.exprs.Add(*std::move(count_or));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kReplicateString,
                      .receiver = concat_id},
              .arguments = {BuildToInt64Call(unit, block, value_id)}},
      .type = result_type};
}

auto BuildPositionalAggregate(
    mir::CompilationUnit& unit, mir::Block& block, mir::TypeId result_type,
    std::vector<mir::ExprId> elements) -> mir::Expr {
  const auto& result_ty = unit.types.Get(result_type);
  if (BuildsFromAnElementList(result_ty)) {
    return BuildArrayConstructionCall(
        unit, block, result_type, std::move(elements));
  }
  if (mir::ProductElements(unit, result_type).has_value()) {
    return mir::Expr{
        .data = mir::CompositeExpr{.parts = std::move(elements)},
        .type = result_type};
  }
  return BuildValueConversion(
      unit, block, BuildPackedConcat(unit, block, elements), result_type);
}

// A pattern that states every element by position, dispatched on the
// destination type's runtime shape. A struct's keys reach this form already
// resolved into a member-ordered element list (LRM 10.9.2), because members
// differ in type and a key names one of them; an array's keys do not, and are
// lowered from the keys themselves.
//
// The shapes differ only in how they package the positional elements: a packed
// target joins them into one bit plane, because its members share one; an array
// is built by its construction, which takes the element list as an argument;
// and an unpacked struct -- whose members are independent value slots, not a
// shared bit plane -- is those elements and nothing more.
template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignmentPatternExpr& a,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> element_ids;
  element_ids.reserve(a.elements.size());
  for (const auto& id : a.elements) {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(block.exprs.Add(*std::move(lowered)));
  }
  return BuildPositionalAggregate(
      lowerer.Owner().Unit(), block, result_type, std::move(element_ids));
}

// The whole target before any key names an element of it: every element holding
// the pattern's own `default`, or -- where it wrote none, which LRM 10.9.1
// allows only when every element is named -- the target type's own default
// value, which no key can leave showing.
//
// A packed array's elements share a bit plane, so the default replicated to the
// plane's width is the value; an unpacked array's are separate storage, so its
// construction lays the one default down until the array is full. Neither
// counts out elements, which is what keeps a 32768-element target from costing
// its own length to describe.
template <ExprLowerer Lowerer>
auto BuildKeyedPatternBase(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignmentPatternKeyedExpr& k,
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  if (!k.default_value.has_value()) {
    return BuildDefaultValueFromHir(lowerer.Owner(), block, hir_result_type);
  }
  auto fill_or =
      lowerer.LowerExpr(lowerer.HirExprs().Get(*k.default_value), frame);
  if (!fill_or) return std::unexpected(std::move(fill_or.error()));
  const mir::ExprId fill = block.exprs.Add(*std::move(fill_or));

  const auto& hir_ty = lowerer.Owner().Hir().types.Get(hir_result_type);
  if (hir_ty.template Is<hir::PackedArrayType>()) {
    // By value: the pool's view does not survive the interning below.
    const mir::IntegralType result_integral =
        unit.types.Get(result_type).Integral();
    const mir::TypeId copies = mir::PackedVectorOf(
        unit.types, result_integral.bit_width, result_integral.state_kind);
    return BuildValueConversion(
        unit, block,
        block.exprs.Add(BuildBitsReplication(unit, block, fill, copies)),
        result_type);
  }
  return BuildFixedArrayConstructionCall(unit, block, result_type, {fill});
}

// LRM 10.9.1 `'{index: value, ..., default: value}`: the elements no index
// named take the default, and how many that is comes from the target type
// rather than from anything the source wrote. Keeping the default rather than
// the elements it stands for is what holds a mostly-uniform array at O(named)
// to describe where an element list would make it O(size) -- written out, a
// 32768-element array reaches the target language as a four-megabyte
// expression that no compiler accepts.
//
// So the target is built filled and the elements the pattern names are written
// over it, whatever its representation and whatever its keys. A key is an
// expression evaluated here, so which element it names is reached by descending
// onto it rather than by a position counted out while lowering: what settles a
// key may be a value the construction supplies, and every key in one pattern is
// reached the same way whether or not this one's did.
template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternKeyedExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignmentPatternKeyedExpr& k,
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto& hir_ty = lowerer.Owner().Hir().types.Get(hir_result_type);
  const mir::TypeId element_type = [&] {
    if (const auto* packed = hir_ty.template As<hir::PackedArrayType>()) {
      const mir::IntegralType result_integral =
          unit.types.Get(result_type).Integral();
      return mir::PackedVectorOf(
          unit.types, result_integral.bit_width / packed->dim.ElementCount(),
          result_integral.state_kind);
    }
    return unit.types.Get(result_type)
        .Get<mir::UnpackedArrayType>()
        .element_type;
  }();

  const auto build_base = [&](WalkFrame at) -> diag::Result<mir::Expr> {
    return BuildKeyedPatternBase(lowerer, at, k, hir_result_type, result_type);
  };

  if (k.entries.empty()) {
    return build_base(frame);
  }

  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const WalkFrame& step_frame = steps.Frame();

  auto base = build_base(step_frame);
  if (!base) return std::unexpected(std::move(base.error()));
  const mir::LocalId built = steps.Bindings().DeclareAnonymous(result_type);
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = built, .init = body.exprs.Add(*std::move(base))});

  for (const auto& entry : k.entries) {
    auto index =
        lowerer.LowerExpr(lowerer.HirExprs().Get(entry.index), step_frame);
    if (!index) return std::unexpected(std::move(index.error()));
    auto value =
        lowerer.LowerExpr(lowerer.HirExprs().Get(entry.value), step_frame);
    if (!value) return std::unexpected(std::move(value.error()));
    const mir::ExprId index_id = body.exprs.Add(*std::move(index));
    const mir::ExprId value_id = body.exprs.Add(*std::move(value));
    const mir::ExprId owner = body.LocalPlace(built, result_type);
    const AccessPath target = DescendInto(
        AccessPath{.owner = owner, .descent = {}},
        ElementStep(
            lowerer.Owner(), body, hir_result_type, index_id, element_type));
    body.AppendStmt(
        mir::ExprStmt{
            .expr =
                body.exprs.Add(BuildStoreExpr(unit, body, target, value_id))});
  }

  return steps.Build(body.LocalPlace(built, result_type));
}

template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternReplicationExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::AssignmentPatternReplicationExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto lower_item = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return block.exprs.Add(*std::move(lowered));
  };

  // A structure's members differ in type, so there is no repeat for the target
  // to carry out: the items land in member positions here, and how many
  // positions there are is what the structure's own type says (LRM 10.9). The
  // multiplier states the same number the type does, so nothing reads it. An
  // item is stated at each position it lands in, which evaluates it once per
  // position; LRM 10.9.1 leaves that count undefined.
  if (const std::optional<std::span<const mir::TypeId>> parts =
          mir::ProductElements(unit, result_type)) {
    const std::size_t position_count = parts->size();
    std::vector<mir::ExprId> components;
    components.reserve(position_count);
    for (std::size_t i = 0; i < position_count; ++i) {
      auto item = lower_item(a.items[i % a.items.size()]);
      if (!item) return std::unexpected(std::move(item.error()));
      components.push_back(*item);
    }
    return mir::Expr{
        .data = mir::CompositeExpr{.parts = std::move(components)},
        .type = result_type};
  }

  std::vector<mir::ExprId> item_ids;
  item_ids.reserve(a.items.size());
  for (const auto& id : a.items) {
    auto item = lower_item(id);
    if (!item) return std::unexpected(std::move(item.error()));
    item_ids.push_back(*item);
  }

  // A fixed-size array is the items laid down until it is full, and how many
  // times that is its own type says, as a structure's does.
  if (unit.types.Get(result_type).Is<mir::UnpackedArrayType>()) {
    return BuildFixedArrayConstructionCall(
        unit, block, result_type, std::move(item_ids));
  }

  // Every type read below is read by value or where it is used: lowering the
  // count and building the concatenation intern types, which can relocate what
  // the pool holds.
  if (BuildsFromAnElementList(unit.types.Get(result_type))) {
    auto count_or = lowerer.LowerExpr(lowerer.HirExprs().Get(a.count), frame);
    if (!count_or) return std::unexpected(std::move(count_or.error()));
    const mir::ExprId count_value = block.exprs.Add(*std::move(count_or));
    const mir::ExprId count_id = BuildToInt64Call(unit, block, count_value);
    return BuildArrayRepeatCall(
        unit, block, result_type, std::move(item_ids), count_id);
  }
  // A packed target is as many copies of the items as fill it, which its own
  // type states.
  const mir::ExprId inner_id = BuildPackedConcat(unit, block, item_ids);
  const mir::IntegralStateKind inner_state =
      unit.types.Get(block.exprs.Get(inner_id).type).Integral().state_kind;
  const std::uint64_t result_width =
      unit.types.Get(result_type).Integral().bit_width;
  const mir::TypeId copies =
      mir::PackedVectorOf(unit.types, result_width, inner_state);
  const mir::ExprId repl_id =
      block.exprs.Add(BuildBitsReplication(unit, block, inner_id, copies));
  return BuildValueConversion(unit, block, repl_id, result_type);
}

// LRM 7.5.1 `new[N]` / `new[N](other)`. The lowered call's arguments are the
// size and, where the source wrote one, the array whose elements the new one
// starts from, truncated or padded to the size. What a slot nothing is copied
// into holds is the element type's to say, so the call states no value for it.
template <ExprLowerer Lowerer>
auto LowerHirDynamicArrayNewExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::DynamicArrayNewExpr& n,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  auto size_or = lowerer.LowerExpr(lowerer.HirExprs().Get(n.size), frame);
  if (!size_or) return std::unexpected(std::move(size_or.error()));
  const mir::ExprId size_id =
      BuildToInt64Call(unit, block, block.exprs.Add(*std::move(size_or)));

  std::vector<mir::ExprId> args{size_id};
  if (n.initializer.has_value()) {
    auto init_or =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*n.initializer), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    args.push_back(block.exprs.Add(*std::move(init_or)));
  }
  const support::BuiltinFn form =
      n.initializer.has_value() ? support::BuiltinFn::kMakeDynamicArrayNewCopy
                                : support::BuiltinFn::kMakeDynamicArrayNew;
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = form},
              .arguments = std::move(args)},
      .type = result_type};
}

// LRM 7.9.11 associative literal. Each (key, value) entry is lowered into a
// pair of MIR ExprIds and handed to the shared construction helper, which wraps
// them as tuples; the `default:` clause, where the literal writes one, is the
// value a read of an absent key answers with (LRM 7.8.6).
template <ExprLowerer Lowerer>
auto LowerHirAssociativeAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::AssociativeAssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  const mir::CompilationUnit& unit = lowerer.Owner().Unit();
  std::vector<std::pair<mir::ExprId, mir::ExprId>> entries;
  entries.reserve(a.entries.size());
  for (const auto& entry : a.entries) {
    auto key_or = lowerer.LowerExpr(lowerer.HirExprs().Get(entry.key), frame);
    if (!key_or) return std::unexpected(std::move(key_or.error()));
    const mir::ExprId key_id = block.exprs.Add(*std::move(key_or));
    auto value_or =
        lowerer.LowerExpr(lowerer.HirExprs().Get(entry.value), frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::ExprId value_id = block.exprs.Add(*std::move(value_or));
    entries.emplace_back(key_id, value_id);
  }
  std::optional<mir::ExprId> stated_default;
  if (a.default_value.has_value()) {
    auto default_or =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*a.default_value), frame);
    if (!default_or) return std::unexpected(std::move(default_or.error()));
    stated_default = block.exprs.Add(*std::move(default_or));
  }
  return BuildAssociativeConstructionCall(
      unit, block, result_type, std::move(entries), stated_default);
}

// One concrete instantiation per pass class. The handler templates are defined
// in this file rather than the header so the file-local helpers stay private,
// so each pass class's dispatcher links against the symbols emitted here.
template auto LowerHirConcatExpr(
    ProcessLowerer&, WalkFrame, const hir::ConcatExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirConcatExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::ConcatExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirStreamingConcatExpr(
    ProcessLowerer&, WalkFrame, const hir::StreamingConcatExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirStreamingConcatExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::StreamingConcatExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternExpr(
    ProcessLowerer&, WalkFrame, const hir::AssignmentPatternExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::AssignmentPatternExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternReplicationExpr(
    ProcessLowerer&, WalkFrame, const hir::AssignmentPatternReplicationExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternReplicationExpr(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::AssignmentPatternReplicationExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternKeyedExpr(
    ProcessLowerer&, WalkFrame, const hir::AssignmentPatternKeyedExpr&,
    hir::TypeId, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternKeyedExpr(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::AssignmentPatternKeyedExpr&, hir::TypeId, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirDynamicArrayNewExpr(
    ProcessLowerer&, WalkFrame, const hir::DynamicArrayNewExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirDynamicArrayNewExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::DynamicArrayNewExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssociativeAssignmentPatternExpr(
    ProcessLowerer&, WalkFrame, const hir::AssociativeAssignmentPatternExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssociativeAssignmentPatternExpr(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::AssociativeAssignmentPatternExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirReplicationExpr(
    ProcessLowerer&, WalkFrame, const hir::ReplicationExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirReplicationExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::ReplicationExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
