#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/bitstream.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/integral_literal.hpp"
#include "lyra/lowering/hir_to_mir/lhs_store.hpp"
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

// The value a run repeated `count_id` times denotes, landing in the type given
// (LRM 11.4.12). What the run is made of -- bits or characters -- is the
// entry's own question, so the same call serves both.
auto BuildReplicateCall(
    mir::ExprId run, mir::ExprId count_id, mir::TypeId result_type)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = support::BuiltinFn::kReplicate,
                      .receiver = run},
              .arguments = {count_id}},
      .type = result_type};
}

// An unpacked array concatenation (LRM 10.10) as the chain of appends it folds
// to: the empty accumulator the destination declares, then each part appended
// left to right -- one element, or, where the part is itself a container, every
// element it holds in order. No entry composes a part list of arbitrary length,
// so the fold is here and each step is a two-operand call; the accumulator's
// own domain (a queue enforcing its bound, a dynamic array unbounded) selects
// the realization. A spread part crosses erased, its own container domain being
// no concern of the entry that appends its elements. The empty `{}` is the seed
// with no step folded onto it.
auto BuildUnpackedConcatChain(
    const UnitLowerer& owner, WalkFrame frame, mir::TypeId acc_type,
    mir::ExprId element_default, const std::vector<mir::ExprId>& operand_ids)
    -> mir::Expr {
  auto& block = *frame.current_block;
  const mir::CompilationUnit& unit = owner.Unit();
  const mir::TypeId element_type = RequiredContainerElementType(unit, acc_type);
  mir::Expr acc =
      BuildArrayConstructionCall(unit, block, acc_type, element_default, {});
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
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
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
    return BuildUnpackedConcatChain(
        lowerer.Owner(), frame, result_type,
        BuildElementDefault(lowerer.Owner(), block, hir_result_type),
        operand_ids);
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
    const mir::ExprId element_default =
        BuildElementDefault(lowerer.Owner(), block, hir_result_type);
    if (!has_spread) {
      return BuildArrayConstructionCall(
          unit, block, result_type, element_default, std::move(operand_ids));
    }
    const mir::TypeId dyn_type = unit.types.Intern(
        mir::Type{mir::DynamicArrayType{.element_type = element_type}});
    const mir::ExprId dyn_id = block.exprs.Add(BuildUnpackedConcatChain(
        lowerer.Owner(), frame, dyn_type, element_default, operand_ids));
    const mir::ExprId count_id = BuildMachineIntLiteral(
        unit, block,
        static_cast<std::int64_t>(
            result_ty.Get<mir::UnpackedArrayType>().Size()));
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{
                        .target = support::BuiltinFn::kArrayConformSize},
                .arguments = {dyn_id, count_id}},
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
    auto lowered = lowerer.LowerExpr(operand, frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    auto run_or = BuildToBitstream(
        unit, block, block.exprs.Add(*std::move(lowered)), operand.span);
    if (!run_or) return std::unexpected(std::move(run_or.error()));
    runs.push_back(*run_or);
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
  auto count_or = lowerer.LowerExpr(lowerer.HirExprs().Get(r.count), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const mir::ExprId value_id = block.exprs.Add(*std::move(count_or));
  return BuildReplicateCall(
      concat_id, block.exprs.Add(MakeToInt64Call(unit, value_id)), result_type);
}

// A pattern that states every element by position, dispatched on the
// destination type's runtime shape. A struct's keys reach this form already
// resolved into a member-ordered element list (LRM 10.9.2), because members
// differ in type and a key names one of them; an array's keys do not, and are
// lowered from the keys themselves.
//
// The shapes differ only in how they package the positional elements: a packed
// target joins them into one bit plane, because its members share one; an array
// container is a library type and takes the element list as its constructor's
// argument; and an unpacked struct -- whose members are independent value
// slots, not a shared bit plane -- is those elements and nothing more.
template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignmentPatternExpr& a,
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> element_ids;
  element_ids.reserve(a.elements.size());
  for (const auto& id : a.elements) {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(block.exprs.Add(*std::move(lowered)));
  }
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto& result_ty = unit.types.Get(result_type);
  if (BuildsFromAnElementList(result_ty)) {
    return BuildArrayConstructionCall(
        unit, block, result_type,
        BuildElementDefault(lowerer.Owner(), block, hir_result_type),
        std::move(element_ids));
  }
  if (result_ty.IsProduct()) {
    return mir::Expr{
        .data = mir::CompositeExpr{.parts = std::move(element_ids)},
        .type = result_type};
  }
  return BuildValueConversion(
      unit, block, BuildPackedConcat(unit, block, element_ids), result_type);
}

// The whole target before any key names an element of it: every element holding
// the pattern's own `default`, or -- where it wrote none, which LRM 10.9.1
// allows only when every element is named -- the target type's own default
// value, which no key can leave showing.
//
// A packed array's elements share a bit plane, so the default replicated to the
// plane's width is the value; an unpacked array's are separate storage, so the
// repeat entry its family carries builds it. Neither counts out elements, which
// is what keeps a 32768-element target from costing its own length to describe.
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
  if (const auto* packed = hir_ty.template As<hir::PackedArrayType>()) {
    // By value: the pool's view does not survive the interning below.
    const mir::PackedArrayType result_pa =
        unit.types.Get(result_type).PackedShape();
    return BuildValueConversion(
        unit, block,
        block.exprs.Add(BuildReplicateCall(
            fill,
            BuildMachineIntLiteral(
                unit, block,
                static_cast<std::int64_t>(packed->dim.ElementCount())),
            mir::PackedVectorOf(
                unit.types, result_pa.BitWidth(), result_pa.state_kind))),
        result_type);
  }
  const auto& array_ty =
      unit.types.Get(result_type).Get<mir::UnpackedArrayType>();
  return BuildArrayRepeatCall(
      unit, block, result_type,
      BuildElementDefault(lowerer.Owner(), block, hir_result_type), {fill},
      BuildMachineIntLiteral(
          unit, block, static_cast<std::int64_t>(array_ty.dim.ElementCount())));
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
      const mir::PackedArrayType result_pa =
          unit.types.Get(result_type).PackedShape();
      return mir::PackedVectorOf(
          unit.types, result_pa.BitWidth() / packed->dim.ElementCount(),
          result_pa.state_kind);
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
    const mir::ExprId owner =
        body.exprs.Add(mir::MakeLocalRefExpr(built, result_type));
    const WriteTarget target = DescendInto(
        WriteTarget{.owner = owner, .descent = {}},
        DescentStep{
            .value_entry = support::BuiltinFn::kElement,
            .part_entry = support::BuiltinFn::kElementRef,
            .position = std::nullopt,
            .operands = ElementStepOperands(
                lowerer.Owner(), body, result_type, index_id),
            .part_type = element_type});
    body.AppendStmt(
        mir::ExprStmt{
            .expr = body.exprs.Add(BuildStoreExpr(
                unit, body, target, value_id, std::nullopt, element_type))});
  }

  return steps.Build(body.exprs.Add(mir::MakeLocalRefExpr(built, result_type)));
}

template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternReplicationExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::AssignmentPatternReplicationExpr& a, hir::TypeId hir_result_type,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> item_ids;
  item_ids.reserve(a.items.size());
  for (const auto& id : a.items) {
    auto lowered = lowerer.LowerExpr(lowerer.HirExprs().Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(block.exprs.Add(*std::move(lowered)));
  }
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto& result_ty = unit.types.Get(result_type);

  // A structure's members differ in type, so there is no repeat for the target
  // to carry out: the items land in member positions here, and how many
  // positions there are is what the structure's own type says (LRM 10.9). The
  // multiplier states the same number the type does, so nothing reads it.
  if (result_ty.IsProduct()) {
    const std::size_t position_count = result_ty.ProductComponentTypes().size();
    std::vector<mir::ExprId> components;
    components.reserve(position_count);
    for (std::size_t i = 0; i < position_count; ++i) {
      components.push_back(item_ids[i % item_ids.size()]);
    }
    return mir::Expr{
        .data = mir::CompositeExpr{.parts = std::move(components)},
        .type = result_type};
  }

  auto count_or = lowerer.LowerExpr(lowerer.HirExprs().Get(a.count), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const mir::ExprId count_value = block.exprs.Add(*std::move(count_or));
  const mir::ExprId count_id =
      block.exprs.Add(MakeToInt64Call(unit, count_value));
  if (BuildsFromAnElementList(result_ty)) {
    return BuildArrayRepeatCall(
        unit, block, result_type,
        BuildElementDefault(lowerer.Owner(), block, hir_result_type),
        std::move(item_ids), count_id);
  }
  const mir::ExprId inner_id = BuildPackedConcat(unit, block, item_ids);
  const mir::PackedArrayType& inner_pa =
      unit.types.Get(block.exprs.Get(inner_id).type).PackedShape();
  const mir::ExprId repl_id = block.exprs.Add(BuildReplicateCall(
      inner_id, count_id,
      mir::PackedVectorOf(
          unit.types, result_ty.PackedShape().BitWidth(),
          inner_pa.state_kind)));
  return BuildValueConversion(unit, block, repl_id, result_type);
}

// LRM 7.5.1 `new[N]` / `new[N](other)`. The argument list on the lowered
// construction call is `[size, element-default prototype, optional copy
// source]`: the prototype carries the element type's default value -- a
// struct element's own member initializers included (LRM 7.2.2) -- so the
// runtime ctor populates new slots without re-querying the type, and the
// optional copy source feeds the LRM 7.5.1 truncate / pad behaviour on
// `new[N](other)`.
template <ExprLowerer Lowerer>
auto LowerHirDynamicArrayNewExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::DynamicArrayNewExpr& n,
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  auto size_or = lowerer.LowerExpr(lowerer.HirExprs().Get(n.size), frame);
  if (!size_or) return std::unexpected(std::move(size_or.error()));
  const mir::ExprId size_id = block.exprs.Add(*std::move(size_or));

  const hir::Type& hir_result_ty =
      lowerer.Owner().Hir().types.Get(hir_result_type);
  const auto* hir_da = hir_result_ty.As<hir::DynamicArrayType>();
  if (hir_da == nullptr) {
    throw InternalError(
        "LowerHirDynamicArrayNewExpr: result type is not DynamicArrayType");
  }
  const mir::ExprId prototype_id = block.exprs.Add(
      BuildDefaultValueFromHir(lowerer.Owner(), block, hir_da->element_type));

  std::vector<mir::ExprId> args;
  args.reserve(n.initializer.has_value() ? 3U : 2U);
  args.push_back(size_id);
  args.push_back(prototype_id);
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
// them as tuples and threads the optional persistent default through the
// associative constructor.
template <ExprLowerer Lowerer>
auto LowerHirAssociativeAssignmentPatternExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::AssociativeAssignmentPatternExpr& a, hir::TypeId hir_result_type,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
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
  std::optional<mir::ExprId> user_default;
  if (a.default_value.has_value()) {
    auto default_or =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*a.default_value), frame);
    if (!default_or) return std::unexpected(std::move(default_or.error()));
    user_default = block.exprs.Add(*std::move(default_or));
  }
  return BuildAssociativeConstructionCall(
      lowerer.Owner().Unit(), block, result_type,
      BuildElementDefault(lowerer.Owner(), block, hir_result_type),
      std::move(entries), user_default);
}

// One concrete instantiation per pass class. The handler templates are defined
// in this file rather than the header so the file-local helpers stay private,
// so the dispatchers in process_lowerer.cpp / structural_scope_lowerer.cpp link
// against the symbols emitted here.
template auto LowerHirConcatExpr(
    ProcessLowerer&, WalkFrame, const hir::ConcatExpr&, hir::TypeId,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirConcatExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::ConcatExpr&,
    hir::TypeId, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirStreamingConcatExpr(
    ProcessLowerer&, WalkFrame, const hir::StreamingConcatExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirStreamingConcatExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::StreamingConcatExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternExpr(
    ProcessLowerer&, WalkFrame, const hir::AssignmentPatternExpr&, hir::TypeId,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::AssignmentPatternExpr&,
    hir::TypeId, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternReplicationExpr(
    ProcessLowerer&, WalkFrame, const hir::AssignmentPatternReplicationExpr&,
    hir::TypeId, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternReplicationExpr(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::AssignmentPatternReplicationExpr&, hir::TypeId, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternKeyedExpr(
    ProcessLowerer&, WalkFrame, const hir::AssignmentPatternKeyedExpr&,
    hir::TypeId, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssignmentPatternKeyedExpr(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::AssignmentPatternKeyedExpr&, hir::TypeId, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirDynamicArrayNewExpr(
    ProcessLowerer&, WalkFrame, const hir::DynamicArrayNewExpr&, hir::TypeId,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirDynamicArrayNewExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::DynamicArrayNewExpr&,
    hir::TypeId, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssociativeAssignmentPatternExpr(
    ProcessLowerer&, WalkFrame, const hir::AssociativeAssignmentPatternExpr&,
    hir::TypeId, mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirAssociativeAssignmentPatternExpr(
    const StructuralScopeLowerer&, WalkFrame,
    const hir::AssociativeAssignmentPatternExpr&, hir::TypeId, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirReplicationExpr(
    ProcessLowerer&, WalkFrame, const hir::ReplicationExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirReplicationExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::ReplicationExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
