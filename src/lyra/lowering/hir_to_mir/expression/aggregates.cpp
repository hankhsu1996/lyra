#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/block_builder.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
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

auto IsArrayContainerType(const mir::Type& ty) -> bool {
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
              .callee = mir::Direct{.target = support::BuiltinFn::kReplicate},
              .arguments = {run, count_id}},
      .type = result_type};
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
                  .callee = mir::Direct{.target = support::BuiltinFn::kConcat},
                  .arguments = {lhs, rhs}},
          .type = result_type};
    };
    mir::ExprId lhs = operand_ids.front();
    for (std::size_t i = 1; i + 1 < operand_ids.size(); ++i) {
      lhs = block.exprs.Add(join(lhs, operand_ids[i]));
    }
    return join(lhs, operand_ids.back());
  }
  // An unpacked queue needs more than its parts: a default value of its
  // declared element type (an empty `{}` part list cannot supply one) and its
  // LRM 7.10.5 bound, built here as ordinary arguments ahead of them. A part
  // whose value is itself an array contributes its elements in order (LRM
  // 10.10), which is spread and is marked so -- the same array value would be
  // one element if the queue's element type were an array, so the role is the
  // program's fact, not the operand type's.
  if (const auto* q = result_ty.As<mir::QueueType>()) {
    const mir::TypeId element_type = q->element_type;
    const std::int64_t bound = q->max_bound.has_value()
                                   ? static_cast<std::int64_t>(*q->max_bound)
                                   : -1;
    std::vector<mir::ExprId> args;
    args.reserve(operand_ids.size() + 2);
    args.push_back(block.exprs.Add(
        BuildDefaultValueExpr(lowerer.Owner(), frame, element_type)));
    args.push_back(BuildMachineIntLiteral(unit, block, bound));
    for (const mir::ExprId part : operand_ids) {
      const mir::TypeId part_type = block.exprs.Get(part).type;
      if (!IsArrayContainerType(unit.types.Get(part_type))) {
        args.push_back(part);
        continue;
      }
      args.push_back(block.exprs.Add(
          mir::Expr{
              .data =
                  mir::CallExpr{
                      .callee =
                          mir::Direct{.target = support::BuiltinFn::kSpread},
                      .arguments = {part}},
              .type = part_type}));
    }
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee =
                    mir::Direct{.target = support::BuiltinFn::kMakeQueueConcat},
                .arguments = std::move(args)},
        .type = result_type};
  }
  return BuildValueConversion(
      unit, block, BuildPackedConcat(unit, block, operand_ids), result_type);
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
// target joins them into one bit plane, because its members share one; an
// array container (unpacked, dynamic, queue) lands as `ArrayLiteralExpr` slots
// wrapped by a construction call; and an unpacked struct -- whose members are
// independent value slots, not a shared bit plane -- folds into a positional
// `TupleExpr`.
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
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto& result_ty = unit.types.Get(result_type);
  if (IsArrayContainerType(result_ty)) {
    return BuildArrayConstructionCall(
        lowerer.Owner(), frame, result_type, std::move(element_ids));
  }
  if (result_ty.Is<mir::TupleType>()) {
    return mir::Expr{
        .data = mir::TupleExpr{.components = std::move(element_ids)},
        .type = result_type};
  }
  return BuildValueConversion(
      unit, block, BuildPackedConcat(unit, block, element_ids), result_type);
}

// Resolves each `index: value` entry to the offset its index names, leaving
// the offsets no index named empty.
//
// An `index:value` names the element `a[index]` (LRM 10.9.1), while an offset
// counts from the dimension's left end -- the most significant element of a
// packed array (LRM 7.4.1), storage ordinal zero of an unpacked one (LRM 7.6).
// The two orders agree only for an ascending dimension, so asking the dimension
// is what keeps the correspondence from being stated a second time and
// silently diverging.
template <ExprLowerer Lowerer, typename Range>
auto LowerKeyedEntriesByOffset(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignmentPatternKeyedExpr& k,
    const Range& dim) -> diag::Result<std::vector<std::optional<mir::Expr>>> {
  std::vector<std::optional<mir::Expr>> by_offset(dim.ElementCount());
  for (const auto& entry : k.entries) {
    auto value = lowerer.LowerExpr(lowerer.HirExprs().Get(entry.value), frame);
    if (!value) return std::unexpected(std::move(value.error()));
    by_offset[dim.LinearOffset(entry.index)] = *std::move(value);
  }
  return by_offset;
}

// LRM 10.9.1 `'{index: value, ..., default: value}` over a packed array. The
// elements share one bit plane, so what builds the value is the plane's own
// vocabulary: a run of elements taking the default is one replication however
// long it is, and the named ones sit between those runs.
template <ExprLowerer Lowerer>
auto LowerPackedKeyedPattern(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignmentPatternKeyedExpr& k,
    const hir::PackedRange& dim, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const mir::PackedArrayType& result_pa =
      unit.types.Get(result_type).PackedShape();
  const std::uint64_t element_width = result_pa.BitWidth() / dim.ElementCount();

  auto by_offset = LowerKeyedEntriesByOffset(lowerer, frame, k, dim);
  if (!by_offset) return std::unexpected(std::move(by_offset.error()));

  std::optional<mir::ExprId> fill_id;
  if (k.default_value.has_value()) {
    auto fill =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*k.default_value), frame);
    if (!fill) return std::unexpected(std::move(fill.error()));
    fill_id = block.exprs.Add(*std::move(fill));
  }

  std::vector<mir::ExprId> parts;
  std::uint64_t run = 0;
  const auto flush_run = [&] {
    if (run == 0) return;
    if (!fill_id.has_value()) {
      throw InternalError(
          "LowerPackedKeyedPattern: no index named this element and the "
          "pattern carries no default, which LRM 10.9.1 forbids");
    }
    const mir::ExprId run_id =
        BuildMachineIntLiteral(unit, block, static_cast<std::int64_t>(run));
    parts.push_back(block.exprs.Add(BuildReplicateCall(
        *fill_id, run_id,
        mir::PackedVectorOf(
            unit.types, run * element_width, result_pa.state_kind))));
    run = 0;
  };
  for (auto& element : *by_offset) {
    if (!element.has_value()) {
      ++run;
      continue;
    }
    flush_run();
    parts.push_back(block.exprs.Add(*std::move(element)));
  }
  flush_run();

  return BuildValueConversion(
      unit, block, BuildPackedConcat(unit, block, parts), result_type);
}

// LRM 10.9.1 `'{index: value, ..., default: value}`: the elements no index
// named take the default, and how many that is comes from the target type
// rather than from anything the source wrote. Keeping the default rather than
// the elements it stands for is what holds a mostly-uniform array at O(named)
// to describe where an element list would make it O(size) -- written out, a
// 32768-element array reaches the target language as a four-megabyte
// expression that no compiler accepts.
//
// The target's representation decides how that is built. A packed array is a
// single value with a replication form of its own, so it stays one expression
// whatever the keys are. An unpacked array's elements are separate storage, so
// a default fills the array and the named elements are written over it, which
// takes several steps and therefore a block yielding the array it built --
// unless there is no default, in which case every element carries an index and
// the complete list constructs the array directly.
template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternKeyedExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::AssignmentPatternKeyedExpr& k,
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  mir::CompilationUnit& unit = lowerer.Owner().Unit();
  const auto& hir_ty = lowerer.Owner().Hir().types.Get(hir_result_type);
  if (const auto* packed = hir_ty.template As<hir::PackedArrayType>()) {
    return LowerPackedKeyedPattern(lowerer, frame, k, packed->dim, result_type);
  }

  const auto& array_ty =
      unit.types.Get(result_type).Get<mir::UnpackedArrayType>();

  if (!k.default_value.has_value()) {
    auto& block = *frame.current_block;
    auto by_offset = LowerKeyedEntriesByOffset(lowerer, frame, k, array_ty.dim);
    if (!by_offset) return std::unexpected(std::move(by_offset.error()));
    std::vector<mir::ExprId> elements;
    elements.reserve(by_offset->size());
    for (auto& element : *by_offset) {
      if (!element.has_value()) {
        throw InternalError(
            "LowerHirAssignmentPatternKeyedExpr: no index named this element "
            "and the pattern carries no default, which LRM 10.9.1 forbids");
      }
      elements.push_back(block.exprs.Add(*std::move(element)));
    }
    return BuildArrayConstructionCall(
        lowerer.Owner(), frame, result_type, std::move(elements));
  }

  const mir::TypeId element_type = ArrayContainerElementType(unit, result_type);
  const auto build_filled = [&](WalkFrame at) -> diag::Result<mir::Expr> {
    auto& target = *at.current_block;
    auto value =
        lowerer.LowerExpr(lowerer.HirExprs().Get(*k.default_value), at);
    if (!value) return std::unexpected(std::move(value.error()));
    const mir::ExprId value_id = target.exprs.Add(*std::move(value));
    const mir::ExprId element_default = target.exprs.Add(
        BuildDefaultValueExpr(lowerer.Owner(), at, element_type));
    const mir::ExprId size_id = BuildMachineIntLiteral(
        unit, target, static_cast<std::int64_t>(array_ty.dim.ElementCount()));
    return BuildArrayRepeatCall(
        lowerer.Owner(), at, result_type, element_default, {value_id}, size_id);
  };

  if (k.entries.empty()) {
    return build_filled(frame);
  }

  BlockBuilder steps(frame);
  mir::Block& body = steps.Body();
  const WalkFrame& step_frame = steps.Frame();

  auto filled = build_filled(step_frame);
  if (!filled) return std::unexpected(std::move(filled.error()));
  const mir::LocalId array = steps.Bindings().DeclareAnonymous(
      mir::LocalDecl{.name = "_lyra_pattern", .type = result_type});
  body.AppendStmt(
      mir::LocalDeclStmt{
          .target = array, .init = body.exprs.Add(*std::move(filled))});

  for (const auto& entry : k.entries) {
    auto value =
        lowerer.LowerExpr(lowerer.HirExprs().Get(entry.value), step_frame);
    if (!value) return std::unexpected(std::move(value.error()));
    const mir::ExprId index_id = BuildIntLiteral(unit, body, entry.index);
    const mir::ExprId value_id = body.exprs.Add(*std::move(value));
    const mir::ExprId owner = body.exprs.Add(
        mir::Expr{.data = mir::LocalRef{.var = array}, .type = result_type});
    const mir::ExprId target = body.exprs.Add(
        mir::Expr{
            .data =
                mir::ValueProjectionExpr{
                    .owner = owner,
                    .path = {mir::ElementSelector{
                        .operands =
                            {index_id,
                             BuildIntLiteral(unit, body, array_ty.dim.left),
                             BuildIntLiteral(unit, body, array_ty.dim.right)},
                        .projected_type = element_type}}},
            .type = element_type});
    const mir::ExprId assign = body.exprs.Add(
        mir::Expr{
            .data = mir::AssignExpr{.target = target, .value = value_id},
            .type = element_type});
    body.AppendStmt(mir::ExprStmt{.expr = assign});
  }

  const mir::ExprId result = body.exprs.Add(
      mir::Expr{.data = mir::LocalRef{.var = array}, .type = result_type});
  return steps.Build(result);
}

template <ExprLowerer Lowerer>
auto LowerHirAssignmentPatternReplicationExpr(
    Lowerer& lowerer, WalkFrame frame,
    const hir::AssignmentPatternReplicationExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
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
  if (const auto* tuple = result_ty.As<mir::TupleType>()) {
    std::vector<mir::ExprId> components;
    components.reserve(tuple->elements.size());
    for (std::size_t i = 0; i < tuple->elements.size(); ++i) {
      components.push_back(item_ids[i % item_ids.size()]);
    }
    return mir::Expr{
        .data = mir::TupleExpr{.components = std::move(components)},
        .type = result_type};
  }

  auto count_or = lowerer.LowerExpr(lowerer.HirExprs().Get(a.count), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const mir::ExprId count_value = block.exprs.Add(*std::move(count_or));
  const mir::ExprId count_id =
      block.exprs.Add(MakeToInt64Call(unit, count_value));
  if (IsArrayContainerType(result_ty)) {
    const mir::TypeId element_type =
        ArrayContainerElementType(unit, result_type);
    const mir::ExprId element_default = block.exprs.Add(
        BuildDefaultValueExpr(lowerer.Owner(), frame, element_type));
    return BuildArrayRepeatCall(
        lowerer.Owner(), frame, result_type, element_default,
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
auto LowerHirDynamicArrayNewExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::DynamicArrayNewExpr& n,
    hir::TypeId hir_result_type, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto& block = *frame.current_block;
  auto size_or = process.LowerExpr(process.HirExprs().Get(n.size), frame);
  if (!size_or) return std::unexpected(std::move(size_or.error()));
  const mir::ExprId size_id = block.exprs.Add(*std::move(size_or));

  const auto& hir_result_ty = process.Owner().Hir().types.Get(hir_result_type);
  const auto* hir_da = hir_result_ty.As<hir::DynamicArrayType>();
  if (hir_da == nullptr) {
    throw InternalError(
        "LowerHirDynamicArrayNewExprProc: result type is not DynamicArrayType");
  }
  const mir::ExprId prototype_id = block.exprs.Add(
      BuildDefaultValueFromHir(process.Owner(), frame, hir_da->element_type));

  std::vector<mir::ExprId> args;
  args.reserve(n.initializer.has_value() ? 3U : 2U);
  args.push_back(size_id);
  args.push_back(prototype_id);
  if (n.initializer.has_value()) {
    auto init_or =
        process.LowerExpr(process.HirExprs().Get(*n.initializer), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    args.push_back(block.exprs.Add(*std::move(init_or)));
  }
  const support::BuiltinFn form =
      n.initializer.has_value() ? support::BuiltinFn::kMakeDynamicArrayNewCopy
                                : support::BuiltinFn::kMakeDynamicArrayNew;
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = form,
                      .qualification = mir::TypeQualifier{.type = result_type}},
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
    const hir::AssociativeAssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
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
      lowerer.Owner(), frame, result_type, std::move(entries), user_default);
}

// One concrete instantiation per pass class. The handler templates are defined
// in this file rather than the header so the file-local helpers stay private,
// so the dispatchers in process_lowerer.cpp / structural_scope_lowerer.cpp link
// against the symbols emitted here.
template auto LowerHirConcatExpr(
    ProcessLowerer&, WalkFrame, const hir::ConcatExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirConcatExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::ConcatExpr&,
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
