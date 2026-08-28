#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/flat_packed_type.hpp"
#include "lyra/lowering/hir_to_mir/packed_concat.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 10.9.1 and 11.4.12.1 both make a replication count a constant
// expression, so slang has already evaluated it to an integer literal and the
// count is a number rather than an operand to evaluate at run time.
auto ExtractHirLiteralUint64(const hir::Expr& expr) -> std::uint64_t {
  const auto* primary = std::get_if<hir::PrimaryExpr>(&expr.data);
  if (primary == nullptr) {
    throw InternalError(
        "ExtractHirLiteralUint64: expected a primary expression");
  }
  const auto* lit = std::get_if<hir::IntegerLiteral>(&primary->data);
  if (lit == nullptr) {
    throw InternalError("ExtractHirLiteralUint64: expected an integer literal");
  }
  return lit->value.value_words[0];
}

auto IsArrayContainerType(const mir::Type& ty) -> bool {
  return std::holds_alternative<mir::UnpackedArrayType>(ty.data) ||
         std::holds_alternative<mir::DynamicArrayType>(ty.data) ||
         std::holds_alternative<mir::QueueType>(ty.data);
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
  if (result_ty.Kind() == mir::TypeKind::kString) {
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
  if (const auto* q = std::get_if<mir::QueueType>(&result_ty.data)) {
    const mir::TypeId element_type = q->element_type;
    const std::int64_t bound = q->max_bound.has_value()
                                   ? static_cast<std::int64_t>(*q->max_bound)
                                   : -1;
    std::vector<mir::ExprId> args;
    args.reserve(operand_ids.size() + 2);
    args.push_back(block.exprs.Add(
        BuildDefaultValueExpr(lowerer.Owner(), frame, element_type)));
    args.push_back(block.exprs.Add(
        mir::Expr{
            .data = mir::MachineIntLiteral{.value = bound},
            .type = unit.builtins.machine_int64}));
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
  return BuildPackedConcat(unit, block, std::move(operand_ids), result_type);
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
  // Repeating characters is the string entry's operation, the same split a
  // join makes. Its multiplier is an ordinary integral expression the run
  // evaluates, because LRM 11.4.12.2 allows a non-constant one where 11.4.12.1
  // requires a constant; reading that value as a machine count is a value
  // reshape, so it is stated rather than left for a consumer to insert.
  if (unit.types.Get(result_type).Kind() == mir::TypeKind::kString) {
    auto count_or = lowerer.LowerExpr(lowerer.HirExprs().Get(r.count), frame);
    if (!count_or) return std::unexpected(std::move(count_or.error()));
    const mir::ExprId count_id = block.exprs.Add(*std::move(count_or));
    const mir::ExprId machine_count_id = block.exprs.Add(
        mir::Expr{
            .data =
                mir::CallExpr{
                    .callee =
                        mir::Direct{.target = support::BuiltinFn::kToInt64},
                    .arguments = {count_id}},
            .type = unit.builtins.machine_int64});
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = mir::Direct{.target = support::BuiltinFn::kReplicate},
                .arguments = {concat_id, machine_count_id}},
        .type = result_type};
  }
  return mir::Expr{
      .data =
          mir::ReplicationExpr{
              .count = ExtractHirLiteralUint64(lowerer.HirExprs().Get(r.count)),
              .concat = concat_id},
      .type = result_type};
}

// Lowers an HIR AssignmentPatternExpr by dispatching on the destination type's
// runtime shape. Slang has already resolved any named / type-key / `default`
// keys into a member-ordered positional element list (LRM 10.9.2), so the
// shapes differ only in how they package those positional elements: a packed
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
  if (std::holds_alternative<mir::TupleType>(result_ty.data)) {
    return mir::Expr{
        .data = mir::TupleExpr{.components = std::move(element_ids)},
        .type = result_type};
  }
  const auto& result_pa = result_ty.AsIntegralPacked();
  const mir::ExprId concat_id = block.exprs.Add(BuildPackedConcat(
      unit, block, std::move(element_ids),
      InternFlatPacked(unit, result_pa.BitWidth(), result_pa.atom)));
  return BuildValueConversion(unit, block, concat_id, result_type);
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
  const std::uint64_t count =
      ExtractHirLiteralUint64(lowerer.HirExprs().Get(a.count));
  if (IsArrayContainerType(result_ty)) {
    const mir::TypeId element_type =
        ArrayContainerElementType(unit, result_type);
    const mir::ExprId element_default = block.exprs.Add(
        BuildDefaultValueExpr(lowerer.Owner(), frame, element_type));
    return BuildArrayRepeatCall(
        lowerer.Owner(), frame, result_type, element_default,
        std::move(item_ids), count);
  }
  if (std::holds_alternative<mir::TupleType>(result_ty.data)) {
    std::vector<mir::ExprId> components;
    components.reserve(item_ids.size() * count);
    for (std::uint64_t i = 0; i < count; ++i) {
      components.insert(components.end(), item_ids.begin(), item_ids.end());
    }
    return mir::Expr{
        .data = mir::TupleExpr{.components = std::move(components)},
        .type = result_type};
  }
  const auto& result_pa = result_ty.AsIntegralPacked();
  const std::uint64_t inner_width =
      count == 0 ? 0 : result_pa.BitWidth() / count;
  const mir::ExprId inner_concat_id = block.exprs.Add(BuildPackedConcat(
      unit, block, std::move(item_ids),
      InternFlatPacked(unit, inner_width, result_pa.atom)));
  const mir::ExprId repl_id = block.exprs.Add(
      mir::Expr{
          .data =
              mir::ReplicationExpr{.count = count, .concat = inner_concat_id},
          .type =
              InternFlatPacked(unit, result_pa.BitWidth(), result_pa.atom)});
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
  const auto* hir_da = std::get_if<hir::DynamicArrayType>(&hir_result_ty.data);
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
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Construct{}, .arguments = std::move(args)},
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
