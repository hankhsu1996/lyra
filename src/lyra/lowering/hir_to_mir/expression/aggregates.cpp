#include "lyra/lowering/hir_to_mir/expression/aggregates.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/class_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/default_value.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// LRM 10.9.1: the replication count of an assignment pattern is a constant
// expression; slang has already evaluated it to an integer literal. The
// unpacked-target path uses this value to materialize the element list at
// compile time (no runtime replication primitive exists for unpacked
// aggregates).
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
  const auto& c = lit->value;
  if (c.value_words.empty()) return 0;
  return c.value_words[0];
}

auto BuildArrayReplicationFlatList(
    const ModuleLowerer& module, WalkFrame frame,
    std::span<const mir::ExprId> items_ids, std::uint64_t count,
    mir::TypeId result_type) -> mir::Expr {
  std::vector<mir::ExprId> flat;
  flat.reserve(items_ids.size() * count);
  for (std::uint64_t i = 0; i < count; ++i) {
    flat.insert(flat.end(), items_ids.begin(), items_ids.end());
  }
  return BuildArrayConstructionCall(
      module, frame, result_type, std::move(flat));
}

auto IsArrayContainerType(const mir::Type& ty) -> bool {
  return std::holds_alternative<mir::UnpackedArrayType>(ty.data) ||
         std::holds_alternative<mir::DynamicArrayType>(ty.data) ||
         std::holds_alternative<mir::QueueType>(ty.data);
}

}  // namespace

auto LowerHirConcatExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::ConcatExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> operand_ids;
  operand_ids.reserve(c.operands.size());
  for (const auto& id : c.operands) {
    auto lowered = process.LowerExpr(hir_process.exprs.at(id.value), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    operand_ids.push_back(block.AddExpr(*std::move(lowered)));
  }
  return mir::Expr{
      .data = mir::ConcatExpr{.operands = std::move(operand_ids)},
      .type = result_type};
}

auto LowerHirReplicationExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::ReplicationExpr& r,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;
  auto count_or = process.LowerExpr(hir_process.exprs.at(r.count.value), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const mir::ExprId count_id = block.AddExpr(*std::move(count_or));
  auto concat_or =
      process.LowerExpr(hir_process.exprs.at(r.concat.value), frame);
  if (!concat_or) return std::unexpected(std::move(concat_or.error()));
  const mir::ExprId concat_id = block.AddExpr(*std::move(concat_or));
  return mir::Expr{
      .data = mir::ReplicationExpr{.count = count_id, .concat = concat_id},
      .type = result_type};
}

// Lowers an HIR AssignmentPatternExpr by dispatching on the destination
// type's runtime shape: packed targets fold into MIR `ConcatExpr` (bit
// concatenation matches the packed bit plane), array containers (unpacked,
// dynamic, queue) land as `ArrayLiteralExpr` over distinct element slots
// wrapped by a construction call against the container ctor.
auto LowerHirAssignmentPatternExprProc(
    ProcessLowerer& process, WalkFrame frame,
    const hir::AssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> element_ids;
  element_ids.reserve(a.elements.size());
  for (const auto& id : a.elements) {
    auto lowered = process.LowerExpr(hir_process.exprs.at(id.value), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(block.AddExpr(*std::move(lowered)));
  }
  if (IsArrayContainerType(process.Module().Unit().GetType(result_type))) {
    return BuildArrayConstructionCall(
        process.Module(), frame, result_type, std::move(element_ids));
  }
  return mir::Expr{
      .data = mir::ConcatExpr{.operands = std::move(element_ids)},
      .type = result_type};
}

auto LowerHirAssignmentPatternReplicationExprProc(
    ProcessLowerer& process, WalkFrame frame,
    const hir::AssignmentPatternReplicationExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> item_ids;
  item_ids.reserve(a.items.size());
  for (const auto& id : a.items) {
    auto lowered = process.LowerExpr(hir_process.exprs.at(id.value), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(block.AddExpr(*std::move(lowered)));
  }
  if (IsArrayContainerType(process.Module().Unit().GetType(result_type))) {
    const std::uint64_t count =
        ExtractHirLiteralUint64(hir_process.exprs.at(a.count.value));
    return BuildArrayReplicationFlatList(
        process.Module(), frame, item_ids, count, result_type);
  }
  const mir::ExprId inner_concat_id = block.AddExpr(
      mir::Expr{
          .data = mir::ConcatExpr{.operands = std::move(item_ids)},
          .type = result_type});
  auto count_or = process.LowerExpr(hir_process.exprs.at(a.count.value), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const mir::ExprId count_id = block.AddExpr(*std::move(count_or));
  return mir::Expr{
      .data =
          mir::ReplicationExpr{
              .count = count_id,
              .concat = inner_concat_id,
          },
      .type = result_type};
}

// LRM 7.5.1 `new[N]` / `new[N](other)`. The argument list on the lowered
// construction call is `[size, element-default prototype, optional copy
// source]`: the prototype carries the element type's LRM Table 6-7 default
// so the runtime ctor populates new slots without re-querying the type, and
// the optional copy source feeds the LRM 7.5.1 truncate / pad behaviour on
// `new[N](other)`.
auto LowerHirDynamicArrayNewExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::DynamicArrayNewExpr& n,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;
  auto size_or = process.LowerExpr(hir_process.exprs.at(n.size.value), frame);
  if (!size_or) return std::unexpected(std::move(size_or.error()));
  const mir::ExprId size_id = block.AddExpr(*std::move(size_or));

  const auto& result_ty = process.Module().Unit().GetType(result_type);
  const auto* da = std::get_if<mir::DynamicArrayType>(&result_ty.data);
  if (da == nullptr) {
    throw InternalError(
        "LowerHirDynamicArrayNewExprProc: result type is not DynamicArrayType");
  }
  const mir::ExprId prototype_id = block.AddExpr(
      BuildDefaultValueExpr(process.Module(), frame, da->element_type));

  std::vector<mir::ExprId> args;
  args.reserve(n.initializer.has_value() ? 3U : 2U);
  args.push_back(size_id);
  args.push_back(prototype_id);
  if (n.initializer.has_value()) {
    auto init_or =
        process.LowerExpr(hir_process.exprs.at(n.initializer->value), frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    args.push_back(block.AddExpr(*std::move(init_or)));
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::ConstructorCallee{}, .arguments = std::move(args)},
      .type = result_type};
}

// LRM 7.9.11 associative literal. Each (key, value) entry is lowered into a
// pair of MIR ExprIds and handed to the shared construction helper, which wraps
// them as tuples and threads the optional persistent default through the
// associative constructor.
auto LowerHirAssociativeAssignmentPatternExprProc(
    ProcessLowerer& process, WalkFrame frame,
    const hir::AssociativeAssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_process = process.HirBody();
  auto& block = *frame.current_block;
  std::vector<std::pair<mir::ExprId, mir::ExprId>> entries;
  entries.reserve(a.entries.size());
  for (const auto& entry : a.entries) {
    auto key_or =
        process.LowerExpr(hir_process.exprs.at(entry.key.value), frame);
    if (!key_or) return std::unexpected(std::move(key_or.error()));
    const mir::ExprId key_id = block.AddExpr(*std::move(key_or));
    auto value_or =
        process.LowerExpr(hir_process.exprs.at(entry.value.value), frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::ExprId value_id = block.AddExpr(*std::move(value_or));
    entries.emplace_back(key_id, value_id);
  }
  std::optional<mir::ExprId> user_default;
  if (a.default_value.has_value()) {
    auto default_or =
        process.LowerExpr(hir_process.exprs.at(a.default_value->value), frame);
    if (!default_or) return std::unexpected(std::move(default_or.error()));
    user_default = block.AddExpr(*std::move(default_or));
  }
  return BuildAssociativeConstructionCall(
      process.Module(), frame, result_type, std::move(entries), user_default);
}

auto LowerHirConcatExprStructural(
    const ClassLowerer& lowerer, WalkFrame frame, const hir::ConcatExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_scope = lowerer.HirScope();
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> operand_ids;
  operand_ids.reserve(c.operands.size());
  for (const auto& id : c.operands) {
    auto lowered = lowerer.LowerExpr(hir_scope.GetExpr(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    operand_ids.push_back(block.AddExpr(*std::move(lowered)));
  }
  return mir::Expr{
      .data = mir::ConcatExpr{.operands = std::move(operand_ids)},
      .type = result_type};
}

auto LowerHirAssignmentPatternExprStructural(
    const ClassLowerer& lowerer, WalkFrame frame,
    const hir::AssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_scope = lowerer.HirScope();
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> element_ids;
  element_ids.reserve(a.elements.size());
  for (const auto& id : a.elements) {
    auto lowered = lowerer.LowerExpr(hir_scope.GetExpr(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(block.AddExpr(*std::move(lowered)));
  }
  if (IsArrayContainerType(lowerer.Module().Unit().GetType(result_type))) {
    return BuildArrayConstructionCall(
        lowerer.Module(), frame, result_type, std::move(element_ids));
  }
  return mir::Expr{
      .data = mir::ConcatExpr{.operands = std::move(element_ids)},
      .type = result_type};
}

auto LowerHirAssignmentPatternReplicationExprStructural(
    const ClassLowerer& lowerer, WalkFrame frame,
    const hir::AssignmentPatternReplicationExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_scope = lowerer.HirScope();
  auto& block = *frame.current_block;
  std::vector<mir::ExprId> item_ids;
  item_ids.reserve(a.items.size());
  for (const auto& id : a.items) {
    auto lowered = lowerer.LowerExpr(hir_scope.GetExpr(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(block.AddExpr(*std::move(lowered)));
  }
  if (IsArrayContainerType(lowerer.Module().Unit().GetType(result_type))) {
    const std::uint64_t count =
        ExtractHirLiteralUint64(hir_scope.GetExpr(a.count));
    return BuildArrayReplicationFlatList(
        lowerer.Module(), frame, item_ids, count, result_type);
  }
  const mir::ExprId inner_concat_id = block.AddExpr(
      mir::Expr{
          .data = mir::ConcatExpr{.operands = std::move(item_ids)},
          .type = result_type});
  auto count_or = lowerer.LowerExpr(hir_scope.GetExpr(a.count), frame);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const mir::ExprId count_id = block.AddExpr(*std::move(count_or));
  return mir::Expr{
      .data =
          mir::ReplicationExpr{
              .count = count_id,
              .concat = inner_concat_id,
          },
      .type = result_type};
}

auto LowerHirAssociativeAssignmentPatternExprStructural(
    const ClassLowerer& lowerer, WalkFrame frame,
    const hir::AssociativeAssignmentPatternExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_scope = lowerer.HirScope();
  auto& block = *frame.current_block;
  std::vector<std::pair<mir::ExprId, mir::ExprId>> entries;
  entries.reserve(a.entries.size());
  for (const auto& entry : a.entries) {
    auto key_or = lowerer.LowerExpr(hir_scope.GetExpr(entry.key), frame);
    if (!key_or) return std::unexpected(std::move(key_or.error()));
    const mir::ExprId key_id = block.AddExpr(*std::move(key_or));
    auto value_or = lowerer.LowerExpr(hir_scope.GetExpr(entry.value), frame);
    if (!value_or) return std::unexpected(std::move(value_or.error()));
    const mir::ExprId value_id = block.AddExpr(*std::move(value_or));
    entries.emplace_back(key_id, value_id);
  }
  std::optional<mir::ExprId> user_default;
  if (a.default_value.has_value()) {
    auto default_or =
        lowerer.LowerExpr(hir_scope.GetExpr(*a.default_value), frame);
    if (!default_or) return std::unexpected(std::move(default_or.error()));
    user_default = block.AddExpr(*std::move(default_or));
  }
  return BuildAssociativeConstructionCall(
      lowerer.Module(), frame, result_type, std::move(entries), user_default);
}

}  // namespace lyra::lowering::hir_to_mir
