#include "lyra/lowering/hir_to_mir/expression/selects.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

auto GetAggregateFields(const hir::Type& t)
    -> const std::vector<hir::PackedAggregateField>& {
  if (t.IsPackedStruct()) {
    return t.AsPackedStruct().fields;
  }
  if (t.IsPackedUnion()) {
    return t.AsPackedUnion().fields;
  }
  throw InternalError(
      "GetAggregateFields: base type is not a packed struct or union");
}

struct RangeOffsetCount {
  mir::ExprId offset_expr;
  std::uint32_t count;
};

auto MirIntegralConstantToInt64(const mir::IntegralConstant& c)
    -> std::int64_t {
  if (c.value_words.empty()) {
    throw InternalError("MirIntegralConstantToInt64: zero width");
  }
  return static_cast<std::int64_t>(c.value_words.front());
}

auto IntConstFromMirExpr(const mir::Expr& expr) -> std::int64_t {
  if (const auto* lit = std::get_if<mir::IntegerLiteral>(&expr.data)) {
    return MirIntegralConstantToInt64(lit->value);
  }
  throw InternalError(
      "IntConstFromMirExpr: expression is not an IntegerLiteral");
}

// LRM 7.4.5 + 11.5.1: the three source-faithful bound forms collapse to a
// single (offset, count) shape at HIR -> MIR. ConstantBounds reduce by
// reading both folded literals; IndexedUp uses base/width directly;
// IndexedDown synthesizes `base - (count - 1)` as the offset since the
// `-:` lsb sits below the named base.
template <typename LowerOne>
auto UnfoldHirRangeBoundsToOffsetCount(
    const ModuleLowerer& module, mir::ProceduralScope& proc_scope,
    const hir::RangeBounds& bounds, LowerOne lower_one)
    -> diag::Result<RangeOffsetCount> {
  const mir::TypeId int32_type = module.Unit().builtins.int32;
  return std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto msb = lower_one(b.msb_expr);
            if (!msb) return std::unexpected(std::move(msb.error()));
            auto lsb = lower_one(b.lsb_expr);
            if (!lsb) return std::unexpected(std::move(lsb.error()));
            const auto msb_val = IntConstFromMirExpr(proc_scope.GetExpr(*msb));
            const auto lsb_val = IntConstFromMirExpr(proc_scope.GetExpr(*lsb));
            const auto count = static_cast<std::uint32_t>(
                (msb_val >= lsb_val ? msb_val - lsb_val : lsb_val - msb_val) +
                1);
            const auto offset_val = std::min(msb_val, lsb_val);
            const auto offset_id = proc_scope.AddExpr(
                mir::MakeInt32Literal(int32_type, offset_val));
            return RangeOffsetCount{.offset_expr = offset_id, .count = count};
          },
          [&](const hir::RangeIndexedUpBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            auto width = lower_one(b.width);
            if (!width) return std::unexpected(std::move(width.error()));
            const auto count = static_cast<std::uint32_t>(
                IntConstFromMirExpr(proc_scope.GetExpr(*width)));
            return RangeOffsetCount{.offset_expr = *base, .count = count};
          },
          [&](const hir::RangeIndexedDownBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            auto width = lower_one(b.width);
            if (!width) return std::unexpected(std::move(width.error()));
            const auto count = static_cast<std::uint32_t>(
                IntConstFromMirExpr(proc_scope.GetExpr(*width)));
            const auto base_type = proc_scope.GetExpr(*base).type;
            const auto sub_lit = proc_scope.AddExpr(
                mir::MakeInt32Literal(
                    int32_type, static_cast<std::int64_t>(count) - 1));
            const auto offset = proc_scope.AddExpr(
                mir::Expr{
                    .data =
                        mir::BinaryExpr{
                            .op = mir::BinaryOp::kSub,
                            .lhs = *base,
                            .rhs = sub_lit},
                    .type = base_type});
            return RangeOffsetCount{.offset_expr = offset, .count = count};
          },
      },
      bounds);
}

// Translates an SV-declared-range index into a zero-based C++ vector offset
// for an unpacked array base. `[0:N]` (ascending from zero) needs no rewrite;
// other ranges fold the base offset into the index expression so the
// downstream backend can emit a uniform `vec[i]` access.
auto WrapUnpackedIndex(
    const ModuleLowerer& module, mir::ProceduralScope& proc_scope,
    const hir::UnpackedRange& declared, mir::ExprId raw_idx,
    mir::TypeId idx_type) -> mir::ExprId {
  if (declared.left == 0 && declared.right >= 0) {
    return raw_idx;
  }
  const mir::ExprId literal_id = proc_scope.AddExpr(
      mir::MakeInt32Literal(module.Unit().builtins.int32, declared.left));
  const bool descending = declared.left >= declared.right;
  return proc_scope.AddExpr(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kSub,
                  .lhs = descending ? literal_id : raw_idx,
                  .rhs = descending ? raw_idx : literal_id},
          .type = idx_type});
}

// LRM 7.4.5: an unpacked-array slice's first in-memory element is the
// syntactic-leftmost SV position of the slice (slang enforces direction
// match), translated through the declared range to a zero-based vector
// offset. The slice's element count comes from the result type rather than
// the bounds expressions, so the helper does not need to fold either bound
// to a literal -- negative-base sources can therefore survive.
template <typename LowerOne>
auto UnfoldHirRangeBoundsForUnpacked(
    const ModuleLowerer& module, mir::ProceduralScope& proc_scope,
    const hir::RangeBounds& bounds, const hir::UnpackedRange& declared,
    std::uint32_t count, LowerOne lower_one) -> diag::Result<RangeOffsetCount> {
  const mir::TypeId int32_type = module.Unit().builtins.int32;
  const bool descending = declared.left >= declared.right;
  auto with_delta = [&](mir::ExprId base, std::int64_t delta,
                        mir::BinaryOp op) -> mir::ExprId {
    const auto base_type = proc_scope.GetExpr(base).type;
    const auto delta_lit =
        proc_scope.AddExpr(mir::MakeInt32Literal(int32_type, delta));
    return proc_scope.AddExpr(
        mir::Expr{
            .data = mir::BinaryExpr{.op = op, .lhs = base, .rhs = delta_lit},
            .type = base_type});
  };
  return std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto msb = lower_one(b.msb_expr);
            if (!msb) return std::unexpected(std::move(msb.error()));
            const auto msb_type = proc_scope.GetExpr(*msb).type;
            const auto vec_offset =
                WrapUnpackedIndex(module, proc_scope, declared, *msb, msb_type);
            return RangeOffsetCount{.offset_expr = vec_offset, .count = count};
          },
          [&](const hir::RangeIndexedUpBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            const auto base_type = proc_scope.GetExpr(*base).type;
            const auto leftmost_sv =
                descending ? with_delta(
                                 *base, static_cast<std::int64_t>(count) - 1,
                                 mir::BinaryOp::kAdd)
                           : *base;
            const auto vec_offset = WrapUnpackedIndex(
                module, proc_scope, declared, leftmost_sv, base_type);
            return RangeOffsetCount{.offset_expr = vec_offset, .count = count};
          },
          [&](const hir::RangeIndexedDownBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            const auto base_type = proc_scope.GetExpr(*base).type;
            const auto leftmost_sv =
                descending ? *base
                           : with_delta(
                                 *base, static_cast<std::int64_t>(count) - 1,
                                 mir::BinaryOp::kSub);
            const auto vec_offset = WrapUnpackedIndex(
                module, proc_scope, declared, leftmost_sv, base_type);
            return RangeOffsetCount{.offset_expr = vec_offset, .count = count};
          },
      },
      bounds);
}

}  // namespace

auto LowerHirElementSelectExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& module = process.Module();
  const auto& hir_process = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  const auto& hir_base = hir_process.exprs.at(sel.base_value.value);
  auto base_or = process.LowerExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope.AddExpr(*std::move(base_or));

  const auto& hir_idx = hir_process.exprs.at(sel.index.value);
  auto idx_or = process.LowerExpr(hir_idx, frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  mir::ExprId idx_id = proc_scope.AddExpr(*std::move(idx_or));

  const auto& hir_base_ty = module.Hir().GetType(hir_base.type);
  if (const auto* ua = std::get_if<hir::UnpackedArrayType>(&hir_base_ty.data)) {
    idx_id = WrapUnpackedIndex(
        module, proc_scope, ua->dim, idx_id,
        module.TranslateType(hir_idx.type));
  }

  return mir::Expr{
      .data =
          mir::ElementSelectExpr{
              .base_value = base_id,
              .index = idx_id,
          },
      .type = result_type};
}

auto LowerHirRangeSelectExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& module = process.Module();
  const auto& hir_process = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  const auto& hir_base = hir_process.exprs.at(sel.base_value.value);
  auto base_or = process.LowerExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope.AddExpr(*std::move(base_or));

  auto lower_one = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = process.LowerExpr(hir_process.exprs.at(id.value), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return proc_scope.AddExpr(*std::move(lowered));
  };
  const auto& hir_base_ty = module.Hir().GetType(hir_base.type);
  auto unfolded = [&]() {
    if (const auto* ua =
            std::get_if<hir::UnpackedArrayType>(&hir_base_ty.data)) {
      const auto& result_ty = module.Unit().GetType(result_type);
      const auto count = static_cast<std::uint32_t>(
          std::get<mir::UnpackedArrayType>(result_ty.data).size);
      return UnfoldHirRangeBoundsForUnpacked(
          module, proc_scope, sel.bounds, ua->dim, count, lower_one);
    }
    return UnfoldHirRangeBoundsToOffsetCount(
        module, proc_scope, sel.bounds, lower_one);
  }();
  if (!unfolded) return std::unexpected(std::move(unfolded.error()));

  return mir::Expr{
      .data =
          mir::RangeSelectExpr{
              .base_value = base_id,
              .offset_expr = unfolded->offset_expr,
              .count = unfolded->count,
          },
      .type = result_type};
}

// LRM 7.2.1: packed struct field access "can be selected as if it were a
// packed array". HIR -> MIR resolves the field-table index to a concrete
// (offset, count) RangeSelectExpr -- the same MIR shape `s[hi:lo]` produces.
// MIR carries no struct-specific node.
auto LowerHirMemberAccessExprProc(
    ProcessLowerer& process, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& module = process.Module();
  const auto& hir_process = process.HirBody();
  auto& proc_scope = *frame.current_procedural_scope;
  const auto& base_hir_expr = hir_process.exprs.at(sel.base_value.value);
  const auto& base_hir_type = module.Hir().GetType(base_hir_expr.type);
  const auto& fields = GetAggregateFields(base_hir_type);
  if (sel.field_index >= fields.size()) {
    throw InternalError(
        "LowerHirMemberAccessExprProc: field_index out of range");
  }
  const auto& field = fields[sel.field_index];
  auto base_or = process.LowerExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope.AddExpr(*std::move(base_or));
  const auto offset_id = proc_scope.AddExpr(
      mir::MakeInt32Literal(
          module.Unit().builtins.int32,
          static_cast<std::int64_t>(field.bit_offset)));
  return mir::Expr{
      .data =
          mir::RangeSelectExpr{
              .base_value = base_id,
              .offset_expr = offset_id,
              .count = static_cast<std::uint32_t>(field.bit_width),
          },
      .type = result_type};
}

auto LowerHirElementSelectExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::ElementSelectExpr& sel, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& module = scope.Module();
  const auto& hir_scope = scope.HirScope();
  auto& proc_scope = *frame.current_procedural_scope;
  const auto& hir_base = hir_scope.GetExpr(sel.base_value);
  auto base_or = scope.LowerExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope.AddExpr(*std::move(base_or));

  const auto& hir_idx = hir_scope.GetExpr(sel.index);
  auto idx_or = scope.LowerExpr(hir_idx, frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  mir::ExprId idx_id = proc_scope.AddExpr(*std::move(idx_or));

  const auto& hir_base_ty = module.Hir().GetType(hir_base.type);
  if (const auto* ua = std::get_if<hir::UnpackedArrayType>(&hir_base_ty.data)) {
    idx_id = WrapUnpackedIndex(
        module, proc_scope, ua->dim, idx_id,
        module.TranslateType(hir_idx.type));
  }

  return mir::Expr{
      .data = mir::ElementSelectExpr{.base_value = base_id, .index = idx_id},
      .type = result_type};
}

auto LowerHirRangeSelectExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::RangeSelectExpr& sel, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& module = scope.Module();
  const auto& hir_scope = scope.HirScope();
  auto& proc_scope = *frame.current_procedural_scope;
  const auto& hir_base = hir_scope.GetExpr(sel.base_value);
  auto base_or = scope.LowerExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope.AddExpr(*std::move(base_or));

  auto lower_one = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = scope.LowerExpr(hir_scope.GetExpr(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return proc_scope.AddExpr(*std::move(lowered));
  };
  const auto& hir_base_ty = module.Hir().GetType(hir_base.type);
  auto unfolded = [&]() {
    if (const auto* ua =
            std::get_if<hir::UnpackedArrayType>(&hir_base_ty.data)) {
      const auto& result_ty = module.Unit().GetType(result_type);
      const auto count = static_cast<std::uint32_t>(
          std::get<mir::UnpackedArrayType>(result_ty.data).size);
      return UnfoldHirRangeBoundsForUnpacked(
          module, proc_scope, sel.bounds, ua->dim, count, lower_one);
    }
    return UnfoldHirRangeBoundsToOffsetCount(
        module, proc_scope, sel.bounds, lower_one);
  }();
  if (!unfolded) return std::unexpected(std::move(unfolded.error()));

  return mir::Expr{
      .data =
          mir::RangeSelectExpr{
              .base_value = base_id,
              .offset_expr = unfolded->offset_expr,
              .count = unfolded->count,
          },
      .type = result_type};
}

auto LowerHirMemberAccessExprStructural(
    const StructuralScopeLowerer& scope, WalkFrame frame,
    const hir::MemberAccessExpr& sel, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& module = scope.Module();
  const auto& hir_scope = scope.HirScope();
  auto& proc_scope = *frame.current_procedural_scope;
  const auto& base_hir_expr = hir_scope.GetExpr(sel.base_value);
  const auto& base_hir_type = module.Hir().GetType(base_hir_expr.type);
  const auto& fields = GetAggregateFields(base_hir_type);
  if (sel.field_index >= fields.size()) {
    throw InternalError(
        "LowerHirMemberAccessExprStructural: field_index out of range");
  }
  const auto& field = fields[sel.field_index];
  auto base_or = scope.LowerExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope.AddExpr(*std::move(base_or));
  const auto offset_id = proc_scope.AddExpr(
      mir::MakeInt32Literal(
          module.Unit().builtins.int32,
          static_cast<std::int64_t>(field.bit_offset)));
  return mir::Expr{
      .data =
          mir::RangeSelectExpr{
              .base_value = base_id,
              .offset_expr = offset_id,
              .count = static_cast<std::uint32_t>(field.bit_width),
          },
      .type = result_type};
}

}  // namespace lyra::lowering::hir_to_mir
