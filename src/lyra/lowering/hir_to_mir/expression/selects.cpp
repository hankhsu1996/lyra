#include "lyra/lowering/hir_to_mir/expression/selects.hpp"

#include <cstdint>
#include <expected>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/cast_lowering.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/process_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/structural_scope_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"

// HIR-to-MIR lowering for the three select families (`a[i]`, `a[hi:lo]`,
// `s.field`). Each family has a read-side and a write-side entry point; a
// select's meaning is independent of whether a process or a structural scope
// encloses it, so each entry point is one template over the pass class. The
// bodies fan into a single per-family `Build*` factory plus a per-family inner
// helper that handles the wrapping decisions.
//
// Naming convention used here, matching the rest of HIR-to-MIR:
//   - `Lower*` -- top-level HIR-to-MIR for a HIR construct, returns
//     `diag::Result<mir::Expr>`. The caller commits the returned node.
//   - `Build*` -- factory for a specific MIR node shape, returns `mir::Expr`
//     (or `diag::Result<mir::Expr>`). Does not commit unless documented.
//   - `Wrap*`  -- transforms an existing node into another node; may commit
//     intermediate steps as a side effect.
//   - `Unfold*` -- projects HIR structure to MIR-shaped data without
//     emitting a single node directly.

namespace lyra::lowering::hir_to_mir {

namespace {

// Read vs LHS side dispatch for `arr[i]`. The split is uniform across all
// container kinds (LRM 7.8.6 / 7.8.7 AA, LRM 7.10.1 queue, LRM 7.4 packed /
// unpacked) and mirrors the runtime API's bare-vs-`Ref`-suffix protocol;
// see `include/lyra/value/concepts.hpp`.
enum class AccessSide : std::uint8_t { kRead, kLhs };

auto ElementAccessCallee(AccessSide side) -> mir::Direct {
  return mir::Direct{
      .target = side == AccessSide::kLhs ? support::BuiltinFn::kElementRef
                                         : support::BuiltinFn::kElement};
}

// LRM 7.2.1 / 7.2.2: packed struct or union field table accessor. Used by
// the member-access lowerings.
auto GetAggregateFields(const hir::Type& t)
    -> const std::vector<hir::PackedAggregateField>& {
  if (t.IsPackedStruct()) return t.AsPackedStruct().fields;
  if (t.IsPackedUnion()) return t.AsPackedUnion().fields;
  throw InternalError(
      "GetAggregateFields: base type is not a packed struct or union");
}

// Read-side wrap that materialises a borrowed `PackedArrayRef` result into
// an owning `PackedArray` (Rust's `&[T]::to_owned() -> Vec<T>` pattern).
// LHS chains keep the view because they write through it; non-packed
// receivers fall through unchanged because their access already returns an
// owning value.
auto WrapPackedAsOwned(
    const mir::CompilationUnit& unit, mir::Block& block, mir::Expr access_call,
    mir::TypeId result_type) -> mir::Expr {
  if (!std::holds_alternative<mir::PackedArrayType>(
          unit.types.Get(result_type).data)) {
    return access_call;
  }
  const mir::ExprId access_id = block.exprs.Add(std::move(access_call));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = mir::Direct{.target = support::BuiltinFn::kToOwned},
              .arguments = {access_id}},
      .type = result_type};
}

// LRM 7.4.1: part-select returns unsigned. The runtime `Slice` honours this
// (always returns an unsigned `PackedArrayRef`). When the consumer's MIR type
// is signed -- a `logic signed [...]` packed-struct / union field per LRM
// 7.2.1 -- return the unsigned-signedness counterpart so the slice's MIR
// `.type` matches what the runtime produces; an explicit `ConversionExpr`
// then re-tags to the consumer's signed type.
auto UnsignedPackedCounterpart(
    mir::CompilationUnit& unit, mir::TypeId result_type) -> mir::TypeId {
  const auto& ty = unit.types.Get(result_type);
  if (!ty.IsPackedArray()) return result_type;
  const auto& pa = ty.AsPackedArray();
  if (pa.signedness == mir::Signedness::kUnsigned) return result_type;
  auto unsigned_pa = pa;
  unsigned_pa.signedness = mir::Signedness::kUnsigned;
  return unit.types.Intern(std::move(unsigned_pa));
}

// Wraps a slice result that was rendered at `slice_type` (unsigned) with an
// explicit `ConversionExpr` to `final_type` when the two differ. No-op when
// the slice already matches the consumer's MIR type.
auto WrapSliceSignReTag(
    const mir::CompilationUnit& unit, mir::Block& block, mir::Expr owned,
    mir::TypeId slice_type, mir::TypeId final_type) -> mir::Expr {
  if (slice_type == final_type) return owned;
  const mir::ExprId owned_id = block.exprs.Add(std::move(owned));
  return BuildValueConversion(unit, block, owned_id, final_type);
}

struct RangeLoHi {
  mir::ExprId lo;
  mir::ExprId hi;
};

enum class RangeContainer : std::uint8_t { kPacked, kQueue, kUnpacked };

// Container-specific projection inputs for `UnfoldRangeBoundsToLoHi`. The
// three container kinds project SV-source `[a:b]` / `[base+:w]` / `[base-:w]`
// differently; this struct carries the per-container facts needed.
struct RangeStrategy {
  RangeContainer container = RangeContainer::kPacked;
  // Only meaningful for `kUnpacked`. The declared SV range determines whether
  // the slice's leftmost-in-memory position is the syntactic base or the
  // upper end derived from it, and supplies the SV-to-vector index
  // projection. `count` is the result type's element count (LRM 7.4.5 fixed
  // width).
  const hir::UnpackedRange* unpacked_declared = nullptr;
  std::uint32_t unpacked_count = 0;
  // Only meaningful for `kPacked`. The declared SV range maps each part-select
  // endpoint to its LSB-relative offset (LRM 11.5.2); a descending zero-based
  // range is the identity.
  const hir::PackedRange* packed_declared = nullptr;
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

// Builds the `base <op> value` expression for a slice-bound offset. The delta
// literal matches `base`'s state-kind so the runtime arithmetic never mixes a
// 2-state literal with a 4-state index; an x / z index then propagates into
// the offset and invalidates the whole slice (LRM 7.4.5). The caller commits
// the returned expression.
auto BuildOffsetDeltaExpr(
    const ModuleLowerer& module, mir::Block& block, mir::ExprId base,
    std::int64_t value, mir::BinaryOp op) -> mir::Expr {
  const auto base_type = block.exprs.Get(base).type;
  const auto& base_ty = module.Unit().types.Get(base_type);
  const bool four_state =
      base_ty.IsIntegralPacked() && base_ty.AsIntegralPacked().IsFourState();
  const auto delta_lit = block.exprs.Add(
      four_state
          ? mir::MakeIntegerLiteral(module.Unit().builtins.integer, value)
          : mir::MakeInt32Literal(module.Unit().builtins.int32, value));
  return mir::Expr{
      .data = mir::BinaryExpr{.op = op, .lhs = base, .rhs = delta_lit},
      .type = base_type};
}

// Translates an SV-declared-range index into a zero-based C++ vector offset
// for an unpacked array base. `[0:N]` (ascending from zero) needs no rewrite;
// other ranges fold the base offset into the index expression so the
// downstream backend can emit a uniform `vec[i]` access. Commits and returns
// the resulting `ExprId`.
auto WrapUnpackedIndex(
    const ModuleLowerer& module, mir::Block& block,
    const hir::UnpackedRange& declared, mir::ExprId raw_idx,
    mir::TypeId idx_type) -> mir::ExprId {
  if (declared.left == 0 && declared.right >= 0) {
    return raw_idx;
  }
  const mir::ExprId literal_id = block.exprs.Add(
      mir::MakeInt32Literal(module.Unit().builtins.int32, declared.left));
  const bool descending = declared.left >= declared.right;
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kSub,
                  .lhs = descending ? literal_id : raw_idx,
                  .rhs = descending ? raw_idx : literal_id},
          .type = idx_type});
}

// Translates an SV-declared packed bit-select index into the LSB-relative bit
// offset the runtime element protocol consumes. The packed sibling of
// `WrapUnpackedIndex`, but folding the `right` bound rather than `left`:
// packed bit numbering counts from the LSB (the declared `right` end) while
// unpacked memory order counts from the declared left. A descending
// zero-based range (`[N:0]`) needs no rewrite. Commits and returns the
// resulting `ExprId`.
auto WrapPackedIndex(
    const ModuleLowerer& module, mir::Block& block,
    const hir::PackedRange& declared, mir::ExprId raw_idx, mir::TypeId idx_type)
    -> mir::ExprId {
  const bool descending = declared.left >= declared.right;
  if (descending && declared.right == 0) {
    return raw_idx;
  }
  const mir::ExprId literal_id = block.exprs.Add(
      mir::MakeInt32Literal(module.Unit().builtins.int32, declared.right));
  return block.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kSub,
                  .lhs = descending ? raw_idx : literal_id,
                  .rhs = descending ? literal_id : raw_idx},
          .type = idx_type});
}

// Emits `lo + (count - 1)`, committed. `count == 1` returns `lo` directly
// (no zero delta). Used by the fixed-width range-bound projections where
// the slice width is statically known.
auto BuildHiFromLoAndCount(
    const ModuleLowerer& module, mir::Block& block, mir::ExprId lo,
    std::int64_t count) -> mir::ExprId {
  if (count == 1) return lo;
  return block.exprs.Add(
      BuildOffsetDeltaExpr(module, block, lo, count - 1, mir::BinaryOp::kAdd));
}

enum class IndexedDir : std::uint8_t { kUp, kDown };

// LRM 11.5.1 packed indexed part-select (`base +: w` / `base -: w`). The base
// index is mapped to its LSB-relative offset; the slice then extends toward
// the MSB or LSB. The width grows upward exactly when the indexed direction
// agrees with the declared range direction (`+:` on a descending range, or
// `-:` on an ascending range), so a declared-ascending range mirrors the
// growth direction a declared-descending range would take.
auto PackedIndexedLoHi(
    const ModuleLowerer& module, mir::Block& block,
    const hir::PackedRange& declared, mir::ExprId base, std::int64_t count,
    IndexedDir dir) -> RangeLoHi {
  const bool descending = declared.left >= declared.right;
  const mir::ExprId pbase = WrapPackedIndex(
      module, block, declared, base, block.exprs.Get(base).type);
  const bool extend_up = (dir == IndexedDir::kUp) == descending;
  if (extend_up) {
    return RangeLoHi{
        .lo = pbase, .hi = BuildHiFromLoAndCount(module, block, pbase, count)};
  }
  return RangeLoHi{
      .lo = block.exprs.Add(BuildOffsetDeltaExpr(
          module, block, pbase, count - 1, mir::BinaryOp::kSub)),
      .hi = pbase};
}

// LRM 7.10.1 queue slice projection. `q[a:b]` requires `a <= b`, so slang
// always puts source-lo on the MSB side and source-hi on the LSB side --
// no folding or `min/max` needed. Bounds may be arbitrary runtime
// expressions; indexed forms build `base + (width - 1)` symbolically.
template <typename LowerOne>
auto BuildQueueRangeLoHi(
    const ModuleLowerer& module, mir::Block& block,
    const hir::RangeBounds& bounds, LowerOne lower_one)
    -> diag::Result<RangeLoHi> {
  const mir::TypeId int32_type = module.Unit().builtins.int32;
  // Build `base <op> (width - 1)` symbolically -- `width` is a runtime
  // expression on the queue path, so the delta cannot be folded to a
  // literal.
  auto build_base_at_width_offset = [&](mir::ExprId base, mir::ExprId width,
                                        mir::BinaryOp op) -> mir::ExprId {
    const auto base_type = block.exprs.Get(base).type;
    const auto width_type = block.exprs.Get(width).type;
    const auto one = block.exprs.Add(mir::MakeInt32Literal(int32_type, 1));
    const auto w_minus_1 = block.exprs.Add(
        mir::Expr{
            .data =
                mir::BinaryExpr{
                    .op = mir::BinaryOp::kSub, .lhs = width, .rhs = one},
            .type = width_type});
    return block.exprs.Add(
        mir::Expr{
            .data = mir::BinaryExpr{.op = op, .lhs = base, .rhs = w_minus_1},
            .type = base_type});
  };
  return std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& b) -> diag::Result<RangeLoHi> {
            auto lo = lower_one(b.msb_expr);
            if (!lo) return std::unexpected(std::move(lo.error()));
            auto hi = lower_one(b.lsb_expr);
            if (!hi) return std::unexpected(std::move(hi.error()));
            return RangeLoHi{.lo = *lo, .hi = *hi};
          },
          [&](const hir::RangeIndexedUpBounds& b) -> diag::Result<RangeLoHi> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            auto width = lower_one(b.width);
            if (!width) return std::unexpected(std::move(width.error()));
            return RangeLoHi{
                .lo = *base,
                .hi = build_base_at_width_offset(
                    *base, *width, mir::BinaryOp::kAdd)};
          },
          [&](const hir::RangeIndexedDownBounds& b) -> diag::Result<RangeLoHi> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            auto width = lower_one(b.width);
            if (!width) return std::unexpected(std::move(width.error()));
            return RangeLoHi{
                .lo = build_base_at_width_offset(
                    *base, *width, mir::BinaryOp::kSub),
                .hi = *base};
          },
      },
      bounds);
}

// LRM 11.5.2 packed-array slice projection. Bounds are constants per the LRM;
// each declared endpoint maps through `declared` to its LSB-relative offset,
// then `[min, max]` gives the physical range the runtime Slice consumes. A
// descending zero-based range is the identity, so this is transparent for the
// common case.
// Orders two already-rebased endpoint offsets into `(lo, hi)` at runtime: `lo`
// is the lower offset, the physical start the Slice protocol consumes. Picked
// with a conditional rather than a compile-time min / max so a constant slice
// folds downstream (the optimizer's job) and a genvar slice stays runtime,
// without assuming a static ordering -- a packed descending range, a
// dynamic-array ascending range, and a raw `[width-1:0]` projection each order
// their endpoints differently.
auto OrderRebasedEndpoints(
    const ModuleLowerer& module, mir::Block& block, mir::ExprId a,
    mir::ExprId b) -> RangeLoHi {
  const mir::TypeId off_type = block.exprs.Get(a).type;
  const mir::ExprId le = block.exprs.Add(
      mir::Expr{
          .data =
              mir::BinaryExpr{
                  .op = mir::BinaryOp::kLessEqual, .lhs = a, .rhs = b},
          .type = module.Unit().builtins.bit1});
  return RangeLoHi{
      .lo = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::ConditionalExpr{
                      .condition = le, .then_value = a, .else_value = b},
              .type = off_type}),
      .hi = block.exprs.Add(
          mir::Expr{
              .data =
                  mir::ConditionalExpr{
                      .condition = le, .then_value = b, .else_value = a},
              .type = off_type})};
}

template <typename LowerOne>
auto BuildPackedRangeLoHi(
    const ModuleLowerer& module, mir::Block& block,
    const hir::RangeBounds& bounds, const hir::PackedRange& declared,
    LowerOne lower_one) -> diag::Result<RangeLoHi> {
  return std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& b) -> diag::Result<RangeLoHi> {
            auto msb = lower_one(b.msb_expr);
            if (!msb) return std::unexpected(std::move(msb.error()));
            auto lsb = lower_one(b.lsb_expr);
            if (!lsb) return std::unexpected(std::move(lsb.error()));
            // Rebase each declared endpoint to its LSB-relative offset at
            // runtime (the same mapping `WrapPackedIndex` applies to an element
            // index), then order them, so a constant bound folds downstream and
            // a genvar-dependent bound stays a runtime read.
            return OrderRebasedEndpoints(
                module, block,
                WrapPackedIndex(
                    module, block, declared, *msb, block.exprs.Get(*msb).type),
                WrapPackedIndex(
                    module, block, declared, *lsb, block.exprs.Get(*lsb).type));
          },
          [&](const hir::RangeIndexedUpBounds& b) -> diag::Result<RangeLoHi> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            auto width = lower_one(b.width);
            if (!width) return std::unexpected(std::move(width.error()));
            const auto count = IntConstFromMirExpr(block.exprs.Get(*width));
            return PackedIndexedLoHi(
                module, block, declared, *base, count, IndexedDir::kUp);
          },
          [&](const hir::RangeIndexedDownBounds& b) -> diag::Result<RangeLoHi> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            auto width = lower_one(b.width);
            if (!width) return std::unexpected(std::move(width.error()));
            const auto count = IntConstFromMirExpr(block.exprs.Get(*width));
            return PackedIndexedLoHi(
                module, block, declared, *base, count, IndexedDir::kDown);
          },
      },
      bounds);
}

// LRM 7.4.5 unpacked-array slice projection. SV positions are projected to
// zero-based vector positions through the declared range; `count` comes
// from the slice's result type. The leftmost-in-memory position depends on
// the declared direction -- for a descending declared range, indexed-up's
// base is at the high end of the slice and must be offset down to the
// memory-leftmost position before projecting.
template <typename LowerOne>
auto BuildUnpackedRangeLoHi(
    const ModuleLowerer& module, mir::Block& block,
    const hir::RangeBounds& bounds, const hir::UnpackedRange& declared,
    std::uint32_t count_u32, LowerOne lower_one) -> diag::Result<RangeLoHi> {
  const std::int64_t count = count_u32;
  const bool descending = declared.left >= declared.right;
  auto project_sv = [&](mir::ExprId sv_expr) -> mir::ExprId {
    const auto type = block.exprs.Get(sv_expr).type;
    return WrapUnpackedIndex(module, block, declared, sv_expr, type);
  };
  return std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& b) -> diag::Result<RangeLoHi> {
            auto msb = lower_one(b.msb_expr);
            if (!msb) return std::unexpected(std::move(msb.error()));
            const auto lo = project_sv(*msb);
            return RangeLoHi{
                .lo = lo,
                .hi = BuildHiFromLoAndCount(module, block, lo, count)};
          },
          [&](const hir::RangeIndexedUpBounds& b) -> diag::Result<RangeLoHi> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            const auto leftmost_sv =
                descending
                    ? block.exprs.Add(BuildOffsetDeltaExpr(
                          module, block, *base, count - 1, mir::BinaryOp::kAdd))
                    : *base;
            const auto lo = project_sv(leftmost_sv);
            return RangeLoHi{
                .lo = lo,
                .hi = BuildHiFromLoAndCount(module, block, lo, count)};
          },
          [&](const hir::RangeIndexedDownBounds& b) -> diag::Result<RangeLoHi> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            const auto leftmost_sv = descending
                                         ? *base
                                         : block.exprs.Add(BuildOffsetDeltaExpr(
                                               module, block, *base, count - 1,
                                               mir::BinaryOp::kSub));
            const auto lo = project_sv(leftmost_sv);
            return RangeLoHi{
                .lo = lo,
                .hi = BuildHiFromLoAndCount(module, block, lo, count)};
          },
      },
      bounds);
}

// Project an `hir::RangeBounds` to the `(lo, hi)` pair the runtime Slice
// protocol consumes. Three container kinds (queue / packed / unpacked) each
// have their own projection rules per LRM section; the dispatch picks the
// right helper and forwards.
template <typename LowerOne>
auto UnfoldRangeBoundsToLoHi(
    const ModuleLowerer& module, mir::Block& block,
    const hir::RangeBounds& bounds, const RangeStrategy& strategy,
    LowerOne lower_one) -> diag::Result<RangeLoHi> {
  switch (strategy.container) {
    case RangeContainer::kQueue:
      return BuildQueueRangeLoHi(module, block, bounds, lower_one);
    case RangeContainer::kPacked:
      return BuildPackedRangeLoHi(
          module, block, bounds, *strategy.packed_declared, lower_one);
    case RangeContainer::kUnpacked:
      return BuildUnpackedRangeLoHi(
          module, block, bounds, *strategy.unpacked_declared,
          strategy.unpacked_count, lower_one);
  }
  throw InternalError("UnfoldRangeBoundsToLoHi: unknown RangeContainer");
}

// Extract the slice's element count from its MIR result type. For a packed
// result the count is the outer dim's element count; for an unpacked-array
// result it is the size field. Used by the slice factories to materialise
// the `count` argument for fixed-width Slice calls (the `Sliceable`
// protocol's second argument; see `concepts.hpp`).
auto SliceResultOuterCount(const mir::Type& result_ty) -> std::uint32_t {
  if (result_ty.IsIntegralPacked()) {
    return static_cast<std::uint32_t>(
        result_ty.AsIntegralPacked().dims.front().ElementCount());
  }
  if (std::holds_alternative<mir::UnpackedArrayType>(result_ty.data)) {
    return static_cast<std::uint32_t>(
        std::get<mir::UnpackedArrayType>(result_ty.data).size);
  }
  throw InternalError(
      "SliceResultOuterCount: result type is not a fixed-width slice");
}

// `arr[i]` element access (LRM 7.4.5 / 7.5 / 7.10). The callee carries
// just read-vs-write (`kElement` / `kElementRef`); the receiver's container
// kind picks the runtime overload at C++ render time. Projects the SV-source
// index through the declared range -- to a zero-based vector position for an
// unpacked array, to an LSB-relative bit offset for a packed one -- so the
// runtime needs no declared-direction awareness.
auto BuildElementAccessCallExpr(
    const ModuleLowerer& module, mir::Block& block,
    const hir::Type& hir_base_ty, mir::TypeId hir_idx_type, mir::ExprId base_id,
    mir::ExprId idx_id, AccessSide side, mir::TypeId result_type) -> mir::Expr {
  mir::ExprId effective_idx = idx_id;
  if (const auto* ua = std::get_if<hir::UnpackedArrayType>(&hir_base_ty.data)) {
    effective_idx =
        WrapUnpackedIndex(module, block, ua->dim, idx_id, hir_idx_type);
  } else if (
      const auto* pa = std::get_if<hir::PackedArrayType>(&hir_base_ty.data)) {
    effective_idx =
        WrapPackedIndex(module, block, pa->dim, idx_id, hir_idx_type);
  } else if (std::holds_alternative<hir::ScalarBitType>(hir_base_ty.data)) {
    // A bit-select on a single-bit scalar: its declared range is `[0:0]`.
    static constexpr hir::PackedRange kScalarDim{.left = 0, .right = 0};
    effective_idx =
        WrapPackedIndex(module, block, kScalarDim, idx_id, hir_idx_type);
  }
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee = ElementAccessCallee(side),
              .arguments = {base_id, effective_idx}},
      .type = result_type};
}

// `arr[hi:lo]` / `arr[base+:w]` / `arr[base-:w]` range select. Queue
// carries `[base, lo, hi]` because LRM 7.10.1 derives the dynamic count
// from the bounds; every other container carries `[base, offset, count]`
// because LRM 7.4.5 / 11.5.2 require the runtime to canonical-fill at the
// type-determined width even when bounds carry X/Z, and that width is not
// derivable from `(lo, hi)` alone.
template <typename LowerOne>
auto BuildRangeSliceCallExpr(
    const ModuleLowerer& module, mir::Block& block,
    const hir::Type& hir_base_ty, const hir::RangeBounds& bounds,
    mir::ExprId base_id, mir::TypeId result_type, AccessSide side,
    LowerOne lower_one) -> diag::Result<mir::Expr> {
  RangeStrategy strategy;
  const bool is_queue =
      std::holds_alternative<hir::QueueType>(hir_base_ty.data);
  if (is_queue) {
    strategy.container = RangeContainer::kQueue;
  } else if (
      const auto* ua = std::get_if<hir::UnpackedArrayType>(&hir_base_ty.data)) {
    const auto& result_ty = module.Unit().types.Get(result_type);
    strategy.container = RangeContainer::kUnpacked;
    strategy.unpacked_declared = &ua->dim;
    strategy.unpacked_count = static_cast<std::uint32_t>(
        std::get<mir::UnpackedArrayType>(result_ty.data).size);
  } else if (
      const auto* pa = std::get_if<hir::PackedArrayType>(&hir_base_ty.data)) {
    strategy.container = RangeContainer::kPacked;
    strategy.packed_declared = &pa->dim;
  } else {
    // A scalar bit, a dynamic array, and a packed struct / union all index by
    // raw zero-based offset -- their bounds are already physical -- so an
    // identity declared range leaves them untranslated.
    static constexpr hir::PackedRange kRawOffset{.left = 0, .right = 0};
    strategy.container = RangeContainer::kPacked;
    strategy.packed_declared = &kRawOffset;
  }
  auto bounds_or =
      UnfoldRangeBoundsToLoHi(module, block, bounds, strategy, lower_one);
  if (!bounds_or) return std::unexpected(std::move(bounds_or.error()));
  if (is_queue) {
    // LRM 7.10 defines no write-side queue slice; slang rejects a queue
    // slice in lvalue context, so the LHS path never reaches here.
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = mir::Direct{.target = support::BuiltinFn::kSlice},
                .arguments = {base_id, bounds_or->lo, bounds_or->hi}},
        .type = result_type};
  }
  const auto count =
      SliceResultOuterCount(module.Unit().types.Get(result_type));
  const auto count_id = block.exprs.Add(
      mir::MakeInt32Literal(
          module.Unit().builtins.int32, static_cast<std::int64_t>(count)));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = side == AccessSide::kLhs
                                    ? support::BuiltinFn::kSliceRef
                                    : support::BuiltinFn::kSlice},
              .arguments = {base_id, bounds_or->lo, count_id}},
      .type = result_type};
}

// LRM 7.2.1 packed struct / union field-as-slice. The field's
// `(bit_offset, bit_width)` projects to the same packed-path slice shape a
// range-select emits, so the runtime sees one slice form regardless of
// whether the source was `s.field` or `s[hi:lo]`.
auto BuildFieldSliceCallExpr(
    const ModuleLowerer& module, mir::Block& block, mir::ExprId base_id,
    std::uint32_t bit_offset, std::uint32_t bit_width, mir::TypeId result_type,
    AccessSide side) -> mir::Expr {
  const mir::TypeId int32_type = module.Unit().builtins.int32;
  const auto offset_id = block.exprs.Add(
      mir::MakeInt32Literal(int32_type, static_cast<std::int64_t>(bit_offset)));
  const auto count_id = block.exprs.Add(
      mir::MakeInt32Literal(int32_type, static_cast<std::int64_t>(bit_width)));
  return mir::Expr{
      .data =
          mir::CallExpr{
              .callee =
                  mir::Direct{
                      .target = side == AccessSide::kLhs
                                    ? support::BuiltinFn::kSliceRef
                                    : support::BuiltinFn::kSlice},
              .arguments = {base_id, offset_id, count_id}},
      .type = result_type};
}

// Per-kind inner helpers that combine the factory call with the
// read/write-side wrapping. RHS readers wrap with `WrapPackedAsOwned`
// (no-op for queue / AA); LHS writers leave the borrowed-view chain
// intact for `operator=` to consume.

auto LowerElementSelectInner(
    const ModuleLowerer& module, mir::Block& block,
    const hir::Type& hir_base_ty, mir::TypeId hir_idx_type, mir::ExprId base_id,
    mir::ExprId idx_id, AccessSide side, mir::TypeId result_type,
    bool wrap_packed_as_owned) -> mir::Expr {
  mir::Expr access_call = BuildElementAccessCallExpr(
      module, block, hir_base_ty, hir_idx_type, base_id, idx_id, side,
      result_type);
  if (!wrap_packed_as_owned) return access_call;
  return WrapPackedAsOwned(
      module.Unit(), block, std::move(access_call), result_type);
}

template <typename LowerOne>
auto LowerRangeSelectInner(
    const ModuleLowerer& module, mir::Block& block,
    const hir::Type& hir_base_ty, const hir::RangeBounds& bounds,
    mir::ExprId base_id, mir::TypeId result_type, LowerOne lower_one,
    AccessSide side) -> diag::Result<mir::Expr> {
  auto slice_or = BuildRangeSliceCallExpr(
      module, block, hir_base_ty, bounds, base_id, result_type, side,
      lower_one);
  if (!slice_or) return std::unexpected(std::move(slice_or.error()));
  if (side == AccessSide::kLhs) return *std::move(slice_or);
  return WrapPackedAsOwned(
      module.Unit(), block, *std::move(slice_or), result_type);
}

// Packed-struct / union field access. RHS readers route through the
// unsigned-slice path with an explicit `ConversionExpr` re-tag to preserve
// the field's declared signedness (LRM 7.4.1 -- part-select returns
// unsigned). LHS writers emit the slice call against the field's declared
// type without the re-tag.
auto LowerMemberAccessInner(
    ModuleLowerer& module, mir::Block& block,
    const hir::PackedAggregateField& field, mir::ExprId base_id,
    mir::TypeId result_type, AccessSide side) -> mir::Expr {
  if (side == AccessSide::kLhs) {
    return BuildFieldSliceCallExpr(
        module, block, base_id, field.bit_offset, field.bit_width, result_type,
        side);
  }
  const mir::TypeId slice_type =
      UnsignedPackedCounterpart(module.Unit(), result_type);
  mir::Expr slice_call = BuildFieldSliceCallExpr(
      module, block, base_id, field.bit_offset, field.bit_width, slice_type,
      side);
  mir::Expr owned = WrapPackedAsOwned(
      module.Unit(), block, std::move(slice_call), slice_type);
  return WrapSliceSignReTag(
      module.Unit(), block, std::move(owned), slice_type, result_type);
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerHirElementSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const bool is_write = frame.is_lvalue_target;
  const WalkFrame sub_frame = frame.WithLvalueTarget(false);

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerExpr(hir_base, sub_frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));

  const auto& hir_idx = exprs.Get(sel.index);
  auto idx_or = lowerer.LowerExpr(hir_idx, sub_frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));

  const auto& hir_base_ty = module.Hir().types.Get(hir_base.type);
  // LRM 6.16: indexed character read `s[i]` is the element-value access, the
  // read-side dual of the element-reference write. It joins the generic
  // element-access path (the explicit `getc` / `putc` methods are a separate
  // lowering); the value-vs-reference pair mirrors a packed array element.
  if (hir_base_ty.Kind() == hir::TypeKind::kString) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = ElementAccessCallee(AccessSide::kRead),
                .arguments = {base_id, idx_id}},
        .type = result_type};
  }
  // LRM 7.10.1: queue's `q[$+1] = v` appends, so a queue element write
  // must dispatch as LHS even from this RHS entry point. The lvalue-target
  // marker carried on the walk frame surfaces that case. The string and queue
  // arms are reachable only in a procedural scope: slang forbids referencing
  // an element of a dynamic-typed variable outside a procedural context, so a
  // structural instantiation never takes them.
  const AccessSide side =
      (is_write && std::holds_alternative<hir::QueueType>(hir_base_ty.data))
          ? AccessSide::kLhs
          : AccessSide::kRead;
  return LowerElementSelectInner(
      module, block, hir_base_ty, module.TranslateType(hir_idx.type), base_id,
      idx_id, side, result_type, true);
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));

  auto lower_one = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(exprs.Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return block.exprs.Add(*std::move(lowered));
  };
  const auto& hir_base_ty = module.Hir().types.Get(hir_base.type);
  return LowerRangeSelectInner(
      module, block, hir_base_ty, sel.bounds, base_id, result_type, lower_one,
      AccessSide::kRead);
}

// LRM 7.2.1: packed struct / union field access "can be selected as if it
// were a packed array". HIR -> MIR resolves the field-table index to a
// concrete `(offset, count)` slice -- the same MIR shape `s[hi:lo]`
// produces.
template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExpr(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const auto& base_hir_expr = exprs.Get(sel.base_value);
  // LRM 8.4: a class property access reaches the object through the handle. The
  // handle is read (the receiver), and the property is named by its class-local
  // member id, which is its declaration-order field index.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kClassHandle) {
    auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::MakeMemberAccessExpr(
        base_id, mir::MemberRef{.var = mir::MemberId{sel.field_index}},
        result_type);
  }
  // LRM 7.2: an unpacked struct lowers to a generic product (`TupleType`);
  // member access is a positional projection by declaration-order index.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedStruct) {
    auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::Expr{
        .data = mir::TupleGetExpr{.tuple = base_id, .index = sel.field_index},
        .type = result_type};
  }
  // LRM 7.3: an unpacked union member read projects the active member by index
  // out of the overlapping-storage value (`UnionGetExpr`); reading an
  // inactive member is undefined and the backend returns that member's default.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedUnion) {
    auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::Expr{
        .data =
            mir::UnionGetExpr{.union_value = base_id, .index = sel.field_index},
        .type = result_type};
  }
  const auto& fields =
      GetAggregateFields(module.Hir().types.Get(base_hir_expr.type));
  if (sel.field_index >= fields.size()) {
    throw InternalError("LowerHirMemberAccessExpr: field_index out of range");
  }
  auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  return LowerMemberAccessInner(
      module, block, fields[sel.field_index], base_id, result_type,
      AccessSide::kRead);
}

template <ExprLowerer Lowerer>
auto LowerHirElementSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerLhsExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));

  const auto& hir_idx = exprs.Get(sel.index);
  auto idx_or = lowerer.LowerExpr(hir_idx, frame);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  const mir::ExprId idx_id = block.exprs.Add(*std::move(idx_or));

  const auto& hir_base_ty = module.Hir().types.Get(hir_base.type);
  // LRM 6.16: indexed character write `s[i] = ...` is the element-reference
  // access, the write-side dual of the element-value read. It joins the generic
  // element-access path so the place flows through the observable mutate route
  // and the assignment shape uniformly.
  if (hir_base_ty.Kind() == hir::TypeKind::kString) {
    return mir::Expr{
        .data =
            mir::CallExpr{
                .callee = ElementAccessCallee(AccessSide::kLhs),
                .arguments = {base_id, idx_id}},
        .type = result_type};
  }
  return LowerElementSelectInner(
      module, block, hir_base_ty, module.TranslateType(hir_idx.type), base_id,
      idx_id, AccessSide::kLhs, result_type, false);
}

template <ExprLowerer Lowerer>
auto LowerHirRangeSelectExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;

  const auto& hir_base = exprs.Get(sel.base_value);
  auto base_or = lowerer.LowerLhsExpr(hir_base, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));

  auto lower_one = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = lowerer.LowerExpr(exprs.Get(id), frame);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return block.exprs.Add(*std::move(lowered));
  };
  const auto& hir_base_ty = module.Hir().types.Get(hir_base.type);
  return LowerRangeSelectInner(
      module, block, hir_base_ty, sel.bounds, base_id, result_type, lower_one,
      AccessSide::kLhs);
}

template <ExprLowerer Lowerer>
auto LowerHirMemberAccessExprLhs(
    Lowerer& lowerer, WalkFrame frame, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto& module = lowerer.Module();
  const auto& exprs = lowerer.HirExprs();
  auto& block = *frame.current_block;
  const auto& base_hir_expr = exprs.Get(sel.base_value);
  // LRM 8.4: a class property write reaches the object through the handle. The
  // handle is read to reach the shared object, and the property place is the
  // member access through it; the write itself targets the object's storage.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kClassHandle) {
    auto base_or = lowerer.LowerExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::MakeMemberAccessExpr(
        base_id, mir::MemberRef{.var = mir::MemberId{sel.field_index}},
        result_type);
  }
  // LRM 7.2: an unpacked-struct member write is a positional projection by
  // index over the base place. The observable root's write routes through the
  // cell's mutate path later, so the place is just the projection here.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedStruct) {
    auto base_or = lowerer.LowerLhsExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::Expr{
        .data = mir::TupleGetExpr{.tuple = base_id, .index = sel.field_index},
        .type = result_type};
  }
  // LRM 7.3: an unpacked union member write is the write-side access form
  // `UnionGetRefExpr` over the union place (the by-reference counterpart of
  // the `UnionGetExpr` read). The observable root routes through the cell's
  // mutate path later; the place is just the projection here.
  if (module.Hir().types.Get(base_hir_expr.type).Kind() ==
      hir::TypeKind::kUnpackedUnion) {
    auto base_or = lowerer.LowerLhsExpr(base_hir_expr, frame);
    if (!base_or) return std::unexpected(std::move(base_or.error()));
    const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
    return mir::Expr{
        .data =
            mir::UnionGetRefExpr{
                .union_value = base_id, .index = sel.field_index},
        .type = result_type};
  }
  const auto& fields =
      GetAggregateFields(module.Hir().types.Get(base_hir_expr.type));
  if (sel.field_index >= fields.size()) {
    throw InternalError(
        "LowerHirMemberAccessExprLhs: field_index out of range");
  }
  auto base_or = lowerer.LowerLhsExpr(base_hir_expr, frame);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = block.exprs.Add(*std::move(base_or));
  return LowerMemberAccessInner(
      module, block, fields[sel.field_index], base_id, result_type,
      AccessSide::kLhs);
}

// One concrete instantiation per pass class. The handler templates are defined
// in this file rather than the header so the file-local helpers stay private,
// so the dispatchers in process_lowerer.cpp / structural_scope_lowerer.cpp link
// against the symbols emitted here.
template auto LowerHirElementSelectExpr(
    ProcessLowerer&, WalkFrame, const hir::ElementSelectExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirElementSelectExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::ElementSelectExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirRangeSelectExpr(
    ProcessLowerer&, WalkFrame, const hir::RangeSelectExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirRangeSelectExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::RangeSelectExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirMemberAccessExpr(
    ProcessLowerer&, WalkFrame, const hir::MemberAccessExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirMemberAccessExpr(
    const StructuralScopeLowerer&, WalkFrame, const hir::MemberAccessExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirElementSelectExprLhs(
    ProcessLowerer&, WalkFrame, const hir::ElementSelectExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirElementSelectExprLhs(
    const StructuralScopeLowerer&, WalkFrame, const hir::ElementSelectExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirRangeSelectExprLhs(
    ProcessLowerer&, WalkFrame, const hir::RangeSelectExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirRangeSelectExprLhs(
    const StructuralScopeLowerer&, WalkFrame, const hir::RangeSelectExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;
template auto LowerHirMemberAccessExprLhs(
    ProcessLowerer&, WalkFrame, const hir::MemberAccessExpr&, mir::TypeId)
    -> diag::Result<mir::Expr>;
template auto LowerHirMemberAccessExprLhs(
    const StructuralScopeLowerer&, WalkFrame, const hir::MemberAccessExpr&,
    mir::TypeId) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
