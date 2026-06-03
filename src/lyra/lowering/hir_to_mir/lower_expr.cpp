#include "lyra/lowering/hir_to_mir/lower_expr.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <memory>
#include <optional>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/procedural_body.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/unary_op.hpp"
#include "lyra/hir/value_ref.hpp"
#include "lyra/lowering/hir_to_mir/inside_predicate.hpp"
#include "lyra/lowering/hir_to_mir/lower_system_subroutine.hpp"
#include "lyra/lowering/hir_to_mir/state.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/conversion.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/unary_op.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Selects how a HIR `LoopVarRef` lowers when reached from a constructor
// expression. The choice is set by the caller via the public
// `LowerProceduralExpr` / `LowerStructuralExpr` wrappers; it is never
// inferred from runtime state inside the recursion.
enum class LoopVarLoweringMode : std::uint8_t {
  kProceduralInduction,
  kStructuralParam,
};

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

// LRM 10.9.1: the replication count of an assignment pattern is a constant
// expression; slang has already evaluated it to an integer literal. The
// unpacked-target path uses this value to materialize the element list at
// compile time (no runtime replication primitive exists for unpacked
// aggregates -- the C++ mental model is a flat `std::vector` initializer).
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

auto BuildUnpackedReplicationFlatList(
    std::span<const mir::ExprId> items_ids, std::uint64_t count,
    mir::TypeId result_type) -> mir::Expr {
  std::vector<mir::ExprId> flat;
  flat.reserve(items_ids.size() * count);
  for (std::uint64_t i = 0; i < count; ++i) {
    flat.insert(flat.end(), items_ids.begin(), items_ids.end());
  }
  return mir::Expr{
      .data = mir::ArrayLiteralExpr{.elements = std::move(flat)},
      .type = result_type};
}

auto LowerBinaryOp(hir::BinaryOp op) -> mir::BinaryOp {
  switch (op) {
    case hir::BinaryOp::kAdd:
      return mir::BinaryOp::kAdd;
    case hir::BinaryOp::kSub:
      return mir::BinaryOp::kSub;
    case hir::BinaryOp::kMul:
      return mir::BinaryOp::kMul;
    case hir::BinaryOp::kDiv:
      return mir::BinaryOp::kDiv;
    case hir::BinaryOp::kMod:
      return mir::BinaryOp::kMod;
    case hir::BinaryOp::kPower:
      return mir::BinaryOp::kPower;
    case hir::BinaryOp::kBitwiseAnd:
      return mir::BinaryOp::kBitwiseAnd;
    case hir::BinaryOp::kBitwiseOr:
      return mir::BinaryOp::kBitwiseOr;
    case hir::BinaryOp::kBitwiseXor:
      return mir::BinaryOp::kBitwiseXor;
    case hir::BinaryOp::kBitwiseXnor:
      return mir::BinaryOp::kBitwiseXnor;
    case hir::BinaryOp::kEquality:
      return mir::BinaryOp::kEquality;
    case hir::BinaryOp::kInequality:
      return mir::BinaryOp::kInequality;
    case hir::BinaryOp::kCaseEquality:
      return mir::BinaryOp::kCaseEquality;
    case hir::BinaryOp::kCaseInequality:
      return mir::BinaryOp::kCaseInequality;
    case hir::BinaryOp::kWildcardEquality:
      return mir::BinaryOp::kWildcardEquality;
    case hir::BinaryOp::kWildcardInequality:
      return mir::BinaryOp::kWildcardInequality;
    case hir::BinaryOp::kGreaterEqual:
      return mir::BinaryOp::kGreaterEqual;
    case hir::BinaryOp::kGreaterThan:
      return mir::BinaryOp::kGreaterThan;
    case hir::BinaryOp::kLessEqual:
      return mir::BinaryOp::kLessEqual;
    case hir::BinaryOp::kLessThan:
      return mir::BinaryOp::kLessThan;
    case hir::BinaryOp::kLogicalAnd:
      return mir::BinaryOp::kLogicalAnd;
    case hir::BinaryOp::kLogicalOr:
      return mir::BinaryOp::kLogicalOr;
    case hir::BinaryOp::kLogicalImplication:
      return mir::BinaryOp::kLogicalImplication;
    case hir::BinaryOp::kLogicalEquivalence:
      return mir::BinaryOp::kLogicalEquivalence;
    case hir::BinaryOp::kLogicalShiftLeft:
    case hir::BinaryOp::kArithmeticShiftLeft:
      return mir::BinaryOp::kShiftLeft;
    case hir::BinaryOp::kLogicalShiftRight:
      return mir::BinaryOp::kLogicalShiftRight;
    case hir::BinaryOp::kArithmeticShiftRight:
      return mir::BinaryOp::kArithmeticShiftRight;
  }
  throw InternalError("LowerBinaryOp: unknown HIR BinaryOp");
}

auto LowerUnaryOp(hir::UnaryOp op) -> mir::UnaryOp {
  switch (op) {
    case hir::UnaryOp::kPlus:
      return mir::UnaryOp::kPlus;
    case hir::UnaryOp::kMinus:
      return mir::UnaryOp::kMinus;
    case hir::UnaryOp::kBitwiseNot:
      return mir::UnaryOp::kBitwiseNot;
    case hir::UnaryOp::kLogicalNot:
      return mir::UnaryOp::kLogicalNot;
    case hir::UnaryOp::kReductionAnd:
      return mir::UnaryOp::kReductionAnd;
    case hir::UnaryOp::kReductionOr:
      return mir::UnaryOp::kReductionOr;
    case hir::UnaryOp::kReductionXor:
      return mir::UnaryOp::kReductionXor;
    case hir::UnaryOp::kReductionNand:
      return mir::UnaryOp::kReductionNand;
    case hir::UnaryOp::kReductionNor:
      return mir::UnaryOp::kReductionNor;
    case hir::UnaryOp::kReductionXnor:
      return mir::UnaryOp::kReductionXnor;
  }
  throw InternalError("LowerUnaryOp: unknown HIR UnaryOp");
}

auto LowerIncDecOp(hir::IncDecOp op) -> mir::IncDecOp {
  switch (op) {
    case hir::IncDecOp::kPreInc:
      return mir::IncDecOp::kPreInc;
    case hir::IncDecOp::kPostInc:
      return mir::IncDecOp::kPostInc;
    case hir::IncDecOp::kPreDec:
      return mir::IncDecOp::kPreDec;
    case hir::IncDecOp::kPostDec:
      return mir::IncDecOp::kPostDec;
  }
  throw InternalError("LowerIncDecOp: unknown HIR IncDecOp");
}

auto LowerTimeScale(hir::TimeScale s) -> mir::TimeScale {
  switch (s) {
    case hir::TimeScale::kFs:
      return mir::TimeScale::kFs;
    case hir::TimeScale::kPs:
      return mir::TimeScale::kPs;
    case hir::TimeScale::kNs:
      return mir::TimeScale::kNs;
    case hir::TimeScale::kUs:
      return mir::TimeScale::kUs;
    case hir::TimeScale::kMs:
      return mir::TimeScale::kMs;
    case hir::TimeScale::kS:
      return mir::TimeScale::kS;
  }
  throw InternalError("LowerTimeScale: unknown HIR TimeScale");
}

auto LowerSignedness(hir::Signedness s) -> mir::Signedness {
  switch (s) {
    case hir::Signedness::kSigned:
      return mir::Signedness::kSigned;
    case hir::Signedness::kUnsigned:
      return mir::Signedness::kUnsigned;
  }
  throw InternalError("LowerSignedness: unknown HIR Signedness");
}

auto LowerStateKind(hir::IntegralStateKind k) -> mir::IntegralStateKind {
  switch (k) {
    case hir::IntegralStateKind::kTwoState:
      return mir::IntegralStateKind::kTwoState;
    case hir::IntegralStateKind::kFourState:
      return mir::IntegralStateKind::kFourState;
  }
  throw InternalError("LowerStateKind: unknown HIR IntegralStateKind");
}

auto LowerHirIntegralConstant(const hir::IntegralConstant& c)
    -> mir::IntegralConstant {
  return mir::IntegralConstant{
      .value_words = c.value_words,
      .state_words = c.state_words,
      .width = c.width,
      .signedness = LowerSignedness(c.signedness),
      .state_kind = LowerStateKind(c.state_kind),
  };
}

auto LowerHirConversionKind(hir::ConversionKind k) -> mir::ConversionKind {
  switch (k) {
    case hir::ConversionKind::kImplicit:
      return mir::ConversionKind::kImplicit;
    case hir::ConversionKind::kPropagated:
      return mir::ConversionKind::kPropagated;
    case hir::ConversionKind::kStreamingConcat:
      return mir::ConversionKind::kStreamingConcat;
    case hir::ConversionKind::kExplicit:
      return mir::ConversionKind::kExplicit;
    case hir::ConversionKind::kBitstreamCast:
      return mir::ConversionKind::kBitstreamCast;
  }
  throw InternalError("LowerHirConversionKind: unknown HIR ConversionKind");
}

auto LowerStructuralVarRefExpr(
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralVarRef& m, mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data = scope_state.TranslateStructuralVar(m.hops, m.var), .type = type};
}

auto LowerProceduralVarRefExpr(
    const ProcessLoweringState& proc_state, const hir::ProceduralVarRef& l,
    mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data = proc_state.TranslateProceduralVar(l.var), .type = type};
}

auto LowerLoopVarRefExpr(
    const ConstructorLoweringState& ctor_state, const hir::LoopVarRef& lv,
    mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data = ctor_state.TranslateLoopVar(lv.loop_var), .type = type};
}

auto LowerStructuralParamRefExpr(
    const StructuralScopeLoweringState& scope_state, const hir::LoopVarRef& lv,
    mir::TypeId type) -> mir::Expr {
  return mir::Expr{
      .data =
          scope_state.TranslateLoopVarAsStructuralParam(lv.hops, lv.loop_var),
      .type = type};
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
    const UnitLoweringState& unit_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::RangeBounds& bounds, LowerOne lower_one)
    -> diag::Result<RangeOffsetCount> {
  return std::visit(
      Overloaded{
          [&](const hir::RangeConstantBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto msb = lower_one(b.msb_expr);
            if (!msb) return std::unexpected(std::move(msb.error()));
            auto lsb = lower_one(b.lsb_expr);
            if (!lsb) return std::unexpected(std::move(lsb.error()));
            const auto msb_val =
                IntConstFromMirExpr(proc_scope_state.GetExpr(*msb));
            const auto lsb_val =
                IntConstFromMirExpr(proc_scope_state.GetExpr(*lsb));
            const auto count = static_cast<std::uint32_t>(
                (msb_val >= lsb_val ? msb_val - lsb_val : lsb_val - msb_val) +
                1);
            // The offset is the numerically lower of the two bounds. For a
            // descending range this is the syntactic right (`lsb_expr`), for
            // ascending it is the syntactic left (`msb_expr`).
            const auto offset_val = std::min(msb_val, lsb_val);
            const auto offset_id = proc_scope_state.AddExpr(
                unit_state.MakeInt32LiteralExpr(offset_val));
            return RangeOffsetCount{.offset_expr = offset_id, .count = count};
          },
          [&](const hir::RangeIndexedUpBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            auto width = lower_one(b.width);
            if (!width) return std::unexpected(std::move(width.error()));
            const auto count = static_cast<std::uint32_t>(
                IntConstFromMirExpr(proc_scope_state.GetExpr(*width)));
            return RangeOffsetCount{.offset_expr = *base, .count = count};
          },
          [&](const hir::RangeIndexedDownBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            auto width = lower_one(b.width);
            if (!width) return std::unexpected(std::move(width.error()));
            const auto count = static_cast<std::uint32_t>(
                IntConstFromMirExpr(proc_scope_state.GetExpr(*width)));
            const auto base_type = proc_scope_state.GetExpr(*base).type;
            const auto sub_lit =
                proc_scope_state.AddExpr(unit_state.MakeInt32LiteralExpr(
                    static_cast<std::int64_t>(count) - 1));
            const auto offset = proc_scope_state.AddExpr(
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

auto LowerUserCallee(
    const StructuralScopeLoweringState& scope_state,
    const hir::StructuralSubroutineRef& u) -> mir::Callee {
  return scope_state.TranslateStructuralSubroutine(u.hops, u.subroutine);
}

auto LowerHirIntegerLiteral(const hir::IntegerLiteral& i, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{
      .data = mir::IntegerLiteral{.value = LowerHirIntegralConstant(i.value)},
      .type = type};
}

auto LowerEnumMethodKind(hir::EnumMethodKind k) -> mir::EnumMethodKind {
  switch (k) {
    case hir::EnumMethodKind::kFirst:
      return mir::EnumMethodKind::kFirst;
    case hir::EnumMethodKind::kLast:
      return mir::EnumMethodKind::kLast;
    case hir::EnumMethodKind::kNum:
      return mir::EnumMethodKind::kNum;
    case hir::EnumMethodKind::kNext:
      return mir::EnumMethodKind::kNext;
    case hir::EnumMethodKind::kPrev:
      return mir::EnumMethodKind::kPrev;
    case hir::EnumMethodKind::kName:
      return mir::EnumMethodKind::kName;
  }
  throw InternalError("LowerEnumMethodKind: unknown hir::EnumMethodKind");
}

auto LowerStringMethodKind(hir::StringMethodKind k) -> mir::StringMethodKind {
  switch (k) {
    case hir::StringMethodKind::kLen:
      return mir::StringMethodKind::kLen;
    case hir::StringMethodKind::kGetc:
      return mir::StringMethodKind::kGetc;
    case hir::StringMethodKind::kPutc:
      return mir::StringMethodKind::kPutc;
    case hir::StringMethodKind::kToupper:
      return mir::StringMethodKind::kToupper;
    case hir::StringMethodKind::kTolower:
      return mir::StringMethodKind::kTolower;
    case hir::StringMethodKind::kCompare:
      return mir::StringMethodKind::kCompare;
    case hir::StringMethodKind::kIcompare:
      return mir::StringMethodKind::kIcompare;
    case hir::StringMethodKind::kSubstr:
      return mir::StringMethodKind::kSubstr;
    case hir::StringMethodKind::kAtoi:
      return mir::StringMethodKind::kAtoi;
    case hir::StringMethodKind::kAtohex:
      return mir::StringMethodKind::kAtohex;
    case hir::StringMethodKind::kAtooct:
      return mir::StringMethodKind::kAtooct;
    case hir::StringMethodKind::kAtobin:
      return mir::StringMethodKind::kAtobin;
    case hir::StringMethodKind::kAtoreal:
      return mir::StringMethodKind::kAtoreal;
    case hir::StringMethodKind::kItoa:
      return mir::StringMethodKind::kItoa;
    case hir::StringMethodKind::kHextoa:
      return mir::StringMethodKind::kHextoa;
    case hir::StringMethodKind::kOcttoa:
      return mir::StringMethodKind::kOcttoa;
    case hir::StringMethodKind::kBintoa:
      return mir::StringMethodKind::kBintoa;
    case hir::StringMethodKind::kRealtoa:
      return mir::StringMethodKind::kRealtoa;
  }
  throw InternalError("LowerStringMethodKind: unknown hir::StringMethodKind");
}

auto LowerEventMethodKind(hir::EventMethodKind k) -> mir::EventMethodKind {
  switch (k) {
    case hir::EventMethodKind::kTrigger:
      return mir::EventMethodKind::kTrigger;
    case hir::EventMethodKind::kAwait:
      return mir::EventMethodKind::kAwait;
    case hir::EventMethodKind::kTriggered:
      return mir::EventMethodKind::kTriggered;
  }
  throw InternalError("LowerEventMethodKind: unknown hir::EventMethodKind");
}

auto LowerHirStringLiteral(const hir::StringLiteral& s, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{.data = mir::StringLiteral{.value = s.value}, .type = type};
}

auto LowerHirTimeLiteral(
    const UnitLoweringState& unit_state, const hir::TimeLiteral& t)
    -> mir::Expr {
  return mir::Expr{
      .data =
          mir::TimeLiteral{.value = t.value, .scale = LowerTimeScale(t.scale)},
      .type = unit_state.Builtins().realtime};
}

auto LowerHirRealLiteral(const hir::RealLiteral& r, mir::TypeId type)
    -> mir::Expr {
  return mir::Expr{.data = mir::RealLiteral{.value = r.value}, .type = type};
}

auto LowerHirPrimaryProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state, const hir::Primary& p,
    mir::TypeId type) -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return LowerHirIntegerLiteral(i, type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(s, type);
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return LowerHirTimeLiteral(unit_state, t);
          },
          [&](const hir::RealLiteral& r) -> mir::Expr {
            return LowerHirRealLiteral(r, type);
          },
          [&](const hir::StructuralVarRef& m) -> mir::Expr {
            return LowerStructuralVarRefExpr(scope_state, m, type);
          },
          [&](const hir::ProceduralVarRef& l) -> mir::Expr {
            return LowerProceduralVarRefExpr(proc_state, l, type);
          },
          [&](const hir::LoopVarRef& lv) -> mir::Expr {
            return LowerStructuralParamRefExpr(scope_state, lv, type);
          },
          [&](const hir::CrossUnitVarRef& c) -> mir::Expr {
            return mir::Expr{
                .data = mir::CrossUnitVarRef{.id = {.value = c.id.value}},
                .type = type};
          },
      },
      p);
}

auto LowerHirPrimaryStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state, const hir::Primary& p,
    mir::TypeId type, LoopVarLoweringMode loop_var_mode) -> mir::Expr {
  return std::visit(
      Overloaded{
          [&](const hir::IntegerLiteral& i) -> mir::Expr {
            return LowerHirIntegerLiteral(i, type);
          },
          [&](const hir::StringLiteral& s) -> mir::Expr {
            return LowerHirStringLiteral(s, type);
          },
          [&](const hir::TimeLiteral& t) -> mir::Expr {
            return LowerHirTimeLiteral(unit_state, t);
          },
          [&](const hir::RealLiteral& r) -> mir::Expr {
            return LowerHirRealLiteral(r, type);
          },
          [&](const hir::StructuralVarRef& m) -> mir::Expr {
            return LowerStructuralVarRefExpr(scope_state, m, type);
          },
          [](const hir::ProceduralVarRef&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryStructural: HIR ProceduralVarRef does not "
                "appear in constructor expressions");
          },
          [&](const hir::LoopVarRef& lv) -> mir::Expr {
            if (loop_var_mode == LoopVarLoweringMode::kProceduralInduction) {
              if (ctor_state == nullptr) {
                throw InternalError(
                    "LowerHirPrimaryStructural: kProceduralInduction mode "
                    "requires a non-null ConstructorLoweringState");
              }
              return LowerLoopVarRefExpr(*ctor_state, lv, type);
            }
            return LowerStructuralParamRefExpr(scope_state, lv, type);
          },
          [](const hir::CrossUnitVarRef&) -> mir::Expr {
            throw InternalError(
                "LowerHirPrimaryStructural: HIR CrossUnitVarRef does not "
                "appear "
                "in constructor expressions");
          },
      },
      p);
}

// Snapshot a single outer-scope subexpression into the closure body. Literal
// values clone verbatim so the body still sees an IntegerLiteral (some
// downstream code paths extract constants from literal-shape exprs). Other
// expressions are captured by value into a fresh procedural var so the body
// reads the value as it stood at submit time.
auto SnapshotNonLhsSubexpr(
    const ProceduralScopeLoweringState& outer_scope_state,
    ProceduralScopeLoweringState& body, std::vector<mir::Capture>& captures,
    std::uint32_t& snapshot_counter, mir::ExprId outer_id) -> mir::ExprId {
  const auto& outer_expr = outer_scope_state.GetExpr(outer_id);
  if (std::holds_alternative<mir::IntegerLiteral>(outer_expr.data)) {
    return body.AddExpr(outer_expr);
  }
  const auto binding = body.AddProceduralVar(
      mir::ProceduralVarDecl{
          .name = std::format("_lyra_nba_idx{}", snapshot_counter++),
          .type = outer_expr.type});
  captures.emplace_back(
      mir::ByValueCapture{.value = outer_id, .binding = binding});
  return body.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = binding},
          .type = outer_expr.type});
}

// Recursively clone an LHS-shaped expression into the closure body. The LHS
// structure (PrimaryExpr, ElementSelectExpr, RangeSelectExpr, ConcatExpr) is
// reproduced as-is; per-layer subexpressions that are NOT part of the LHS
// structure (selector indices, range bounds) are snapshotted by value so
// the closure body sees the values that were live at submit time.
auto CloneLhsExprForNbaBody(
    const ProceduralScopeLoweringState& outer_scope_state,
    ProceduralScopeLoweringState& body, std::vector<mir::Capture>& captures,
    std::uint32_t& snapshot_counter, mir::ExprId outer_id) -> mir::ExprId {
  const auto& outer_expr = outer_scope_state.GetExpr(outer_id);
  return std::visit(
      Overloaded{
          [&](const mir::ElementSelectExpr& s) -> mir::ExprId {
            const mir::ExprId base = CloneLhsExprForNbaBody(
                outer_scope_state, body, captures, snapshot_counter,
                s.base_value);
            const mir::ExprId index = SnapshotNonLhsSubexpr(
                outer_scope_state, body, captures, snapshot_counter, s.index);
            return body.AddExpr(
                mir::Expr{
                    .data =
                        mir::ElementSelectExpr{
                            .base_value = base, .index = index},
                    .type = outer_expr.type});
          },
          [&](const mir::RangeSelectExpr& s) -> mir::ExprId {
            const mir::ExprId base = CloneLhsExprForNbaBody(
                outer_scope_state, body, captures, snapshot_counter,
                s.base_value);
            const mir::ExprId offset = SnapshotNonLhsSubexpr(
                outer_scope_state, body, captures, snapshot_counter,
                s.offset_expr);
            return body.AddExpr(
                mir::Expr{
                    .data =
                        mir::RangeSelectExpr{
                            .base_value = base,
                            .offset_expr = offset,
                            .count = s.count},
                    .type = outer_expr.type});
          },
          [&](const mir::ConcatExpr& c) -> mir::ExprId {
            std::vector<mir::ExprId> body_operands;
            body_operands.reserve(c.operands.size());
            for (const mir::ExprId op_id : c.operands) {
              body_operands.push_back(CloneLhsExprForNbaBody(
                  outer_scope_state, body, captures, snapshot_counter, op_id));
            }
            return body.AddExpr(
                mir::Expr{
                    .data =
                        mir::ConcatExpr{.operands = std::move(body_operands)},
                    .type = outer_expr.type});
          },
          // Leaf var refs -- clone verbatim. Any other form reaching here
          // would mean an unaddressable LHS slipped past the AST -> HIR
          // validator.
          [&](const mir::StructuralVarRef&) -> mir::ExprId {
            return body.AddExpr(outer_expr);
          },
          [&](const mir::ProceduralVarRef&) -> mir::ExprId {
            return body.AddExpr(outer_expr);
          },
          [&](const auto&) -> mir::ExprId {
            throw InternalError(
                "CloneLhsExprForNbaBody: non-addressable expression form in "
                "NBA LHS clone walk");
          },
      },
      outer_expr.data);
}

// Walks the LHS expression to determine whether its addressable root is a
// structural var (vs procedural local). NBA assignment requires a structural
// target; procedural-local NBA is a known gap.
auto IsExprRootedAtStructuralVar(
    const ProceduralScopeLoweringState& proc_scope_state, mir::ExprId expr_id)
    -> bool {
  const auto& expr = proc_scope_state.GetExpr(expr_id);
  return std::visit(
      Overloaded{
          [](const mir::StructuralVarRef&) { return true; },
          [](const mir::ProceduralVarRef&) { return false; },
          [&](const mir::ElementSelectExpr& s) {
            return IsExprRootedAtStructuralVar(proc_scope_state, s.base_value);
          },
          [&](const mir::RangeSelectExpr& s) {
            return IsExprRootedAtStructuralVar(proc_scope_state, s.base_value);
          },
          [&](const mir::ConcatExpr& c) {
            return std::ranges::all_of(c.operands, [&](mir::ExprId op) {
              return IsExprRootedAtStructuralVar(proc_scope_state, op);
            });
          },
          [](const auto&) { return false; },
      },
      expr.data);
}

auto LowerHirUnaryExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::UnaryExpr& u,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto operand_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(u.operand.value));
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      proc_scope_state.AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data = mir::UnaryExpr{.op = LowerUnaryOp(u.op), .operand = operand_id},
      .type = result_type};
}

auto LowerHirBinaryExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::BinaryExpr& b,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto lhs_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(b.lhs.value));
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = proc_scope_state.AddExpr(*std::move(lhs_or));
  auto rhs_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(b.rhs.value));
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = proc_scope_state.AddExpr(*std::move(rhs_or));
  return mir::Expr{
      .data =
          mir::BinaryExpr{
              .op = LowerBinaryOp(b.op), .lhs = lhs_id, .rhs = rhs_id},
      .type = result_type};
}

auto LowerHirConditionalExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::ConditionalExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto cond_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(c.condition.value));
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = proc_scope_state.AddExpr(*std::move(cond_or));
  auto then_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(c.then_value.value));
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::ExprId then_id = proc_scope_state.AddExpr(*std::move(then_or));
  auto else_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(c.else_value.value));
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const mir::ExprId else_id = proc_scope_state.AddExpr(*std::move(else_or));
  return mir::Expr{
      .data =
          mir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id},
      .type = result_type};
}

auto LowerHirAssignExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::AssignExpr& a,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto rhs_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(a.rhs.value));
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::TypeId rhs_type = (*rhs_or).type;
  const mir::ExprId rhs_id = proc_scope_state.AddExpr(*std::move(rhs_or));
  auto lhs_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(a.lhs.value));
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = proc_scope_state.AddExpr(*std::move(lhs_or));

  if (a.kind == hir::AssignKind::kBlocking) {
    const std::optional<mir::BinaryOp> compound_op =
        a.compound_op.has_value() ? std::optional{LowerBinaryOp(*a.compound_op)}
                                  : std::nullopt;
    return mir::Expr{
        .data =
            mir::AssignExpr{
                .target = lhs_id, .compound_op = compound_op, .value = rhs_id},
        .type = result_type};
  }

  if (a.compound_op.has_value()) {
    throw InternalError(
        "LowerHirAssignExprProc: compound assignment with non-blocking kind "
        "is not a legal SV form (LRM A.6.2 grammar)");
  }

  if (!IsExprRootedAtStructuralVar(proc_scope_state, lhs_id)) {
    return diag::Unsupported(
        span, diag::DiagCode::kUnsupportedAssignmentTarget,
        "non-blocking assignment to procedural local is not supported yet",
        diag::UnsupportedCategory::kFeature);
  }

  mir::Expr closure_expr = BuildNbaSubmitClosureExpr(
      unit_state, proc_scope_state, lhs_id, rhs_id, rhs_type);
  const mir::ExprId closure_id =
      proc_scope_state.AddExpr(std::move(closure_expr));
  return mir::Expr{
      .data =
          mir::RuntimeCallExpr{
              .call = mir::RuntimeSubmitNbaCall{.closure = closure_id}},
      .type = unit_state.Builtins().void_type};
}

auto LowerHirIncDecExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::IncDecExpr& inc,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto target_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(inc.target.value));
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  const mir::ExprId target_id = proc_scope_state.AddExpr(*std::move(target_or));
  return mir::Expr{
      .data = mir::IncDecExpr{.op = LowerIncDecOp(inc.op), .target = target_id},
      .type = result_type};
}

auto LowerHirConversionExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::ConversionExpr& cv,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto operand_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(cv.operand.value));
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      proc_scope_state.AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data =
          mir::ConversionExpr{
              .operand = operand_id, .kind = LowerHirConversionKind(cv.kind)},
      .type = result_type};
}

auto LowerHirCallExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::CallExpr& c,
    diag::SourceSpan span, mir::TypeId result_type) -> diag::Result<mir::Expr> {
  return std::visit(
      Overloaded{
          [&](const hir::SystemSubroutineRef& sys) -> diag::Result<mir::Expr> {
            return LowerSystemSubroutineCall(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, c, sys, span);
          },
          [&](const hir::StructuralSubroutineRef& usr)
              -> diag::Result<mir::Expr> {
            // LRM 13.5: a call with output / inout actuals is desugared to
            // copy-in-copy-out at statement position (see LowerExprStmt).
            // Reaching it here means the call is a nested expression operand,
            // where the copy-out statement has nowhere to be sequenced; that
            // form is not yet supported. ref / const ref alias the actual and
            // copy nothing back (LRM 13.5.2), so they are fine as operands and
            // fall through to the value-only argument lowering below.
            const hir::StructuralSubroutineDecl& decl =
                scope_state.LookupHirSubroutine(usr.hops, usr.subroutine);
            for (const auto& param : decl.params) {
              if (hir::RequiresWriteback(param.direction)) {
                return diag::Unsupported(
                    span, diag::DiagCode::kUnsupportedSubroutineArgument,
                    "a call with output / inout arguments is only supported "
                    "in statement position, not as a nested expression",
                    diag::UnsupportedCategory::kFeature);
              }
            }
            std::vector<mir::ExprId> args;
            args.reserve(c.arguments.size());
            for (const auto arg : c.arguments) {
              auto arg_or = LowerExpr(
                  unit_state, scope_state, proc_state, proc_scope_state,
                  hir_process, hir_process.exprs.at(arg.value));
              if (!arg_or) return std::unexpected(std::move(arg_or.error()));
              args.push_back(proc_scope_state.AddExpr(*std::move(arg_or)));
            }
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = LowerUserCallee(scope_state, usr),
                        .arguments = std::move(args)},
                .type = result_type};
          },
          [&](const hir::BuiltinMethodRef& b) -> diag::Result<mir::Expr> {
            if (c.arguments.empty()) {
              throw InternalError(
                  "BuiltinMethodRef call has no receiver argument");
            }
            const hir::TypeId hir_receiver_type =
                hir_process.exprs.at(c.arguments.front().value).type;
            std::vector<mir::ExprId> args;
            args.reserve(c.arguments.size());
            for (const auto arg : c.arguments) {
              auto arg_or = LowerExpr(
                  unit_state, scope_state, proc_state, proc_scope_state,
                  hir_process, hir_process.exprs.at(arg.value));
              if (!arg_or) {
                return std::unexpected(std::move(arg_or.error()));
              }
              args.push_back(proc_scope_state.AddExpr(*std::move(arg_or)));
            }
            auto callee = std::visit(
                Overloaded{
                    [&](hir::EnumMethodKind k) -> mir::BuiltinMethodCallee {
                      return {
                          .method = mir::EnumMethodInfo{
                              .enum_type =
                                  unit_state.TranslateType(hir_receiver_type),
                              .kind = LowerEnumMethodKind(k)}};
                    },
                    [&](hir::StringMethodKind k) -> mir::BuiltinMethodCallee {
                      return {
                          .method = mir::StringMethodInfo{
                              .kind = LowerStringMethodKind(k)}};
                    },
                    [&](hir::EventMethodKind k) -> mir::BuiltinMethodCallee {
                      return {
                          .method = mir::EventMethodInfo{
                              .kind = LowerEventMethodKind(k)}};
                    },
                },
                b.method);
            return mir::Expr{
                .data =
                    mir::CallExpr{
                        .callee = std::move(callee),
                        .arguments = std::move(args)},
                .type = result_type};
          },
      },
      c.callee);
}

auto LowerHirInsideExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::InsideExpr& in,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto lhs_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(in.lhs.value));
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = proc_scope_state.AddExpr(*std::move(lhs_or));

  if (in.items.empty()) {
    throw InternalError(
        "LowerHirInsideExprProc: hir::InsideExpr has empty item list");
  }
  std::optional<mir::ExprId> acc;
  for (const auto& item : in.items) {
    auto pred_or = BuildHirInsideItemPredicate(
        unit_state, scope_state, proc_state, proc_scope_state, hir_process,
        lhs_id, item, result_type);
    if (!pred_or) return std::unexpected(std::move(pred_or.error()));
    if (acc.has_value()) {
      acc = proc_scope_state.AddExpr(
          mir::Expr{
              .data =
                  mir::BinaryExpr{
                      .op = mir::BinaryOp::kLogicalOr,
                      .lhs = *acc,
                      .rhs = *pred_or},
              .type = result_type});
    } else {
      acc = *pred_or;
    }
  }
  return mir::Expr{proc_scope_state.GetExpr(*acc)};
}

// Translates an SV-declared-range index into a zero-based C++ vector offset
// for an unpacked array base. `[0:N]` (ascending from zero) needs no rewrite;
// other ranges fold the base offset into the index expression so the
// downstream backend can emit a uniform `vec[i]` access.
auto WrapUnpackedIndex(
    const UnitLoweringState& unit_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::UnpackedRange& declared, mir::ExprId raw_idx,
    mir::TypeId idx_type) -> mir::ExprId {
  if (declared.left == 0 && declared.right >= 0) {
    return raw_idx;
  }
  const mir::ExprId literal_id =
      proc_scope_state.AddExpr(unit_state.MakeInt32LiteralExpr(declared.left));
  const bool descending = declared.left >= declared.right;
  return proc_scope_state.AddExpr(
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
// offset. This is asymmetric to packed slice's `lowest LSB-bit` convention
// because packed addresses are flat-bit and unpacked stores elements in
// declared source order. The slice's element count comes from the result
// type rather than the bounds expressions, so the helper does not need to
// fold either bound to a literal -- negative-base sources can therefore
// survive (slang exposes `-1` as a UnaryOp around an IntegerLiteral and
// preserves it through binding).
template <typename LowerOne>
auto UnfoldHirRangeBoundsForUnpacked(
    const UnitLoweringState& unit_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::RangeBounds& bounds, const hir::UnpackedRange& declared,
    std::uint32_t count, LowerOne lower_one) -> diag::Result<RangeOffsetCount> {
  const bool descending = declared.left >= declared.right;
  auto with_delta = [&](mir::ExprId base, std::int64_t delta,
                        mir::BinaryOp op) -> mir::ExprId {
    const auto base_type = proc_scope_state.GetExpr(base).type;
    const auto delta_lit =
        proc_scope_state.AddExpr(unit_state.MakeInt32LiteralExpr(delta));
    return proc_scope_state.AddExpr(
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
            const auto msb_type = proc_scope_state.GetExpr(*msb).type;
            const auto vec_offset = WrapUnpackedIndex(
                unit_state, proc_scope_state, declared, *msb, msb_type);
            return RangeOffsetCount{.offset_expr = vec_offset, .count = count};
          },
          [&](const hir::RangeIndexedUpBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            const auto base_type = proc_scope_state.GetExpr(*base).type;
            const auto leftmost_sv =
                descending ? with_delta(
                                 *base, static_cast<std::int64_t>(count) - 1,
                                 mir::BinaryOp::kAdd)
                           : *base;
            const auto vec_offset = WrapUnpackedIndex(
                unit_state, proc_scope_state, declared, leftmost_sv, base_type);
            return RangeOffsetCount{.offset_expr = vec_offset, .count = count};
          },
          [&](const hir::RangeIndexedDownBounds& b)
              -> diag::Result<RangeOffsetCount> {
            auto base = lower_one(b.base_index);
            if (!base) return std::unexpected(std::move(base.error()));
            const auto base_type = proc_scope_state.GetExpr(*base).type;
            const auto leftmost_sv =
                descending ? *base
                           : with_delta(
                                 *base, static_cast<std::int64_t>(count) - 1,
                                 mir::BinaryOp::kSub);
            const auto vec_offset = WrapUnpackedIndex(
                unit_state, proc_scope_state, declared, leftmost_sv, base_type);
            return RangeOffsetCount{.offset_expr = vec_offset, .count = count};
          },
      },
      bounds);
}

auto LowerHirElementSelectExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::ElementSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_base = hir_process.exprs.at(sel.base_value.value);
  auto base_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_base);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope_state.AddExpr(*std::move(base_or));

  const auto& hir_idx = hir_process.exprs.at(sel.index.value);
  auto idx_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_idx);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  mir::ExprId idx_id = proc_scope_state.AddExpr(*std::move(idx_or));

  const auto& hir_base_ty = unit_state.GetHirType(hir_base.type);
  if (const auto* ua = std::get_if<hir::UnpackedArrayType>(&hir_base_ty.data)) {
    idx_id = WrapUnpackedIndex(
        unit_state, proc_scope_state, ua->dim, idx_id,
        unit_state.TranslateType(hir_idx.type));
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
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::RangeSelectExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& hir_base = hir_process.exprs.at(sel.base_value.value);
  auto base_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_base);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope_state.AddExpr(*std::move(base_or));

  auto lower_one = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_process,
        hir_process.exprs.at(id.value));
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return proc_scope_state.AddExpr(*std::move(lowered));
  };
  const auto& hir_base_ty = unit_state.GetHirType(hir_base.type);
  auto unfolded = [&]() {
    if (const auto* ua =
            std::get_if<hir::UnpackedArrayType>(&hir_base_ty.data)) {
      const auto& result_ty = unit_state.GetType(result_type);
      const auto count = static_cast<std::uint32_t>(
          std::get<mir::UnpackedArrayType>(result_ty.data).size);
      return UnfoldHirRangeBoundsForUnpacked(
          unit_state, proc_scope_state, sel.bounds, ua->dim, count, lower_one);
    }
    return UnfoldHirRangeBoundsToOffsetCount(
        unit_state, proc_scope_state, sel.bounds, lower_one);
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
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::MemberAccessExpr& sel,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  const auto& base_hir_expr = hir_process.exprs.at(sel.base_value.value);
  const auto& base_hir_type = unit_state.GetHirType(base_hir_expr.type);
  const auto& fields = GetAggregateFields(base_hir_type);
  if (sel.field_index >= fields.size()) {
    throw InternalError(
        "LowerHirMemberAccessExprProc: field_index out of range");
  }
  const auto& field = fields[sel.field_index];
  auto base_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      base_hir_expr);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope_state.AddExpr(*std::move(base_or));
  const auto offset_id =
      proc_scope_state.AddExpr(unit_state.MakeInt32LiteralExpr(
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

auto LowerHirConcatExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::ConcatExpr& c,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  std::vector<mir::ExprId> operand_ids;
  operand_ids.reserve(c.operands.size());
  for (const auto& id : c.operands) {
    auto lowered = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_process,
        hir_process.exprs.at(id.value));
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    operand_ids.push_back(proc_scope_state.AddExpr(*std::move(lowered)));
  }
  return mir::Expr{
      .data = mir::ConcatExpr{.operands = std::move(operand_ids)},
      .type = result_type};
}

auto LowerHirReplicationExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::ReplicationExpr& r,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  auto count_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(r.count.value));
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const mir::ExprId count_id = proc_scope_state.AddExpr(*std::move(count_or));
  auto concat_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(r.concat.value));
  if (!concat_or) return std::unexpected(std::move(concat_or.error()));
  const mir::ExprId concat_id = proc_scope_state.AddExpr(*std::move(concat_or));
  return mir::Expr{
      .data = mir::ReplicationExpr{.count = count_id, .concat = concat_id},
      .type = result_type};
}

// Lowers an HIR AssignmentPatternExpr by dispatching on the destination
// type's runtime shape: packed targets fold into MIR `ConcatExpr` (bit
// concatenation matches the packed bit plane), unpacked arrays land as
// `ArrayLiteralExpr` over distinct std::vector slots.
auto LowerHirAssignmentPatternExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::AssignmentPatternExpr& a,
    mir::TypeId result_type) -> diag::Result<mir::Expr> {
  std::vector<mir::ExprId> element_ids;
  element_ids.reserve(a.elements.size());
  for (const auto& id : a.elements) {
    auto lowered = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_process,
        hir_process.exprs.at(id.value));
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(proc_scope_state.AddExpr(*std::move(lowered)));
  }
  if (std::holds_alternative<mir::UnpackedArrayType>(
          unit_state.GetType(result_type).data)) {
    return mir::Expr{
        .data = mir::ArrayLiteralExpr{.elements = std::move(element_ids)},
        .type = result_type};
  }
  return mir::Expr{
      .data = mir::ConcatExpr{.operands = std::move(element_ids)},
      .type = result_type};
}

auto LowerHirAssignmentPatternReplicationExprProc(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process,
    const hir::AssignmentPatternReplicationExpr& a, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  std::vector<mir::ExprId> item_ids;
  item_ids.reserve(a.items.size());
  for (const auto& id : a.items) {
    auto lowered = LowerExpr(
        unit_state, scope_state, proc_state, proc_scope_state, hir_process,
        hir_process.exprs.at(id.value));
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(proc_scope_state.AddExpr(*std::move(lowered)));
  }
  if (std::holds_alternative<mir::UnpackedArrayType>(
          unit_state.GetType(result_type).data)) {
    const std::uint64_t count =
        ExtractHirLiteralUint64(hir_process.exprs.at(a.count.value));
    return BuildUnpackedReplicationFlatList(item_ids, count, result_type);
  }
  const mir::ExprId inner_concat_id = proc_scope_state.AddExpr(
      mir::Expr{
          .data = mir::ConcatExpr{.operands = std::move(item_ids)},
          .type = result_type});
  auto count_or = LowerExpr(
      unit_state, scope_state, proc_state, proc_scope_state, hir_process,
      hir_process.exprs.at(a.count.value));
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const mir::ExprId count_id = proc_scope_state.AddExpr(*std::move(count_or));
  return mir::Expr{
      .data =
          mir::ReplicationExpr{
              .count = count_id,
              .concat = inner_concat_id,
          },
      .type = result_type};
}

}  // namespace

auto LowerExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ProcessLoweringState& proc_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::ProceduralBody& hir_process, const hir::Expr& expr)
    -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = unit_state.TranslateType(expr.type);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerHirPrimaryProc(
                unit_state, scope_state, proc_state, p.data, result_type);
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            return LowerHirUnaryExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, u, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            return LowerHirBinaryExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, b, result_type);
          },
          [&](const hir::ConditionalExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConditionalExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, c, result_type);
          },
          [&](const hir::AssignExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, a, expr.span, result_type);
          },
          [&](const hir::IncDecExpr& inc) -> diag::Result<mir::Expr> {
            return LowerHirIncDecExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, inc, result_type);
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, cv, result_type);
          },
          [&](const hir::CallExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirCallExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, c, expr.span, result_type);
          },
          [&](const hir::InsideExpr& in) -> diag::Result<mir::Expr> {
            return LowerHirInsideExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, in, result_type);
          },
          [&](const hir::ElementSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirElementSelectExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, sel, result_type);
          },
          [&](const hir::RangeSelectExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, sel, result_type);
          },
          [&](const hir::MemberAccessExpr& sel) -> diag::Result<mir::Expr> {
            return LowerHirMemberAccessExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, sel, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, c, result_type);
          },
          [&](const hir::ReplicationExpr& r) -> diag::Result<mir::Expr> {
            return LowerHirReplicationExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, r, result_type);
          },
          [&](const hir::AssignmentPatternExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, a, result_type);
          },
          [&](const hir::AssignmentPatternReplicationExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternReplicationExprProc(
                unit_state, scope_state, proc_state, proc_scope_state,
                hir_process, a, result_type);
          },
      },
      expr.data);
}

namespace {

auto LowerExprImpl(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::Expr& expr,
    LoopVarLoweringMode loop_var_mode) -> diag::Result<mir::Expr>;

auto LowerHirUnaryExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::UnaryExpr& u,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto operand_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(u.operand), loop_var_mode);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      proc_scope_state.AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data = mir::UnaryExpr{.op = LowerUnaryOp(u.op), .operand = operand_id},
      .type = result_type};
}

auto LowerHirBinaryExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::BinaryExpr& b,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto lhs_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(b.lhs), loop_var_mode);
  if (!lhs_or) return std::unexpected(std::move(lhs_or.error()));
  const mir::ExprId lhs_id = proc_scope_state.AddExpr(*std::move(lhs_or));
  auto rhs_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(b.rhs), loop_var_mode);
  if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
  const mir::ExprId rhs_id = proc_scope_state.AddExpr(*std::move(rhs_or));
  return mir::Expr{
      .data =
          mir::BinaryExpr{
              .op = LowerBinaryOp(b.op), .lhs = lhs_id, .rhs = rhs_id},
      .type = result_type};
}

auto LowerHirConditionalExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::ConditionalExpr& c,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto cond_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(c.condition), loop_var_mode);
  if (!cond_or) return std::unexpected(std::move(cond_or.error()));
  const mir::ExprId cond_id = proc_scope_state.AddExpr(*std::move(cond_or));
  auto then_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(c.then_value), loop_var_mode);
  if (!then_or) return std::unexpected(std::move(then_or.error()));
  const mir::ExprId then_id = proc_scope_state.AddExpr(*std::move(then_or));
  auto else_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(c.else_value), loop_var_mode);
  if (!else_or) return std::unexpected(std::move(else_or.error()));
  const mir::ExprId else_id = proc_scope_state.AddExpr(*std::move(else_or));
  return mir::Expr{
      .data =
          mir::ConditionalExpr{
              .condition = cond_id,
              .then_value = then_id,
              .else_value = else_id},
      .type = result_type};
}

auto LowerHirElementSelectExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::ElementSelectExpr& sel,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_base = scope.GetExpr(sel.base_value);
  auto base_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope, hir_base,
      loop_var_mode);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope_state.AddExpr(*std::move(base_or));

  const auto& hir_idx = scope.GetExpr(sel.index);
  auto idx_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope, hir_idx,
      loop_var_mode);
  if (!idx_or) return std::unexpected(std::move(idx_or.error()));
  mir::ExprId idx_id = proc_scope_state.AddExpr(*std::move(idx_or));

  const auto& hir_base_ty = unit_state.GetHirType(hir_base.type);
  if (const auto* ua = std::get_if<hir::UnpackedArrayType>(&hir_base_ty.data)) {
    idx_id = WrapUnpackedIndex(
        unit_state, proc_scope_state, ua->dim, idx_id,
        unit_state.TranslateType(hir_idx.type));
  }

  return mir::Expr{
      .data = mir::ElementSelectExpr{.base_value = base_id, .index = idx_id},
      .type = result_type};
}

auto LowerHirRangeSelectExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::RangeSelectExpr& sel,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& hir_base = scope.GetExpr(sel.base_value);
  auto base_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope, hir_base,
      loop_var_mode);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope_state.AddExpr(*std::move(base_or));

  auto lower_one = [&](hir::ExprId id) -> diag::Result<mir::ExprId> {
    auto lowered = LowerExprImpl(
        unit_state, scope_state, ctor_state, proc_scope_state, scope,
        scope.GetExpr(id), loop_var_mode);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    return proc_scope_state.AddExpr(*std::move(lowered));
  };
  const auto& hir_base_ty = unit_state.GetHirType(hir_base.type);
  auto unfolded = [&]() {
    if (const auto* ua =
            std::get_if<hir::UnpackedArrayType>(&hir_base_ty.data)) {
      const auto& result_ty = unit_state.GetType(result_type);
      const auto count = static_cast<std::uint32_t>(
          std::get<mir::UnpackedArrayType>(result_ty.data).size);
      return UnfoldHirRangeBoundsForUnpacked(
          unit_state, proc_scope_state, sel.bounds, ua->dim, count, lower_one);
    }
    return UnfoldHirRangeBoundsToOffsetCount(
        unit_state, proc_scope_state, sel.bounds, lower_one);
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
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::MemberAccessExpr& sel,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  const auto& base_hir_expr = scope.GetExpr(sel.base_value);
  const auto& base_hir_type = unit_state.GetHirType(base_hir_expr.type);
  const auto& fields = GetAggregateFields(base_hir_type);
  if (sel.field_index >= fields.size()) {
    throw InternalError(
        "LowerHirMemberAccessExprStructural: field_index out of range");
  }
  const auto& field = fields[sel.field_index];
  auto base_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      base_hir_expr, loop_var_mode);
  if (!base_or) return std::unexpected(std::move(base_or.error()));
  const mir::ExprId base_id = proc_scope_state.AddExpr(*std::move(base_or));
  const auto offset_id =
      proc_scope_state.AddExpr(unit_state.MakeInt32LiteralExpr(
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

auto LowerHirConcatExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::ConcatExpr& c,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  std::vector<mir::ExprId> operand_ids;
  operand_ids.reserve(c.operands.size());
  for (const auto& id : c.operands) {
    auto lowered = LowerExprImpl(
        unit_state, scope_state, ctor_state, proc_scope_state, scope,
        scope.GetExpr(id), loop_var_mode);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    operand_ids.push_back(proc_scope_state.AddExpr(*std::move(lowered)));
  }
  return mir::Expr{
      .data = mir::ConcatExpr{.operands = std::move(operand_ids)},
      .type = result_type};
}

auto LowerHirAssignmentPatternExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::AssignmentPatternExpr& a,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  std::vector<mir::ExprId> element_ids;
  element_ids.reserve(a.elements.size());
  for (const auto& id : a.elements) {
    auto lowered = LowerExprImpl(
        unit_state, scope_state, ctor_state, proc_scope_state, scope,
        scope.GetExpr(id), loop_var_mode);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    element_ids.push_back(proc_scope_state.AddExpr(*std::move(lowered)));
  }
  if (std::holds_alternative<mir::UnpackedArrayType>(
          unit_state.GetType(result_type).data)) {
    return mir::Expr{
        .data = mir::ArrayLiteralExpr{.elements = std::move(element_ids)},
        .type = result_type};
  }
  return mir::Expr{
      .data = mir::ConcatExpr{.operands = std::move(element_ids)},
      .type = result_type};
}

auto LowerHirAssignmentPatternReplicationExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope,
    const hir::AssignmentPatternReplicationExpr& a,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  std::vector<mir::ExprId> item_ids;
  item_ids.reserve(a.items.size());
  for (const auto& id : a.items) {
    auto lowered = LowerExprImpl(
        unit_state, scope_state, ctor_state, proc_scope_state, scope,
        scope.GetExpr(id), loop_var_mode);
    if (!lowered) return std::unexpected(std::move(lowered.error()));
    item_ids.push_back(proc_scope_state.AddExpr(*std::move(lowered)));
  }
  if (std::holds_alternative<mir::UnpackedArrayType>(
          unit_state.GetType(result_type).data)) {
    const std::uint64_t count = ExtractHirLiteralUint64(scope.GetExpr(a.count));
    return BuildUnpackedReplicationFlatList(item_ids, count, result_type);
  }
  const mir::ExprId inner_concat_id = proc_scope_state.AddExpr(
      mir::Expr{
          .data = mir::ConcatExpr{.operands = std::move(item_ids)},
          .type = result_type});
  auto count_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(a.count), loop_var_mode);
  if (!count_or) return std::unexpected(std::move(count_or.error()));
  const mir::ExprId count_id = proc_scope_state.AddExpr(*std::move(count_or));
  return mir::Expr{
      .data =
          mir::ReplicationExpr{
              .count = count_id,
              .concat = inner_concat_id,
          },
      .type = result_type};
}

auto LowerHirConversionExprStructural(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::ConversionExpr& cv,
    LoopVarLoweringMode loop_var_mode, mir::TypeId result_type)
    -> diag::Result<mir::Expr> {
  auto operand_or = LowerExprImpl(
      unit_state, scope_state, ctor_state, proc_scope_state, scope,
      scope.GetExpr(cv.operand), loop_var_mode);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const mir::ExprId operand_id =
      proc_scope_state.AddExpr(*std::move(operand_or));
  return mir::Expr{
      .data =
          mir::ConversionExpr{
              .operand = operand_id, .kind = LowerHirConversionKind(cv.kind)},
      .type = result_type};
}

auto LowerExprImpl(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState* ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::Expr& expr,
    LoopVarLoweringMode loop_var_mode) -> diag::Result<mir::Expr> {
  const mir::TypeId result_type = unit_state.TranslateType(expr.type);
  return std::visit(
      Overloaded{
          [&](const hir::PrimaryExpr& p) -> diag::Result<mir::Expr> {
            return LowerHirPrimaryStructural(
                unit_state, scope_state, ctor_state, p.data, result_type,
                loop_var_mode);
          },
          [&](const hir::UnaryExpr& u) -> diag::Result<mir::Expr> {
            return LowerHirUnaryExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, u,
                loop_var_mode, result_type);
          },
          [&](const hir::BinaryExpr& b) -> diag::Result<mir::Expr> {
            return LowerHirBinaryExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, b,
                loop_var_mode, result_type);
          },
          [&](const hir::ConditionalExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConditionalExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, c,
                loop_var_mode, result_type);
          },
          [](const hir::AssignExpr&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerExprImpl (structural): HIR AssignExpr does not appear "
                "in constructor-side expressions; structural code has no "
                "general assignment");
          },
          [](const hir::IncDecExpr&) -> diag::Result<mir::Expr> {
            throw InternalError(
                "LowerExprImpl (structural): HIR IncDecExpr does not appear "
                "in constructor-side expressions; structural code has no "
                "increment / decrement");
          },
          [&](const hir::ConversionExpr& cv) -> diag::Result<mir::Expr> {
            return LowerHirConversionExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope,
                cv, loop_var_mode, result_type);
          },
          [](const hir::CallExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "calls are not allowed in constructor expressions",
                diag::UnsupportedCategory::kFeature);
          },
          [](const hir::InsideExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "inside operator is not allowed in constructor expressions",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const hir::ElementSelectExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirElementSelectExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, s,
                loop_var_mode, result_type);
          },
          [&](const hir::RangeSelectExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirRangeSelectExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, s,
                loop_var_mode, result_type);
          },
          [&](const hir::MemberAccessExpr& s) -> diag::Result<mir::Expr> {
            return LowerHirMemberAccessExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, s,
                loop_var_mode, result_type);
          },
          [&](const hir::ConcatExpr& c) -> diag::Result<mir::Expr> {
            return LowerHirConcatExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, c,
                loop_var_mode, result_type);
          },
          [](const hir::ReplicationExpr&) -> diag::Result<mir::Expr> {
            return diag::Unsupported(
                diag::DiagCode::kUnsupportedStructuralExpressionForm,
                "replication in constructor expressions is not yet supported",
                diag::UnsupportedCategory::kFeature);
          },
          [&](const hir::AssignmentPatternExpr& a) -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, a,
                loop_var_mode, result_type);
          },
          [&](const hir::AssignmentPatternReplicationExpr& a)
              -> diag::Result<mir::Expr> {
            return LowerHirAssignmentPatternReplicationExprStructural(
                unit_state, scope_state, ctor_state, proc_scope_state, scope, a,
                loop_var_mode, result_type);
          },
      },
      expr.data);
}

}  // namespace

auto LowerProceduralExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    const ConstructorLoweringState& ctor_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::Expr& expr)
    -> diag::Result<mir::Expr> {
  return LowerExprImpl(
      unit_state, scope_state, &ctor_state, proc_scope_state, scope, expr,
      LoopVarLoweringMode::kProceduralInduction);
}

auto LowerStructuralExpr(
    const UnitLoweringState& unit_state,
    const StructuralScopeLoweringState& scope_state,
    ProceduralScopeLoweringState& proc_scope_state,
    const hir::StructuralScope& scope, const hir::Expr& expr)
    -> diag::Result<mir::Expr> {
  return LowerExprImpl(
      unit_state, scope_state, nullptr, proc_scope_state, scope, expr,
      LoopVarLoweringMode::kStructuralParam);
}

// Builds a ClosureExpr that snapshots the RHS by value into the body and
// writes the snapshot to the LHS. The closure is the deferred-write vehicle
// the NBA region invokes. The returned Expr has type `void` -- the closure
// value flows only into RuntimeSubmitNbaCall. `lhs_in_outer` must be an
// addressable expression.
auto BuildNbaSubmitClosureExpr(
    const UnitLoweringState& unit_state,
    const ProceduralScopeLoweringState& outer_scope_state,
    mir::ExprId lhs_in_outer, mir::ExprId rhs_id_in_outer, mir::TypeId rhs_type)
    -> mir::Expr {
  ProceduralScopeLoweringState body;
  std::vector<mir::Capture> captures;

  const mir::ProceduralVarId rhs_binding = body.AddProceduralVar(
      mir::ProceduralVarDecl{.name = "_lyra_nba_rhs", .type = rhs_type});
  captures.emplace_back(
      mir::ByValueCapture{.value = rhs_id_in_outer, .binding = rhs_binding});
  const mir::ExprId rhs_ref_id = body.AddExpr(
      mir::Expr{
          .data =
              mir::ProceduralVarRef{
                  .hops = mir::ProceduralHops{.value = 0}, .var = rhs_binding},
          .type = rhs_type});

  std::uint32_t snapshot_counter = 0;
  const mir::ExprId body_lhs_id = CloneLhsExprForNbaBody(
      outer_scope_state, body, captures, snapshot_counter, lhs_in_outer);

  const mir::ExprId assign_id = body.AddExpr(
      mir::Expr{
          .data = mir::AssignExpr{.target = body_lhs_id, .value = rhs_ref_id},
          .type = rhs_type});
  const mir::StmtId stmt_id = body.AddStmt(
      mir::Stmt{
          .label = std::nullopt,
          .data = mir::ExprStmt{.expr = assign_id},
          .child_procedural_scopes = {}});
  body.AddRootStmt(stmt_id);

  mir::ClosureExpr closure;
  closure.captures = std::move(captures);
  closure.body = std::make_unique<mir::ProceduralScope>(body.Finish());

  return mir::Expr{
      .data = std::move(closure), .type = unit_state.Builtins().void_type};
}

}  // namespace lyra::lowering::hir_to_mir
