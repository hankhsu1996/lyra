#include "lyra/lowering/ast_to_hir/expression/query.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/SystemSubroutine.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/types/Type.h>
#include <slang/ast/types/TypePrinter.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

using slang::parsing::KnownSystemName;

// The LRM 20.6 / 20.7 query family. Each member reports a property of its
// operand's type or of one of that type's dimensions.
enum class QueryKind : std::uint8_t {
  kBits,
  kTypename,
  kDimensions,
  kUnpackedDimensions,
  kLeft,
  kRight,
  kLow,
  kHigh,
  kIncrement,
  kSize,
};

auto ClassifyQuery(KnownSystemName name) -> std::optional<QueryKind> {
  switch (name) {
    case KnownSystemName::Bits:
      return QueryKind::kBits;
    case KnownSystemName::Typename:
      return QueryKind::kTypename;
    case KnownSystemName::Dimensions:
      return QueryKind::kDimensions;
    case KnownSystemName::UnpackedDimensions:
      return QueryKind::kUnpackedDimensions;
    case KnownSystemName::Left:
      return QueryKind::kLeft;
    case KnownSystemName::Right:
      return QueryKind::kRight;
    case KnownSystemName::Low:
      return QueryKind::kLow;
    case KnownSystemName::High:
      return QueryKind::kHigh;
    case KnownSystemName::Increment:
      return QueryKind::kIncrement;
    case KnownSystemName::Size:
      return QueryKind::kSize;
    default:
      return std::nullopt;
  }
}

// The type whose own range is the dimension the query names. Dimension 1 is the
// operand itself and each further one unwraps an array element (LRM 20.7: the
// slowest varying dimension is dimension 1). Null when the operand has no such
// dimension.
auto DimensionType(const slang::ast::Type& operand, std::int32_t index)
    -> const slang::ast::Type* {
  const slang::ast::Type* type = &operand;
  for (std::int32_t i = 1; i < index; ++i) {
    if (!type->isArray()) {
      return nullptr;
    }
    type = type->getArrayElementType();
  }
  return type;
}

// A dimension with declared bounds; its six query results all follow from them
// (LRM 20.7). `$increment` is 1 when the left bound is at or above the right
// one and -1 otherwise; `$low` and `$high` follow it, and `$size` spans them.
auto FixedDimensionValue(QueryKind query, slang::ConstantRange range)
    -> std::int64_t {
  const std::int64_t left = range.left;
  const std::int64_t right = range.right;
  const std::int64_t increment = left >= right ? 1 : -1;
  const std::int64_t low = increment == 1 ? right : left;
  const std::int64_t high = increment == 1 ? left : right;
  switch (query) {
    case QueryKind::kLeft:
      return left;
    case QueryKind::kRight:
      return right;
    case QueryKind::kLow:
      return low;
    case QueryKind::kHigh:
      return high;
    case QueryKind::kIncrement:
      return increment;
    case QueryKind::kSize:
      return high - low + 1;
    default:
      throw InternalError("FixedDimensionValue: not a dimension function");
  }
}

// LRM 20.7: `$dimensions` counts every dimension of the operand's type,
// `$unpacked_dimensions` stops at the packed ones. A string or a non-scalar
// integral is itself one packed dimension; any other leaf contributes none.
auto DimensionCount(const slang::ast::Type& operand, bool unpacked_only)
    -> std::uint64_t {
  std::uint64_t count = 0;
  const slang::ast::Type* type = &operand;
  while (type->isArray()) {
    if (unpacked_only && !type->isUnpackedArray()) {
      break;
    }
    ++count;
    type = type->getArrayElementType();
  }
  if (!unpacked_only &&
      (type->isString() || (type->isIntegral() && !type->isScalar()))) {
    ++count;
  }
  return count;
}

// The dimension index the call names, defaulting to 1 when the call omits it
// (LRM 20.7). Nullopt when the index is an expression whose value only
// simulation knows -- legal, but then the query is not an elaboration-time
// constant.
auto ConstantDimensionIndex(
    const UnitLowerer& unit_lowerer, const slang::ast::CallExpression& call)
    -> std::optional<std::int64_t> {
  if (call.arguments().size() < 2) {
    return 1;
  }
  slang::ast::EvalContext eval_context(unit_lowerer.SourceScope().asSymbol());
  const slang::ConstantValue index = call.arguments()[1]->eval(eval_context);
  if (!index || !index.isInteger()) {
    return std::nullopt;
  }
  return index.integer().as<std::int64_t>();
}

// A value of the type slang gave the call, so the literal carries that type's
// state domain: LRM 20.7 types a dimension function's result as `integer`, a
// 4-state type, and LRM 20.6.1 types `$typename`'s as `string`.
auto MakeQueryConstant(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, const slang::ConstantValue& value,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = unit_lowerer.InternType(*call.type, span);
  if (!type_id) {
    return std::unexpected(std::move(type_id.error()));
  }
  return MakeConstantValueExpr(
      unit_lowerer.Unit(), frame, call.type->coerceValue(value), *type_id,
      span);
}

auto MakeQueryInt(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, std::int64_t value,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  return MakeQueryConstant(
      unit_lowerer, frame, call,
      slang::ConstantValue{
          slang::SVInt(32, static_cast<std::uint64_t>(value), true)},
      span);
}

// The number of elements the operand currently holds, as a value of the query's
// result type: LRM 6.16.1 `len()` for a string, the LRM 7.5.1 / 7.9.2 / 7.10.2
// `size()` for the container families. Both yield an SV `int`, which the
// conversion lands in the `integer` the query reports.
template <ExprLowerer Lowerer>
auto BuildElementCountExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto operand_or = lowerer.LowerExpr(*call.arguments()[0], frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  const bool is_string =
      unit_lowerer.Unit().types.Get(operand_or->type).Kind() ==
      hir::TypeKind::kString;
  const hir::ExprId operand_id = frame.Exprs().Add(*std::move(operand_or));

  auto result_type = unit_lowerer.InternType(*call.type, span);
  if (!result_type) {
    return std::unexpected(std::move(result_type.error()));
  }
  const hir::ExprId count_id = frame.Exprs().Add(
      hir::Expr{
          .type = unit_lowerer.Unit().builtins.int_type,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::BuiltinMethodRef{
                          .method = is_string ? support::BuiltinFn::kLen
                                              : support::BuiltinFn::kSize},
                  .arguments = {operand_id}},
          .span = span});
  return hir::Expr{
      .type = *result_type,
      .data =
          hir::ConversionExpr{
              .operand = count_id, .kind = hir::ConversionKind::kImplicit},
      .span = span};
}

auto MakeQueryBinary(
    WalkFrame frame, hir::BinaryOp op, hir::Expr lhs, hir::Expr rhs,
    diag::SourceSpan span) -> hir::Expr {
  const hir::TypeId type = lhs.type;
  return hir::Expr{
      .type = type,
      .data =
          hir::BinaryExpr{
              .op = op,
              .lhs = frame.Exprs().Add(std::move(lhs)),
              .rhs = frame.Exprs().Add(std::move(rhs))},
      .span = span};
}

// LRM 20.7 over a dimension whose size only the running simulation knows. The
// dimension's direction is still fixed by its kind -- a queue or dynamic array
// dimension ascends from 0 -- so `$left`, `$low`, and `$increment` remain
// constants; only the dimension's extent has to be read from the value.
template <ExprLowerer Lowerer>
auto LowerOrderedDynamicDimensionQuery(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    QueryKind query, diag::SourceSpan span) -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  switch (query) {
    case QueryKind::kLeft:
    case QueryKind::kLow:
      return MakeQueryInt(unit_lowerer, frame, call, 0, span);
    case QueryKind::kIncrement:
      return MakeQueryInt(unit_lowerer, frame, call, -1, span);
    case QueryKind::kSize:
      return BuildElementCountExpr(lowerer, frame, call, span);
    case QueryKind::kRight:
    case QueryKind::kHigh: {
      // The last position of an ascending dimension, which is -1 when the
      // dimension is currently empty (LRM 20.7).
      auto count_or = BuildElementCountExpr(lowerer, frame, call, span);
      if (!count_or) {
        return std::unexpected(std::move(count_or.error()));
      }
      auto one_or = MakeQueryInt(unit_lowerer, frame, call, 1, span);
      if (!one_or) {
        return std::unexpected(std::move(one_or.error()));
      }
      return MakeQueryBinary(
          frame, hir::BinaryOp::kSub, *std::move(count_or), *std::move(one_or),
          span);
    }
    default:
      throw InternalError(
          "LowerOrderedDynamicDimensionQuery: not a dimension function");
  }
}

// The largest value the index type can hold, which is the highest index an
// associative dimension can ever be given (LRM 20.7 `$right`). All ones, less
// the sign bit when the index type is signed.
auto HighestIndexValue(const slang::ast::Type& index_type) -> slang::SVInt {
  slang::SVInt value(index_type.getBitWidth(), 0, index_type.isSigned());
  value.setAllOnes();
  if (index_type.isSigned()) {
    value = value.lshr(1);
  }
  return value;
}

// LRM 20.7 over an associative dimension. The index space is a property of the
// index type: it ascends from 0 up to that type's highest value, which leaves
// `$left`, `$right`, and `$increment` constants. What the array currently holds
// decides only how many indices are allocated and which of them are the
// smallest and the largest. Every result here is typed as the index type, not
// `integer`.
template <ExprLowerer Lowerer>
auto LowerAssociativeDimensionQuery(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    const slang::ast::Type& dimension, QueryKind query, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  switch (query) {
    case QueryKind::kLeft:
      return MakeQueryInt(unit_lowerer, frame, call, 0, span);
    case QueryKind::kIncrement:
      return MakeQueryInt(unit_lowerer, frame, call, -1, span);
    case QueryKind::kRight:
      return MakeQueryConstant(
          unit_lowerer, frame, call,
          slang::ConstantValue{
              HighestIndexValue(*dimension.getAssociativeIndexType())},
          span);
    case QueryKind::kSize:
      return BuildElementCountExpr(lowerer, frame, call, span);
    case QueryKind::kLow:
    case QueryKind::kHigh: {
      auto operand_or = lowerer.LowerExpr(*call.arguments()[0], frame);
      if (!operand_or) {
        return std::unexpected(std::move(operand_or.error()));
      }
      auto result_type = unit_lowerer.InternType(*call.type, span);
      if (!result_type) {
        return std::unexpected(std::move(result_type.error()));
      }
      return hir::Expr{
          .type = *result_type,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::BuiltinMethodRef{
                          .method = query == QueryKind::kLow
                                        ? support::BuiltinFn::kAssocMinIndex
                                        : support::BuiltinFn::kAssocMaxIndex},
                  .arguments = {frame.Exprs().Add(*std::move(operand_or))}},
          .span = span};
    }
    default:
      throw InternalError(
          "LowerAssociativeDimensionQuery: not a dimension function");
  }
}

// LRM 20.6.2 over a dynamically sized value: the bit count of what it currently
// holds, read at run time. The value reports it by summing each element's own
// bit count, so an element that is itself dynamically sized contributes its
// current width -- the same query one layer down -- and the lowering never
// inspects the element shape. The read lands in the `integer` the query
// reports.
template <ExprLowerer Lowerer>
auto LowerDynamicBitsQuery(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  auto operand_or = lowerer.LowerExpr(*call.arguments()[0], frame);
  if (!operand_or) {
    return std::unexpected(std::move(operand_or.error()));
  }
  auto result_type = unit_lowerer.InternType(*call.type, span);
  if (!result_type) {
    return std::unexpected(std::move(result_type.error()));
  }
  const hir::ExprId width_id = frame.Exprs().Add(
      hir::Expr{
          .type = unit_lowerer.Unit().builtins.int_type,
          .data =
              hir::CallExpr{
                  .callee =
                      hir::BuiltinMethodRef{
                          .method = support::BuiltinFn::kBitstreamWidth},
                  .arguments = {frame.Exprs().Add(*std::move(operand_or))}},
          .span = span});
  return hir::Expr{
      .type = *result_type,
      .data =
          hir::ConversionExpr{
              .operand = width_id, .kind = hir::ConversionKind::kImplicit},
      .span = span};
}

// LRM 20.6.2: the bit count of the operand's type, taken without evaluating the
// operand. A dynamically sized operand instead reports the bit count of what it
// currently holds.
template <ExprLowerer Lowerer>
auto LowerBitsQuery(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  const slang::ast::Type& operand_type = *call.arguments()[0]->type;
  if (!operand_type.isFixedSize()) {
    return LowerDynamicBitsQuery(lowerer, frame, call, span);
  }
  return MakeQueryConstant(
      lowerer.Owner(), frame, call,
      slang::ConstantValue{
          slang::SVInt(32, operand_type.getBitstreamWidth(), true)},
      span);
}

// LRM 20.6.1: the operand's resolved type name. The string is canonical, and
// the rules that make it so -- a typedef resolved back to what it names, the
// default signing dropped, a user-defined name qualified by its declaring scope
// -- are the frontend type printer's.
auto LowerTypenameQuery(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  slang::ast::TypePrinter printer;
  printer.append(*call.arguments()[0]->type);
  return MakeQueryConstant(
      unit_lowerer, frame, call, slang::ConstantValue{printer.toString()},
      span);
}

auto LowerDimensionCountQuery(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, bool unpacked_only,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  return MakeQueryInt(
      unit_lowerer, frame, call,
      static_cast<std::int64_t>(
          DimensionCount(*call.arguments()[0]->type, unpacked_only)),
      span);
}

// The query's result for the dimension whose own range is `dimension` (LRM
// 20.7). A fixed dimension folds to a constant; the operand's own top
// dimension, when it is dynamic or associative, reads the value's current
// state. The caller guarantees `dimension` is either fixed-size or the
// operand's top dimension -- a variable-sized dimension below the top has no
// single extent (LRM 20.7.1) and is rejected before reaching here.
template <ExprLowerer Lowerer>
auto LowerDimensionResult(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    const slang::ast::Type& dimension, QueryKind query, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  if (dimension.hasFixedRange() && !dimension.isScalar()) {
    return MakeQueryInt(
        unit_lowerer, frame, call,
        FixedDimensionValue(query, dimension.getFixedRange()), span);
  }
  if (dimension.isAssociativeArray()) {
    return LowerAssociativeDimensionQuery(
        lowerer, frame, call, dimension, query, span);
  }
  return LowerOrderedDynamicDimensionQuery(lowerer, frame, call, query, span);
}

// LRM 20.7 with a dimension the running simulation names. Which dimensions the
// operand has is a property of its type, so each dimension's result is built at
// lowering -- a constant for a fixed dimension, a current-state read for a
// dynamic top dimension -- and the query is a select, by the index, out of the
// array of them. The array is declared over `[1 : dimension count]`, so the
// index selects directly; an index the type has no dimension for falls outside
// that range and reads the element default, which for the `integer` a dimension
// function reports is `'x` -- what LRM 20.7 requires of an out-of-range
// dimension.
template <ExprLowerer Lowerer>
auto LowerComposedDimensionQuery(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    QueryKind query, diag::SourceSpan span) -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const slang::ast::Type& operand_type = *call.arguments()[0]->type;
  const auto dimension_count =
      static_cast<std::int64_t>(DimensionCount(operand_type, false));

  std::vector<hir::ExprId> rows;
  rows.reserve(static_cast<std::size_t>(dimension_count));
  for (std::int64_t i = 1; i <= dimension_count; ++i) {
    const slang::ast::Type* dimension =
        DimensionType(operand_type, static_cast<std::int32_t>(i));
    if (dimension == nullptr) {
      throw InternalError(
          "LowerComposedDimensionQuery: dimension index out of range");
    }
    // LRM 20.7.1: a variable-sized dimension below the top has no single extent
    // -- each outer element carries its own -- so naming it is an error. A
    // run-time index could land there, and the value-build table cannot carry a
    // per-arm error, so the operand shape is rejected rather than half-served.
    if (i != 1 && !dimension->hasFixedRange()) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedExpressionForm,
          "an array query with a run-time dimension index is not yet supported "
          "over an array with a variable-sized dimension below the top");
    }
    auto row_or =
        LowerDimensionResult(lowerer, frame, call, *dimension, query, span);
    if (!row_or) {
      return std::unexpected(std::move(row_or.error()));
    }
    rows.push_back(frame.Exprs().Add(*std::move(row_or)));
  }

  auto element_type = unit_lowerer.InternType(*call.type, span);
  if (!element_type) {
    return std::unexpected(std::move(element_type.error()));
  }
  const hir::TypeId table_type = unit_lowerer.AddComposedType(
      hir::UnpackedArrayType{
          .element_type = *element_type,
          .dim = hir::UnpackedRange{.left = 1, .right = dimension_count}});
  const hir::ExprId table_id = frame.Exprs().Add(
      hir::Expr{
          .type = table_type,
          .data = hir::AssignmentPatternExpr{.elements = std::move(rows)},
          .span = span});
  auto index_or = lowerer.LowerExpr(*call.arguments()[1], frame);
  if (!index_or) {
    return std::unexpected(std::move(index_or.error()));
  }
  return hir::Expr{
      .type = *element_type,
      .data =
          hir::ElementSelectExpr{
              .base_value = table_id,
              .index = frame.Exprs().Add(*std::move(index_or))},
      .span = span};
}

template <ExprLowerer Lowerer>
auto LowerDimensionQuery(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    QueryKind query, diag::SourceSpan span) -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  const std::optional<std::int64_t> index =
      ConstantDimensionIndex(unit_lowerer, call);
  if (!index.has_value()) {
    return LowerComposedDimensionQuery(lowerer, frame, call, query, span);
  }

  // A constant index names a dimension the frontend has already checked the
  // operand has, so failing to reach it means this unwrapping and the
  // frontend's disagree.
  const slang::ast::Type* dimension = DimensionType(
      *call.arguments()[0]->type, static_cast<std::int32_t>(*index));
  if (dimension == nullptr) {
    throw InternalError("LowerDimensionQuery: dimension index out of range");
  }
  return LowerDimensionResult(lowerer, frame, call, *dimension, query, span);
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerQueryExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    diag::SourceSpan span) -> diag::Result<std::optional<hir::Expr>> {
  if (!call.isSystemCall()) {
    return std::optional<hir::Expr>{};
  }
  const auto& info =
      std::get<slang::ast::CallExpression::SystemCallInfo>(call.subroutine);
  if (info.subroutine == nullptr) {
    return std::optional<hir::Expr>{};
  }
  const std::optional<QueryKind> query =
      ClassifyQuery(info.subroutine->knownNameId);
  if (!query.has_value()) {
    return std::optional<hir::Expr>{};
  }

  UnitLowerer& unit_lowerer = lowerer.Owner();
  diag::Result<hir::Expr> result = [&] {
    switch (*query) {
      case QueryKind::kBits:
        return LowerBitsQuery(lowerer, frame, call, span);
      case QueryKind::kTypename:
        return LowerTypenameQuery(unit_lowerer, frame, call, span);
      case QueryKind::kDimensions:
        return LowerDimensionCountQuery(unit_lowerer, frame, call, false, span);
      case QueryKind::kUnpackedDimensions:
        return LowerDimensionCountQuery(unit_lowerer, frame, call, true, span);
      default:
        return LowerDimensionQuery(lowerer, frame, call, *query, span);
    }
  }();
  if (!result) {
    return std::unexpected(std::move(result.error()));
  }
  return std::optional<hir::Expr>{*std::move(result)};
}

template auto LowerQueryExpr(
    ProcessLowerer& lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::Expr>>;
template auto LowerQueryExpr(
    StructuralScopeLowerer& lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, diag::SourceSpan span)
    -> diag::Result<std::optional<hir::Expr>>;

}  // namespace lyra::lowering::ast_to_hir
