#include "lyra/lowering/ast_to_hir/expression/query.hpp"

#include <cstddef>
#include <cstdint>
#include <expected>
#include <optional>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/SystemSubroutine.h>
#include <slang/ast/expressions/CallExpression.h>
#include <slang/ast/types/Type.h>
#include <slang/ast/types/TypePrinter.h>
#include <slang/numeric/ConstantValue.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/hir/binary_op.hpp"
#include "lyra/hir/conversion.hpp"
#include "lyra/hir/subroutine_ref.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/expression/query_reads_its_operand.hpp"
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

// A value of the type slang gave the call, so the literal carries that type's
// state domain: LRM 20.7 types a dimension function's result as `integer`, a
// 4-state type, and LRM 20.6.1 types `$typename`'s as `string`.
auto MakeQueryConstant(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, const slang::ConstantValue& value,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  auto type_id = unit_lowerer.InternExpressionType(call, span);
  if (!type_id) {
    return std::unexpected(std::move(type_id.error()));
  }
  return MakeConstantValueExpr(
      unit_lowerer, frame, call.type->coerceValue(value), *type_id, span);
}

auto QueryInt(std::int64_t value) -> slang::SVInt {
  return {32, static_cast<std::uint64_t>(value), true};
}

auto MakeQueryInt(
    UnitLowerer& unit_lowerer, WalkFrame frame,
    const slang::ast::CallExpression& call, std::int64_t value,
    diag::SourceSpan span) -> diag::Result<hir::Expr> {
  return MakeQueryConstant(
      unit_lowerer, frame, call, slang::ConstantValue{QueryInt(value)}, span);
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
  const hir::TypeId operand_type = operand_or->type;
  const bool is_string =
      unit_lowerer.Unit().types.Get(operand_type).Is<hir::StringType>();
  const hir::ExprId operand_id = frame.Exprs().Add(*std::move(operand_or));

  auto result_type = unit_lowerer.InternExpressionType(call, span);
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
                                              : support::BuiltinFn::kSize,
                          .receiver = operand_id},
                  .arguments = {}},
          .span = span});
  return hir::Expr{
      .type = *result_type,
      .data =
          hir::ConversionExpr{
              .kind = hir::ConversionKind::kImplicit, .operand = count_id},
      .span = span};
}

auto BuildQueryBinary(
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

// The query's answer about `dimension` where the dimension's type fixes it
// (LRM 20.7), and none where only the value can say. A fixed dimension fixes
// every answer. A queue or dynamic array dimension ascends from 0, so its
// left and low ends and its direction are fixed and its extent is not; an
// associative dimension's index space ascends from 0 to the index type's
// highest value, so only which indices are allocated is left to the value,
// and its answers are of the index type rather than `integer`.
auto AnswerTheTypeFixes(QueryKind query, const slang::ast::Type& dimension)
    -> std::optional<slang::SVInt> {
  if (dimension.hasFixedRange() && !dimension.isScalar()) {
    return QueryInt(FixedDimensionValue(query, dimension.getFixedRange()));
  }
  switch (query) {
    case QueryKind::kLeft:
      return QueryInt(0);
    case QueryKind::kIncrement:
      return QueryInt(-1);
    case QueryKind::kLow:
      return dimension.isAssociativeArray()
                 ? std::nullopt
                 : std::optional<slang::SVInt>{QueryInt(0)};
    case QueryKind::kRight:
      return dimension.isAssociativeArray()
                 ? std::optional<slang::SVInt>{HighestIndexValue(
                       *dimension.getAssociativeIndexType())}
                 : std::nullopt;
    case QueryKind::kHigh:
    case QueryKind::kSize:
      return std::nullopt;
    case QueryKind::kBits:
    case QueryKind::kTypename:
    case QueryKind::kDimensions:
    case QueryKind::kUnpackedDimensions:
      break;
  }
  throw InternalError("AnswerTheTypeFixes: not a dimension function");
}

// Whether a query's answer is read from its operand's value rather than fixed
// by the operand's type: the bit count of a dynamically sized value (LRM
// 20.6.2), and an answer about the operand's own dimension that the dimension's
// type does not fix (LRM 20.7). Whichever dimension a run-time index names,
// only the operand's own can be one whose answers the value holds.
auto OperandIsRead(QueryKind query, const slang::ast::Type& operand) -> bool {
  switch (query) {
    case QueryKind::kBits:
      return !operand.isFixedSize();
    case QueryKind::kTypename:
    case QueryKind::kDimensions:
    case QueryKind::kUnpackedDimensions:
      return false;
    case QueryKind::kLeft:
    case QueryKind::kRight:
    case QueryKind::kLow:
    case QueryKind::kHigh:
    case QueryKind::kIncrement:
    case QueryKind::kSize:
      return !AnswerTheTypeFixes(query, operand).has_value();
  }
  throw InternalError("OperandIsRead: unknown QueryKind");
}

// The query a call is, where it is one.
auto QueryOf(const slang::ast::CallExpression& call)
    -> std::optional<QueryKind> {
  if (!call.isSystemCall()) {
    return std::nullopt;
  }
  const auto& info =
      std::get<slang::ast::CallExpression::SystemCallInfo>(call.subroutine);
  if (info.subroutine == nullptr) {
    return std::nullopt;
  }
  return ClassifyQuery(info.subroutine->knownNameId);
}

// LRM 20.7 over a queue or dynamic array dimension, for an answer only its
// current extent gives.
template <ExprLowerer Lowerer>
auto LowerOrderedDynamicDimensionQuery(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    QueryKind query, diag::SourceSpan span) -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  switch (query) {
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
      return BuildQueryBinary(
          frame, hir::BinaryOp::kSub, *std::move(count_or), *std::move(one_or),
          span);
    }
    default:
      throw InternalError(
          "LowerOrderedDynamicDimensionQuery: not a dimension function");
  }
}

// LRM 20.7 over an associative dimension, for an answer only what the array
// currently holds gives: how many indices are allocated, and which of them are
// the smallest and the largest.
template <ExprLowerer Lowerer>
auto LowerAssociativeDimensionQuery(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    QueryKind query, diag::SourceSpan span) -> diag::Result<hir::Expr> {
  UnitLowerer& unit_lowerer = lowerer.Owner();
  switch (query) {
    case QueryKind::kSize:
      return BuildElementCountExpr(lowerer, frame, call, span);
    case QueryKind::kLow:
    case QueryKind::kHigh: {
      auto operand_or = lowerer.LowerExpr(*call.arguments()[0], frame);
      if (!operand_or) {
        return std::unexpected(std::move(operand_or.error()));
      }
      auto result_type = unit_lowerer.InternExpressionType(call, span);
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
                                        : support::BuiltinFn::kAssocMaxIndex,
                          .receiver =
                              frame.Exprs().Add(*std::move(operand_or))},
                  .arguments = {}},
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
  auto result_type = unit_lowerer.InternExpressionType(call, span);
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
                          .method = support::BuiltinFn::kBitstreamWidth,
                          .receiver =
                              frame.Exprs().Add(*std::move(operand_or))},
                  .arguments = {}},
          .span = span});
  return hir::Expr{
      .type = *result_type,
      .data =
          hir::ConversionExpr{
              .kind = hir::ConversionKind::kImplicit, .operand = width_id},
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
  if (OperandIsRead(QueryKind::kBits, operand_type)) {
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
// 20.7): a constant where the dimension's type fixes it, and a read of the
// value's current state otherwise. The caller guarantees `dimension` is either
// fixed-size or the operand's top dimension -- a variable-sized dimension below
// the top has no single extent (LRM 20.7.1) and is rejected before reaching
// here.
template <ExprLowerer Lowerer>
auto LowerDimensionResult(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    const slang::ast::Type& dimension, QueryKind query, diag::SourceSpan span)
    -> diag::Result<hir::Expr> {
  if (std::optional<slang::SVInt> fixed = AnswerTheTypeFixes(query, dimension);
      fixed.has_value()) {
    return MakeQueryConstant(
        lowerer.Owner(), frame, call, slang::ConstantValue{*std::move(fixed)},
        span);
  }
  if (dimension.isAssociativeArray()) {
    return LowerAssociativeDimensionQuery(lowerer, frame, call, query, span);
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
    // -- each outer element carries its own -- so the query has no answer for
    // it and reports the `'x` it reports for any dimension it cannot answer.
    // Naming such a dimension is an error, and every index that names one and
    // can be read before the program runs is rejected where it is written, so
    // what reaches this entry is an index only the run reveals, in a program
    // the standard has already called erroneous.
    auto row_or =
        i != 1 && !dimension->hasFixedRange()
            ? MakeQueryConstant(
                  unit_lowerer, frame, call,
                  slang::ConstantValue{slang::SVInt::createFillX(
                      call.type->getBitWidth(), call.type->isSigned())},
                  span)
            : LowerDimensionResult(
                  lowerer, frame, call, *dimension, query, span);
    if (!row_or) {
      return std::unexpected(std::move(row_or.error()));
    }
    rows.push_back(frame.Exprs().Add(*std::move(row_or)));
  }

  auto element_type = unit_lowerer.InternExpressionType(call, span);
  if (!element_type) {
    return std::unexpected(std::move(element_type.error()));
  }
  const hir::TypeId table_type = unit_lowerer.AddComposedType(
      hir::Type{hir::UnpackedArrayType{
          .element_type = *element_type,
          .dim = hir::UnpackedRange{.left = 1, .right = dimension_count}}});
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

// The dimension the query asks about, where the source wrote none: LRM 20.7
// defaults it to 1, so there is no expression and nothing to select among.
template <ExprLowerer Lowerer>
auto LowerDimensionQuery(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    QueryKind query, diag::SourceSpan span) -> diag::Result<hir::Expr> {
  if (call.arguments().size() < 2) {
    const slang::ast::Type* dimension =
        DimensionType(*call.arguments()[0]->type, 1);
    if (dimension == nullptr) {
      throw InternalError("LowerDimensionQuery: operand has no dimension");
    }
    return LowerDimensionResult(lowerer, frame, call, *dimension, query, span);
  }
  return LowerComposedDimensionQuery(lowerer, frame, call, query, span);
}

}  // namespace

template <ExprLowerer Lowerer>
auto LowerQueryExpr(
    Lowerer& lowerer, WalkFrame frame, const slang::ast::CallExpression& call,
    diag::SourceSpan span) -> diag::Result<std::optional<hir::Expr>> {
  const std::optional<QueryKind> query = QueryOf(call);
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
      // The rest ask about one dimension of the operand (LRM 20.7), and the
      // kind names which of its bounds or extents the answer is.
      case QueryKind::kLeft:
      case QueryKind::kRight:
      case QueryKind::kLow:
      case QueryKind::kHigh:
      case QueryKind::kIncrement:
      case QueryKind::kSize:
        return LowerDimensionQuery(lowerer, frame, call, *query, span);
    }
    throw InternalError("LowerQueryCall: unknown QueryKind");
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

auto QueryReadsItsOperand(const slang::ast::CallExpression& call) -> bool {
  const std::optional<QueryKind> query = QueryOf(call);
  return query.has_value() && OperandIsRead(*query, *call.arguments()[0]->type);
}

}  // namespace lyra::lowering::ast_to_hir
