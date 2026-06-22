#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"

#include <slang/ast/expressions/ConversionExpression.h>
#include <slang/ast/expressions/LiteralExpressions.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/syntax/AllSyntax.h>

#include "lyra/base/internal_error.hpp"

namespace lyra::lowering::ast_to_hir {

auto LowerSlangLiteralBase(const slang::syntax::SyntaxNode* syntax)
    -> hir::IntegerLiteralBase {
  if (syntax != nullptr &&
      syntax->kind == slang::syntax::SyntaxKind::IntegerVectorExpression) {
    const auto& iv = syntax->as<slang::syntax::IntegerVectorExpressionSyntax>();
    switch (iv.base.numericFlags().base()) {
      case slang::LiteralBase::Binary:
        return hir::IntegerLiteralBase::kBinary;
      case slang::LiteralBase::Octal:
        return hir::IntegerLiteralBase::kOctal;
      case slang::LiteralBase::Decimal:
        return hir::IntegerLiteralBase::kDecimal;
      case slang::LiteralBase::Hex:
        return hir::IntegerLiteralBase::kHexadecimal;
    }
  }
  return hir::IntegerLiteralBase::kDecimal;
}

auto LowerConversionKind(slang::ast::ConversionKind k) -> hir::ConversionKind {
  switch (k) {
    case slang::ast::ConversionKind::Implicit:
      return hir::ConversionKind::kImplicit;
    case slang::ast::ConversionKind::Propagated:
      return hir::ConversionKind::kPropagated;
    case slang::ast::ConversionKind::StreamingConcat:
      return hir::ConversionKind::kStreamingConcat;
    case slang::ast::ConversionKind::Explicit:
      return hir::ConversionKind::kExplicit;
    case slang::ast::ConversionKind::BitstreamCast:
      return hir::ConversionKind::kBitstreamCast;
  }
  throw InternalError("LowerConversionKind: unknown slang ConversionKind");
}

auto LowerBinaryOp(slang::ast::BinaryOperator op) -> hir::BinaryOp {
  switch (op) {
    case slang::ast::BinaryOperator::Add:
      return hir::BinaryOp::kAdd;
    case slang::ast::BinaryOperator::Subtract:
      return hir::BinaryOp::kSub;
    case slang::ast::BinaryOperator::Multiply:
      return hir::BinaryOp::kMul;
    case slang::ast::BinaryOperator::Divide:
      return hir::BinaryOp::kDiv;
    case slang::ast::BinaryOperator::Mod:
      return hir::BinaryOp::kMod;
    case slang::ast::BinaryOperator::Power:
      return hir::BinaryOp::kPower;
    case slang::ast::BinaryOperator::BinaryAnd:
      return hir::BinaryOp::kBitwiseAnd;
    case slang::ast::BinaryOperator::BinaryOr:
      return hir::BinaryOp::kBitwiseOr;
    case slang::ast::BinaryOperator::BinaryXor:
      return hir::BinaryOp::kBitwiseXor;
    case slang::ast::BinaryOperator::BinaryXnor:
      return hir::BinaryOp::kBitwiseXnor;
    case slang::ast::BinaryOperator::Equality:
      return hir::BinaryOp::kEquality;
    case slang::ast::BinaryOperator::Inequality:
      return hir::BinaryOp::kInequality;
    case slang::ast::BinaryOperator::CaseEquality:
      return hir::BinaryOp::kCaseEquality;
    case slang::ast::BinaryOperator::CaseInequality:
      return hir::BinaryOp::kCaseInequality;
    case slang::ast::BinaryOperator::WildcardEquality:
      return hir::BinaryOp::kWildcardEquality;
    case slang::ast::BinaryOperator::WildcardInequality:
      return hir::BinaryOp::kWildcardInequality;
    case slang::ast::BinaryOperator::GreaterThanEqual:
      return hir::BinaryOp::kGreaterEqual;
    case slang::ast::BinaryOperator::GreaterThan:
      return hir::BinaryOp::kGreaterThan;
    case slang::ast::BinaryOperator::LessThanEqual:
      return hir::BinaryOp::kLessEqual;
    case slang::ast::BinaryOperator::LessThan:
      return hir::BinaryOp::kLessThan;
    case slang::ast::BinaryOperator::LogicalAnd:
      return hir::BinaryOp::kLogicalAnd;
    case slang::ast::BinaryOperator::LogicalOr:
      return hir::BinaryOp::kLogicalOr;
    case slang::ast::BinaryOperator::LogicalImplication:
      return hir::BinaryOp::kLogicalImplication;
    case slang::ast::BinaryOperator::LogicalEquivalence:
      return hir::BinaryOp::kLogicalEquivalence;
    case slang::ast::BinaryOperator::LogicalShiftLeft:
      return hir::BinaryOp::kLogicalShiftLeft;
    case slang::ast::BinaryOperator::LogicalShiftRight:
      return hir::BinaryOp::kLogicalShiftRight;
    case slang::ast::BinaryOperator::ArithmeticShiftLeft:
      return hir::BinaryOp::kArithmeticShiftLeft;
    case slang::ast::BinaryOperator::ArithmeticShiftRight:
      return hir::BinaryOp::kArithmeticShiftRight;
  }
  throw InternalError("LowerBinaryOp: unknown slang BinaryOperator");
}

auto LowerUnaryOp(slang::ast::UnaryOperator op) -> hir::UnaryOp {
  switch (op) {
    case slang::ast::UnaryOperator::Plus:
      return hir::UnaryOp::kPlus;
    case slang::ast::UnaryOperator::Minus:
      return hir::UnaryOp::kMinus;
    case slang::ast::UnaryOperator::BitwiseNot:
      return hir::UnaryOp::kBitwiseNot;
    case slang::ast::UnaryOperator::LogicalNot:
      return hir::UnaryOp::kLogicalNot;
    case slang::ast::UnaryOperator::BitwiseAnd:
      return hir::UnaryOp::kReductionAnd;
    case slang::ast::UnaryOperator::BitwiseOr:
      return hir::UnaryOp::kReductionOr;
    case slang::ast::UnaryOperator::BitwiseXor:
      return hir::UnaryOp::kReductionXor;
    case slang::ast::UnaryOperator::BitwiseNand:
      return hir::UnaryOp::kReductionNand;
    case slang::ast::UnaryOperator::BitwiseNor:
      return hir::UnaryOp::kReductionNor;
    case slang::ast::UnaryOperator::BitwiseXnor:
      return hir::UnaryOp::kReductionXnor;
    case slang::ast::UnaryOperator::Preincrement:
    case slang::ast::UnaryOperator::Predecrement:
    case slang::ast::UnaryOperator::Postincrement:
    case slang::ast::UnaryOperator::Postdecrement:
      throw InternalError(
          "LowerUnaryOp: increment / decrement is handled via "
          "LowerSlangIncDecOp into hir::IncDecExpr, not hir::UnaryExpr");
  }
  throw InternalError("LowerUnaryOp: unknown slang UnaryOperator");
}

auto LowerSlangIncDecOp(slang::ast::UnaryOperator op) -> hir::IncDecOp {
  switch (op) {
    case slang::ast::UnaryOperator::Preincrement:
      return hir::IncDecOp::kPreInc;
    case slang::ast::UnaryOperator::Postincrement:
      return hir::IncDecOp::kPostInc;
    case slang::ast::UnaryOperator::Predecrement:
      return hir::IncDecOp::kPreDec;
    case slang::ast::UnaryOperator::Postdecrement:
      return hir::IncDecOp::kPostDec;
    default:
      throw InternalError(
          "LowerSlangIncDecOp: not an increment / decrement operator");
  }
}

auto LowerTimeUnit(slang::TimeUnit u) -> hir::TimeScale {
  switch (u) {
    case slang::TimeUnit::Femtoseconds:
      return hir::TimeScale::kFs;
    case slang::TimeUnit::Picoseconds:
      return hir::TimeScale::kPs;
    case slang::TimeUnit::Nanoseconds:
      return hir::TimeScale::kNs;
    case slang::TimeUnit::Microseconds:
      return hir::TimeScale::kUs;
    case slang::TimeUnit::Milliseconds:
      return hir::TimeScale::kMs;
    case slang::TimeUnit::Seconds:
      return hir::TimeScale::kS;
  }
  throw InternalError("LowerTimeUnit: unknown slang TimeUnit");
}

auto FromSlangSubroutineKind(slang::ast::SubroutineKind k)
    -> support::SystemSubroutineKind {
  switch (k) {
    case slang::ast::SubroutineKind::Function:
      return support::SystemSubroutineKind::kFunction;
    case slang::ast::SubroutineKind::Task:
      return support::SystemSubroutineKind::kTask;
  }
  throw InternalError("FromSlangSubroutineKind: unknown SubroutineKind");
}

auto LowerEnumMethodName(std::string_view name)
    -> std::optional<support::BuiltinFn> {
  if (name == "first") return support::BuiltinFn::kEnumFirst;
  if (name == "last") return support::BuiltinFn::kEnumLast;
  if (name == "num") return support::BuiltinFn::kEnumNum;
  if (name == "next") return support::BuiltinFn::kEnumNext;
  if (name == "prev") return support::BuiltinFn::kEnumPrev;
  if (name == "name") return support::BuiltinFn::kEnumName;
  return std::nullopt;
}

auto LowerStringMethodName(std::string_view name)
    -> std::optional<support::BuiltinFn> {
  if (name == "len") return support::BuiltinFn::kLen;
  if (name == "getc") return support::BuiltinFn::kGetc;
  if (name == "putc") return support::BuiltinFn::kPutc;
  if (name == "toupper") return support::BuiltinFn::kToupper;
  if (name == "tolower") return support::BuiltinFn::kTolower;
  if (name == "compare") return support::BuiltinFn::kCompare;
  if (name == "icompare") return support::BuiltinFn::kIcompare;
  if (name == "substr") return support::BuiltinFn::kSubstr;
  if (name == "atoi") return support::BuiltinFn::kAtoi;
  if (name == "atohex") return support::BuiltinFn::kAtohex;
  if (name == "atooct") return support::BuiltinFn::kAtooct;
  if (name == "atobin") return support::BuiltinFn::kAtobin;
  if (name == "atoreal") return support::BuiltinFn::kAtoreal;
  if (name == "itoa") return support::BuiltinFn::kItoa;
  if (name == "hextoa") return support::BuiltinFn::kHextoa;
  if (name == "octtoa") return support::BuiltinFn::kOcttoa;
  if (name == "bintoa") return support::BuiltinFn::kBintoa;
  if (name == "realtoa") return support::BuiltinFn::kRealtoa;
  return std::nullopt;
}

auto LowerArrayMethodName(std::string_view name)
    -> std::optional<support::BuiltinFn> {
  if (name == "size") return support::BuiltinFn::kSize;
  if (name == "delete") return support::BuiltinFn::kDelete;
  if (name == "reverse") return support::BuiltinFn::kReverse;
  if (name == "sort") return support::BuiltinFn::kSort;
  if (name == "rsort") return support::BuiltinFn::kRsort;
  if (name == "sum") return support::BuiltinFn::kSum;
  if (name == "product") return support::BuiltinFn::kProduct;
  if (name == "and") return support::BuiltinFn::kAnd;
  if (name == "or") return support::BuiltinFn::kOr;
  if (name == "xor") return support::BuiltinFn::kXor;
  if (name == "find") return support::BuiltinFn::kFind;
  if (name == "find_index") return support::BuiltinFn::kFindIndex;
  if (name == "find_first") return support::BuiltinFn::kFindFirst;
  if (name == "find_first_index") return support::BuiltinFn::kFindFirstIndex;
  if (name == "find_last") return support::BuiltinFn::kFindLast;
  if (name == "find_last_index") return support::BuiltinFn::kFindLastIndex;
  if (name == "min") return support::BuiltinFn::kMin;
  if (name == "max") return support::BuiltinFn::kMax;
  if (name == "unique") return support::BuiltinFn::kUnique;
  if (name == "unique_index") return support::BuiltinFn::kUniqueIndex;
  if (name == "map") return support::BuiltinFn::kMap;
  return std::nullopt;
}

auto LowerQueueMethodName(std::string_view name)
    -> std::optional<support::BuiltinFn> {
  if (name == "size") return support::BuiltinFn::kSize;
  if (name == "insert") return support::BuiltinFn::kInsert;
  if (name == "delete") return support::BuiltinFn::kDelete;
  if (name == "pop_front") return support::BuiltinFn::kPopFront;
  if (name == "pop_back") return support::BuiltinFn::kPopBack;
  if (name == "push_front") return support::BuiltinFn::kPushFront;
  if (name == "push_back") return support::BuiltinFn::kPushBack;
  return std::nullopt;
}

auto LowerAssociativeMethodName(std::string_view name)
    -> std::optional<support::BuiltinFn> {
  // LRM 7.9 `num` is an alias of LRM 7.4.3 `size`; both flatten onto kSize.
  if (name == "num" || name == "size") return support::BuiltinFn::kSize;
  if (name == "exists") return support::BuiltinFn::kExists;
  if (name == "delete") return support::BuiltinFn::kDelete;
  if (name == "first") return support::BuiltinFn::kAssocFirst;
  if (name == "last") return support::BuiltinFn::kAssocLast;
  if (name == "next") return support::BuiltinFn::kAssocNext;
  if (name == "prev") return support::BuiltinFn::kAssocPrev;
  return std::nullopt;
}

auto BareCompoundUserRhs(const slang::ast::Expression& slang_expanded_rhs)
    -> const slang::ast::Expression& {
  // Slang's `convertAssignment` adds at most one outer Conversion when the
  // BinaryOp result type differs from the lhs type; it never nests, so a
  // single conditional peel is exact (not defensive). An extra Conversion
  // here would mean slang changed its emission invariant.
  const auto& binop_layer =
      slang_expanded_rhs.kind == slang::ast::ExpressionKind::Conversion
          ? slang_expanded_rhs.as<slang::ast::ConversionExpression>().operand()
          : slang_expanded_rhs;
  if (binop_layer.kind != slang::ast::ExpressionKind::BinaryOp) {
    throw InternalError(
        "BareCompoundUserRhs: expected BinaryOp under slang's compound "
        "expansion; slang's invariant (zero or one Conversion above a "
        "BinaryOp via convertAssignment) has changed");
  }
  const auto& bin = binop_layer.as<slang::ast::BinaryExpression>();

  // `BinaryExpression::fromComponents` adds at most one promotion
  // Conversion per operand. Peel exactly one conditionally; one side is
  // the synthetic LValueReference, the other is the user's rhs.
  const auto peel_one =
      [](const slang::ast::Expression& e) -> const slang::ast::Expression& {
    return e.kind == slang::ast::ExpressionKind::Conversion
               ? e.as<slang::ast::ConversionExpression>().operand()
               : e;
  };
  const auto& left = peel_one(bin.left());
  const auto& right = peel_one(bin.right());

  const bool left_is_ref =
      left.kind == slang::ast::ExpressionKind::LValueReference;
  const bool right_is_ref =
      right.kind == slang::ast::ExpressionKind::LValueReference;
  if (left_is_ref == right_is_ref) {
    throw InternalError(
        "BareCompoundUserRhs: expected exactly one LValueReference operand "
        "in slang's compound expansion");
  }
  return left_is_ref ? right : left;
}

}  // namespace lyra::lowering::ast_to_hir
