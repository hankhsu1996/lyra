#pragma once

#include <ostream>
#include <string>

#include <fmt/core.h>
#include <slang/ast/expressions/Operator.h>

namespace lyra::mir {

enum class UnaryOperator {
  // Arithmetic
  kPlus,
  kMinus,
  kPreincrement,   // ++a
  kPostincrement,  // a++
  kPredecrement,   // --a
  kPostdecrement,  // a--

  // Logical
  kLogicalNot,

  // Bitwise
  kBitwiseNot,

  // Reduction
  kReductionAnd,
  kReductionNand,
  kReductionOr,
  kReductionNor,
  kReductionXor,
  kReductionXnor
};

// Convert UnaryOperator enum to string
inline auto ToString(UnaryOperator operator_kind) -> std::string {
  switch (operator_kind) {
    // Arithmetic
    case UnaryOperator::kPlus:
      return "Plus";
    case UnaryOperator::kMinus:
      return "Minus";
    case UnaryOperator::kPreincrement:
      return "Preincrement";
    case UnaryOperator::kPostincrement:
      return "Postincrement";
    case UnaryOperator::kPredecrement:
      return "Predecrement";
    case UnaryOperator::kPostdecrement:
      return "Postdecrement";

    // Logical
    case UnaryOperator::kLogicalNot:
      return "LogicalNot";

    // Bitwise
    case UnaryOperator::kBitwiseNot:
      return "BitwiseNot";

    // Reduction
    case UnaryOperator::kReductionAnd:
      return "ReductionAnd";
    case UnaryOperator::kReductionNand:
      return "ReductionNand";
    case UnaryOperator::kReductionOr:
      return "ReductionOr";
    case UnaryOperator::kReductionNor:
      return "ReductionNor";
    case UnaryOperator::kReductionXor:
      return "ReductionXor";
    case UnaryOperator::kReductionXnor:
      return "ReductionXnor";
  }
  return "Unknown";
}

// Stream operator for UnaryOperator enum
inline auto operator<<(std::ostream& os, UnaryOperator operator_kind)
    -> std::ostream& {
  return os << ToString(operator_kind);
}

enum class BinaryOperator {
  // Arithmetic
  kAddition,
  kSubtraction,
  kMultiplication,
  kDivision,
  kModulo,
  kPower,

  // Bitwise
  kBitwiseAnd,
  kBitwiseOr,
  kBitwiseXor,
  kBitwiseXnor,

  // Logical
  kLogicalAnd,
  kLogicalOr,
  kLogicalImplication,
  kLogicalEquivalence,

  // Comparison
  kEquality,
  kInequality,
  kCaseEquality,
  kCaseInequality,
  kWildcardEquality,
  kWildcardInequality,
  kGreaterThan,
  kGreaterThanEqual,
  kLessThan,
  kLessThanEqual,

  // Shift
  kLogicalShiftLeft,
  kLogicalShiftRight,
  kArithmeticShiftLeft,
  kArithmeticShiftRight
};

// Convert BinaryOperator enum to string
inline auto ToString(BinaryOperator operator_kind) -> std::string {
  switch (operator_kind) {
    // Arithmetic
    case BinaryOperator::kAddition:
      return "Addition";
    case BinaryOperator::kSubtraction:
      return "Subtraction";
    case BinaryOperator::kMultiplication:
      return "Multiplication";
    case BinaryOperator::kDivision:
      return "Division";
    case BinaryOperator::kModulo:
      return "Modulo";
    case BinaryOperator::kPower:
      return "Power";

    // Bitwise
    case BinaryOperator::kBitwiseAnd:
      return "BitwiseAnd";
    case BinaryOperator::kBitwiseOr:
      return "BitwiseOr";
    case BinaryOperator::kBitwiseXor:
      return "BitwiseXor";
    case BinaryOperator::kBitwiseXnor:
      return "BitwiseXnor";

    // Logical
    case BinaryOperator::kLogicalAnd:
      return "LogicalAnd";
    case BinaryOperator::kLogicalOr:
      return "LogicalOr";
    case BinaryOperator::kLogicalImplication:
      return "LogicalImplication";
    case BinaryOperator::kLogicalEquivalence:
      return "LogicalEquivalence";

    // Comparison
    case BinaryOperator::kEquality:
      return "Equality";
    case BinaryOperator::kInequality:
      return "Inequality";
    case BinaryOperator::kCaseEquality:
      return "CaseEquality";
    case BinaryOperator::kCaseInequality:
      return "CaseInequality";
    case BinaryOperator::kWildcardEquality:
      return "WildcardEquality";
    case BinaryOperator::kWildcardInequality:
      return "WildcardInequality";
    case BinaryOperator::kGreaterThan:
      return "GreaterThan";
    case BinaryOperator::kGreaterThanEqual:
      return "GreaterThanEqual";
    case BinaryOperator::kLessThan:
      return "LessThan";
    case BinaryOperator::kLessThanEqual:
      return "LessThanEqual";

    // Shift
    case BinaryOperator::kLogicalShiftLeft:
      return "LogicalShiftLeft";
    case BinaryOperator::kLogicalShiftRight:
      return "LogicalShiftRight";
    case BinaryOperator::kArithmeticShiftLeft:
      return "ArithmeticShiftLeft";
    case BinaryOperator::kArithmeticShiftRight:
      return "ArithmeticShiftRight";
  }
}

// Stream operator for BinaryOperator enum
inline auto operator<<(std::ostream& os, BinaryOperator operator_kind)
    -> std::ostream& {
  return os << ToString(operator_kind);
}

}  // namespace lyra::mir

// Formatter for fmt::format for UnaryOperator
template <>
struct fmt::formatter<lyra::mir::UnaryOperator> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& context) {
    return context.begin();
  }

  template <typename FormatContext>
  auto format(
      const lyra::mir::UnaryOperator& operator_kind,
      FormatContext& context) const {
    return fmt::format_to(
        context.out(), "{}", lyra::mir::ToString(operator_kind));
  }
};

// Formatter for fmt::format for BinaryOperator
template <>
struct fmt::formatter<lyra::mir::BinaryOperator> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& context) {
    return context.begin();
  }

  template <typename FormatContext>
  auto format(
      const lyra::mir::BinaryOperator& operator_kind,
      FormatContext& context) const {
    return fmt::format_to(
        context.out(), "{}", lyra::mir::ToString(operator_kind));
  }
};

namespace lyra::mir {

// Convert slang::ast::UnaryOperator to mir::UnaryOperator
inline auto ConvertSlangUnaryOperatorToMir(
    slang::ast::UnaryOperator slang_operator) -> UnaryOperator {
  using SlangOp = slang::ast::UnaryOperator;
  using Operator = UnaryOperator;

  switch (slang_operator) {
    case SlangOp::Plus:
      return Operator::kPlus;
    case SlangOp::Minus:
      return Operator::kMinus;
    case SlangOp::BitwiseNot:
      return Operator::kBitwiseNot;
    case SlangOp::BitwiseAnd:
      return Operator::kReductionAnd;
    case SlangOp::BitwiseOr:
      return Operator::kReductionOr;
    case SlangOp::BitwiseXor:
      return Operator::kReductionXor;
    case SlangOp::BitwiseNand:
      return Operator::kReductionNand;
    case SlangOp::BitwiseNor:
      return Operator::kReductionNor;
    case SlangOp::BitwiseXnor:
      return Operator::kReductionXnor;
    case SlangOp::LogicalNot:
      return Operator::kLogicalNot;
    case SlangOp::Preincrement:
      return Operator::kPreincrement;
    case SlangOp::Predecrement:
      return Operator::kPredecrement;
    case SlangOp::Postincrement:
      return Operator::kPostincrement;
    case SlangOp::Postdecrement:
      return Operator::kPostdecrement;
  }
}

// Convert slang::ast::BinaryOperator to mir::BinaryOperator
inline auto ConvertSlangBinaryOperatorToMir(
    slang::ast::BinaryOperator slang_operator) -> BinaryOperator {
  using SlangOp = slang::ast::BinaryOperator;
  using Operator = BinaryOperator;

  switch (slang_operator) {
    case SlangOp::Add:
      return Operator::kAddition;
    case SlangOp::Subtract:
      return Operator::kSubtraction;
    case SlangOp::Multiply:
      return Operator::kMultiplication;
    case SlangOp::Divide:
      return Operator::kDivision;
    case SlangOp::Mod:
      return Operator::kModulo;
    case SlangOp::Power:
      return Operator::kPower;

    case SlangOp::BinaryAnd:
      return Operator::kBitwiseAnd;
    case SlangOp::BinaryOr:
      return Operator::kBitwiseOr;
    case SlangOp::BinaryXor:
      return Operator::kBitwiseXor;
    case SlangOp::BinaryXnor:
      return Operator::kBitwiseXnor;

    case SlangOp::LogicalAnd:
      return Operator::kLogicalAnd;
    case SlangOp::LogicalOr:
      return Operator::kLogicalOr;
    case SlangOp::LogicalImplication:
      return Operator::kLogicalImplication;
    case SlangOp::LogicalEquivalence:
      return Operator::kLogicalEquivalence;

    case SlangOp::Equality:
      return Operator::kEquality;
    case SlangOp::Inequality:
      return Operator::kInequality;
    case SlangOp::CaseEquality:
      return Operator::kCaseEquality;
    case SlangOp::CaseInequality:
      return Operator::kCaseInequality;
    case SlangOp::WildcardEquality:
      return Operator::kWildcardEquality;
    case SlangOp::WildcardInequality:
      return Operator::kWildcardInequality;
    case SlangOp::GreaterThan:
      return Operator::kGreaterThan;
    case SlangOp::GreaterThanEqual:
      return Operator::kGreaterThanEqual;
    case SlangOp::LessThan:
      return Operator::kLessThan;
    case SlangOp::LessThanEqual:
      return Operator::kLessThanEqual;

    case SlangOp::LogicalShiftLeft:
      return Operator::kLogicalShiftLeft;
    case SlangOp::LogicalShiftRight:
      return Operator::kLogicalShiftRight;
    case SlangOp::ArithmeticShiftLeft:
      return Operator::kArithmeticShiftLeft;
    case SlangOp::ArithmeticShiftRight:
      return Operator::kArithmeticShiftRight;
  }
}

}  // namespace lyra::mir
