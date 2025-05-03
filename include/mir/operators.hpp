#pragma once

#include <ostream>
#include <string>

#include <fmt/core.h>
#include <slang/ast/expressions/Operator.h>

namespace lyra::mir {

enum class Operator {
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

// Convert Operator enum to string
inline auto ToString(Operator operator_kind) -> std::string {
  switch (operator_kind) {
    // Arithmetic
    case Operator::kAddition:
      return "Addition";
    case Operator::kSubtraction:
      return "Subtraction";
    case Operator::kMultiplication:
      return "Multiplication";
    case Operator::kDivision:
      return "Division";
    case Operator::kModulo:
      return "Modulo";
    case Operator::kPower:
      return "Power";

    // Bitwise
    case Operator::kBitwiseAnd:
      return "BitwiseAnd";
    case Operator::kBitwiseOr:
      return "BitwiseOr";
    case Operator::kBitwiseXor:
      return "BitwiseXor";
    case Operator::kBitwiseXnor:
      return "BitwiseXnor";

    // Logical
    case Operator::kLogicalAnd:
      return "LogicalAnd";
    case Operator::kLogicalOr:
      return "LogicalOr";
    case Operator::kLogicalImplication:
      return "LogicalImplication";
    case Operator::kLogicalEquivalence:
      return "LogicalEquivalence";

    // Comparison
    case Operator::kEquality:
      return "Equality";
    case Operator::kInequality:
      return "Inequality";
    case Operator::kCaseEquality:
      return "CaseEquality";
    case Operator::kCaseInequality:
      return "CaseInequality";
    case Operator::kWildcardEquality:
      return "WildcardEquality";
    case Operator::kWildcardInequality:
      return "WildcardInequality";
    case Operator::kGreaterThan:
      return "GreaterThan";
    case Operator::kGreaterThanEqual:
      return "GreaterThanEqual";
    case Operator::kLessThan:
      return "LessThan";
    case Operator::kLessThanEqual:
      return "LessThanEqual";

    // Shift
    case Operator::kLogicalShiftLeft:
      return "LogicalShiftLeft";
    case Operator::kLogicalShiftRight:
      return "LogicalShiftRight";
    case Operator::kArithmeticShiftLeft:
      return "ArithmeticShiftLeft";
    case Operator::kArithmeticShiftRight:
      return "ArithmeticShiftRight";
  }
  return "Unknown";
}

// Stream operator for Operator enum
inline auto operator<<(std::ostream& os, Operator operator_kind)
    -> std::ostream& {
  return os << ToString(operator_kind);
}

}  // namespace lyra::mir

// Formatter for fmt::format
template <>
struct fmt::formatter<lyra::mir::Operator> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& context) {
    return context.begin();
  }

  template <typename FormatContext>
  auto format(
      const lyra::mir::Operator& operator_kind, FormatContext& context) const {
    return fmt::format_to(
        context.out(), "{}", lyra::mir::ToString(operator_kind));
  }
};

namespace lyra::mir {

// Convert slang::ast::BinaryOperator to mir::Operator
inline auto ConvertSlangBinaryOperatorToMir(
    slang::ast::BinaryOperator slang_operator) -> Operator {
  using SlangOp = slang::ast::BinaryOperator;

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
