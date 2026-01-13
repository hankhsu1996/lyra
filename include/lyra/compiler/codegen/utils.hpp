#pragma once

#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

#include "lyra/common/hierarchical_path.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/mir/operators.hpp"

namespace lyra::compiler::codegen {

// Formats a hierarchical instance path as a C++ member access string.
// Instance names get _ suffix, array indices get [N] notation, dots separate.
// Example: [{gen_block, null}, {"", 0}] -> "gen_block_[0]"
// Note: Does NOT include the target variable - just the path to reach it.
auto FormatCppInstancePath(
    const std::vector<common::HierarchicalPathElement>& elements,
    const common::SymbolNameResolver& resolver) -> std::string;

// Returns true if name is a C++ reserved keyword.
auto IsCppKeyword(std::string_view name) -> bool;

// Escapes C++ reserved keywords by appending underscore suffix.
// Follows Google Protocol Buffers convention (e.g., "double" -> "double_").
auto EscapeIdentifier(std::string_view name) -> std::string;

// Escapes C++ reserved keywords, avoiding collisions with existing identifiers.
// If "name_" collides, tries "name__", "name_0_", "name_1_", etc.
auto EscapeIdentifier(
    std::string_view name,
    const std::unordered_set<std::string_view>& existing_names) -> std::string;

// Escapes qualified names like "MyPkg::double" -> "MyPkg::double_".
// Only escapes the last component (after the final "::").
auto EscapeQualifiedName(std::string_view qualified_name) -> std::string;

// C++ operator precedence (higher value = binds tighter)
// See: https://en.cppreference.com/w/cpp/language/operator_precedence
constexpr int kPrecLowest = 0;           // Top level (no parent)
constexpr int kPrecAssign = 1;           // = += -= etc.
constexpr int kPrecTernary = 2;          // ?:
constexpr int kPrecLogicalOr = 3;        // ||
constexpr int kPrecLogicalAnd = 4;       // &&
constexpr int kPrecBitwiseOr = 5;        // |
constexpr int kPrecBitwiseXor = 6;       // ^
constexpr int kPrecBitwiseAnd = 7;       // &
constexpr int kPrecEquality = 8;         // == !=
constexpr int kPrecRelational = 9;       // < <= > >=
constexpr int kPrecShift = 10;           // << >>
constexpr int kPrecAdditive = 11;        // + -
constexpr int kPrecMultiplicative = 12;  // * / %
constexpr int kPrecUnary = 13;           // ! ~ - + ++ --
constexpr int kPrecPrimary = 14;         // Literals, identifiers, calls

inline auto GetBinaryPrecedence(mir::BinaryOperator op) -> int {
  switch (op) {
    case mir::BinaryOperator::kLogicalOr:
      return kPrecLogicalOr;
    case mir::BinaryOperator::kLogicalAnd:
      return kPrecLogicalAnd;
    case mir::BinaryOperator::kLogicalImplication:
    case mir::BinaryOperator::kLogicalEquivalence:
      return kPrecLogicalOr;  // Treat like ||
    case mir::BinaryOperator::kBitwiseOr:
      return kPrecBitwiseOr;
    case mir::BinaryOperator::kBitwiseXor:
    case mir::BinaryOperator::kBitwiseXnor:
      return kPrecBitwiseXor;
    case mir::BinaryOperator::kBitwiseAnd:
      return kPrecBitwiseAnd;
    case mir::BinaryOperator::kEquality:
    case mir::BinaryOperator::kInequality:
    case mir::BinaryOperator::kCaseEquality:
    case mir::BinaryOperator::kCaseInequality:
    case mir::BinaryOperator::kWildcardEquality:
    case mir::BinaryOperator::kWildcardInequality:
      return kPrecEquality;
    case mir::BinaryOperator::kGreaterThan:
    case mir::BinaryOperator::kGreaterThanEqual:
    case mir::BinaryOperator::kLessThan:
    case mir::BinaryOperator::kLessThanEqual:
      return kPrecRelational;
    case mir::BinaryOperator::kLogicalShiftLeft:
    case mir::BinaryOperator::kLogicalShiftRight:
    case mir::BinaryOperator::kArithmeticShiftLeft:
    case mir::BinaryOperator::kArithmeticShiftRight:
      return kPrecShift;
    case mir::BinaryOperator::kAddition:
    case mir::BinaryOperator::kSubtraction:
      return kPrecAdditive;
    case mir::BinaryOperator::kMultiplication:
    case mir::BinaryOperator::kDivision:
    case mir::BinaryOperator::kModulo:
    case mir::BinaryOperator::kPower:
      return kPrecMultiplicative;
  }
  return kPrecPrimary;
}

inline auto ToCppOperator(mir::BinaryOperator op) -> const char* {
  switch (op) {
    case mir::BinaryOperator::kAddition:
      return "+";
    case mir::BinaryOperator::kSubtraction:
      return "-";
    case mir::BinaryOperator::kMultiplication:
      return "*";
    case mir::BinaryOperator::kDivision:
      return "/";
    case mir::BinaryOperator::kModulo:
      return "%";
    case mir::BinaryOperator::kPower:
      return "/* power */";  // C++ doesn't have ** operator

    case mir::BinaryOperator::kBitwiseAnd:
      return "&";
    case mir::BinaryOperator::kBitwiseOr:
      return "|";
    case mir::BinaryOperator::kBitwiseXor:
      return "^";
    case mir::BinaryOperator::kBitwiseXnor:
      return "/* xnor */";  // C++ doesn't have xnor operator

    case mir::BinaryOperator::kLogicalAnd:
      return "&&";
    case mir::BinaryOperator::kLogicalOr:
      return "||";
    case mir::BinaryOperator::kLogicalImplication:
      return "/* implication */";
    case mir::BinaryOperator::kLogicalEquivalence:
      return "/* equivalence */";

    case mir::BinaryOperator::kEquality:
      return "==";
    case mir::BinaryOperator::kInequality:
      return "!=";
    case mir::BinaryOperator::kCaseEquality:
      return "==";  // In C++, just use ==
    case mir::BinaryOperator::kCaseInequality:
      return "!=";
    case mir::BinaryOperator::kWildcardEquality:
      return "/* wildcard eq */";
    case mir::BinaryOperator::kWildcardInequality:
      return "/* wildcard neq */";
    case mir::BinaryOperator::kGreaterThan:
      return ">";
    case mir::BinaryOperator::kGreaterThanEqual:
      return ">=";
    case mir::BinaryOperator::kLessThan:
      return "<";
    case mir::BinaryOperator::kLessThanEqual:
      return "<=";

    case mir::BinaryOperator::kLogicalShiftLeft:
      return "<<";
    case mir::BinaryOperator::kLogicalShiftRight:
      return ">>";
    case mir::BinaryOperator::kArithmeticShiftLeft:
      return "<<";
    case mir::BinaryOperator::kArithmeticShiftRight:
      return ">>";
  }
  return "/* unknown */";
}

}  // namespace lyra::compiler::codegen
