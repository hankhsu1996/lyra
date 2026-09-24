#pragma once

#include <cstdint>

#include "lyra/base/internal_error.hpp"

namespace lyra::backend::cpp {

// C++ operator precedence, loosest first. A form written into the output has
// one level, and a position inside a larger form needs a minimum one. `kPrefix`
// is a unary operator or a cast; `kPostfix` is a call, a member, a subscript,
// or anything that is a single name or literal, which nothing binds tighter
// than.
enum class Precedence : std::uint8_t {
  kAssignment,
  kLogicalOr,
  kLogicalAnd,
  kBitwiseOr,
  kBitwiseXor,
  kBitwiseAnd,
  kEquality,
  kRelational,
  kAdditive,
  kMultiplicative,
  kPrefix,
  kPostfix,
};

// A level's rank, loosest lowest. A switch rather than the enum's value, so a
// level added later fails to compile until it is ranked.
[[nodiscard]] inline auto RankOf(Precedence level) -> std::uint8_t {
  switch (level) {
    case Precedence::kAssignment:
      return 0;
    case Precedence::kLogicalOr:
      return 1;
    case Precedence::kLogicalAnd:
      return 2;
    case Precedence::kBitwiseOr:
      return 3;
    case Precedence::kBitwiseXor:
      return 4;
    case Precedence::kBitwiseAnd:
      return 5;
    case Precedence::kEquality:
      return 6;
    case Precedence::kRelational:
      return 7;
    case Precedence::kAdditive:
      return 8;
    case Precedence::kMultiplicative:
      return 9;
    case Precedence::kPrefix:
      return 10;
    case Precedence::kPostfix:
      return 11;
  }
  throw InternalError("RankOf: unknown precedence level");
}

// Whether a form at precedence `held` needs parentheses in a position that
// needs at least `needed`.
[[nodiscard]] inline auto NeedsParentheses(Precedence held, Precedence needed)
    -> bool {
  return RankOf(held) < RankOf(needed);
}

// The level one step tighter than a binary operator's: what its right operand
// needs, since the operators are left-associative. `a - (b - c)` keeps its
// parentheses and `a - b - c` needs none. No binary operator is at the prefix
// or postfix level, so those two have no answer.
[[nodiscard]] inline auto TighterThan(Precedence level) -> Precedence {
  switch (level) {
    case Precedence::kAssignment:
      return Precedence::kLogicalOr;
    case Precedence::kLogicalOr:
      return Precedence::kLogicalAnd;
    case Precedence::kLogicalAnd:
      return Precedence::kBitwiseOr;
    case Precedence::kBitwiseOr:
      return Precedence::kBitwiseXor;
    case Precedence::kBitwiseXor:
      return Precedence::kBitwiseAnd;
    case Precedence::kBitwiseAnd:
      return Precedence::kEquality;
    case Precedence::kEquality:
      return Precedence::kRelational;
    case Precedence::kRelational:
      return Precedence::kAdditive;
    case Precedence::kAdditive:
      return Precedence::kMultiplicative;
    case Precedence::kMultiplicative:
      return Precedence::kPrefix;
    case Precedence::kPrefix:
    case Precedence::kPostfix:
      throw InternalError(
          "TighterThan: asked for the right operand of a level no binary "
          "operator has -- please report this as a bug");
  }
  throw InternalError("TighterThan: unknown precedence level");
}

}  // namespace lyra::backend::cpp
