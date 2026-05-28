#pragma once

#include <cstdint>
#include <string>
#include <variant>

#include "lyra/hir/conversion.hpp"
#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

enum class TimeScale : std::uint8_t { kFs, kPs, kNs, kUs, kMs, kS };

struct IntegerLiteral {
  IntegralConstant value;
  IntegerLiteralBase base = IntegerLiteralBase::kDecimal;
  bool declared_unsized = false;
};

struct StringLiteral {
  std::string value;
};

struct TimeLiteral {
  double value;
  TimeScale scale;
};

struct RealLiteral {
  double value;
};

// Primary mirrors LRM 11.2.1 - the atomic leaf level of the expression
// grammar. Refs are listed directly here (not behind a wrapper) so they share
// type identity with the Lvalue variant: the same StructuralVarRef value can
// appear here when read and inside an Lvalue when written. Read vs write is
// determined by where in the tree the ref appears, not by an extra type tag.
using Primary = std::variant<
    IntegerLiteral, StringLiteral, TimeLiteral, RealLiteral, StructuralVarRef,
    ProceduralVarRef, LoopVarRef>;

}  // namespace lyra::hir
