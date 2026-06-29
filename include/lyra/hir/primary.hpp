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

// LRM 8.4 `null`: the handle literal that refers to no object. Its type is the
// class handle type it is compared or assigned against.
struct NullLiteral {};

// Primary mirrors LRM 11.2.1 - the atomic leaf level of the expression
// grammar. Refs are listed directly here so the same StructuralDataObjectRef /
// ProceduralVarRef value appears identically when the expression is read and
// when it is written. Read vs write is determined by where in the tree the
// ref appears (a PrimaryExpr held under `AssignExpr.lhs` is a write target;
// the same shape under `BinaryExpr.lhs` is a read), not by an extra type
// tag.
using Primary = std::variant<
    IntegerLiteral, StringLiteral, TimeLiteral, RealLiteral, NullLiteral,
    StructuralDataObjectRef, ProceduralVarRef, ClassPropertyRef, LoopVarRef,
    CrossUnitVarRef, IterationBindingRef>;

}  // namespace lyra::hir
