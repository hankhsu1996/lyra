#pragma once

#include <string>
#include <variant>

#include "lyra/hir/integral_constant.hpp"
#include "lyra/hir/value_ref.hpp"

namespace lyra::hir {

struct IntegerLiteral {
  IntegralConstant value;
};

struct StringLiteral {
  std::string value;
};

struct RealLiteral {
  double value;
};

// LRM 8.4 `null`: the handle literal that refers to no object. Its type is the
// class handle type it is compared or assigned against.
struct NullLiteral {};

// LRM 8.11 `this`: the handle referring to the object the subroutine it appears
// in was invoked on -- a constructor as much as a method. The other
// class-handle primary, and the counterpart of `null`: one names no object,
// this one names the object running. It appears only where the source asks for
// the object itself; qualifying a member with `this` names what the bare name
// names and never reaches here.
struct ThisHandle {};

// Primary mirrors LRM 11.2.1 - the atomic leaf level of the expression
// grammar. Refs are listed directly here so the same DirectMemberRef /
// ProceduralVarRef value appears identically when the expression is read and
// when it is written. Read vs write is determined by where in the tree the
// ref appears (a PrimaryExpr held under `AssignExpr.lhs` is a write target;
// the same shape under `BinaryExpr.lhs` is a read), not by an extra type
// tag.
using Primary = std::variant<
    IntegerLiteral, StringLiteral, RealLiteral, NullLiteral, ThisHandle,
    DirectMemberRef, ProceduralVarRef, ClassPropertyRef, StaticPropertyRef,
    RoutedRef, IterationBindingRef, PatternVarRef, ExternalUnitValueRef>;

}  // namespace lyra::hir
