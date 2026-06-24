#pragma once

#include "lyra/mir/expr_id.hpp"

namespace lyra::mir {

// Reinterprets the operand as a value of a different type. The destination
// type is stated on the enclosing `Expr::type`; the source type is read off
// the operand. The (source, destination) type pair fully determines the
// implementation -- integral resize / sign-handling, integral-to-real,
// real-to-integral, packed-to-enum, pointer-to-different-pointee -- so the
// primitive carries no kind axis. Backends consult the type pair to select
// the realization (C++ chooses among `static_cast` and `lyra::value` helpers;
// LLVM picks `zext` / `sext` / `trunc` / `fptosi` / `bitcast` / `inttoptr`).
struct CastExpr {
  ExprId operand;
};

}  // namespace lyra::mir
