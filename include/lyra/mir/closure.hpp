#pragma once

#include <memory>
#include <variant>
#include <vector>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/procedural_var.hpp"
#include "lyra/mir/value_ref.hpp"

namespace lyra::mir {

struct ProceduralScope;

struct ByValueCapture {
  ExprId value;
  ProceduralVarId binding;
};

struct ByReferenceCapture {
  Lvalue source;
  ProceduralVarId binding;
};

using Capture = std::variant<ByValueCapture, ByReferenceCapture>;

// A callable value with captured state and a procedural body.
//
// Closures are synthesized exclusively by HIR-to-MIR lowering. No SystemVerilog
// source construct produces one directly; both the capture list and the body
// statements are compiler-generated.
//
// Invariants enforced by lowering (violations are compiler-bug class):
//   1. Each capture's binding is a ProceduralVarId valid in body.vars and is
//      unique across the capture list.
//   2. Writes inside body to a binding require that binding's capture to be
//      ByReferenceCapture. ByValueCapture bindings are read-only inside body.
//   3. ProceduralVarRef inside body uses hops bounded by body's own scope
//      nesting and does not escape into the enclosing process scope. Outer
//      procedural state reaches body only through captures.
//
// StructuralVarRef inside body may carry hops that reach module-scope storage
// directly, the same way C++ lambdas may reference globals without capture.
struct ClosureExpr {
  std::vector<Capture> captures;
  std::unique_ptr<ProceduralScope> body;

  ClosureExpr();
  ~ClosureExpr();
  ClosureExpr(const ClosureExpr&);
  auto operator=(const ClosureExpr&) -> ClosureExpr&;
  ClosureExpr(ClosureExpr&&) noexcept;
  auto operator=(ClosureExpr&&) noexcept -> ClosureExpr&;
};

}  // namespace lyra::mir
