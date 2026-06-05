#pragma once

#include <memory>
#include <variant>
#include <vector>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/procedural_var.hpp"

namespace lyra::mir {

struct ProceduralScope;

struct ByValueCapture {
  ExprId value{};
  ProceduralVarId binding{};
};

// A by-reference capture binds the body's `binding` var to an enclosing-scope
// lvalue (`target`). The spawned body reaches it through a frame-copied
// reference parameter; the enclosing storage outlives the body (LRM 6.21), so
// reads and writes share the one live location. Used by fork branches that
// touch their enclosing process's variables.
struct ByReferenceCapture {
  ExprId target{};
  ProceduralVarId binding{};
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
//   2. A ByValueCapture binding is read-only inside body. A ByReferenceCapture
//      binding may be read and written -- it aliases the enclosing storage.
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
