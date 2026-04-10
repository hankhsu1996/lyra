#pragma once

#include <variant>
#include <vector>

#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/assign_target.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// Stable readable reference target for deferred execution.
// Represents the target of a ref/const-ref capture: a symbol root with
// an optional projection path to a sub-element.
//
// Uses the shared Projection vocabulary from assign_target.hpp.
// Deferred-specific legality (constant indices only, no packed/union/dynamic
// forms) is enforced by ExtractHirCaptureTarget() at AST-to-HIR time.
// After validation, the path uses the same representation as assignment
// targets, lvalue paths, etc.
struct HirCaptureTarget {
  SymbolId root_symbol;
  TypeId root_type;
  std::vector<Projection> path;
  TypeId result_type;
  SourceSpan span;
  // The outermost HIR expression that produced this capture target.
  // MIR place derivation uses this as projection origin for all steps
  // in the path. This is an intentional normalized-origin policy:
  // deferred capture targets are validated as one atomic unit by
  // ExtractHirCaptureTarget(), so all derived projections share one
  // origin identifying the whole captured reference expression.
  ExpressionId source_expr;
  auto operator==(const HirCaptureTarget&) const -> bool = default;
};

// Capture variants for a deferred action, matching the C++ closure model:
//   [captured_v = expr]()   -> ValueCapture
//   [&ref_var]()            -> RefCapture
//   [const& ref_var]()      -> ConstRefCapture
struct ValueCapture {
  ExpressionId expr;
  TypeId type;
  auto operator==(const ValueCapture&) const -> bool = default;
};

struct RefCapture {
  HirCaptureTarget target;
  TypeId type;
  auto operator==(const RefCapture&) const -> bool = default;
};

struct ConstRefCapture {
  HirCaptureTarget target;
  TypeId type;
  auto operator==(const ConstRefCapture&) const -> bool = default;
};

using DeferredCapture = std::variant<ValueCapture, RefCapture, ConstRefCapture>;

// The semantic center: a closure-like deferred action.
// Callee is identified by SymbolId (not MIR FunctionId).
// Captures are ordered by formal position.
// Inline-owned by DeferredAssertionStatementData, not by any side table.
struct DeferredAction {
  SymbolId callee_symbol;
  std::vector<DeferredCapture> captures;
  auto operator==(const DeferredAction&) const -> bool = default;
};

}  // namespace lyra::hir
