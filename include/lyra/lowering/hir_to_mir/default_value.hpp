#pragma once

#include <optional>
#include <utility>
#include <vector>

#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/module_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// Builds a primitive MIR expression evaluating to the LRM Table 6-7 default
// value of `type`, returning the top node detached for the caller to intern.
// Composite types (UnpackedArray, DynamicArray, Queue) recurse and register
// each element default into `frame.current_block` as children before
// returning the outer composite, so interning the result yields a
// self-contained subtree of arena entries.
//
// `int x;` and `int x = 0;` are different in SV source -- HIR preserves that
// distinction via `optional<initializer>`. By MIR every variable has an
// explicit initializer expression: the SV "no initializer means LRM default"
// sugar is decomposed into a primitive Expr at the HIR-to-MIR boundary so
// downstream layers see one shape (an Expr) instead of two.
[[nodiscard]] auto BuildDefaultValueExpr(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId type)
    -> mir::Expr;

// Default-construct a value from its source (HIR) type, honoring
// unpacked-struct member declaration initializers (LRM 7.2.2). Use this at
// every site that materializes the SV default initial value of a declared
// variable, member, or element. The MIR-type-keyed default builder above cannot
// honor member initializers -- the canonicalized MIR type drops them -- so it
// serves only placeholder and transient-product defaults, where the source
// initializer does not apply.
[[nodiscard]] auto BuildDefaultValueFromHir(
    const ModuleLowerer& module, WalkFrame frame, hir::TypeId hir_type)
    -> mir::Expr;

// Wraps a list of element ExprIds destined for an array container constructor
// (`UnpackedArrayType` or `DynamicArrayType`) in a construction call whose
// arguments are `[element_default, ArrayLiteralExpr{elements}]`. This is the
// construction shape every site that produces an array-container value must
// use: the canonical-default element required by the wrapper's runtime ctor
// is supplied here via `BuildDefaultValueExpr` on the element type, and the
// elements ride in an `ArrayLiteralExpr` that the renderer emits as
// `std::array<T, N>{...}`.
[[nodiscard]] auto BuildArrayConstructionCall(
    const ModuleLowerer& module, WalkFrame frame, mir::TypeId array_type,
    std::vector<mir::ExprId> elements) -> mir::Expr;

// Builds the construction call for an associative-array literal (LRM 7.9.11).
// Each (key, value) entry becomes a `TupleExpr`; the entries ride in an
// `ArrayLiteralExpr` of those tuples (rendered `std::array<std::tuple<K, V>,
// N>{...}`), and the constructor arguments are `[element_default, entries,
// optional user_default]`. `user_default` is the LRM 7.9.11 persistent fallback
// a read of an absent key returns; when absent the constructor seeds only the
// element type default. Interns the tuple and tuple-array MIR types, so the
// module must be the mutable in-progress unit.
[[nodiscard]] auto BuildAssociativeConstructionCall(
    ModuleLowerer& module, WalkFrame frame, mir::TypeId assoc_type,
    std::vector<std::pair<mir::ExprId, mir::ExprId>> entries,
    std::optional<mir::ExprId> user_default) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
