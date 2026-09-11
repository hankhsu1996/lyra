#pragma once

#include <optional>
#include <utility>
#include <vector>

#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM Table 6-7 default constant for an integral type: all-`x` for 4-state,
// all-zero for 2-state. A conversion / from-int factory call passes this (as a
// literal of its destination type) so the destination representation reaches
// the runtime as an ordinary MIR value, never composed by the backend.
[[nodiscard]] auto DefaultIntegralConstant(const mir::PackedArrayType& pa)
    -> mir::IntegralConstant;

// Builds a primitive MIR expression evaluating to the LRM Table 6-7 default
// value of `type`, returning the top node detached for the caller to intern.
// A composite default registers the child expressions it references into
// `block` before returning the outer node, so interning the result yields a
// self-contained subtree of arena entries.
//
// `int x;` and `int x = 0;` are different in SV source -- HIR preserves that
// distinction via `optional<initializer>`. By MIR every variable has an
// explicit initializer expression: the SV "no initializer means LRM default"
// sugar is decomposed into a primitive Expr at the HIR-to-MIR boundary so
// downstream layers see one shape (an Expr) instead of two.
[[nodiscard]] auto BuildDefaultValueExpr(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId type)
    -> mir::Expr;

// Default-construct a value from its source (HIR) type, honoring
// unpacked-struct member declaration initializers (LRM 7.2.2). Use this at
// every site that materializes the SV default initial value of a declared
// variable, member, or element. The MIR-type-keyed default builder above cannot
// honor member initializers -- the canonicalized MIR type drops them -- so it
// serves only placeholder and transient-product defaults, where the source
// initializer does not apply.
[[nodiscard]] auto BuildDefaultValueFromHir(
    const UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId hir_type)
    -> mir::Expr;

// Whether a type is one of the three array container types (unpacked, dynamic,
// or queue), which is what a caller asks before reaching for its element type.
[[nodiscard]] auto IsArrayContainerType(const mir::Type& type) -> bool;

// Whether an assignment between the two types crosses from one of those three
// kinds to another. Two containers of one kind hold their elements the same
// way, so a value crosses between them as it stands.
[[nodiscard]] auto CrossesArrayContainerKinds(
    const mir::Type& source, const mir::Type& destination) -> bool;

// The element type of a container that holds elements of one type, absent for
// a type that holds none. Which types those are is stated here and nowhere
// else; a caller that must decide what a non-container means says so at its
// own site. There is one of these per type universe, because the two layers
// name types in different ones and no single function spans both.
[[nodiscard]] auto ContainerElementType(
    const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::optional<mir::TypeId>;

[[nodiscard]] auto ContainerElementType(const hir::Type& type)
    -> std::optional<hir::TypeId>;

// The same, for a caller whose own construction guarantees a container. Where
// that guarantee did not hold, the producer built something it should not
// have, so this reports a compiler bug rather than answering.
[[nodiscard]] auto RequiredContainerElementType(
    const mir::CompilationUnit& unit, mir::TypeId container) -> mir::TypeId;

// The element default a container carries for every position it does not hold
// (LRM 7.4.5, Table 7-1): what an invalid read answers with, what a grow fills
// a new slot with, and what an invalid write is discarded into. It is built
// from the source container type, because a member's declaration initializer
// (LRM 7.2.2) is part of that value and the lowered element type has dropped
// it, so a container built without the source type in reach cannot reproduce
// it.
[[nodiscard]] auto BuildElementDefault(
    const UnitLowerer& unit_lowerer, mir::Block& block, hir::TypeId container)
    -> mir::ExprId;

// Builds a container from an explicit element list, laid down once, whose
// constructor arguments are `[element_default, elements, count]` plus the LRM
// 7.10.5 bound for a bounded queue. This is the construction shape every site
// producing such a value must use: the elements ride as one literal of the
// plain-data array of that element, so the container is what the construction
// produces, never what the literal itself claims to be. A uniform value is
// built by the repeat call below instead.
[[nodiscard]] auto BuildArrayConstructionCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId array_type,
    mir::ExprId element_default, std::vector<mir::ExprId> elements)
    -> mir::Expr;

// Builds the construction call for a uniform array-container value: `count`
// replications of `repeat_unit`, seeded with the element default above. The
// repeat unit rides as an aggregate literal and the
// count as a machine scalar, so the constructor arguments are
// `[element_default, repeat_unit, count]` (plus the LRM 7.10.5 bound for a
// bounded queue). This is the shape every site that produces an all-default or
// `'{count{...}}` array value must use, so the value's MIR and emitted text
// stay O(repeat_unit) rather than O(repeat_unit * count). A list whose elements
// differ is built by the construction call above instead.
[[nodiscard]] auto BuildArrayRepeatCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId array_type,
    mir::ExprId element_default, std::vector<mir::ExprId> repeat_unit,
    mir::ExprId count_id) -> mir::Expr;

// Builds the construction call for a sequence: the values it holds, in order.
// A sequence seeds no default and repeats nothing, so the element list is the
// constructor's one argument, and a sequence holding nothing is that list with
// no elements rather than a second form of the call.
[[nodiscard]] auto BuildSequenceConstructionCall(
    const mir::CompilationUnit& unit, mir::Block& block,
    mir::TypeId sequence_type, std::vector<mir::ExprId> elements) -> mir::Expr;

// Builds the construction call for an associative-array literal (LRM 7.9.11).
// Each (key, value) entry is a pair, and the entries ride in the plain-data
// array of those pairs, so the constructor arguments are `[element_default,
// entries, absent_key_answer]`. `user_default` is the LRM 7.9.11 persistent
// fallback a read of an absent key returns; a literal that writes no `default:`
// clause answers such a read with the element type's own default, so that is
// what stands there instead, and the operand is never missing.
[[nodiscard]] auto BuildAssociativeConstructionCall(
    const mir::CompilationUnit& unit, mir::Block& block, mir::TypeId assoc_type,
    mir::ExprId element_default,
    std::vector<std::pair<mir::ExprId, mir::ExprId>> entries,
    std::optional<mir::ExprId> user_default) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
