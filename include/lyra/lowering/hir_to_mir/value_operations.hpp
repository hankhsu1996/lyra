#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// A method for every operation on a whole value the struct `record` names has
// (LRM 7.2), over values of `structure` whose members are of `members`, and
// the value what it declares, `source`, gives a variable of it to start as.
// What the struct itself answers is read off its members; a member that is
// itself a struct is reached by the record naming it, which is defined before
// any record's methods are asked for.
[[nodiscard]] auto StructMethodsOf(
    UnitLowerer& lowerer, const hir::UnpackedStructDefinition& source,
    mir::RecordId record, mir::TypeId structure,
    std::span<const mir::TypeId> members) -> std::vector<mir::RecordMethod>;

// A method for every operation on a whole value the union `record` names
// has (LRM 7.3), over values of `overlay`, whose members are of `members`:
// each is its first member's, read as that member's type holds a value (LRM
// 7.3.2). What the union itself answers is read off its members.
[[nodiscard]] auto UnionMethodsOf(
    UnitLowerer& lowerer, mir::RecordId record, mir::TypeId overlay,
    std::span<const mir::TypeId> members) -> std::vector<mir::RecordMethod>;

// The same for a tagged union, each method that of the member its tag names
// (LRM 7.3.2); a tagged union has no stream and no net's folds.
[[nodiscard]] auto TaggedUnionMethodsOf(
    UnitLowerer& lowerer, mir::RecordId record, mir::TypeId overlay,
    std::span<const mir::TypeId> members) -> std::vector<mir::RecordMethod>;

// How a body answering an operation of a type takes each value it is asked
// about, the value itself and every other operand: by a read-only reference,
// as Rust's comparison operators borrow theirs, so a value is read where it
// lies rather than copied in.
[[nodiscard]] auto ReferenceTo(
    const mir::CompilationUnit& unit, mir::TypeId type) -> mir::TypeId;

// A body for every operation on a whole value the fixed-size unpacked array
// `type` has (LRM 7.4.2), each element's in turn, which the unit naming the
// type states as its own.
[[nodiscard]] auto ArrayOperationsOf(UnitLowerer& lowerer, mir::TypeId type)
    -> std::vector<mir::TypeOperation>;

// How many positions a procedural continuous assignment may hold some of in a
// value of `type` (LRM 10.6.2): an integral value's bits, a structure's
// members' and a fixed-size unpacked array's elements' positions one part
// after another, the first part's first, and one for any other value, which is
// held whole.
[[nodiscard]] auto PositionCountOf(
    const mir::CompilationUnit& unit, mir::TypeId type) -> std::uint64_t;

// Whether `operation` is one asked of a whole value, which a type with
// operations of its own answers with its own body, rather than an operation of
// a container's on its elements.
[[nodiscard]] auto IsWholeValueOperation(support::ValueOperation operation)
    -> bool;

// `entry`'s question asked of a value, with `operands` after the value it is
// asked of: the type's own body where the type deciding it has its own
// operations -- `receiver`'s, or for a question asked of a type rather than of
// a value, the result's -- and the library's entry for every other value. The
// operands are the same either way, since such a type answers the question
// with the parameters any value does. Each is handed as it stands, a place or
// a value: the body borrows a place where it lies, as clang settles an
// operand's conversion once overload resolution chose the callee, and the
// library's entry takes each operand's value.
[[nodiscard]] auto BuildValueOperation(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::BuiltinFn entry, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId;

// Whether a bit of a value of `type` can be unknown, so that read for its truth
// or compared it can answer neither true nor false (LRM 11.4.5, 11.4.7).
[[nodiscard]] auto CarriesUnknowns(
    const mir::CompilationUnit& unit, mir::TypeId type) -> bool;

// The one-bit type of a truth or a comparison over values of `operands`, which
// is four-state exactly where one of them carries unknowns.
[[nodiscard]] auto OneBitAnswerType(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> operands)
    -> mir::TypeId;

// LRM 11.4.5 `==` or `!=` on `lhs` and `rhs`, answering at `result`: the
// operand type's own body where it has one, which takes each operand where it
// lies, and otherwise the operator on the two values.
[[nodiscard]] auto BuildEquality(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::ValueOperator comparison, mir::ExprId lhs, mir::ExprId rhs,
    mir::TypeId result) -> mir::ExprId;

// Whether two values of one type are the same bits, unknown bits included, as
// a machine boolean: the type's own comparison, whether it asks if a value
// changed or if two keys are one value.
[[nodiscard]] auto BuildBitIdentity(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId;

// LRM 11.4.5 `===`, a known bit.
[[nodiscard]] auto BuildCaseEquality(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId lhs,
    mir::ExprId rhs) -> mir::ExprId;

// Whether `value` matches `label` as a case statement of `keyword` compares
// them: a known bit, where logical equality would answer x (LRM 12.5). Which
// bits a form skips is the form's own business, so each form is a question of
// its own rather than an operator a target applies.
[[nodiscard]] auto BuildCaseComparison(
    const mir::CompilationUnit& unit, mir::Builder block,
    hir::CaseKeyword keyword, mir::ExprId value, mir::ExprId label)
    -> mir::ExprId;

// LRM 20.9 `$isunknown`, a known bit.
[[nodiscard]] auto BuildUnknownTest(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value)
    -> mir::ExprId;

// LRM 20.9 `$countbits` of `value` under the control set `control`, an `int`.
[[nodiscard]] auto BuildBitCount(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value,
    mir::ExprId control) -> mir::ExprId;

// LRM 20.6.2 `$bits` of what `value` holds, an `int`.
[[nodiscard]] auto BuildBitWidth(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
