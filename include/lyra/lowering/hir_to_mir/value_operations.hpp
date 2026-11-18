#pragma once

#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// The operations on a whole value the struct or the union `record` names has,
// over values of `type`, each answered by a method of the record: what it
// answers is read off its members, a struct's (LRM 7.2) each member in turn,
// an untagged union's (LRM 7.3) its first member read through its storage but
// for what the storage answers itself -- whether it changed, its positions, a
// fill -- and a tagged union's the member its tag names, which has no stream.
// Which there are follows from the members alone, so the unit declaring the
// record and every unit reaching it list the same methods. `structure` is the
// record's declaration where it is a struct, and nothing for a union.
[[nodiscard]] auto RecordOperationsOf(
    UnitLowerer& lowerer, const hir::UnpackedStructDefinition* structure,
    mir::RecordId record, mir::TypeId type)
    -> std::vector<support::ValueOperation>;

// One of those operations, with the body of the method answering it.
struct RecordMethodBody {
  support::ValueOperation answers;
  mir::CallableCode code;
};

// Every one of those, each with its body, in the same order, the value the
// struct's declaration gives a variable of it to start as among them. A member
// that is itself a record is reached by the methods the record naming it
// declares, which are declared before any record's bodies are built.
[[nodiscard]] auto RecordMethodBodies(
    UnitLowerer& lowerer, const hir::UnpackedStructDefinition* structure,
    mir::RecordId record, mir::TypeId type) -> std::vector<RecordMethodBody>;

// Whether the net types' tables resolve a net of `type` position by position
// (LRM 6.6.1, 6.6.3, 6.6.7). They are tables over 4-state bits, and a net
// holds its drivers' contributions as 4-state positions whatever its type,
// showing the result at its type, where a 2-state bit takes x and z as 0 (LRM
// 6.11.2). So a net resolves where every bit it has is a bit of an integral
// value: an integral type of either state, and a fixed-size unpacked array,
// structure or untagged union of such. A net of any other type -- one holding
// a real, a string or a handle, which have no bits a table reads -- has no
// resolution function and is unresolved (LRM 6.6.7).
[[nodiscard]] auto NetResolves(
    const mir::CompilationUnit& unit, mir::TypeId type) -> bool;

// How a body answering an operation of a type takes each value it is asked
// about, the value itself and every other operand: by a read-only reference,
// as Rust's comparison operators borrow theirs, so a value is read where it
// lies rather than copied in.
[[nodiscard]] auto ReferenceTo(
    const mir::CompilationUnit& unit, mir::TypeId type) -> mir::TypeId;

// The body answering `answers` for a value of `type`, a type no declaration
// states the operations of, or nothing where `type` has no such operation: a
// fixed-size unpacked array's are each element's in turn (LRM 7.4.2), and a
// tuple's each component's in turn, where every component has it.
[[nodiscard]] auto BuildStructuralOperationBody(
    const mir::CompilationUnit& unit, mir::TypeId type,
    support::ValueOperation answers) -> std::optional<mir::CallableCode>;

// How many positions a value of `type` has, which a procedural continuous
// assignment may hold some of (LRM 10.6.2) and a net resolves each of (LRM
// 6.6.7): an integral value's bits; a real's IEEE 754 bits (LRM 6.12); a
// product's components' and a fixed-size unpacked array's elements' positions
// one part after another, the first part's first; an untagged union's
// storage's, which each member is a view of from the first (LRM 7.3); a tagged
// union's widest member's and then its tag's (LRM 7.3.2); and one for any
// other value, which is held whole.
[[nodiscard]] auto PositionCountOf(
    const mir::CompilationUnit& unit, mir::TypeId type) -> std::uint64_t;

// How many positions the widest of `members` has, which every member of a
// union lies within from the first (LRM 7.3, 7.3.2).
[[nodiscard]] auto WidestPositionCountOf(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> members)
    -> std::uint64_t;

// The integral type holding every position of a value of `type`, the first
// least significant: what a driver of a part of an unpacked type gives, and
// what a type's positions operations take and answer.
[[nodiscard]] auto PositionsTypeOf(
    const mir::CompilationUnit& unit, mir::TypeId type) -> mir::TypeId;

// `value`'s positions as one integral value of `positions_type`, and the value
// of `type` an integral value of its positions holds: an integral value's bits
// as they are, a real's IEEE 754 bits (LRM 6.12, 20.5), and any other value's
// by its type's own positions operations. Together they make a member of an
// untagged union a view of the union's storage (LRM 7.3).
[[nodiscard]] auto BuildToPositions(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId value,
    mir::TypeId positions_type) -> mir::ExprId;
[[nodiscard]] auto BuildFromPositions(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId positions,
    mir::TypeId type) -> mir::Expr;

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
