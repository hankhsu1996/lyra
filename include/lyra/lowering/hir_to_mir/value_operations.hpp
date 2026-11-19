#pragma once

#include <optional>
#include <vector>

#include "lyra/hir/case_keyword.hpp"
#include "lyra/hir/type_definition.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

class UnitLowerer;

// One operation on a whole value the struct or the union a record names has,
// with the body of the method answering it.
struct RecordMethodBody {
  support::ValueOperation answers;
  mir::CallableCode code;
};

// Every operation the interface MIR states for `type`, the record `record`
// names, has, each with its body, in the interface's order: a struct's (LRM
// 7.2) read off each member in turn, the value its declaration `structure`
// gives a variable of it to start as among them; an untagged union's (LRM 7.3)
// off its first member read through its storage but for what the storage
// answers itself -- whether it changed, its positions; and a tagged union's off
// the member its tag names. `structure` is nothing for a union. A member that
// is itself a record is reached by the methods the record naming it declares,
// which are declared before any record's bodies are built.
[[nodiscard]] auto RecordMethodBodies(
    UnitLowerer& lowerer, const hir::UnpackedStructDefinition* structure,
    mir::RecordId record, mir::TypeId type) -> std::vector<RecordMethodBody>;

// How a body answering an operation of a type takes each value it is asked
// about, the value itself and every other operand: by a read-only reference,
// as Rust's comparison operators borrow theirs, so a value is read where it
// lies rather than copied in.
[[nodiscard]] auto ReferenceTo(
    const mir::CompilationUnit& unit, mir::TypeId type) -> mir::TypeId;

// The body of the operation `source` names of a tuple, each component's in
// turn, which the tuple's interface declares.
[[nodiscard]] auto BuildTupleOperationBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> mir::CallableCode;

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

// An item of a trait asked of a value, with `operands` after the value it is
// asked of: the body the type deciding it implements it with, where that type
// is one the compiler builds operations of -- `receiver`'s, or for a question
// asked of a type rather than of a value, the result's -- and the library's
// entry naming the item for every other type. The operands are the same either
// way, since such a type answers the question with the parameters any value
// does. Each is handed as it stands, a place or a value: the body borrows a
// place where it lies, as clang settles an operand's conversion once overload
// resolution chose the callee, and the library's entry takes each operand's
// value.
[[nodiscard]] auto BuildValueOperation(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::LibraryAnsweredItem item, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId;

// The library's entry `entry` called on `receiver` with `operands`, each taken
// as its value, where no type's own body answers it.
[[nodiscard]] auto BuildLibraryCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::BuiltinFn entry, std::optional<mir::ExprId> receiver,
    std::vector<mir::ExprId> operands, mir::TypeId result) -> mir::ExprId;

// LRM 11.4.5 `==`, `!=` or `===` on `lhs` and `rhs`, answering at `result`:
// the operand type's own body where it has one, which takes each operand
// where it lies, and otherwise the operator on the two values, or the
// library's case equality.
[[nodiscard]] auto BuildEquality(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::EqualityItem item, mir::ExprId lhs, mir::ExprId rhs,
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
