#pragma once

#include <optional>
#include <vector>

#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/builder.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::lowering::hir_to_mir {

// The body answering `source` at an array (LRM 7.4, 7.5, 7.10), built from
// the declaration MIR states for it: an item of a library trait the array's
// type implements, one program over the elements whatever kind of array, or
// one of the kind's own methods or an item of a language trait it implements,
// each built as that kind's table says.
//
// A dynamic array's core decides where its elements lie and when each ends; a
// queue's own bodies decide both, over storage its recycler hands out. A body
// decides every step of the operation and who holds each element at each one.
[[nodiscard]] auto BuildArrayOperationBody(
    const mir::CompilationUnit& unit, const mir::TypeOperationSource& source)
    -> mir::CallableCode;

// The call of `method` on `receiver` as the container's own operation, where
// MIR declares the method an operation of the receiver's type -- of a
// fixed-size array's, a dynamic array's or a queue's interface: the body is
// handed the receiver and each operand by reference, the receiver one it may
// write through where the method changes it, among them where an element of a
// run-time-sized array lies, to read it or to write it. Nothing for a receiver
// whose type declares no such operation.
[[nodiscard]] auto ContainerOperationCall(
    const mir::CompilationUnit& unit, mir::Builder block,
    support::ValueOperation method, mir::ExprId receiver,
    const std::vector<mir::ExprId>& operands, mir::TypeId result_type)
    -> std::optional<mir::Expr>;

// A run-time-sized container of `type` holding `count` replications of
// `items`, each already of the container's element type, in order (LRM
// 10.9.1): a dynamic array's own `new[]` of that many elements, then each
// written in turn; and a queue of none with room for that many, each built in
// storage its core hands out, then held to its bound once (LRM 7.10.5). The
// items are evaluated once, before the count, as they are written, and `count`,
// a machine count, is read once.
[[nodiscard]] auto BuildContainerFromList(
    const mir::CompilationUnit& unit, const WalkFrame& frame, mir::TypeId type,
    std::vector<mir::ExprId> items, mir::ExprId count) -> mir::Expr;

// LRM 7.5.1 `new[count]` and `new[count](source)`: a dynamic array of `type`
// holding `count` elements, a machine count, each the element default, or the
// one at the same position of `source`, an array of `type`, where it has one.
[[nodiscard]] auto BuildDynamicArrayNew(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type,
    mir::ExprId count, std::optional<mir::ExprId> source = std::nullopt)
    -> mir::Expr;

// One part of a random variable walked for a call of `randomize()` (LRM
// 18.4), the part of `type` the reference `part` names, for the problem
// `problem`: walked by its own type's code where the type implements the walk,
// and otherwise named to the problem with its type's record.
[[nodiscard]] auto RandomWalkStep(
    const mir::CompilationUnit& unit, mir::Builder block, mir::ExprId part,
    mir::ExprId problem, mir::TypeId type) -> mir::Expr;

// The record of `type` the library is handed where it is told what a value
// is.
[[nodiscard]] auto TypeRecordOf(
    const mir::CompilationUnit& unit, mir::Builder block, mir::TypeId type)
    -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
