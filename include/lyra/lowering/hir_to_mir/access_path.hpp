#pragma once

#include <cstdint>
#include <optional>
#include <variant>
#include <vector>

#include "lyra/base/component_index.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

// What a step into a member of a tagged packed union requires of the union
// (LRM 7.3.2, 11.9): its tag, the `tag_bits` most significant bits of the
// union's vector, holds the declaration-order position of `member`.
//
//   typedef union tagged packed { logic [3:0] a; logic [3:0] b; } u_t;
//   u.b    a step valid only while the one tag bit of `u` is 1
struct RequiredTag {
  std::uint32_t tag_bits = 0;
  base::ComponentIndex member = {};
};

// One step of a descent into a value: the entry that answers with the part's
// value, the entry that answers with the part itself, and the operands both of
// them take beside it.
//
// Which entries those are follows from the type of the value the step descends
// into, and this is the layer that holds that type, so it is settled here and
// nothing below chooses. Two entries rather than one read two ways, because a
// consumer that had to work out from the position which one a step meant could
// work it out differently.
struct DescentStep {
  support::BuiltinFn value_entry;
  support::BuiltinFn part_entry;
  // The part this step names, where naming it is what the step does rather than
  // a value it is handed. It travels with the entries for the same reason it
  // travels with a callee: the part has a type of its own.
  std::optional<base::ComponentIndex> position;
  // Where the part is, as values the program computes: an index, a start
  // position, a key. A deferred write freezes these where the statement is
  // reached (LRM 10.4.2).
  std::vector<mir::ExprId> operands;
  // How many parts a run takes, where the step reaches a run. The part's type
  // fixes it, so it is a number rather than a value the program computes, and
  // nothing freezes it.
  std::optional<std::uint64_t> count;
  mir::TypeId part_type;
  // What the value stepped into has to hold for the step to be valid, where
  // the part is a member of a tagged union. Reading the part and writing it
  // each check it their own way, and naming the part checks nothing.
  std::optional<RequiredTag> required_tag = std::nullopt;
};

// What a call realizing `step` is handed beside its receiver: the step's
// operands, then its count as a machine count where it has one.
[[nodiscard]] auto StepArguments(
    const mir::CompilationUnit& unit, mir::Block& block,
    const DescentStep& step) -> std::vector<mir::ExprId>;

// A part of a value named from its owner: the place that owns the whole value,
// and the descent that reaches the part. A write lands in it, a reference is
// formed over it, a wait watches it and a join of nets covers it, and each is
// handed this one statement of which part it is. A path that names no part
// descends nowhere and is its owner's own place.
//
// Where the owner is a property of an object (LRM 8.4), `object` is that
// object, as the address its members are reached through. A write to the
// property is opened on the object, and a reference to it carries the object,
// which is what tells it that it was written (LRM 9.4.2); a variable's own
// storage reports a write itself and has none.
//
// This is the lowering's own shape and reaches no layer below. What MIR carries
// is what the descent lowers to -- a run of ordinary calls, each naming its own
// entry, composed through the receiver -- because a consumer that met the
// descent itself would have to decide which operation each step is, which is
// the decision this layer is here to make.
struct AccessPath {
  mir::ExprId owner;
  std::vector<DescentStep> descent;
  std::optional<mir::ExprId> object = std::nullopt;
};

// The same path one step deeper. This is the only thing that builds a descent,
// so the path gains exactly one step per level of the source's own nesting and
// the owner is whatever the peel reached that was not a descent.
[[nodiscard]] auto DescendInto(AccessPath base, DescentStep step) -> AccessPath;

// The type of the value a further step would descend into: the part the descent
// has reached so far, or what the owner's place holds where it has reached
// none. A step asks this to settle which entries realize it.
[[nodiscard]] auto PathValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const AccessPath& path) -> mir::TypeId;

// The place the path designates: the owner's own storage, then one reaching
// call per step. Storing into the result writes the part, and applying a method
// that changes its receiver to it changes the part, because a place is what
// both take.
//
// An owner that is a capability wrapper is opened for a write, since asking one
// which storage it stands for is an operation on it. The steps into parts that
// are storage of their own are then taken within the write in progress,
// starting from the whole of what it designates, which is how it learns what
// forming each did, and the place is where the last of them is dereferenced.
// An owner that is a property of an object is reached through a write opened on
// the object.
[[nodiscard]] auto PathPlace(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId;

// The path as a reference (LRM 13.5.2): a reference to the whole of what the
// owner holds, then one step per part, each taken on the reference before it.
// What a reference to a part belongs to travels with it, so a write through it
// is a write of the owner at the moment it lands, however long the reference is
// held.
[[nodiscard]] auto PathReference(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId;

// The read `step` takes from the value `receiver` names: its value entry,
// answering at the part's type.
[[nodiscard]] auto StepRead(
    const mir::CompilationUnit& unit, mir::Block& block,
    const DescentStep& step, mir::ExprId receiver) -> mir::Expr;

// What a step's read answered with, as a value a consumer may keep. A packed
// part is answered as a view of the value it is a part of, so it is taken as a
// value of its own (Rust's `&[T]::to_owned() -> Vec<T>` pattern); any other
// part already is one.
[[nodiscard]] auto OwnedValue(
    const mir::CompilationUnit& unit, mir::Block& block, mir::Expr read)
    -> mir::Expr;

// The type a read of a part of `source_type` answers at, where the part is
// declared `part_type`. LRM 11.8.1: a part-select is unsigned regardless of the
// operands, and its state domain follows the value it selects from, so a member
// of a packed aggregate (LRM 7.2.1, selected as a part-select of the
// aggregate's vector) is produced with the member's dimensions, unsigned, in
// the aggregate's state domain. The member's declared signedness and, for a
// 2-state member of a 4-state aggregate, its narrower state domain are reached
// by an explicit conversion of what was read. A part of a value that is not
// packed is read at its declared type.
[[nodiscard]] auto PartSelectNaturalType(
    mir::CompilationUnit& unit, mir::TypeId source_type, mir::TypeId part_type)
    -> mir::TypeId;

// The path as an expression yielding the part's value, for a caller that reads
// it and writes nothing. A packed part is yielded as the view its step answers
// with.
[[nodiscard]] auto PathValue(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId;

// The path's value as one a consumer may keep, at the type the part is declared
// with: a packed part is read at the type the read answers at, taken as a value
// of its own, and brought to its declared type.
[[nodiscard]] auto PathOwnedValue(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> mir::ExprId;

// The place `place` names, with whatever is computed on the way to it evaluated
// here, once: a pointer or a handle the place is reached through is bound to a
// local of the enclosing body, and the place is formed over that, so it may
// stand at several places. A place is a name, or a field or a dereference over
// what it is reached through; a value a computation yields is no place and is
// refused.
[[nodiscard]] auto SettledPlace(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId place)
    -> mir::ExprId;

// The same path with everything it computes evaluated here, once: the way to
// its owner, the object a property belongs to, and every operand of its steps.
// A path describes nodes that are evaluated wherever it is taken, so one taken
// at more than one place is settled first.
[[nodiscard]] auto Settled(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> AccessPath;

// A part a construct reads at several places: where its owner lies and the
// descent to it, neither evaluating anything, and the block their nodes are
// named in. Every read of it reaches the same part. A read asks nothing of the
// object a property belongs to, so none is named.
struct SettledPath {
  const mir::Block* named_in = nullptr;
  mir::ExprId owner;
  std::vector<DescentStep> descent;
};

// `path` settled in the frame's block, for a construct that reads through it
// and writes nothing.
[[nodiscard]] auto SettledForRead(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> SettledPath;

// `settled` as a path whose nodes are named in `to`. A node belongs to the
// block it was added to, so a block nested under the one a path was settled in
// -- a loop's body, a run of steps -- names the path's nodes afresh to read
// through it; they evaluate nothing, so the path named again reaches the same
// part.
[[nodiscard]] auto NamedIn(const SettledPath& settled, mir::Block& to)
    -> AccessPath;

// An operand a construct reads and then writes: the place the write lands in,
// and the value it holds going in.
struct ReadThenWritten {
  AccessPath place;
  mir::ExprId incoming;
};

// `place` as an operand that is read and then written (LRM 13.5 `inout`): the
// path is settled here, and the value going in is read through it at the type
// the part is declared with, so the source's one operand is evaluated once
// however far apart the read and the write stand.
[[nodiscard]] auto ReadThenWrite(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath place)
    -> ReadThenWritten;

// Which bits of the owner's packed value a path names: the lowest bit, counted
// from the value's least significant bit in the position type, and how many
// bits the part spans.
struct PathRun {
  mir::ExprId first;
  std::uint64_t width = 0;
};

// The run a path names within its owner (LRM 7.2.1, 11.5.1). Every step into a
// packed value is a run of it, so the part starts at the sum of where the steps
// start, and that sum stays an expression because a step's position may be a
// value the program or a construction supplies.
[[nodiscard]] auto RunWithinOwner(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path)
    -> PathRun;

// What an assignment applies to the value its target holds (LRM 11.4.1): an
// operator the target language applies to two values of one type, or the
// library entry that applies one to the value a place holds. Which of the two
// an operator is follows from the operator alone, and this layer is where an
// assignment is built, so it is settled here and no consumer of the assignment
// classifies anything.
using CompoundOperation = std::variant<mir::BinaryOp, support::BuiltinFn>;

// Builds `lhs = rhs` or `lhs op= rhs` against the part `path` names. Three
// shapes come out of it, each an ordinary MIR node with nothing left to decide:
// replacing the whole of what a capability wrapper holds acts on the wrapper,
// so it is a call taking the wrapper as its destination; applying a
// library-performed operator is a call on the place the path designates, which
// updates what that place holds; every other write is a store into that place,
// compound or not.
[[nodiscard]] auto BuildStoreExpr(
    mir::CompilationUnit& unit, mir::Block& block, const AccessPath& path,
    mir::ExprId rhs_id, std::optional<CompoundOperation> compound_op,
    mir::TypeId result_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
