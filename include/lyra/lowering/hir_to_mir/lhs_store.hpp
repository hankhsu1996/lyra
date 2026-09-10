#pragma once

#include <optional>
#include <vector>

#include "lyra/mir/binary_op.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::lowering::hir_to_mir {

// One step of a write target's descent: the entry that answers with the part's
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
  std::vector<mir::ExprId> operands;
  mir::TypeId part_type;
};

// Where a write lands: the place that owns the whole value, and the descent
// that reaches the part written. A target that designates no part descends
// nowhere and is its own place.
//
// This is the lowering's own shape and reaches no layer below. What MIR carries
// is what the descent lowers to -- a run of ordinary calls, each naming its own
// entry, composed through the receiver -- because a consumer that met the
// descent itself would have to decide which operation each step is, which is
// the decision this layer is here to make.
struct WriteTarget {
  mir::ExprId owner;
  std::vector<DescentStep> descent;
};

// The same target one step deeper. This is the only thing that builds a
// descent, so the path gains exactly one step per level of the source's own
// nesting and the owner is whatever the peel reached that was not a descent.
[[nodiscard]] auto DescendInto(WriteTarget base, DescentStep step)
    -> WriteTarget;

// The type of the value a further step would descend into: the part the descent
// has reached so far, or what the owner's place holds where it has reached
// none. A step asks this to settle which entries realize it.
[[nodiscard]] auto TargetValueType(
    const mir::CompilationUnit& unit, const mir::Block& block,
    const WriteTarget& target) -> mir::TypeId;

// The place a write reaches through a capability wrapper: asking one which
// storage it currently stands for is an operation on it, so the answer is a
// call and the storage is named by dereferencing it. A place that is not a
// wrapper already names its own storage.
[[nodiscard]] auto OpenedPlace(
    mir::CompilationUnit& unit, mir::Block& block, mir::ExprId place)
    -> mir::ExprId;

// The place the target designates: the owner's own storage, then one reaching
// call per step. Storing into the result writes the part, reading it reads the
// part, and handing it to a by-reference formal lends it, because a place is
// what all three take.
[[nodiscard]] auto TargetPlace(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target)
    -> mir::ExprId;

// The target as an expression yielding the part's value, for a caller that
// reads it and writes nothing.
[[nodiscard]] auto ReadTargetValue(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target)
    -> mir::ExprId;

// Builds `lhs = rhs` or `lhs op= rhs` against the target. Replacing the whole
// of what a capability wrapper holds acts on the wrapper, so it is a call
// taking the wrapper as its destination; every other write is a store into the
// place the target designates, which a compound write reads and writes through
// once.
[[nodiscard]] auto BuildStoreExpr(
    mir::CompilationUnit& unit, mir::Block& block, const WriteTarget& target,
    mir::ExprId rhs_id, std::optional<mir::BinaryOp> compound_op,
    mir::TypeId result_type) -> mir::Expr;

}  // namespace lyra::lowering::hir_to_mir
