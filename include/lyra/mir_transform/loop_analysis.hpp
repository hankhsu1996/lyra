#pragma once

#include <functional>
#include <map>
#include <optional>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

// What a rewrite of a loop asks of the values the loop's passes compute, in
// the terms LLVM's loop analyses answer it: which object a place is part of
// and whether two may overlap (`getUnderlyingObject`, alias analysis), which
// values are the same on every pass (`Loop::isLoopInvariant`), which move with
// the loop's induction variable (`ScalarEvolution`'s add recurrences), and how
// a value a pass computes is stated again ahead of the loop (`SCEVExpander`).
namespace lyra::mir_transform {

// A node and the block naming it, as an analysis follows operands across the
// blocks a loop's statements hold.
struct NodeRef {
  const mir::Block* block = nullptr;
  mir::ExprId id;

  [[nodiscard]] auto Get() const -> const mir::Expr& {
    return block->exprs.Get(id);
  }
  // Another node of the same block.
  [[nodiscard]] auto At(mir::ExprId other) const -> NodeRef {
    return {.block = block, .id = other};
  }

  auto operator==(const NodeRef&) const -> bool = default;
};

// The call `expr` makes of the library entry `entry`, if that is what it is.
[[nodiscard]] auto BuiltinCallOf(
    const mir::Expr& expr, support::BuiltinFn entry) -> const mir::CallExpr*;

// A member taken on the way to a place, and whether it lies where its sibling
// members do, as a union's members do (LRM 7.3).
struct MemberStep {
  mir::FieldRef field;
  bool overlays_siblings = false;

  auto operator==(const MemberStep&) const -> bool = default;
};

// The object a place is part of, as far as telling two places apart needs, as
// LLVM's `getUnderlyingObject` names what a pointer is based on: the local it
// is reached from, whether it is what that local's pointer refers to rather
// than the local, and the members taken from there.
struct UnderlyingObject {
  mir::LocalId root;
  bool through_pointer = false;
  std::vector<MemberStep> members;

  auto operator==(const UnderlyingObject&) const -> bool = default;
};

// The object `place` is part of: through a member, through bits, through what
// a cell holds and through a write in progress opened on it; none where it is
// reached otherwise.
[[nodiscard]] auto UnderlyingObjectOf(
    const mir::CompilationUnit& unit, NodeRef place)
    -> std::optional<UnderlyingObject>;

// Whether two places may share storage. Two locals never do, nor two members
// of a structure, while two members of a union always do. The object a body
// is entered on holds none of the body's locals, while any other pointer may
// refer to anything.
[[nodiscard]] auto MayAlias(
    const UnderlyingObject& a, const UnderlyingObject& b,
    std::optional<mir::LocalId> receiver) -> bool;

// What each local a loop declares is initialized with.
using LoopLocals = std::map<mir::LocalId, NodeRef>;

// What initializes the local of `locals` that the read `node` reads, where it
// reads one.
[[nodiscard]] auto LoopLocalRead(NodeRef node, const LoopLocals& locals)
    -> std::optional<NodeRef>;

// A loop's induction variable: the object it lies in, and whether a cell lying
// there holds it, read and written through the cell's own operations.
struct InductionVariable {
  UnderlyingObject object;
  bool in_cell = false;

  auto operator==(const InductionVariable&) const -> bool = default;
};

// The variable the read `node` reads, as an induction variable would be read.
[[nodiscard]] auto InductionVariableReadBy(
    const mir::CompilationUnit& unit, NodeRef node)
    -> std::optional<InductionVariable>;

// A value moving with the induction variable, as ScalarEvolution's add
// recurrence `{start,+,1}` does: the variable plus what every pass computes
// alike, and the integral type that sum was last taken at before it became a
// position, where it may wrap (`SCEVWrapPredicate`).
struct AddRecurrence {
  std::optional<mir::IntegralType> wraps_at;
};

// What a loop's nodes compute as its passes go, as LLVM's ScalarEvolution
// answers of a loop's values: whether a value is the same on every pass, and
// how one moves with the induction variable. Nothing the loop reads is written
// in it but the induction variable, which a rewrite establishes first.
class LoopEvolution {
 public:
  LoopEvolution(
      const mir::CompilationUnit& unit, std::optional<mir::LocalId> receiver,
      const LoopLocals& locals, InductionVariable induction);

  [[nodiscard]] auto ReadsInductionVariable(NodeRef node) const -> bool;
  // Whether the value `node` computes is the same on every pass: it reads
  // nothing that may be the induction variable's.
  [[nodiscard]] auto IsLoopInvariant(NodeRef node) const -> bool;
  // How `node`, a position, moves with the induction variable, or nothing
  // where it is no add recurrence of it.
  [[nodiscard]] auto AddRecurrenceOf(NodeRef node) const
      -> std::optional<AddRecurrence>;

 private:
  [[nodiscard]] auto ReadsBesideInductionVariable(NodeRef place) const -> bool;
  [[nodiscard]] auto IsInvariantIn(const mir::Block& block) const -> bool;

  const mir::CompilationUnit* unit_;
  std::optional<mir::LocalId> receiver_;
  const LoopLocals* locals_;
  InductionVariable induction_;
};

// Whether `node` and everything it takes can be stated again in another
// block, node for node, as `SCEVExpander::isSafeToExpand` asks: no block of
// steps, no choice among them, and no local the loop declares but through a
// read of the value it holds.
[[nodiscard]] auto IsSafeToExpand(NodeRef node, const LoopLocals& locals)
    -> bool;

// `node` stated again in the block `into` builds, node for node, a read of a
// local the loop declares stated as what initializes it, as
// `SCEVExpander::expandCodeFor` states a loop's value ahead of the loop.
using Into = std::function<mir::ExprId(mir::Expr)>;
auto ExpandCode(NodeRef node, const Into& into, const LoopLocals& locals)
    -> mir::ExprId;

}  // namespace lyra::mir_transform
