#pragma once

#include <map>
#include <set>
#include <span>
#include <utility>
#include <vector>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

// What evaluating some operands may do, as far as the order of an earlier or a
// later one can tell: whether one has side effects, whether what one evaluates
// to is read from storage other than the frame's own locals, and which of those
// locals it reads and which it mutates. A local whose address nothing takes is
// reached only by an evaluation naming it, so naming is all these record of it.
struct OperandEffects {
  bool side_effects = false;
  bool reads_other_storage = false;
  std::set<LocalId> locals_read;
  std::set<LocalId> locals_mutated;
};

// What the order of a node's evaluation asks of one of its operands.
struct OperandOrder {
  // Whether it has to run ahead of what follows it, held in a temporary.
  bool needs_temporary = false;
  // What runs after it, among the node's operands and after the node.
  OperandEffects after;
};

// What the order of one body's evaluations asks of its nodes, decided by what
// kind of node each is and never by what a call does.
class EvaluationOrder {
 public:
  // `types` holds every type a node asked about is of, and `body` is the body
  // every node asked about is part of.
  EvaluationOrder(const TypePool& types, const Block& body);

  // Which of `operands`, the operands of the node `node` of `block` in the
  // order it evaluates them, a target could run in the other order to another
  // answer, with `after` running after the node: one whose value what runs
  // after it could change, and one with side effects that could change what a
  // later one evaluates to. Each such operand needs a temporary.
  auto OperandsNeedingTemporaries(
      const Block& block, const ExprData& node,
      std::span<const ExprId> operands, const OperandEffects& after)
      -> std::vector<OperandOrder>;

 private:
  // What evaluating the node `id` of `block` may do. It has side effects
  // unless it is a constant, a read, a reference formed, an operation over
  // values or a trivial call, made of nodes without; a write, a suspension and
  // every other call have them. What it evaluates to is read from other storage
  // where it reads any but a local whose address nothing takes, or is a call
  // that is not trivial, a write, a suspension, a choice or a run of steps.
  auto EffectsOf(const Block& block, ExprId id) -> const OperandEffects&;

  const TypePool* types_;
  // The locals of the body some evaluation borrows: whose address something
  // takes, so that what does not name them can change them.
  std::set<LocalId> borrowed_;
  std::map<std::pair<const Block*, ExprId>, OperandEffects> effects_;
};

// Whether `operand` of `node` is unsequenced among the node's other operands,
// so a target chooses its position: every one is but an arm of a choice, which
// runs after what chooses it, on every target.
[[nodiscard]] auto IsUnsequenced(const ExprData& node, ExprId operand) -> bool;

}  // namespace lyra::mir
