#include "lyra/lowering/hir_to_mir/arm_chain.hpp"

#include <cstddef>
#include <expected>
#include <optional>
#include <utility>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/condition.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/stmt.hpp"

namespace lyra::lowering::hir_to_mir {

auto BuildArmChain(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    std::size_t arm_count, const ArmTest& test_of, const ArmBody& body_of,
    std::optional<mir::Block> fall_through) -> diag::Result<mir::StmtData> {
  const auto arm_at =
      [&](const WalkFrame& at, std::size_t arm,
          std::optional<mir::Block> otherwise) -> diag::Result<mir::IfStmt> {
    mir::Block& block = *at.current_block;
    auto held_or = test_of(at, arm);
    if (!held_or) return std::unexpected(std::move(held_or.error()));
    auto body_or = body_of(at, arm);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    std::optional<mir::BlockId> else_scope;
    if (otherwise.has_value()) {
      else_scope = block.child_scopes.Add(*std::move(otherwise));
    }
    return mir::IfStmt{
        .condition = ReduceToCondition(unit, block, *held_or),
        .then_scope = block.child_scopes.Add(std::move(*body_or)),
        .else_scope = else_scope};
  };

  // No arms is the ordinary shape of `case (x) default: ...`, whose only arm
  // the front end reports apart from the items.
  if (arm_count == 0) {
    if (!fall_through.has_value()) {
      return mir::EmptyStmt{};
    }
    return mir::BlockStmt{
        .scope =
            frame.current_block->child_scopes.Add(*std::move(fall_through))};
  }
  std::optional<mir::Block> tail = std::move(fall_through);
  for (std::size_t arm = arm_count; arm-- > 1;) {
    mir::Block level;
    auto arm_or = arm_at(frame.WithBlock(&level), arm, std::move(tail));
    if (!arm_or) return std::unexpected(std::move(arm_or.error()));
    level.AppendStmt(*std::move(arm_or));
    tail = std::move(level);
  }
  return arm_at(frame, 0, std::move(tail));
}

}  // namespace lyra::lowering::hir_to_mir
