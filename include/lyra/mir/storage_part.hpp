#pragma once

#include <optional>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

// How a call names the part it reaches, and nothing for a call that reaches
// none. The entry's own declaration says, so reading and writing one part
// answer alike.
auto SelectionOf(const CallExpr& call) -> std::optional<support::PartSelection>;

// What a write landing on `target` reaches the part it lands on through, where
// `target` selects a part of a whole, and nothing where `target` names where
// it lands itself. A selection is reached through its whole, so a chain of
// them is followed to the storage the write lands in.
auto SelectedFrom(const Block& block, ExprId target) -> std::optional<ExprId>;

// Whether the value the node `id` of `block` stands for already lies in
// storage -- a place, a read of one, or what an execution completed with in
// the storage its awaiter handed it -- rather than being made here.
auto ValueLiesInStorage(const Block& block, ExprId id) -> bool;

}  // namespace lyra::mir
