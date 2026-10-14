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

// How an expression of `block` names a part of a value that holds its parts as
// storage of their own (LRM 7.2, 7.4), where it names one.
auto StorageSelection(const TypePool& types, const Block& block, ExprId id)
    -> std::optional<support::PartSelection>;

// The wrapper a read of what one holds is made on, where `expr` is such a
// read: the value it answers lies in the storage the wrapper represents.
auto WrapperRead(const Expr& expr) -> std::optional<ExprId>;

// What a write landing on `target` reaches the part it lands on through, where
// `target` selects a part of a whole, and nothing where `target` names where
// it lands itself. A selection is reached through its whole, so a chain of
// them is followed to the storage the write lands in.
auto SelectedFrom(const Block& block, ExprId target) -> std::optional<ExprId>;

// Whether an expression of `block`, read as a value, reads one that lies
// somewhere -- a name, a dereference, a field, what a wrapper holds, what an
// execution completed with -- rather than making one, as rustc's place
// expressions are read where they are and every other is made.
auto IsPlaceExpression(const Block& block, ExprId id) -> bool;

}  // namespace lyra::mir
