#pragma once

#include <optional>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

// The operand an expression reaches an object through, and the class whatever
// reaches through it views the object as.
struct ObjectOperand {
  ExprId operand;
  support::ObjectView view;
};

// The operand `expr`, a node of `block`, reaches an object through, where that
// is a class handle, and the class whatever reaches through it views the object
// as: the pointer a dereference reads through, which views it as the handle's
// class, or the first argument of a library entry acting on an object or of a
// write opened into one (LRM 8.4).
[[nodiscard]] auto HandleReached(
    const TypePool& types, const Block& block, const Expr& expr)
    -> std::optional<ObjectOperand>;

}  // namespace lyra::mir
