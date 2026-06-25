#include "lyra/mir/expr.hpp"

#include <variant>

#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

auto IsMutatingCallee(const Callee& callee) -> bool {
  const auto* direct = std::get_if<Direct>(&callee);
  if (direct == nullptr) return false;
  const auto* id = std::get_if<support::BuiltinFn>(&direct->target);
  return id != nullptr && support::IsMutatingBuiltinFn(*id);
}

auto IsContainerAccessCallee(const Callee& callee) -> bool {
  const auto* direct = std::get_if<Direct>(&callee);
  if (direct == nullptr) return false;
  const auto* id = std::get_if<support::BuiltinFn>(&direct->target);
  return id != nullptr && support::IsContainerAccessFn(*id);
}

}  // namespace lyra::mir
