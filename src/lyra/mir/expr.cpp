#include "lyra/mir/expr.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

auto IsMutatingCallee(const Callee& callee) -> bool {
  return std::visit(
      Overloaded{
          [](const BuiltinFnCallee& bf) {
            return support::IsMutatingBuiltinFn(bf.id);
          },
          [](const auto&) { return false; },
      },
      callee);
}

auto IsContainerAccessCallee(const Callee& callee) -> bool {
  return std::visit(
      Overloaded{
          [](const BuiltinFnCallee& bf) {
            return support::IsContainerAccessFn(bf.id);
          },
          [](const auto&) { return false; },
      },
      callee);
}

}  // namespace lyra::mir
