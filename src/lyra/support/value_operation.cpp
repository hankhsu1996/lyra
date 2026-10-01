#include "lyra/support/value_operation.hpp"

#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::support {

namespace {

auto OperatorToken(ValueOperator op) -> std::string_view {
  switch (op) {
    case ValueOperator::kEquality:
      return "==";
    case ValueOperator::kInequality:
      return "!=";
  }
  throw InternalError("value operation: unknown operator");
}

}  // namespace

auto ReachesAReceiver(ValueOperation operation) -> bool {
  return std::visit(
      Overloaded{
          [](ValueOperator) { return true; },
          [](BuiltinFn fn) {
            return !std::holds_alternative<StaticFactory>(
                RuntimeEntryOf(fn).declaration);
          }},
      operation);
}

auto ValueOperationName(ValueOperation operation) -> std::string_view {
  return std::visit(
      Overloaded{
          [](ValueOperator op) { return OperatorToken(op); },
          [](BuiltinFn fn) { return RuntimeEntryOf(fn).name; }},
      operation);
}

}  // namespace lyra::support
