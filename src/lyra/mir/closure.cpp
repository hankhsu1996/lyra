#include "lyra/mir/closure.hpp"

#include <memory>

#include "lyra/mir/callable_code.hpp"

namespace lyra::mir {

ClosureExpr::ClosureExpr() = default;
ClosureExpr::~ClosureExpr() = default;
ClosureExpr::ClosureExpr(ClosureExpr&&) noexcept = default;
auto ClosureExpr::operator=(ClosureExpr&&) noexcept -> ClosureExpr& = default;

ClosureExpr::ClosureExpr(const ClosureExpr& other)
    : code(
          other.code == nullptr ? nullptr
                                : std::make_unique<CallableCode>(*other.code)),
      environment(other.environment) {
}

auto ClosureExpr::operator=(const ClosureExpr& other) -> ClosureExpr& {
  if (this != &other) {
    code = other.code == nullptr ? nullptr
                                 : std::make_unique<CallableCode>(*other.code);
    environment = other.environment;
  }
  return *this;
}

}  // namespace lyra::mir
