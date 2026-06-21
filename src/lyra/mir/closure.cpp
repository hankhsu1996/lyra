#include "lyra/mir/closure.hpp"

#include <memory>

#include "lyra/mir/stmt.hpp"

namespace lyra::mir {

ClosureExpr::ClosureExpr() = default;
ClosureExpr::~ClosureExpr() = default;
ClosureExpr::ClosureExpr(ClosureExpr&&) noexcept = default;
auto ClosureExpr::operator=(ClosureExpr&&) noexcept -> ClosureExpr& = default;

ClosureExpr::ClosureExpr(const ClosureExpr& other)
    : captures(other.captures),
      params(other.params),
      body(
          other.body == nullptr ? nullptr
                                : std::make_unique<Block>(*other.body)) {
}

auto ClosureExpr::operator=(const ClosureExpr& other) -> ClosureExpr& {
  if (this != &other) {
    captures = other.captures;
    params = other.params;
    body =
        other.body == nullptr ? nullptr : std::make_unique<Block>(*other.body);
  }
  return *this;
}

}  // namespace lyra::mir
