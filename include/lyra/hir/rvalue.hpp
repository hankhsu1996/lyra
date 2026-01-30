#pragma once

#include <variant>

#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// RValue can be either an expression (computes a value) or a pattern (fill
// directive)
struct RValue {
  std::variant<ExpressionId, PatternId> value;

  static auto Expression(ExpressionId id) -> RValue {
    return RValue{.value = id};
  }

  static auto Pattern(PatternId id) -> RValue {
    return RValue{.value = id};
  }

  [[nodiscard]] auto IsExpression() const -> bool {
    return std::holds_alternative<ExpressionId>(value);
  }

  [[nodiscard]] auto IsPattern() const -> bool {
    return std::holds_alternative<PatternId>(value);
  }

  [[nodiscard]] auto AsExpression() const -> ExpressionId {
    return std::get<ExpressionId>(value);
  }

  [[nodiscard]] auto AsPattern() const -> PatternId {
    return std::get<PatternId>(value);
  }
};

}  // namespace lyra::hir
