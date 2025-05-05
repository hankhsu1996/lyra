#pragma once

#include <fmt/core.h>

namespace lyra::common {

enum class EdgeKind { kAnyChange, kPosedge, kNegedge, kBothEdge };

template <typename VariableType>
struct Trigger {
  EdgeKind edge_kind;
  VariableType variable;

  [[nodiscard]] auto ToString() const -> std::string {
    switch (edge_kind) {
      case EdgeKind::kAnyChange:
        return fmt::format("{}", variable);
      case EdgeKind::kPosedge:
        return fmt::format("posedge {}", variable);
      case EdgeKind::kNegedge:
        return fmt::format("negedge {}", variable);
      case EdgeKind::kBothEdge:
        return fmt::format("{}", variable);
    }
  }

  static auto AnyChange(const VariableType& variable) -> Trigger {
    return Trigger{EdgeKind::kAnyChange, variable};
  }
};

template <typename VariableType>
inline auto operator<<(std::ostream& os, const Trigger<VariableType>& trigger)
    -> std::ostream& {
  return os << trigger.ToString();
}

}  // namespace lyra::common

template <>
struct fmt::formatter<lyra::common::Trigger<std::string>> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(
      const lyra::common::Trigger<std::string>& trigger,
      FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", trigger.ToString());
  }
};
