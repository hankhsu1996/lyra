#pragma once

#include <fmt/core.h>

#include "lyra/common/symbol.hpp"

namespace lyra::common {

enum class EdgeKind { kAnyChange, kPosedge, kNegedge, kBothEdge };

struct Trigger {
  EdgeKind edge_kind;
  SymbolRef variable;

  [[nodiscard]] auto ToString() const -> std::string {
    switch (edge_kind) {
      case EdgeKind::kAnyChange:
        return fmt::format("{}", variable->name);
      case EdgeKind::kPosedge:
        return fmt::format("posedge {}", variable->name);
      case EdgeKind::kNegedge:
        return fmt::format("negedge {}", variable->name);
      case EdgeKind::kBothEdge:
        return fmt::format("{}", variable->name);
    }
  }

  static auto AnyChange(const SymbolRef& variable) -> Trigger {
    return Trigger{.edge_kind = EdgeKind::kAnyChange, .variable = variable};
  }
};

inline auto operator<<(std::ostream& os, const Trigger& trigger)
    -> std::ostream& {
  return os << trigger.ToString();
}

}  // namespace lyra::common

template <>
struct fmt::formatter<lyra::common::Trigger> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::common::Trigger& trigger, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", trigger.ToString());
  }
};
