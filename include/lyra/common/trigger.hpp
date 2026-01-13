#pragma once

#include <vector>

#include <fmt/core.h>

#include "lyra/common/hierarchical_path.hpp"
#include "lyra/common/symbol.hpp"

namespace lyra::common {

enum class EdgeKind { kAnyChange, kPosedge, kNegedge, kBothEdge };

struct Trigger {
  EdgeKind edge_kind;
  SymbolId variable;
  std::vector<HierarchicalPathElement> instance_path;  // Empty for local

  [[nodiscard]] auto IsHierarchical() const -> bool {
    return !instance_path.empty();
  }

  [[nodiscard]] auto ToString() const -> std::string {
    std::string path_str = fmt::format("sym#{}", variable);
    switch (edge_kind) {
      case EdgeKind::kAnyChange:
        return path_str;
      case EdgeKind::kPosedge:
        return fmt::format("posedge {}", path_str);
      case EdgeKind::kNegedge:
        return fmt::format("negedge {}", path_str);
      case EdgeKind::kBothEdge:
        return path_str;
    }
  }

  // Factory for local signal (no hierarchical path)
  static auto AnyChange(SymbolId variable) -> Trigger {
    return Trigger{
        .edge_kind = EdgeKind::kAnyChange,
        .variable = variable,
        .instance_path = {}};
  }

  // Factory for hierarchical signal
  static auto AnyChange(
      SymbolId variable, std::vector<HierarchicalPathElement> instance_path)
      -> Trigger {
    return Trigger{
        .edge_kind = EdgeKind::kAnyChange,
        .variable = variable,
        .instance_path = std::move(instance_path)};
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
