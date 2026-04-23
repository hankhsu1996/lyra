#pragma once

#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/hir/process.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::lowering::ast_to_hir {

[[nodiscard]] inline auto ProcessKindName(const hir::ProcessData& data)
    -> std::string_view {
  return std::visit(
      support::Overloaded{
          [](const hir::Initial&) -> std::string_view { return "initial"; },
      },
      data);
}

// Canonical process name: user_label when present, else
// "<kind>_<kind_ordinal>". Producer-side synthesis policy; called once at HIR
// construction and then carried unchanged through MIR and downstream consumers.
[[nodiscard]] inline auto CanonicalProcessName(
    const hir::ProcessData& data, std::size_t kind_ordinal,
    std::optional<std::string_view> user_label) -> std::string {
  if (user_label.has_value()) {
    return std::string{*user_label};
  }
  return std::format("{}_{}", ProcessKindName(data), kind_ordinal);
}

}  // namespace lyra::lowering::ast_to_hir
