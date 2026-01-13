#pragma once

#include <ostream>
#include <string>

#include <fmt/core.h>

#include "lyra/lir/context.hpp"

namespace lyra::lir {

/// SSA operand - always a temp reference.
/// This thin wrapper exists for type safety and future extensibility.
struct Operand {
  TempRef temp;

  static auto Temp(TempRef t) -> Operand {
    return Operand{.temp = t};
  }

  [[nodiscard]] auto IsTemp() const -> bool {
    return true;
  }

  [[nodiscard]] auto AsTempRef() const -> TempRef {
    return temp;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    return temp->name;
  }
};

inline auto operator<<(std::ostream& os, const Operand& op) -> std::ostream& {
  return os << op.ToString();
}

}  // namespace lyra::lir

template <>
struct fmt::formatter<lyra::lir::Operand> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::Operand& op, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", op.ToString());
  }
};
