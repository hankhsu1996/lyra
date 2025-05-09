#pragma once

#include <ostream>
#include <string>

#include <fmt/core.h>

#include "lyra/common/literal.hpp"
#include "lyra/common/symbol.hpp"

namespace lyra::lir {

struct TempSymbol {
  std::string name;
};

using SymbolRef = common::SymbolRef;
using TempRef = TempSymbol*;

struct Operand {
  enum class Kind { kTemp, kVariable, kLiteral, kLabel };

  Kind kind;
  common::Literal literal{};
  SymbolRef symbol{};
  std::string name{};

  static auto Temp(std::string name) -> Operand {
    return Operand{.kind = Kind::kTemp, .name = std::move(name)};
  }

  static auto Variable(SymbolRef symbol) -> Operand {
    return Operand{.kind = Kind::kVariable, .symbol = symbol};
  }

  static auto Literal(common::Literal literal) -> Operand {
    return Operand{.kind = Kind::kLiteral, .literal = std::move(literal)};
  }

  static auto Label(const std::string& name) -> Operand {
    return Operand{.kind = Kind::kLabel, .name = name};
  }

  [[nodiscard]] auto IsTemp() const -> bool {
    return kind == Kind::kTemp;
  }

  [[nodiscard]] auto IsVariable() const -> bool {
    return kind == Kind::kVariable;
  }

  [[nodiscard]] auto IsLiteral() const -> bool {
    return kind == Kind::kLiteral;
  }

  [[nodiscard]] auto IsLabel() const -> bool {
    return kind == Kind::kLabel;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    switch (kind) {
      case Kind::kTemp:
        return name;
      case Kind::kVariable:
        return name;
      case Kind::kLiteral:
        return literal.ToString();
      case Kind::kLabel:
        return name;
    }
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

template <>
struct fmt::formatter<lyra::lir::Operand::Kind> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::Operand::Kind& kind, FormatContext& ctx) const {
    using K = lyra::lir::Operand::Kind;
    switch (kind) {
      case K::kTemp:
        return fmt::format_to(ctx.out(), "temp");
      case K::kVariable:
        return fmt::format_to(ctx.out(), "variable");
      case K::kLiteral:
        return fmt::format_to(ctx.out(), "literal");
      case K::kLabel:
        return fmt::format_to(ctx.out(), "label");
    }
    return fmt::format_to(ctx.out(), "unknown");
  }
};
