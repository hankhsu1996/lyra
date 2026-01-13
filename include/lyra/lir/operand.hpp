#pragma once

#include <ostream>
#include <string>
#include <variant>

#include <fmt/core.h>

#include "lyra/common/symbol.hpp"
#include "lyra/lir/context.hpp"

namespace lyra::lir {

using SymbolRef = common::SymbolRef;
using OperandValue = std::variant<TempRef, SymbolRef, ConstantRef, LabelRef>;

struct Operand {
  enum class Kind { kTemp, kVariable, kConstant, kLabel };

  Kind kind;
  OperandValue value{};

  template <typename T>
  static auto Make(Kind kind, T&& val) -> Operand {
    return Operand{.kind = kind, .value = std::forward<T>(val)};
  }

  static auto Temp(TempRef temp) -> Operand {
    return Make(Kind::kTemp, temp);
  }

  static auto Variable(SymbolRef symbol) -> Operand {
    return Make(Kind::kVariable, symbol);
  }

  static auto Constant(ConstantRef constant) -> Operand {
    return Make(Kind::kConstant, constant);
  }

  static auto Label(LabelRef label) -> Operand {
    return Make(Kind::kLabel, label);
  }

  [[nodiscard]] auto IsTemp() const -> bool {
    return kind == Kind::kTemp;
  }

  [[nodiscard]] auto IsVariable() const -> bool {
    return kind == Kind::kVariable;
  }

  [[nodiscard]] auto IsConstant() const -> bool {
    return kind == Kind::kConstant;
  }

  [[nodiscard]] auto IsLabel() const -> bool {
    return kind == Kind::kLabel;
  }

  [[nodiscard]] auto AsTempRef() const -> TempRef {
    assert(IsTemp());
    return std::get<TempRef>(value);
  }

  [[nodiscard]] auto ToString() const -> std::string {
    return std::visit(
        [](const auto& v) -> std::string {
          using T = std::decay_t<decltype(v)>;
          if constexpr (std::is_same_v<T, TempRef>) {
            return v->name;
          } else if constexpr (std::is_same_v<T, SymbolRef>) {
            return std::string(v->name);
          } else if constexpr (std::is_same_v<T, ConstantRef>) {
            return v->ToString();
          } else if constexpr (std::is_same_v<T, LabelRef>) {
            return *v;
          }
        },
        value);
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
      case K::kConstant:
        return fmt::format_to(ctx.out(), "constant");
      case K::kLabel:
        return fmt::format_to(ctx.out(), "label");
    }
    return fmt::format_to(ctx.out(), "unknown");
  }
};
