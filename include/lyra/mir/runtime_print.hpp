#pragma once

#include <cstdint>
#include <string>
#include <utility>
#include <variant>

#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/value/format.hpp"

namespace lyra::mir {

struct FormatModifiers {
  std::int32_t width = -1;
  std::int32_t precision = -1;
  bool zero_pad = false;
  bool left_align = false;
};

struct FormatSpec {
  value::FormatKind kind;
  FormatModifiers modifiers;

  FormatSpec(value::FormatKind k, FormatModifiers m) : kind(k), modifiers(m) {
  }
};

struct RuntimePrintLiteral {
  std::string text;
};

struct RuntimePrintValue {
  ExprId value;
  TypeId type;
  FormatSpec spec;

  RuntimePrintValue(ExprId v, TypeId t, FormatSpec s)
      : value(v), type(t), spec(std::move(s)) {
  }
};

using RuntimePrintItem = std::variant<RuntimePrintLiteral, RuntimePrintValue>;

}  // namespace lyra::mir
