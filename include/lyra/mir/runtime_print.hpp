#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

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
  std::int32_t timeunit_power;

  FormatSpec(value::FormatKind k, FormatModifiers m, std::int32_t tp = 0)
      : kind(k), modifiers(m), timeunit_power(tp) {
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

struct RuntimePrintCall {
  value::PrintKind kind;
  std::optional<ExprId> descriptor;
  std::vector<RuntimePrintItem> items;

  RuntimePrintCall(
      value::PrintKind k, std::optional<ExprId> d,
      std::vector<RuntimePrintItem> i)
      : kind(k), descriptor(d), items(std::move(i)) {
  }
};

}  // namespace lyra::mir
