#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/mir/expr.hpp"

namespace lyra::mir {

enum class PrintKind : std::uint8_t {
  kDisplay,
  kWrite,
  kFDisplay,
  kFWrite,
};

enum class FormatKind : std::uint8_t {
  kDecimal,
  kHex,
  kBinary,
  kOctal,
  kString,
};

struct FormatModifiers {
  std::int32_t width = -1;
  std::int32_t precision = -1;
  bool zero_pad = false;
  bool left_align = false;
};

struct FormatSpec {
  FormatKind kind;
  FormatModifiers modifiers;
  std::int32_t timeunit_power;

  FormatSpec(FormatKind k, FormatModifiers m, std::int32_t tp = 0)
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

struct RuntimePrintSeqStmt {
  PrintKind kind;
  std::optional<ExprId> descriptor;
  std::vector<RuntimePrintItem> items;

  RuntimePrintSeqStmt(
      PrintKind k, std::optional<ExprId> d, std::vector<RuntimePrintItem> i)
      : kind(k), descriptor(d), items(std::move(i)) {
  }
};

}  // namespace lyra::mir
