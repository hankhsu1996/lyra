#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>

#include "lyra/common/type.hpp"
#include "lyra/common/type_arena.hpp"
#include "lyra/mir/interp/runtime_value.hpp"

namespace lyra::mir::interp {

// Bundles a runtime value with its type provenance for semantic decisions
// (e.g., signedness for %d formatting, sign extension, arithmetic shifts).
struct TypedValue {
  RuntimeValue value;
  TypeId type;
};

struct FormatSpec {
  char spec{};  // d, h, x, b, o, s, f
  bool zero_pad = false;
  bool left_align = false;
  std::optional<int> width;
  std::optional<int> precision;
};

// Future-proof context for %t/$timeformat, locale, etc.
struct FormatContext {};

// Format a single value according to a format specifier.
// TypeArena is used to recover signedness for %d.
auto FormatValue(
    const TypedValue& arg, const FormatSpec& spec, const TypeArena& types,
    const FormatContext& ctx) -> std::string;

// Parse and apply a format string to arguments.
auto FormatDisplay(
    std::string_view fmt, std::span<const TypedValue> args, char default_format,
    const TypeArena& types, const FormatContext& ctx) -> std::string;

// Entry point: three-case dispatcher for $display-style formatting
// 1. First arg is string with '%' -> treat as format string
// 2. First arg is string without '%' -> output as prefix + auto-format rest
// 3. First arg not string -> auto-format all with default radix
auto FormatMessage(
    std::span<const TypedValue> args, char default_format,
    const TypeArena& types, const FormatContext& ctx) -> std::string;

}  // namespace lyra::mir::interp
