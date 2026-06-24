#pragma once

#include <cstdint>
#include <span>
#include <string>
#include <variant>

namespace lyra::value {

class PackedArray;
class String;
template <typename Host>
class RealValue;
template <typename T>
class UnpackedArray;
template <typename T>
class DynamicArray;
template <typename T>
class Queue;
template <typename K, typename V>
class AssociativeArray;

enum class FormatKind : std::uint8_t {
  kDecimal,
  kHex,
  kBinary,
  kOctal,
  kString,
  kChar,
  kRealDecimal,
  kRealExponential,
  kRealGeneral,
  kAssignmentPattern,
  kTime,
};

// Two-state vs four-state lives on the integral axis, not as a top-level
// value kind: it's a representation/semantics dimension of integral, in the
// same way `string` is a value category. The two enums are orthogonal.
enum class IntegralStateKind : std::uint8_t {
  kTwoState,
  kFourState,
};

struct FormatSpec {
  FormatKind kind = FormatKind::kDecimal;
  std::int32_t width = -1;
  std::int32_t precision = -1;
  bool zero_pad = false;
  bool left_align = false;
  std::int32_t timeunit_power = 0;

  FormatSpec() = default;

  // Each field arrives as a PackedArray literal -- the value model routes
  // compile-time scalars as SV values, the same way the file-IO runtime entries
  // take their int args -- and converts to its native field type here.
  explicit FormatSpec(const PackedArray& kind);
  FormatSpec(
      const PackedArray& kind, const PackedArray& width,
      const PackedArray& precision, const PackedArray& zero_pad,
      const PackedArray& left_align, const PackedArray& timeunit_power);
};

// LRM 20.4.3 / Table 20-3: the design-wide settings that drive every `%t`. The
// runtime owns one mutable instance (on the Engine); `$timeformat` writes it.
// `units_power` is the display unit as an LRM Table 20-2 power value.
struct TimeFormat {
  std::int8_t units_power = 0;
  std::int32_t precision = 0;
  std::string suffix;
  std::int32_t min_width = 20;
};

// LRM Table 20-2: a power value maps to one of {1, 10, 100} times a unit
// that cycles s / ms / us / ns / ps / fs every three decades below 1 s.
// Used by the `$printtimescale` lowering to compile-time format the
// "Time scale of ..." message and by any future caller that needs the same
// power-to-text projection.
[[nodiscard]] auto TimeUnitText(std::int8_t power) -> std::string;

// Per-call context routed alongside the spec into every Formatter::Format.
// Holds the design-wide bits that a formatter sometimes needs but the spec
// itself cannot carry (the spec is closed and shared across call sites). At
// present only `%t` consults it; future kinds (e.g. `%m` hierarchical name)
// will add fields here. `time_format == nullptr` means the calling context
// has no time-format vocabulary; a formatter that needs one throws.
struct FormatContext {
  const TimeFormat* time_format = nullptr;
};

// Per-type formatter trait. Mirrors `std::formatter<T>` -- each SV type owns
// the knowledge of how to render itself by specializing this template. New
// types add one specialization and touch no central catalog. The primary
// template is intentionally undefined: missing a specialization for a type
// passed to `MakeFormatArg<T>` produces a compile error at the lambda body
// in `MakeFormatArg`, pointing at the missing specialization.
template <typename T>
struct Formatter;

// Type-erased operand passed through the runtime print queue. Mirrors the
// `handle` arm of `std::basic_format_arg`: a borrowing pointer to the
// caller's value plus a function pointer that knows how to dispatch through
// `Formatter<T>` for the underlying type. The arg never owns; the caller
// must hold the underlying value alive for the duration of the print call.
// Always two pointers wide regardless of the underlying type.
struct FormatArg {
  const void* ptr;
  std::string (*format_fn)(
      const FormatSpec&, const void*, const FormatContext&);
};

// Build a FormatArg that borrows `value`. `value` must outlive every
// subsequent use of the returned arg. In practice that means callers pass
// either a stable lvalue (member, local) or a temporary lifetime-extended
// into the same full-expression as the runtime print call (the emit path
// builds the items array inline as the print call argument, so temps in
// the array initializer stay alive through the call).
template <typename T>
[[nodiscard]] auto MakeFormatArg(const T& value) -> FormatArg {
  return FormatArg{
      .ptr = &value,
      .format_fn = [](const FormatSpec& spec, const void* p,
                      const FormatContext& ctx) -> std::string {
        const T& v = *static_cast<const T*>(p);
        // Per-formatter signature lookup: those that consult design-wide
        // context (`%t` needing `TimeFormat`, future `%m` needing scope)
        // declare `Format(spec, value, ctx)`; those that do not (string,
        // aggregate -- never reach a context-bound kind) declare just
        // `Format(spec, value)`. The lambda absorbs both shapes so the
        // `FormatArg.format_fn` signature stays uniform.
        if constexpr (requires { Formatter<T>::Format(spec, v, ctx); }) {
          return Formatter<T>::Format(spec, v, ctx);
        } else {
          return Formatter<T>::Format(spec, v);
        }
      },
  };
}

// Drive the type-erased dispatch. The runtime print loop calls this once
// per `PrintValueItem`. Equivalent to `std::format` for a single arg slot.
[[nodiscard]] auto Format(
    const FormatSpec& spec, FormatArg arg, const FormatContext& ctx = {})
    -> std::string;

// Specializations for the closed leaf set. Each is declared here so callers
// of `MakeFormatArg<T>` can build the format_fn at instantiation time; the
// bodies live in `format.cpp` (singular leaf types) or in the corresponding
// container header (aggregate templates).
template <>
struct Formatter<PackedArray> {
  static auto Format(
      const FormatSpec& spec, const PackedArray& value,
      const FormatContext& ctx) -> std::string;
};

template <>
struct Formatter<String> {
  static auto Format(const FormatSpec& spec, const String& value)
      -> std::string;
};

template <>
struct Formatter<double> {
  static auto Format(
      const FormatSpec& spec, double value, const FormatContext& ctx)
      -> std::string;
};

template <>
struct Formatter<float> {
  static auto Format(
      const FormatSpec& spec, float value, const FormatContext& ctx)
      -> std::string;
};

struct PrintLiteralItem {
  const char* data = nullptr;
  std::uint32_t size = 0;

  PrintLiteralItem() = default;
  PrintLiteralItem(const char* data, std::uint32_t size)
      : data(data), size(size) {
  }

  // Borrows the text bytes of `text`. The caller materializes `text` as a
  // temporary in the same full-expression as the runtime print call, so the
  // borrowed bytes outlive the walk (the same lifetime contract `FormatArg`
  // relies on).
  explicit PrintLiteralItem(const String& text);
};

struct PrintValueItem {
  FormatSpec spec;
  FormatArg arg;

  // The emit side constructs this directly (`PrintValueItem(x, spec)`). The
  // overload set is closed on the leaf formattable types rather than a single
  // greedy template so an operand that is not itself formattable but converts
  // to one -- an `Enum<T>` decaying to `PackedArray` -- selects the converted
  // overload instead of instantiating an undefined `Formatter`. `const T&` so
  // a caller-side temporary (an arithmetic rvalue) binds via lifetime
  // extension through the full expression that contains the runtime print
  // call.
  PrintValueItem(const PackedArray& value, FormatSpec spec)
      : spec(spec), arg(MakeFormatArg(value)) {
  }
  PrintValueItem(const String& value, FormatSpec spec)
      : spec(spec), arg(MakeFormatArg(value)) {
  }
  template <typename Host>
  PrintValueItem(const RealValue<Host>& value, FormatSpec spec)
      : spec(spec), arg(MakeFormatArg(value)) {
  }
  template <typename T>
  PrintValueItem(const UnpackedArray<T>& value, FormatSpec spec)
      : spec(spec), arg(MakeFormatArg(value)) {
  }
  template <typename T>
  PrintValueItem(const DynamicArray<T>& value, FormatSpec spec)
      : spec(spec), arg(MakeFormatArg(value)) {
  }
  template <typename T>
  PrintValueItem(const Queue<T>& value, FormatSpec spec)
      : spec(spec), arg(MakeFormatArg(value)) {
  }
  template <typename K, typename V>
  PrintValueItem(const AssociativeArray<K, V>& value, FormatSpec spec)
      : spec(spec), arg(MakeFormatArg(value)) {
  }
};

using PrintItem = std::variant<PrintLiteralItem, PrintValueItem>;

// Walks the print items into one SV string per their FormatSpec, appending
// literal items verbatim and formatting each value item through the per-arg
// Format overload. `time_format` supplies the `$timeformat` display state `%t`
// reads (LRM 20.4.3 / 21.2.1); the caller threads it from the engine, so this
// step holds no engine state.
[[nodiscard]] auto Format(
    std::span<const PrintItem> items, const TimeFormat& time_format) -> String;

}  // namespace lyra::value
