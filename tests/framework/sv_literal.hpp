#pragma once

#include <cstdint>
#include <expected>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/value/format.hpp"

namespace lyra::test {

// Categorises a YAML node drawn from an `expect.variables` entry and turns
// it into the runtime value + format spec the test framework will use to
// (a) inject the `$display` call into the rewritten SV source and
// (b) compute the expected marker payload via `value::Format`.
//
// Resolution rules:
//   - YAML integer (e.g. `30`)            -> kIntegerScalar
//   - YAML string matching SV literal     -> kSvLiteral (numeric value)
//   - any other YAML string               -> kStringScalar (raw characters)
//   - YAML sequence (e.g. `[10, 20, 30]`) -> kAggregate (LRM 21.2.1.6
//                                            assignment pattern, %p / %0p;
//                                            applies to any container -- fixed
//                                            unpacked, dynamic, queue, assoc)
//
// SV literal pattern: `<width>'[s][bhodBHOD]<digits>`. Digits may contain
// `_` separators and (for binary/hex) `x`/`z` 4-state placeholders.

enum class ExpectedValueKind : std::uint8_t {
  kIntegerScalar,
  kSvLiteral,
  kStringScalar,
  kAggregate,
};

struct ExpectedValue {
  ExpectedValueKind kind = ExpectedValueKind::kIntegerScalar;

  // Owned storage that the materialized `PackedArray` borrows into when the
  // expected value is built into a `value::FormatArg` (singular cases).
  std::int64_t integer_value = 0;
  std::vector<std::uint64_t> value_words;
  std::vector<std::uint64_t> unknown_words;
  std::uint64_t bit_width = 0;
  bool is_signed = false;
  value::IntegralStateKind state_kind = value::IntegralStateKind::kTwoState;
  std::string string_value;

  // LRM 21.2.1.6 aggregate elements, owned and traversed recursively for
  // `kAggregate`. Empty for singular kinds.
  std::vector<ExpectedValue> elements;

  value::FormatSpec format_spec;

  // The SV `$display` format specifier corresponding to `format_spec`,
  // injected into the rewritten source (e.g. "%0d", "%b", "%s", "%p").
  std::string sv_format_specifier;

  // Build a type-erased FormatArg that borrows into this object's storage.
  // The arg stays valid as long as the parent ExpectedValue is alive.
  [[nodiscard]] auto BuildFormatArg() const -> value::FormatArg;
};

// Parse a YAML scalar into an ExpectedValue. `node_is_integer` indicates that
// the source YAML node was a YAML integer (vs. string); this distinction is
// what separates `30` from `"30"`.
auto ParseExpectedValue(std::string_view text, bool node_is_integer)
    -> std::expected<ExpectedValue, std::string>;

// Render the expected marker payload string (what the test framework will
// look for after `<name>=` on the corresponding marker line).
auto RenderExpectedFormatted(const ExpectedValue& value) -> std::string;

}  // namespace lyra::test
