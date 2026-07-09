#pragma once

#include <optional>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

class RuntimeServices;

// LRM 21.6 command-line plusargs source, held on the engine as design-global
// state. Stored tokens are the plusarg content (`+` prefix already stripped)
// so a match compares directly against the user-supplied prefix.
class PlusArgsSource {
 public:
  // Returns the remainder (the portion after `prefix`) of the first stored
  // token whose content starts with `prefix`, or nullopt if none match. The
  // tokens are searched in the order given, matching LRM 21.6.
  [[nodiscard]] auto MatchPrefix(std::string_view prefix) const
      -> std::optional<std::string_view>;

 private:
  std::vector<std::string> tokens_;
};

// LRM 21.6 $test$plusargs. Returns 1 on prefix match, 0 otherwise, as a
// PackedArray shaped for SV `int` (2-state 32-bit signed).
auto TestPlusargs(RuntimeServices& services, const value::String& user_string)
    -> value::PackedArray;

// LRM 21.6 $value$plusargs. `user_string` is `"plusarg_prefix format_spec"`;
// on prefix match, converts the remainder per the format specifier and
// writes it to `out`, returning 1. On no match returns 0 and leaves `out`
// untouched. Legal format specifiers: %d %o %h %x %b %s (uppercase and
// leading 0 permitted). Real-valued conversions (%e %f %g) are not yet
// supported; a call with one of them returns 0 without writing.
auto ValuePlusargs(
    RuntimeServices& services, const value::String& user_string,
    value::PackedArray& out) -> value::PackedArray;
auto ValuePlusargs(
    RuntimeServices& services, const value::String& user_string,
    value::String& out) -> value::PackedArray;

}  // namespace lyra::runtime
