#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/value/integral.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// Which of a simulation's own arguments are plusargs, with the `+` that marks
// one removed (LRM 21.6). Every way of starting a simulation classifies its
// arguments through this, so the answer is decided once.
[[nodiscard]] auto PlusargsFrom(std::span<const std::string> arguments)
    -> std::vector<std::string>;

// LRM 21.6 command-line plusargs source, held on the engine as design-global
// state. Stored tokens are the plusarg content (`+` prefix already stripped)
// so a match compares directly against the user-supplied prefix.
class PlusArgsSource {
 public:
  PlusArgsSource() = default;
  explicit PlusArgsSource(std::vector<std::string> tokens)
      : tokens_(std::move(tokens)) {
  }

  // Returns the remainder (the portion after `prefix`) of the first stored
  // token whose content starts with `prefix`, or nullopt if none match. The
  // tokens are searched in the order given, matching LRM 21.6.
  [[nodiscard]] auto MatchPrefix(std::string_view prefix) const
      -> std::optional<std::string_view>;

 private:
  std::vector<std::string> tokens_;
};

// LRM 21.6 $test$plusargs. Returns 1 on prefix match, 0 otherwise, as an SV
// `int`.
auto TestPlusargs(RuntimeEffects& runtime, const value::String& user_string)
    -> value::Int;

// LRM 21.6 $value$plusargs. `user_string` is `"plusarg_prefix format_spec"`;
// on a prefix match the remainder is converted per the format specifier and
// completes beside a 1. Legal format specifiers: %d %o %h %x %b %s (uppercase
// and leading 0 permitted). Real-valued conversions (%e %f %g) are not yet
// supported; a call with one of them completes with 0.
//
// The destination crosses in and comes back, because the clause leaves the
// variable a miss did not match exactly as it was, and its size is what
// decides whether a converted value is zero-padded or truncated.
auto ValuePlusargs(
    RuntimeEffects& runtime, const value::String& user_string,
    value::String out) -> value::Tuple<value::Int, value::String>;

// The integral form, writing an integral destination of any type through its
// planes, which a miss leaves as they were.
auto ValuePlusargsInto(
    RuntimeEffects& runtime, const value::String& user_string,
    const value::IntegralView& out) -> value::Int;

template <value::IntegralValue T>
auto ValuePlusargs(
    RuntimeEffects& runtime, const value::String& user_string, T out)
    -> value::Tuple<value::Int, T> {
  typename T::Words held = out.Load();
  const value::Int matched =
      ValuePlusargsInto(runtime, user_string, held.MutableView());
  return value::Tuple<value::Int, T>{matched, T::FromWords(held)};
}

}  // namespace lyra::runtime
