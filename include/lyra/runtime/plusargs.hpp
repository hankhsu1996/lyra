#pragma once

#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/value/integral.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/tuple.hpp"

namespace lyra::runtime {

class RuntimeEffects;

// Which words of a command line are plusargs, with the `+` that marks one
// removed (LRM 21.6). The program's own name leads the words and is no
// argument. Every way of starting a simulation classifies its words through
// this, so the answer is decided once.
[[nodiscard]] auto PlusargsFrom(std::span<const std::string> words)
    -> std::vector<std::string>;

// The remainder, after `prefix`, of the first of `plusargs` that starts with
// it, searched in the order given (LRM 21.6), or none.
[[nodiscard]] auto MatchPlusargPrefix(
    std::span<const std::string> plusargs, std::string_view prefix)
    -> std::optional<std::string_view>;

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
    RuntimeEffects& runtime, const value::String& user_string, const T& out)
    -> value::Tuple<value::Int, T> {
  // The destination is written where the answer keeps it.
  value::Tuple<value::Int, T> answer(value::Int{}, out);
  answer.template Component<0>() = ValuePlusargsInto(
      runtime, user_string,
      answer.template Component<1>().AsMutableWords().MutableView());
  return answer;
}

}  // namespace lyra::runtime
