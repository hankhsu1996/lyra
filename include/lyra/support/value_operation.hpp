#pragma once

#include <cstdint>
#include <string_view>
#include <variant>

#include "lyra/support/builtin_fn.hpp"

namespace lyra::support {

// The equality operators the language defines on a value of any type (LRM
// 11.4.5).
enum class ValueOperator : std::uint8_t {
  kEquality,
  kInequality,
};

// One question every value type answers about a whole value, whatever it holds:
// an operator the language spells, or an entry the runtime library names on
// every value it holds. A type the program declares answers each with a body of
// its own, and a value the library holds is asked it by that same name.
using ValueOperation = std::variant<ValueOperator, BuiltinFn>;

// Whether the operation is asked of a value, which the body answering it takes
// as its receiver, rather than of a type, which builds one.
[[nodiscard]] auto ReachesAReceiver(ValueOperation operation) -> bool;

// Whether the body answering the operation is handed where its caller wrote it,
// as the entry it answers is: what a type's own body does can fail wherever
// what it holds can.
[[nodiscard]] auto TakesCallerLocation(ValueOperation operation) -> bool;

// The operation's stable spelling: the operator's token, or the entry's name.
[[nodiscard]] auto ValueOperationName(ValueOperation operation)
    -> std::string_view;

}  // namespace lyra::support
