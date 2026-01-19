#pragma once

#include <variant>
#include <vector>

#include "lyra/common/system_function.hpp"
#include "lyra/mir/builtin.hpp"
#include "lyra/mir/operand.hpp"

namespace lyra::mir {

// DisplayEffect represents a $display/$write family system task.
// These are immediate observable effects that print to stdout.
struct DisplayEffect {
  PrintRadix radix;
  bool append_newline;
  std::vector<Operand> args;
};

// BuiltinCallEffect represents effectful builtin calls (e.g., arr.delete()).
// - receiver: The array/queue being mutated
// - args: Arguments (value for push, idx for delete(idx), etc.)
struct BuiltinCallEffect {
  BuiltinMethod method{};
  PlaceId receiver{};
  std::vector<Operand> args{};
};

// EffectOp is the variant of all effect operations.
// Effect operations produce side effects but no value.
using EffectOp = std::variant<DisplayEffect, BuiltinCallEffect>;

}  // namespace lyra::mir
