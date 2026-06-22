#pragma once

#include <cstdint>
#include <variant>

#include "lyra/support/builtin_fn.hpp"

namespace lyra::hir {

// LRM 7.12.4 iterator intrinsic methods. HIR-only -- the kind is rewritten
// at HIR-to-MIR into a `LocalRef` on the enclosing array-method closure's
// index binding and never appears in MIR.
enum class IteratorMethodKind : std::uint8_t {
  kIndex,
};

struct BuiltinMethodRef {
  std::variant<support::BuiltinFn, IteratorMethodKind> method;
};

}  // namespace lyra::hir
