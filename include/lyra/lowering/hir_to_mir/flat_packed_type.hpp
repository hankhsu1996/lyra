#pragma once

#include <cstdint>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The type of a value built by composing bits: `width` of them, unsigned, in a
// single dimension rather than a stack of them. LRM 11.8.1 fixes that shape --
// "concatenation results are unsigned, regardless of the operands" -- so
// `{a, b, c}` is one unsigned vector of bits even where it is assigned to
// something signed and multi-dimensional, and the store conversion reshapes it
// into that declaration afterwards.
//
// No source declaration names this type, which is why the lowering has to make
// one. Composing bits does not say whether those bits can hold x or z, so how
// many states they have is passed in rather than derived from the composition.
[[nodiscard]] auto InternFlatPacked(
    const mir::CompilationUnit& unit, std::uint64_t width,
    mir::IntegralStateKind state_kind) -> mir::TypeId;

}  // namespace lyra::lowering::hir_to_mir
