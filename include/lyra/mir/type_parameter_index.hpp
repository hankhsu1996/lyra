#pragma once

#include <compare>
#include <cstdint>

namespace lyra::mir {

// The position of a type parameter among those of the generic definition it
// belongs to, as clang's `TemplateTypeParmType` and rustc's `ParamTy` number
// one: the type a call of the definition names at that position stands for it.
struct TypeParameterIndex {
  std::uint32_t value = 0;

  auto operator<=>(const TypeParameterIndex&) const
      -> std::strong_ordering = default;
};

}  // namespace lyra::mir
