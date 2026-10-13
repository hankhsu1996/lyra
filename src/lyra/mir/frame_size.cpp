#include "lyra/mir/frame_size.hpp"

#include <algorithm>
#include <cstdint>
#include <optional>
#include <span>
#include <vector>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::mir {

namespace {

// How many bytes a value of a type takes and to what it is aligned, where the
// type is laid out by what it holds: an integral, an array, a product, a union.
// Every other value is an object of the library's or a handle, a few words.
struct Layout {
  std::uint64_t size;
  std::uint64_t align;
};

auto LayoutOf(const CompilationUnit& unit, TypeId type) -> Layout {
  const Type& ty = unit.types.Get(type);
  if (const auto* integral = ty.As<IntegralType>()) {
    return Layout{
        .size = value::IntegralBytesFor(
            integral->bit_width, StateDomainOf(integral->state_kind)),
        .align = value::IntegralAlignFor(integral->bit_width)};
  }
  if (const auto* array = ty.As<UnpackedArrayType>()) {
    const Layout element = LayoutOf(unit, array->element_type);
    const std::uint64_t stride =
        (element.size + element.align - 1) / element.align * element.align;
    return Layout{.size = stride * array->Size(), .align = element.align};
  }
  if (const std::optional<std::span<const TypeId>> components =
          ProductElements(unit, type)) {
    Layout whole{.size = 0, .align = 1};
    for (const TypeId component : *components) {
      const Layout part = LayoutOf(unit, component);
      whole.size =
          (whole.size + part.align - 1) / part.align * part.align + part.size;
      whole.align = std::max(whole.align, part.align);
    }
    return whole;
  }
  const std::vector<TypeId>* members = nullptr;
  if (const auto* untagged = ty.As<UnionType>()) {
    members = &untagged->members;
  } else if (const auto* tagged = ty.As<TaggedUnionType>()) {
    members = &tagged->members;
  }
  if (members != nullptr) {
    Layout whole{.size = 0, .align = 1};
    for (const TypeId member : *members) {
      const Layout part = LayoutOf(unit, member);
      whole.size = std::max(whole.size, part.size);
      whole.align = std::max(whole.align, part.align);
    }
    return whole;
  }
  return Layout{.size = sizeof(void*), .align = alignof(void*)};
}

}  // namespace

auto IsTooLargeForAFrame(const CompilationUnit& unit, TypeId type) -> bool {
  return LayoutOf(unit, type).size > value::kMaxInlineBytes;
}

}  // namespace lyra::mir
