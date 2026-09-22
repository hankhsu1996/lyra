#pragma once

#include <cstddef>
#include <variant>

#include "lyra/base/interner.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_descriptor_id.hpp"

namespace lyra::mir {

// What one type's declaration says that an operation on a value of it needs.
// The packed family says its representation -- the dimension stack, the
// signedness, the state domain; an unpacked array says the declared range a
// select resolves a coordinate against. Each family says a different thing, so
// each is its own alternative and a consumer answers for every one of them.
//
// A type is where a description is found and not what it is: no consumer of
// one can tell which declaration it came from, so two declarations saying the
// same thing say one description.
using TypeDescription = std::variant<PackedArrayType, UnpackedRange>;

struct TypeDescriptionHash {
  auto operator()(const TypeDescription& description) const -> std::size_t;
};

// The descriptions one compilation unit holds. A description reaches one entry
// however many uses name it, and one nothing names reaches none -- so what an
// artifact writes out is what its own program refers to, rather than what its
// type pool happens to admit.
using TypeDescriptorPool =
    base::Interner<TypeDescription, TypeDescriptorId, TypeDescriptionHash>;

}  // namespace lyra::mir
