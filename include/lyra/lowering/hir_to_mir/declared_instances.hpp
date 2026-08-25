#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::lowering::hir_to_mir {

// One object an instance member declares: where it sits among the declared
// dimensions, and the handle the enclosing class keeps on it. A scalar instance
// declares one such object, whose coordinate list is empty.
struct DeclaredInstance {
  std::vector<std::uint32_t> coordinates;
  mir::FieldId handle;
};

// Every object one instance member declares. An instance array is a single
// declaration spanning every element (LRM 23.3.2), so the source names one
// member while the design holds one object per element.
//
// The declared shape stays here beside the objects because a coordinate means
// nothing without it. They are one answer: given only the handles, a reader
// would go back to the declaration to interpret them, and then two places would
// have to agree on how the objects are laid out.
//
// That layout is: the last dimension counts fastest, which is also the order
// the objects are elaborated in.
class DeclaredInstances {
 public:
  DeclaredInstances() = default;

  // Places one object per element the dimensions declare, asking `mint` for
  // each object's handle as it is placed. Composing the two together is what
  // leaves no caller free to lay the objects out a second way. `mint` is
  // borrowed rather than forwarded: it is called once per object, and a
  // forwarded callable would be consumed by the first call.
  template <typename Mint>
  static auto Declare(std::vector<std::uint32_t> dims, const Mint& mint)
      -> DeclaredInstances {
    DeclaredInstances declared;
    declared.dims_ = std::move(dims);
    std::size_t count = 1;
    for (const std::uint32_t dim : declared.dims_) {
      count *= dim;
    }
    declared.objects_.reserve(count);
    for (std::size_t index = 0; index < count; ++index) {
      std::vector<std::uint32_t> coordinates(declared.dims_.size());
      std::size_t remaining = index;
      for (std::size_t i = declared.dims_.size(); i-- > 0;) {
        coordinates[i] =
            static_cast<std::uint32_t>(remaining % declared.dims_[i]);
        remaining /= declared.dims_[i];
      }
      mir::FieldId handle = mint(std::span<const std::uint32_t>(coordinates));
      declared.objects_.push_back(
          DeclaredInstance{
              .coordinates = std::move(coordinates), .handle = handle});
    }
    return declared;
  }

  [[nodiscard]] auto begin() const {
    return objects_.begin();
  }
  [[nodiscard]] auto end() const {
    return objects_.end();
  }

  // The handle on the object a coordinate names. A coordinate list of a
  // different length than the declared dimensions is a route built against a
  // different declaration than the one it reached.
  [[nodiscard]] auto HandleAt(std::span<const std::uint32_t> coords) const
      -> mir::FieldId {
    if (coords.size() != dims_.size()) {
      throw InternalError(
          "DeclaredInstances::HandleAt: the coordinates do not match the "
          "dimensions the instance member declares");
    }
    std::size_t index = 0;
    for (std::size_t i = 0; i < dims_.size(); ++i) {
      index = index * dims_[i] + coords[i];
    }
    return objects_.at(index).handle;
  }

 private:
  std::vector<std::uint32_t> dims_;
  std::vector<DeclaredInstance> objects_;
};

}  // namespace lyra::lowering::hir_to_mir
