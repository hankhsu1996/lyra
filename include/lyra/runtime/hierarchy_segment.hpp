#pragma once

#include <span>
#include <string>
#include <utility>
#include <vector>

#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

// LRM 23.3.3.5 / 27.6 elaborated identity of a single hierarchy edge: the
// base label registered at the parent (`"loop"`, `"m"`, `"leaf"`) together
// with any per-dimension indices the elaboration assigns (`{0}` for the
// first iteration of a generate-for, `{i, j}` for a multi-dim instance
// array, `{}` for a scalar). Each scope owns one of these from the moment
// its constructor returns; `%m`, debug, and by-name lookup all read it.
class HierarchySegment {
 public:
  HierarchySegment() = default;

  // The emit passes indices as a `std::array<PackedArray, N>` so the span
  // ctor absorbs every fixed-shape literal. The PackedArrays are copied
  // into the segment's owning vector; the source array can be a temporary.
  HierarchySegment(
      std::string base_name, std::span<const value::PackedArray> indices)
      : base_name_(std::move(base_name)),
        indices_(indices.begin(), indices.end()) {
  }

  [[nodiscard]] auto BaseName() const -> std::string_view {
    return base_name_;
  }
  [[nodiscard]] auto Indices() const -> std::span<const value::PackedArray> {
    return indices_;
  }

  // The LRM 27.6 display form: `base[i0][i1]...`. Empty indices yield the
  // bare base label; scalar instances render as their source identifier.
  [[nodiscard]] auto Display() const -> std::string;

 private:
  std::string base_name_;
  std::vector<value::PackedArray> indices_;
};

}  // namespace lyra::runtime
