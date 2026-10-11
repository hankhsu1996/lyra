#include "lyra/runtime/part_path.hpp"

#include <algorithm>
#include <cstdint>
#include <limits>
#include <optional>
#include <span>

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

PartPath::PartPath(std::span<const std::int64_t> steps) {
  for (const std::int64_t step : steps) {
    Append(static_cast<std::uint64_t>(step));
  }
}

void PartPath::Append(std::uint64_t step) {
  if (inexact_ || depth_ == kMaxSteps ||
      step > std::numeric_limits<std::uint32_t>::max()) {
    inexact_ = true;
    return;
  }
  steps_.at(depth_++) = static_cast<std::uint32_t>(step);
}

auto PartPath::Child(std::uint64_t step) -> PartPath {
  PartPath path;
  path.Append(step);
  return path;
}

auto PartPath::Element(std::optional<std::int64_t> ordinal) -> PartPath {
  return ordinal.has_value() ? Child(static_cast<std::uint64_t>(*ordinal))
                             : Inexact();
}

auto PartPath::Inexact() -> PartPath {
  PartPath path;
  path.inexact_ = true;
  return path;
}

auto PartPath::Join(const PartPath& further) const -> PartPath {
  PartPath joined = *this;
  for (const std::uint32_t step : further.Steps()) {
    joined.Append(std::uint64_t{step});
  }
  joined.inexact_ = joined.inexact_ || further.inexact_;
  return joined;
}

auto PartPath::Steps() const -> std::span<const std::uint32_t> {
  return std::span{steps_}.first(depth_);
}

auto PartPath::IsInexact() const -> bool {
  return inexact_;
}

auto PartPath::IsWhole() const -> bool {
  return depth_ == 0 && !inexact_;
}

auto PartPath::IsPrefixOf(const PartPath& other) const -> bool {
  return depth_ <= other.depth_ &&
         std::ranges::equal(Steps(), other.Steps().first(depth_));
}

auto PartPath::StripPrefix(const PartPath& prefix) const -> PartPath {
  if (!prefix.IsPrefixOf(*this)) {
    throw InternalError(
        "PartPath::StripPrefix: the path taken off is no prefix of this one");
  }
  PartPath rest;
  for (const std::uint32_t step : Steps().subspan(prefix.depth_)) {
    rest.Append(std::uint64_t{step});
  }
  rest.inexact_ = inexact_;
  return rest;
}

auto PartPath::operator==(const PartPath& other) const -> bool {
  return inexact_ == other.inexact_ &&
         std::ranges::equal(Steps(), other.Steps());
}

}  // namespace lyra::runtime
