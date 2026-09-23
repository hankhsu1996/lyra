#include "lyra/runtime/hierarchy_segment.hpp"

#include <span>
#include <string>
#include <utility>

#include "lyra/value/packed_array.hpp"

namespace lyra::runtime {

HierarchySegment::HierarchySegment() = default;

HierarchySegment::HierarchySegment(
    std::string base_name, std::span<const value::PackedArray> indices)
    : base_name_(std::move(base_name)),
      indices_(indices.begin(), indices.end()) {
}

HierarchySegment::HierarchySegment(const HierarchySegment&) = default;
auto HierarchySegment::operator=(const HierarchySegment&)
    -> HierarchySegment& = default;
HierarchySegment::HierarchySegment(HierarchySegment&&) noexcept = default;
auto HierarchySegment::operator=(HierarchySegment&&) noexcept
    -> HierarchySegment& = default;
HierarchySegment::~HierarchySegment() = default;

auto HierarchySegment::Display() const -> std::string {
  std::string out(base_name_);
  for (const auto& idx : indices_) {
    out.push_back('[');
    out.append(std::to_string(idx.ToInt64()));
    out.push_back(']');
  }
  return out;
}

}  // namespace lyra::runtime
