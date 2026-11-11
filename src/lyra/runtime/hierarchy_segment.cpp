#include "lyra/runtime/hierarchy_segment.hpp"

#include <cstdint>
#include <span>
#include <string>

#include "lyra/abi/records.hpp"
#include "lyra/support/scope_kind.hpp"

namespace lyra::runtime {

const abi::ElaboratedScopeRecord kDesignRootRecord{
    .name = support::kDesignRootName.data(),
    .kind =
        static_cast<std::uint8_t>(support::ElaboratedScopeKind::kDesignRoot),
    .implicitly_named = 0};

HierarchySegment::HierarchySegment() : declaration_(&kDesignRootRecord) {
}

HierarchySegment::HierarchySegment(
    const abi::ElaboratedScopeRecord* declaration,
    std::span<const std::int64_t> indices)
    : declaration_(declaration), indices_(indices.begin(), indices.end()) {
}

HierarchySegment::HierarchySegment(const HierarchySegment&) = default;
auto HierarchySegment::operator=(const HierarchySegment&)
    -> HierarchySegment& = default;
HierarchySegment::HierarchySegment(HierarchySegment&&) noexcept = default;
auto HierarchySegment::operator=(HierarchySegment&&) noexcept
    -> HierarchySegment& = default;
HierarchySegment::~HierarchySegment() = default;

auto HierarchySegment::Display() const -> std::string {
  std::string out(declaration_->name);
  for (const std::int64_t index : indices_) {
    out.push_back('[');
    out.append(std::to_string(index));
    out.push_back(']');
  }
  return out;
}

}  // namespace lyra::runtime
