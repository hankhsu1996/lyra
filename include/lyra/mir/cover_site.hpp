#pragma once

#include <cstdint>
#include <vector>

#include "lyra/common/source_span.hpp"
#include "lyra/mir/effect.hpp"

namespace lyra::mir {

// Per-site metadata for an immediate cover statement.
// One entry per cover statement in the design, indexed by CoverSiteId.
struct ImmediateCoverSiteInfo {
  SourceSpan span;
};

// Dense allocator for immediate cover sites during HIR-to-MIR lowering.
// Owned by the design lowering scope; shared across all body/process
// lowering contexts via pointer. Produces design-global CoverSiteId values.
class ImmediateCoverSiteRegistry {
 public:
  auto Allocate(SourceSpan span) -> CoverSiteId {
    auto id = CoverSiteId{static_cast<uint32_t>(sites_.size())};
    sites_.push_back(ImmediateCoverSiteInfo{.span = span});
    return id;
  }

  [[nodiscard]] auto Size() const -> uint32_t {
    return static_cast<uint32_t>(sites_.size());
  }

  [[nodiscard]] auto Sites() const
      -> const std::vector<ImmediateCoverSiteInfo>& {
    return sites_;
  }

  auto TakeSites() -> std::vector<ImmediateCoverSiteInfo> {
    return std::move(sites_);
  }

 private:
  std::vector<ImmediateCoverSiteInfo> sites_;
};

}  // namespace lyra::mir
