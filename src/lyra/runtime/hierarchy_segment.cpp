#include "lyra/runtime/hierarchy_segment.hpp"

#include <string>

namespace lyra::runtime {

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
