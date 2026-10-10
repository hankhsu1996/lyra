#pragma once

#include <cstdint>
#include <span>
#include <string>
#include <string_view>
#include <vector>

namespace lyra::runtime {

// LRM 23.3.3.5 / 27.6 elaborated identity of a single hierarchy edge: the
// base label the parent gives it (`"loop"`, `"m"`, `"leaf"`) together
// with any per-dimension indices the elaboration assigns (`{0}` for the
// first iteration of a generate-for, `{i, j}` for a multi-dim instance
// array, `{}` for a scalar). Each scope owns one of these from the moment
// its constructor returns; `%m` and debug output read it.
//
// Every unit that builds a child scope constructs one, hands it over, and
// destroys what is left, so each of those is declared here and defined in the
// library: written in this header, every such unit would compile the string
// and vector handling behind them again.
class HierarchySegment {
 public:
  HierarchySegment();

  // The indices are copied into the segment's owning vector, so the sequence
  // they arrive in can be a temporary.
  HierarchySegment(
      std::string base_name, std::span<const std::int64_t> indices);

  HierarchySegment(const HierarchySegment&);
  auto operator=(const HierarchySegment&) -> HierarchySegment&;
  HierarchySegment(HierarchySegment&&) noexcept;
  auto operator=(HierarchySegment&&) noexcept -> HierarchySegment&;
  ~HierarchySegment();

  [[nodiscard]] auto BaseName() const -> std::string_view {
    return base_name_;
  }

  // The LRM 27.6 display form: `base[i0][i1]...`. Empty indices yield the
  // bare base label, which is all a scalar instance renders as.
  [[nodiscard]] auto Display() const -> std::string;

 private:
  std::string base_name_;
  std::vector<std::int64_t> indices_;
};

}  // namespace lyra::runtime
