#pragma once

#include <cstdint>
#include <span>
#include <string>
#include <vector>

#include "lyra/abi/records.hpp"

namespace lyra::runtime {

// The declaration of the implicit `$root` (LRM 23.3.1), which no source
// declares, so this library does.
extern const abi::ElaboratedScopeRecord kDesignRootRecord;

// LRM 23.3.3.5 / 27.6 elaborated identity of a single hierarchy edge: the
// declaration of the scope holding it that the scope fills -- an instance, an
// array of them, a generate block or loop -- which names it, together with any
// per-dimension indices the elaboration assigns (`{0}` for the first iteration
// of a generate-for, `{i, j}` for a multi-dim instance array, `{}` for a
// scalar). Each scope owns one of these from the first statement of its
// constructor on; `%m`, the names foreign code is handed, a value dump's
// scopes and debug output read it.
//
// Every unit that builds a child scope constructs one, hands it over, and
// destroys what is left, so each of those is declared here and defined in the
// library: written in this header, every such unit would compile the vector
// handling behind them again.
class HierarchySegment {
 public:
  // The root's, which is what a scope holds before its constructor places it.
  HierarchySegment();

  // The indices are copied into the segment's owning vector, so the sequence
  // they arrive in can be a temporary.
  HierarchySegment(
      const abi::ElaboratedScopeRecord* declaration,
      std::span<const std::int64_t> indices);

  HierarchySegment(const HierarchySegment&);
  auto operator=(const HierarchySegment&) -> HierarchySegment&;
  HierarchySegment(HierarchySegment&&) noexcept;
  auto operator=(HierarchySegment&&) noexcept -> HierarchySegment&;
  ~HierarchySegment();

  [[nodiscard]] auto Declaration() const -> const abi::ElaboratedScopeRecord& {
    return *declaration_;
  }

  [[nodiscard]] auto Indices() const -> std::span<const std::int64_t> {
    return indices_;
  }

  // The LRM 27.6 display form: `name[i0][i1]...`. Empty indices yield the
  // bare name, which is all a scalar instance renders as.
  [[nodiscard]] auto Display() const -> std::string;

 private:
  const abi::ElaboratedScopeRecord* declaration_;
  std::vector<std::int64_t> indices_;
};

}  // namespace lyra::runtime
