#include "lyra/mir/class_ref.hpp"

#include <optional>
#include <variant>

#include "lyra/base/overloaded.hpp"

namespace lyra::mir {

auto IntroducesSlot(const std::optional<VirtualDispatchRole>& role) -> bool {
  if (!role.has_value()) {
    return false;
  }
  return std::visit(
      Overloaded{
          [](const IntroducesVirtualSlot&) { return true; },
          [](const OverridesIntraUnitSlot&) { return false; },
          [](const OverridesExternalSlot&) { return false; }},
      *role);
}

}  // namespace lyra::mir
