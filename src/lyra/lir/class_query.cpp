#include "lyra/lir/class_query.hpp"

#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"

namespace lyra::lir {

auto IsObjectTreeNode(const Class& cls) -> bool {
  if (!cls.base.has_value()) {
    return false;
  }
  return std::visit(
      Overloaded{
          [](const RuntimeBase&) { return true; },
          [](const IntraUnitBase&) { return false; },
          [](const CrossUnitBase&) { return false; }},
      *cls.base);
}

}  // namespace lyra::lir
