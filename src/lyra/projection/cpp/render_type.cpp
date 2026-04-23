#include "render_type.hpp"

#include <string>
#include <variant>

#include "lyra/mir/type.hpp"
#include "lyra/support/overloaded.hpp"

namespace lyra::projection::cpp {

auto RenderTypeAsCpp(const mir::Type& type) -> std::string {
  return std::visit(
      support::Overloaded{
          [](const mir::BuiltinIntType&) -> std::string { return "int"; },
          [](const mir::BuiltinLogicType&) -> std::string { return "bool"; },
      },
      type.data);
}

}  // namespace lyra::projection::cpp
