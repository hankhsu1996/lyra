#include "lyra/lir/declaration_name.hpp"

#include <format>
#include <optional>
#include <string>

#include "lyra/base/overloaded.hpp"
#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/type.hpp"

namespace lyra::lir {

auto DeclarationName(const CompilationUnit& unit, TypeId type)
    -> std::optional<std::string> {
  return unit.types.Get(type).Visit(
      Overloaded{
          [&](const ObjectType& o) -> std::optional<std::string> {
            return unit.classes.Get(o.class_id).name;
          },
          [&](const ExternalUnitObjectType& e) -> std::optional<std::string> {
            const ExternalUnitObject& object =
                unit.external_unit_objects.Get(e.object);
            return std::format("{}.{}", object.unit_name, object.class_name);
          },
          [](const CrossUnitClassType& c) -> std::optional<std::string> {
            return std::format("{}.{}", c.unit_name, c.class_name);
          },
          [&](const ClosureType& c) -> std::optional<std::string> {
            return unit.closures.Get(c.closure_id).name;
          },
          [&](const StructType& s) -> std::optional<std::string> {
            return unit.structs.Get(s.struct_id).name;
          },
          [](const auto&) -> std::optional<std::string> {
            return std::nullopt;
          }});
}

}  // namespace lyra::lir
