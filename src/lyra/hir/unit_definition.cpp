#include "lyra/hir/unit_definition.hpp"

#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"

namespace lyra::hir {

auto RootsAnObject(const UnitDefinition& definition) -> bool {
  return std::visit(
      Overloaded{
          [](const DesignElementDefinition&) { return true; },
          [](const PackageDefinition&) { return false; },
          [](const CompilationUnitScope&) { return false; },
          [](const ClassSpecialization&) { return false; },
          [](const ToolText&) { return false; },
          [](const DesignRoot&) { return true; }},
      definition.form);
}

auto IsProgram(const UnitDefinition& definition) -> bool {
  const auto* element = std::get_if<DesignElementDefinition>(&definition.form);
  if (element == nullptr) return false;
  switch (element->kind) {
    case DesignElementKind::kProgram:
      return true;
    case DesignElementKind::kModule:
    case DesignElementKind::kInterface:
      return false;
  }
  throw InternalError("hir::IsProgram: a design element of no kind");
}

auto IsNamespaceObject(const UnitDefinition& definition) -> bool {
  return std::visit(
      Overloaded{
          [](const DesignElementDefinition&) { return false; },
          [](const PackageDefinition&) { return true; },
          [](const CompilationUnitScope&) { return true; },
          [](const ClassSpecialization&) { return false; },
          [](const ToolText&) { return false; },
          [](const DesignRoot&) { return false; }},
      definition.form);
}

}  // namespace lyra::hir
