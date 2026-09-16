#include <algorithm>
#include <format>
#include <optional>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::backend::cpp {

namespace {

// Whether the unit settles where any name lands while the design elaborates. A
// coordinate type exists in a unit's pool exactly when the unit formed one, so
// this reads the whole answer off the types rather than looking for the places
// a coordinate is used.
auto SettlesAnElaboratedCoordinate(const mir::CompilationUnit& unit) -> bool {
  return std::ranges::any_of(unit.types.Ids(), [&](mir::TypeId id) {
    const auto* library = unit.types.Get(id).As<mir::RuntimeLibraryType>();
    return library != nullptr &&
           (library->kind == mir::RuntimeLibraryKind::kPropertyCoordinate ||
            library->kind == mir::RuntimeLibraryKind::kBehaviorCoordinate);
  });
}

}  // namespace

auto RefusalFor(const mir::CompilationUnit& unit)
    -> std::optional<diag::Diagnostic> {
  if (!SettlesAnElaboratedCoordinate(unit)) {
    return std::nullopt;
  }
  // The unit is named as the reader wrote it. What goes to a reader is not what
  // goes to a target, so this is the one name here that is not spelled for a
  // language -- and it is a language that never sees this unit at all.
  return diag::Make(
      diag::DiagCode::kUnsupportedExpressionForm,
      std::format(
          "'{}' reaches a property or a behavior through a reference whose "
          "class no signature publishes; this backend spells a member by name "
          "and has none for a position settled while the design elaborates, so "
          "it is not yet supported here",
          unit.name));
}

}  // namespace lyra::backend::cpp
