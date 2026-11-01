#include "lyra/mir/keeper.hpp"

#include <optional>

#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::mir {

auto KeeperEntriesOf(const TypePool& types, TypeId keeper)
    -> std::optional<support::KeeperEntries> {
  const auto* library = types.Get(keeper).As<LibraryType>();
  if (library == nullptr) {
    return std::nullopt;
  }
  return support::DeclarationOf(library->decl).keeper;
}

}  // namespace lyra::mir
