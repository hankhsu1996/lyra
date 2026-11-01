#pragma once

#include <optional>

#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/library_decl.hpp"

namespace lyra::mir {

// The entries `keeper`, a type, keeps deferred effects by, or nothing where it
// keeps none.
[[nodiscard]] auto KeeperEntriesOf(const TypePool& types, TypeId keeper)
    -> std::optional<support::KeeperEntries>;

}  // namespace lyra::mir
