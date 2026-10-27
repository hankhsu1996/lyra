#pragma once

#include <optional>

#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

// How one keeper of deferred effects implements keeping: the allocation
// function handing out storage for one effect, and the entry committing the
// effect made in it.
struct KeeperEntries {
  support::BuiltinFn allocate;
  support::BuiltinFn commit;
};

// The entries `keeper`, a type, keeps deferred effects by, or nothing where it
// keeps none.
[[nodiscard]] auto KeeperEntriesOf(const TypePool& types, TypeId keeper)
    -> std::optional<KeeperEntries>;

}  // namespace lyra::mir
