#pragma once

#include <optional>

#include "lyra/mir/callable.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

// How one keeper of deferred effects implements keeping: the entry handing out
// storage for one effect, and the entry committing the effect made in it, as a
// Rust `impl Keeper for K` lists the functions a type answers a trait's
// methods by.
struct KeeperEntries {
  support::BuiltinFn allocate;
  support::BuiltinFn commit;
};

// The entries `keeper`, a type, keeps deferred effects by, or nothing where it
// keeps none.
[[nodiscard]] auto KeeperEntriesOf(const TypePool& types, TypeId keeper)
    -> std::optional<KeeperEntries>;

// What every keeper keeps, as a bound on the type of the effect it commits: a
// closure its region calls once, with nothing and answering nothing
// (`void_type`), after whatever kept it has returned -- the signature every
// region runs its effects at, as a Rust trait states the bound on its
// associated type once.
[[nodiscard]] auto KeptEffectBound(TypeId void_type) -> CalledClosureBound;

}  // namespace lyra::mir
