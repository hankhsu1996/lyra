#pragma once

#include <cstdint>

namespace lyra::lir {

// Who may name a symbol, and which artifacts write it. Internal: only this unit
// names it. External: any unit names it and this artifact is the one that
// writes it, so a second definition is a program that does not link. Shared:
// any unit names it and every artifact stating it writes it alike, so whatever
// resolves names across artifacts keeps one -- a name no unit owns, which every
// unit declaring it defines from the name and the prototype alone (LRM 35.4),
// and what a unit compiled to several bodies declares, which each body states.
enum class Linkage : std::uint8_t { kInternal, kExternal, kShared };

}  // namespace lyra::lir
