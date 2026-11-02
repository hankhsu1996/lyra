#pragma once

#include <cstdint>

namespace lyra::support {

// Which artifacts of the program may name a definition, and how many may
// define it. Internal: only the artifact defining it names it, so each artifact
// stating a definition of that name has its own. External: any artifact names
// it and exactly one defines it, so a second definition is a program that does
// not link. Shared: any artifact names it and every artifact stating it
// defines it alike, so whatever resolves names across artifacts keeps one --
// a name no unit owns, which every unit declaring it defines from the name and
// the prototype alone (LRM 35.4), and what a unit compiled to several bodies
// declares, which each body states. A shared definition is kept in place of
// another, so it names nothing whose content is one artifact's own.
enum class Linkage : std::uint8_t { kInternal, kExternal, kShared };

}  // namespace lyra::support
