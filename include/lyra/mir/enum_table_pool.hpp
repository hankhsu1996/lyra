#pragma once

#include <cstddef>

#include "lyra/base/interner.hpp"
#include "lyra/mir/enum_table_id.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

struct EnumTableHash {
  auto operator()(const EnumType& enumeration) const -> std::size_t {
    std::size_t seed = 0;
    HashEnumeration(seed, enumeration);
    return seed;
  }
};

// The enumerations whose members a body of the unit asks about (LRM 6.19.5,
// 6.24.2). The members are constant data of the enumeration, so the unit states
// each table once however many questions name it, and an enumeration nothing
// asks about has none.
using EnumTablePool = base::Interner<EnumType, EnumTableId, EnumTableHash>;

}  // namespace lyra::mir
