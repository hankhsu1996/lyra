#pragma once

#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"

namespace lyra::common {

struct Variable {
  SymbolId symbol{kInvalidSymbolId};
  Type type;
};

}  // namespace lyra::common
