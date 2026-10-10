#pragma once

#include <string>
#include <vector>

#include "lyra/lir/integral_constant.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// One member of an enumeration: its name, and its value at the enumeration's
// base type.
struct EnumTableMember {
  std::string name;
  IntegralConstant value;
};

// One enumeration's member table a unit holds: the integral type the
// enumeration is based on, and its members in declared order. It is the whole
// of what the language asks of a value against the members (LRM 6.19.5,
// 6.24.2), so a target states one as data, laid out as the library that reads
// it says.
struct EnumTableDecl {
  TypeId base;
  std::vector<EnumTableMember> members;
};

}  // namespace lyra::lir
