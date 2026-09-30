#pragma once

#include <string>

namespace lyra::mir {

// The declaration a type is, named the way any unit names it: the unit that
// declares it and the name it has there. The pair identifies the type from
// anywhere, so a unit naming another's reaches what that unit states about it
// by the same pair.
struct TypeDeclarationRef {
  std::string unit_name;
  std::string name;

  auto operator==(const TypeDeclarationRef&) const -> bool = default;
};

}  // namespace lyra::mir
