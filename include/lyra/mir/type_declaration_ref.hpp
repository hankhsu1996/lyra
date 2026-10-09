#pragma once

#include <string>

#include "lyra/support/def_path.hpp"

namespace lyra::mir {

// The declaration a type is, named the way any unit names it: the unit that
// holds it and which declaration of that unit it is. The pair identifies the
// type from anywhere, so a unit naming another's reaches what that unit states
// about it by the same pair. How the pair is spelled belongs to whichever
// target a backend emits, so nothing here composes one.
struct TypeDeclarationRef {
  std::string unit_name;
  support::DefPath path;

  auto operator==(const TypeDeclarationRef&) const -> bool = default;
};

}  // namespace lyra::mir
