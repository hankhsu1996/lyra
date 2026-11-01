#pragma once

#include <string>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/scope_construction.hpp"

namespace lyra::hir {

// One lowering of a unit: an elaborated body the front end did not share,
// lowered for the instance it was elaborated for and named by that instance's
// hierarchical path, with how its objects are built beyond what their code
// states. Lowerings whose HIR is equal are one body, and differ only in how
// their objects are built.
struct Lowering {
  std::string name;
  ScopeConstruction construction;
  CompilationUnit body;
  // Whether this lowering is the first of its unit to come out as its body,
  // so it is the one everything after HIR runs on. The body is held for every
  // lowering because the design root reads what each one builds off it.
  bool compiles_its_body = false;

  auto operator==(const Lowering&) const -> bool = default;
};

}  // namespace lyra::hir
