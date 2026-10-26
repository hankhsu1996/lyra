#pragma once

#include <optional>
#include <vector>

#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/owned_child_step.hpp"

namespace lyra::hir {

// The scope each block body of `unit` is read inside, by the body's id: one
// publishing the class the body's own class is published inside -- the unit's
// own scope where that is none, which `std::nullopt` stands for, and otherwise
// the first block body publishing it. Every scope publishing one class
// publishes alike, and a body reaches the scopes around it only through what
// they published, so whichever stands there is read the same; taking the first
// by id makes the answer a function of the unit alone.
[[nodiscard]] auto EnclosingScopesOf(const CompilationUnit& unit)
    -> std::vector<std::optional<BlockBodyId>>;

}  // namespace lyra::hir
