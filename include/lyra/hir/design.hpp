#pragma once

#include <variant>
#include <vector>

#include "lyra/hir/callable_signature.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/package.hpp"

namespace lyra::hir {

using DesignElement = std::variant<Module, Package>;

struct Design {
  std::vector<DesignElement> elements;
  // Specialization-owned shared module bodies, indexed by ModuleBodyId.
  // One body per specialization group.
  std::vector<ModuleBody> module_bodies;

  // Persistent callable signature table. Populated during AST-to-HIR
  // lowering for every callable symbol (functions, tasks, DPI imports).
  // Queried by deferred assertion lowering and any other HIR consumer
  // that needs callable metadata.
  HirCallableSignatureTable callable_signatures;
};

}  // namespace lyra::hir
