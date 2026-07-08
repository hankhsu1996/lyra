#pragma once

#include <cstdint>
#include <vector>

#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::mir {

struct CompilationUnit;

// How a runtime tree node is represented, read as one settled fact rather than
// re-derived from the base type variant. A unit instance (a module / interface
// / program) carries a UnitDefinition -- its per-specialization identity, its
// def-name (LRM 23.8), and its root program. A generate scope is an intra-unit
// naming scope that carries only a ScopeProgram and publishes no def-name.
enum class ScopeRepresentationKind : std::uint8_t {
  kUnitInstance,
  kGenerateScope,
};

// What an object inherits from its base, resolved through one entry point so a
// consumer reads each fact rather than re-deriving it from a closed
// classification. `renderable` is the base type the one type-mapping dispatch
// turns into the target base-class name. `is_runtime_tree_node` says the object
// is a node in the runtime object tree (it participates in the elaboration
// lifecycle and carries the scope ctor prefix); a plain object that extends
// another plain class is not. `representation` distinguishes a unit instance
// from a generate scope. `ctor_prefix` is the base's construction contract --
// the params an instance forwards straight to the base constructor.
struct BaseContract {
  TypeId renderable;
  bool is_runtime_tree_node;
  ScopeRepresentationKind representation;
  std::vector<ParamDecl> ctor_prefix;
};

auto ResolveBaseContract(const CompilationUnit& unit, const ClassRef& base)
    -> BaseContract;

}  // namespace lyra::mir
