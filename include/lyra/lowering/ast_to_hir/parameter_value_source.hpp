#pragma once

#include <unordered_map>
#include <vector>

namespace slang::ast {
class InstanceSymbol;
class ParameterSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Where a value parameter's value comes from, for the objects built from one
// compiled scope. Settled at elaboration: the code reads it as the constant
// this elaboration settled it to. Supplied at construction: whoever builds
// the object passes it -- an instance's parameter its code reads (LRM 23.10.2),
// or the index a loop's block was built at (LRM 27.4). Computed at
// construction: its own declaration writes it from a supplied value, directly
// or through another such, so each object computes it when built.
enum class ParameterValueSource {
  kSettledAtElaboration,
  kSuppliedAtConstruction,
  kComputedAtConstruction,
};

// Where each value parameter of an instance's body takes its value from, and
// those its objects are handed, in the order the body declares them.
struct ParametersOfInstance {
  std::vector<const slang::ast::ParameterSymbol*> supplied;
  std::unordered_map<const slang::ast::ParameterSymbol*, ParameterValueSource>
      sources;
};

// Where each value parameter of `inst`'s body takes its value from, read off
// the body the instance is read through, and the parameters its objects are
// handed.
[[nodiscard]] auto ParametersOf(const slang::ast::InstanceSymbol& inst)
    -> ParametersOfInstance;

}  // namespace lyra::lowering::ast_to_hir
