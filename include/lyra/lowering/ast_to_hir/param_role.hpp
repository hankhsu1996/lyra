#pragma once

#include <cstdint>
#include <unordered_set>
#include <vector>

namespace slang::ast {
class InstanceSymbol;
class ParameterSymbol;
class DefinitionSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

enum class ParamRole : uint8_t { kShape, kValueOnly };

// Classification of parameter roles across a design.
// Keyed by ParameterSymbol pointer -- stable within a compilation.
class ParamRoleTable {
 public:
  auto Lookup(const slang::ast::ParameterSymbol& p) const -> ParamRole;

  void MarkValueOnly(const slang::ast::ParameterSymbol* p) {
    value_only_params_.insert(p);
  }

 private:
  std::unordered_set<const slang::ast::ParameterSymbol*> value_only_params_;
};

// Classify parameter roles for all instances in the design.
// Conservative: anything referenced outside procedural bodies is Shape.
auto ClassifyParamRoles(
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances)
    -> ParamRoleTable;

}  // namespace lyra::lowering::ast_to_hir
