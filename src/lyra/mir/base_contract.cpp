#include "lyra/mir/base_contract.hpp"

#include <variant>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

auto ResolveBaseContract(const CompilationUnit& unit, const ClassRef& base)
    -> BaseContract {
  const auto& runtime_base = std::get<RuntimeLibraryClassRef>(base);
  const bool is_instance = std::holds_alternative<InstanceType>(
      unit.types.Get(runtime_base.base_type).data);
  const auto& builtins = unit.builtins;
  return BaseContract{
      .renderable = runtime_base.base_type,
      .is_runtime_tree_node = true,
      .representation = is_instance ? ScopeRepresentationKind::kUnitInstance
                                    : ScopeRepresentationKind::kGenerateScope,
      .ctor_prefix = {
          ParamDecl{.name = "parent", .type = builtins.scope_ptr},
          ParamDecl{.name = "segment", .type = builtins.hierarchy_segment},
          ParamDecl{.name = "services", .type = builtins.services}}};
}

}  // namespace lyra::mir
