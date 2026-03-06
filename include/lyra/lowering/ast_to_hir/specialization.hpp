#pragma once

#include <vector>

#include "lyra/common/module_identity.hpp"

namespace slang::ast {
class InstanceSymbol;
class InstanceBodySymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

class ParamRoleTable;

auto ComputeStructuralFingerprint(
    const slang::ast::InstanceBodySymbol& body,
    const ParamRoleTable& param_roles) -> common::StructuralFingerprint;

auto BuildSpecializationMap(
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances,
    const ParamRoleTable& param_roles) -> common::SpecializationMap;

}  // namespace lyra::lowering::ast_to_hir
