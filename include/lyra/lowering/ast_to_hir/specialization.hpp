#pragma once

#include <vector>

#include "lyra/common/module_identity.hpp"

namespace slang::ast {
class InstanceSymbol;
class InstanceBodySymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Compute the v1 declaration-shape fingerprint for an instance body.
// Captures variable/net names and resolved types. Does not capture
// procedural/behavioral compile-owned shape (tracked as M2c).
// Does not include def_id -- that is paired externally by
// BuildSpecializationMap.
auto ComputeStructuralFingerprint(const slang::ast::InstanceBodySymbol& body)
    -> common::StructuralFingerprint;

// Build specialization groups from compile-owned body equivalence.
// v1: declaration-shape grouping only. No parameter classification.
auto BuildSpecializationMap(
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances)
    -> common::SpecializationMap;

}  // namespace lyra::lowering::ast_to_hir
