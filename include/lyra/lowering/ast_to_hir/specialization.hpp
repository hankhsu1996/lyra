#pragma once

#include <vector>

#include "lyra/common/module_identity.hpp"

namespace slang::ast {
class InstanceSymbol;
class InstanceBodySymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Compute compile-owned representation fingerprint for a definition body.
// Hashes the definition-scoped CompileOwnedTypeStore, which captures
// compiled-representation facts (packed layout, signedness, struct/union
// fields, enum values) across all constructor alternatives. Repertoire
// structure (artifact coordinates, generate branches) is not included --
// those are constructor-assembly data. Does not include def_id -- that is
// paired externally by BuildSpecializationMap.
auto ComputeStructuralFingerprint(const slang::ast::InstanceBodySymbol& body)
    -> common::StructuralFingerprint;

// Build specialization groups from compile-owned body equivalence.
// Uses compile-owned repertoire projection. No parameter classification.
auto BuildSpecializationMap(
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances)
    -> common::SpecializationMap;

}  // namespace lyra::lowering::ast_to_hir
