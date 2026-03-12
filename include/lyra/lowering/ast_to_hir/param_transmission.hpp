#pragma once

#include <cstdint>
#include <unordered_set>
#include <vector>

#include "lyra/common/module_identity.hpp"

namespace slang::ast {
class InstanceSymbol;
class ParameterSymbol;
}  // namespace slang::ast

namespace lyra::lowering::ast_to_hir {

// Whether a parameter requires per-instance transmission within its
// specialization group, or was absorbed by the specialization choice.
enum class ParamDisposition : uint8_t {
  // No per-instance transmission required. The parameter value is constant
  // across all instances in the specialization group (or the group has only
  // one instance). The value is folded into the compiled body.
  kAbsorbed,
  // Requires per-instance transmission. The parameter value varies across
  // instances within the same specialization group and needs a runtime slot.
  kTransmitted,
};

// Per-instance parameter transmission decisions, derived from within-group
// parameter variance after specialization grouping.
//
// Core M2a invariant:
//   varying within a specialization group  => kTransmitted (per-instance slot)
//   constant within a specialization group => kAbsorbed (folded into body)
//
// This invariant is only correct because the upstream grouping has already
// separated instances with different compile-owned body facts. If two
// instances have a parameter difference that affects compiled code shape,
// they must already be in different groups before this table is consulted.
//
// Keying: ParameterSymbol* is per-instance-body in slang's elaboration
// model (each elaborated body has its own symbol objects). This is the
// same identity basis as ParamRoleTable (m4 tracks replacing both with
// a group-scoped key).
class ParamTransmissionTable {
 public:
  auto Lookup(const slang::ast::ParameterSymbol& p) const -> ParamDisposition;

  void MarkTransmitted(const slang::ast::ParameterSymbol* p) {
    transmitted_params_.insert(p);
  }

 private:
  std::unordered_set<const slang::ast::ParameterSymbol*> transmitted_params_;
};

// Derive per-instance parameter transmission from within-group variance.
// For each specialization group, compares parameter values across instances.
// Parameters that vary within a group are marked as transmitted (need runtime
// slots); parameters that are constant within their group are absorbed.
auto DeriveParamTransmission(
    const common::SpecializationMap& spec_map,
    const std::vector<const slang::ast::InstanceSymbol*>& all_instances)
    -> ParamTransmissionTable;

}  // namespace lyra::lowering::ast_to_hir
