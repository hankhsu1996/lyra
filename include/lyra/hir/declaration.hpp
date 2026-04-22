#pragma once

#include <string>
#include <utility>
#include <variant>

#include "lyra/common/integral_constant.hpp"
#include "lyra/common/scope_types.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/fwd.hpp"

namespace lyra::hir {

// HIR-owned declaration objects. Each family has its own arena-allocated
// dense-index ID (VariableId, NetId, ParameterId, InstanceMemberId). The
// `bridge_symbol` field carries the legacy SymbolId back-reference for
// call sites that still resolve by SymbolId (callable axis, remote refs,
// dumpers, DPI, repertoire descriptors).

struct Variable {
  std::string name;
  TypeId type;
  SourceSpan span;
  ScopeId scope;
  SymbolId bridge_symbol;
};

struct Net {
  std::string name;
  TypeId type;
  SourceSpan span;
  ScopeId scope;
  SymbolId bridge_symbol;
};

struct Parameter {
  std::string name;
  TypeId type;
  SourceSpan span;
  ScopeId scope;
  SymbolId bridge_symbol;
  IntegralConstant init_value;
  bool is_design_storage = false;
};

struct InstanceMember {
  std::string name;
  TypeId type;
  SourceSpan span;
  ScopeId scope;
  SymbolId bridge_symbol;
};

// Typed reference to an HIR-owned local declaration. Used by HIR payload
// fields (NameRefExpressionData, AssignTarget, HirCaptureTarget) to point
// at a variable/net/parameter/instance-member without going through a
// design-global SymbolId lookup.
//
// The payload is a std::variant over typed IDs. There is no raw integer
// storage -- consumers read by `std::holds_alternative<T>` /
// `std::get<T>` / `std::visit`. No unchecked reinterpretation from a
// numeric id is possible by construction.
struct DeclRef {
  std::variant<VariableId, NetId, ParameterId, InstanceMemberId> payload;

  static auto FromVariable(VariableId v) -> DeclRef {
    return DeclRef{.payload = v};
  }
  static auto FromNet(NetId n) -> DeclRef {
    return DeclRef{.payload = n};
  }
  static auto FromParameter(ParameterId p) -> DeclRef {
    return DeclRef{.payload = p};
  }
  static auto FromInstanceMember(InstanceMemberId m) -> DeclRef {
    return DeclRef{.payload = m};
  }

  auto operator==(const DeclRef&) const -> bool = default;

  template <typename H>
  friend auto AbslHashValue(H h, const DeclRef& d) -> H {
    // Visit-based hashing: each alternative contributes its own typed
    // hash (the typed ID's AbslHashValue), so future DeclRef
    // alternatives do not silently reuse integer indices. No variant
    // index() leaks into the hash.
    return std::visit(
        [h = std::move(h)](const auto& id) mutable -> H {
          return H::combine(std::move(h), id);
        },
        d.payload);
  }
};

// Consumer sites must use common::Overloaded with one lambda per
// alternative and NO default branch, so missing cases are compile
// errors, not silent fallthrough:
//
//   #include "lyra/common/overloaded.hpp"
//
//   std::visit(common::Overloaded{
//       [&](VariableId v) { ... },
//       [&](NetId n) { ... },
//       [&](ParameterId p) { ... },
//       [&](InstanceMemberId m) { ... },
//   }, decl_ref.payload);
//
// Do NOT use `std::holds_alternative` chains or conditional branches
// on the variant index. Future DeclRef alternatives must cause a
// build error at every consumer site that has not been updated,
// which is the desired behavior.

}  // namespace lyra::hir
