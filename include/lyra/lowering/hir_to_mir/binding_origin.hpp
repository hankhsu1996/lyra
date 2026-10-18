#pragma once

#include <cstdint>
#include <variant>

#include "lyra/hir/pattern_id.hpp"
#include "lyra/hir/procedural_var.hpp"
#include "lyra/hir/with_clause_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The receiver-role parameter of a body. The receiver is not a distinct kind
// of binding; it is reached by capture as any other is.
struct ReceiverOrigin {
  auto operator<=>(const ReceiverOrigin&) const = default;
};

// A share of the receiver, which keeps an object that lives while referenced
// (LRM 8.29) alive under a body that can outlive its caller's hold on it; a
// closure under such a body captures the share alone and takes the receiver
// from it.
struct ReceiverShareOrigin {
  auto operator<=>(const ReceiverShareOrigin&) const = default;
};

// The instance of the structural scope that declares a class (LRM 6.22), a
// role-tagged leading parameter of a body of the class that has no object
// recording it -- a static method, or the constructor, which enters its base
// before the object records anything.
struct DeclaringInstanceOrigin {
  auto operator<=>(const DeclaringInstanceOrigin&) const = default;
};

// The table a constructor of the design hierarchy is handed, which says how
// the instance at each of the object's instantiations is made.
struct ConstructionTableOrigin {
  auto operator<=>(const ConstructionTableOrigin&) const = default;
};

// A variable the source declares in a procedure.
struct ProceduralOrigin {
  hir::ProceduralVarId var;

  auto operator<=>(const ProceduralOrigin&) const = default;
};

// An iteration value a `with` clause binds (LRM 7.12.4): the element or the
// index.
struct IteratorOrigin {
  hir::WithClauseId clause;
  hir::IterationBindingRole role{};

  auto operator<=>(const IteratorOrigin&) const = default;
};

// A variable a pattern declares: the `VariablePattern` node that declares the
// identifier owns the declaration (LRM 12.6), so its own id is the binding's
// identity.
struct PatternOrigin {
  hir::PatternId pattern;

  auto operator<=>(const PatternOrigin&) const = default;
};

// A binding the lowering introduces: the deterministic site that owns it --
// the HIR statement that introduced a carrier, say -- and an ordinal within
// that site.
struct SynthesizedOrigin {
  std::uint32_t owner_site;
  std::uint32_t ordinal;

  auto operator<=>(const SynthesizedOrigin&) const = default;
};

// The cross-body identity of a capturable binding: the stable key under which a
// binding is deduplicated in a body and forwarded across callable boundaries.
// It is independent of any one body's physical slot (a `LocalId` or a
// closure-record field), so the same origin matches in the declaring body and
// in every closure that captures it. Each role-tagged parameter is a binding
// rather than a bare local so a closure inside the body reaches it by capture,
// as it reaches any other.
using BindingOriginId = std::variant<
    ReceiverOrigin, ReceiverShareOrigin, DeclaringInstanceOrigin,
    ConstructionTableOrigin, ProceduralOrigin, IteratorOrigin, PatternOrigin,
    SynthesizedOrigin>;

}  // namespace lyra::lowering::hir_to_mir
