#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>

#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/type_id.hpp"

namespace lyra::hir {

struct StructuralDataObjectId {
  std::uint32_t value;

  auto operator<=>(const StructuralDataObjectId&) const
      -> std::strong_ordering = default;
};

// The net type of a net data object (LRM 6.6): it fixes how the net's drivers
// resolve. `wire` and `tri` share one resolution.
enum class NetType : std::uint8_t { kWire, kTri };

// Whether writing through a `ref` port's internal name is permitted
// (LRM 23.3.3.2).
enum class ReferenceBinding : std::uint8_t { kRef, kConstRef };

// A variable (LRM 6.5): it owns mutable storage written by procedural
// assignments or a single continuous driver, with an optional LRM 10.5
// initializer.
struct StructuralVariableDecl {
  std::optional<ExprId> initializer;
};

// A net (LRM 6.5): its value is the resolution of its drivers, not a direct
// write. A net-declaration assignment (`wire w = expr`) is normalized to a
// continuous-driver fact at AST-to-HIR, so a net holds no initializer here.
struct StructuralNetDecl {
  NetType net_type;
};

// A `ref` / `const ref` port's internal name (LRM 23.3.3.2): it owns no cell,
// standing for the connected variable, which the parent binds during
// elaboration.
struct StructuralReferenceDecl {
  ReferenceBinding binding;
};

// A module-scope data object (LRM 6.5: "two main groups of data objects:
// variables and nets"), plus the name a `ref` port introduces for storage the
// object does not own. Peer kinds sharing only identity and value type; each
// kind carries its own payload.
using StructuralDataObjectKind = std::variant<
    StructuralVariableDecl, StructuralNetDecl, StructuralReferenceDecl>;

struct StructuralDataObjectDecl {
  std::string name;
  TypeId type;
  StructuralDataObjectKind kind;
};

}  // namespace lyra::hir
