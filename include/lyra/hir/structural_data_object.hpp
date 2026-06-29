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

// A `ref` / `const ref` port's internal variable (LRM 23.3.3.2) aliases the
// connected variable rather than holding its own cell; the parent binds it
// during elaboration. Present only on a variable, never a net.
enum class ReferenceBinding : std::uint8_t { kRef, kConstRef };

// A variable (LRM 6.5): it owns mutable storage written by procedural
// assignments or a single continuous driver. An optional LRM 10.5 initializer;
// an optional `ref`-port aliasing marker.
struct StructuralVariableDecl {
  std::optional<ExprId> initializer;
  std::optional<ReferenceBinding> reference;
};

// A net (LRM 6.5): its value is the resolution of its drivers, not a direct
// write. A net-declaration assignment (`wire w = expr`) is normalized to a
// continuous-driver fact at AST-to-HIR, so a net holds no initializer here.
struct StructuralNetDecl {
  NetType net_type;
};

// A module-scope data object (LRM 6.5: "two main groups of data objects:
// variables and nets"). Variable and net are peer kinds sharing only identity
// and value type; each kind carries its own payload.
using StructuralDataObjectKind =
    std::variant<StructuralVariableDecl, StructuralNetDecl>;

struct StructuralDataObjectDecl {
  std::string name;
  TypeId type;
  StructuralDataObjectKind kind;
};

}  // namespace lyra::hir
