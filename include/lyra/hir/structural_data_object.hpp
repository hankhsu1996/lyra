#pragma once

#include <compare>
#include <cstdint>
#include <optional>
#include <string>
#include <variant>

#include "lyra/base/pool_id.hpp"
#include "lyra/hir/expr_id.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/support/strength_level.hpp"

namespace lyra::hir {

struct StructuralDataObjectId {
  std::uint32_t value = base::kUnassignedId;

  auto operator<=>(const StructuralDataObjectId&) const
      -> std::strong_ordering = default;
};

// The net type of a net data object (LRM 6.6, Table 6-1): it fixes how the
// net's contributions resolve and what the net shows where nothing drives it.
// `wire` and `tri` are one net type under two spellings, as are `wand` and
// `triand`, and `wor` and `trior` (LRM 6.6.1, 6.6.3). The source spelling is
// kept, and what it states is derived where the net type is translated.
enum class NetType : std::uint8_t {
  kWire,
  kTri,
  kWand,
  kTriand,
  kWor,
  kTrior,
  kTri0,
  kTri1,
  kSupply0,
  kSupply1,
  kUwire,
  kTrireg,
};

// Whether writing through a `ref` port's internal name is permitted
// (LRM 23.3.3.2).
enum class ReferenceBinding : std::uint8_t { kRef, kConstRef };

// A variable (LRM 6.5): it owns mutable storage written by procedural
// assignments or a single continuous driver, with an optional LRM 10.5
// initializer.
struct StructuralVariableDecl {
  std::optional<ExprId> initializer;
};

// A net (LRM 6.5): its value is the resolution of its contributions, not a
// direct write. A net-declaration assignment (`wire w = expr`) is normalized to
// a continuous-driver fact at AST-to-HIR, so a net holds no initializer here.
// `charge_strength` is the strength a `trireg` declaration wrote for the value
// it stores, absent where it wrote none, which is the one net type that takes
// one (LRM 6.6.4).
struct StructuralNetDecl {
  NetType net_type;
  std::optional<support::StrengthLevel> charge_strength;
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
