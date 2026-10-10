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

// What a port's internal name stands for where it owns no storage, and so who
// may write through it. A `ref` port's stands for the connected variable and a
// `const ref` port's does without being written through (LRM 23.3.3.2). An
// input port's stands for whatever the connection drives it with: the
// connection is a continuous assignment into it and nothing else may assign it
// (LRM 23.3.3.2), so it holds nothing that its driver does not.
enum class ReferenceBinding : std::uint8_t { kRef, kConstRef, kInput };

// A variable (LRM 6.5): it owns mutable storage written by procedural
// assignments or a single continuous driver, with an optional LRM 10.5
// initializer.
struct StructuralVariableDecl {
  std::optional<ExprId> initializer;

  auto operator==(const StructuralVariableDecl&) const -> bool = default;
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

  auto operator==(const StructuralNetDecl&) const -> bool = default;
};

// A port's internal name that owns no cell (LRM 23.3.3.2): it stands for
// storage the instantiating scope binds it to during elaboration.
struct StructuralReferenceDecl {
  ReferenceBinding binding;

  auto operator==(const StructuralReferenceDecl&) const -> bool = default;
};

// The index a loop generate counts with (LRM 27.4). It is an integer during
// elaboration and does not exist at simulation time, so it owns no storage the
// design can reach: what advances it is the loop, and what reads it are the
// loop's own expressions and the blocks it builds.
struct StructuralGenvarDecl {
  auto operator==(const StructuralGenvarDecl&) const -> bool = default;
};

// A value the scope is given when it is constructed, holding it for the rest
// of the run. The implicit localparam a loop generate's index name denotes
// inside a block is one (LRM 27.4): an integer parameter usable anywhere a
// normal one is, whose value in each block is the index that block elaborated
// at. A unit's parameter its instantiation overrides with a value the unit only
// reads is another (LRM 23.10.2). No expression of the scope settles either,
// because whoever constructs the scope supplies it -- which is what lets one
// block serve every index, and one unit every instance. A scope receives its
// values in the order it declares them, which is the order a construction
// states its arguments in.
struct StructuralConstructionValueDecl {
  auto operator==(const StructuralConstructionValueDecl&) const
      -> bool = default;
};

// A named constant of the scope (LRM 6.20.4), holding the expression the source
// assigned it. The scope settles it once, when it is built, and nothing writes
// it afterwards; a name from outside reaches it, including a hierarchical one
// inside a loop generate's block (LRM 27.4).
struct StructuralParameterDecl {
  ExprId initializer;

  auto operator==(const StructuralParameterDecl&) const -> bool = default;
};

// A module-scope data object (LRM 6.5: "two main groups of data objects:
// variables and nets"), plus the name a port introduces for storage the
// object does not own, the index a loop counts with, a value construction
// supplies, and a constant the scope settles for itself. Peer kinds sharing
// only identity and value type; each kind carries its own payload.
using StructuralDataObjectKind = std::variant<
    StructuralVariableDecl, StructuralNetDecl, StructuralReferenceDecl,
    StructuralGenvarDecl, StructuralConstructionValueDecl,
    StructuralParameterDecl>;

struct StructuralDataObjectDecl {
  std::string name;
  TypeId type;
  StructuralDataObjectKind kind;

  auto operator==(const StructuralDataObjectDecl&) const -> bool = default;
};

// Whether anything answers this declaration by the name it was written under.
// Every kind does except the index a loop counts with: it does not exist at
// simulation time (LRM 27.4), so the loop's own expressions reach it by
// identity and nothing else reaches it at all. The name stays on the
// declaration because a reader of the lowered form wants it; what this says is
// that no consumer may treat it as an identifier something answers to. Two
// loops of one scope counting with the same genvar name are two declarations,
// not one name written twice, and only this tells them apart.
[[nodiscard]] inline auto AnsweredByName(const StructuralDataObjectDecl& decl)
    -> bool {
  return !std::holds_alternative<StructuralGenvarDecl>(decl.kind);
}

}  // namespace lyra::hir
