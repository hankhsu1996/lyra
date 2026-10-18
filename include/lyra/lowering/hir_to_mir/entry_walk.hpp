#pragma once

#include <functional>
#include <optional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/binding_origin.hpp"
#include "lyra/lowering/hir_to_mir/statement/loops.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// An unpacked array whose entries a loop visits: the path to it, settled where
// the loop stands, so whatever reaching it computes is evaluated once however
// many entries are visited, and the array's type.
struct WalkedArray {
  SettledPath path;
  mir::TypeId type;
};

// The array `path` names, settled in the frame's block.
[[nodiscard]] auto WalkedArrayAt(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, AccessPath path)
    -> WalkedArray;

// The array `object` evaluates to, already in the frame's block: the storage
// it names where it is a place, and otherwise its value, held in a local of
// that block for as long as the loop runs, as Rust's `for` holds the value it
// iterates.
[[nodiscard]] auto WalkedArrayOf(
    const UnitLowerer& unit_lowerer, const WalkFrame& frame, mir::ExprId object)
    -> WalkedArray;

// How many entries `array` holds, read in `block`: fixed by the declaration
// for a fixed-size array (LRM 7.4), and read while the program runs otherwise
// (LRM 7.5, 7.10).
[[nodiscard]] auto EntryCount(
    mir::CompilationUnit& unit, mir::Block& block, const WalkedArray& array)
    -> mir::ExprId;

// Where one entry lies while the loop visits it: the variable holding its
// index (LRM 7.12.4) -- the key an associative array holds it under, the index
// its declaration gives it for a fixed-size array, and its position for an
// array sized while the program runs -- and, for an array that numbers its
// elements, the variable holding its position, counted from 0.
struct EntryPlace {
  mir::LocalId index;
  mir::TypeId index_type;
  std::optional<mir::LocalId> position;
};

// One entry while the loop visits it: the element's value, named once in the
// block the visit runs in, and where the entry lies.
struct VisitedEntry {
  mir::ExprId element;
  EntryPlace place;
};

// The step, added to `block`, from an array laid out as the walked one is --
// the same kind, the same range -- to the entry at `place` in it, whose part
// is of `part_type`: its position for an array that numbers its elements, its
// key for an associative one.
[[nodiscard]] auto StepToVisited(
    mir::CompilationUnit& unit, mir::Block& block, const EntryPlace& place,
    mir::TypeId part_type) -> DescentStep;

// What runs at each entry, lowered into the block the frame names. A `break`
// appended there ends the walk.
using EntryVisit = std::function<diag::Result<void>(
    const WalkFrame& at, const VisitedEntry& entry)>;

// The loop that visits every entry of `array` in the order its kind keeps them
// (LRM 7.12), from the first or from the last as `order` says: by position for
// an array that numbers its elements, and by key in the order its index type
// imposes for an associative one (LRM 7.8, 7.9.4). The variable holding the
// index carries `index_origin` where one is given, so what the visit lowers
// reads it by that identity. The declarations the loop needs are appended to
// the frame's block, whose statements must therefore run before the returned
// loop.
auto BuildEntryLoopStmt(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const WalkedArray& array,
    WalkOrder order, std::optional<BindingOriginId> index_origin,
    const EntryVisit& visit) -> diag::Result<mir::Stmt>;

}  // namespace lyra::lowering::hir_to_mir
