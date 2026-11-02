#pragma once

// What a write names (LRM A.8.5): one place, a join of lvalues, a select of a
// join, or a stream target. Each is written by giving each place it is made of
// its share of the one value written, and a join is read by joining what those
// places hold, so every kind of write -- an assignment now or later, an
// assignment operator, a continuous assignment, an output actual, a takeover
// -- asks this for the shares and makes its own kind of write to each.

#include <cstdint>
#include <functional>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/lowering/hir_to_mir/access_path.hpp"
#include "lyra/lowering/hir_to_mir/expression/expr_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/expression/selects.hpp"
#include "lyra/lowering/hir_to_mir/unit_lowerer.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

// The members share the bits of one packed vector, the first member most
// significant, each as many as it is wide (LRM 11.4.12).
//
//   {a, b[3:0], {c, d}} = v;
struct PackedJoin {};

// Member `i` takes element `i` of an array or member `i` of a structure (LRM
// 10.9).
//
//   pair_t'{a, b} = v;
struct UnpackedJoin {};

using JoinKind = std::variant<PackedJoin, UnpackedJoin>;

struct Lvalue;

struct LvalueJoin {
  std::vector<Lvalue> members;
  JoinKind kind;
};

// Bits of a packed join, from `start` in the numbering of the vector its
// members make together (LRM 11.5.1). A select over one place is a step of the
// place, and over a join it stays a select: each member is written the whole
// value at `start` counted from where the member begins, of which its own slice
// write keeps the bits it has, so `start` may be a position the program
// computes. Only a name a modport joins declarations under is a join a select
// can be written over (LRM 25.5.4).
//
//   modport m(output .Pair({hi, lo}));
//   p.Pair[i +: 4] = v;
struct LvalueSlice {
  LvalueJoin join;
  mir::ExprId start;
};

// LRM 11.4.14.4: an array of `type` in a stream target, of `element`s, of
// which only the elements a `with` range names are filled. The range is
// evaluated when the array's turn comes, so that it reads what the stream
// unpacked into the members before it; `window` evaluates it then, as a select
// of the array where it is located.
//
//   {>> {len, payload with [0 +: len]}} = v;
struct WindowedPlace {
  AccessPath array;
  mir::TypeId type;
  mir::TypeId element;
  std::function<diag::Result<StreamWindow>(
      const WalkFrame& at, const AccessPath& array)>
      window;
};

struct StreamTargetMember;

// The value is read as a stream of bits, the order of its `block_bits`-wide
// blocks is reversed, and the members are filled in order from its most
// significant end (LRM 11.4.14.3). A `block_bits` of zero re-orders nothing.
//
//   {<< 8 {a, b}} = v;
struct StreamTarget {
  std::uint64_t block_bits = 0;
  std::vector<StreamTargetMember> members;
};

// `type` is the type of the value the lvalue holds: the place's own, what the
// join's members make together, or the selected bits'; a stream target holds no
// value, its members' bits being cut from the stream, and has none.
// `source_type` is that type as the source declared it, which is what a drive
// compares the type of its source with.
struct Lvalue {
  std::variant<AccessPath, LvalueJoin, LvalueSlice, StreamTarget> form;
  mir::TypeId type;
  hir::TypeId source_type;
  diag::SourceSpan span;
};

// An lvalue a stream target fills with as many bits as its type fixes (LRM
// 11.4.14.3).
struct StreamVariable {
  Lvalue target;
};

struct StreamTargetMember {
  std::variant<StreamVariable, WindowedPlace, StreamTarget> form;
  diag::SourceSpan span;
};

// One place and the value a write gives it.
struct Share {
  AccessPath place;
  mir::ExprId value;
};

// Where a share goes as it is cut: kept for a write that makes them all
// later, or written at once.
using ShareSink = std::function<void(Share)>;

// An lvalue a construct reads and then writes: every place of it settled, and
// the value those places hold going in.
struct LvalueReadThenWritten {
  Lvalue lvalue;
  mir::ExprId incoming;
};

// What `expr` names as the target of a write, with the checks each place owes
// before a write lands appended where the statement is reached.
template <ExprLowerer L>
auto LowerLvalue(L& lowerer, const hir::Expr& expr, WalkFrame frame)
    -> diag::Result<Lvalue>;

template <typename Visit>
void ForEachPlace(Lvalue& lvalue, Visit visit);

template <typename Visit>
void ForEachPlace(const Lvalue& lvalue, Visit visit);

// Each place a stream target is made of, in the order the source wrote them.
template <typename Visit>
void ForEachPlace(StreamTarget& stream, Visit visit) {
  for (StreamTargetMember& member : stream.members) {
    std::visit(
        Overloaded{
            [&](StreamVariable& variable) {
              ForEachPlace(variable.target, visit);
            },
            [&](WindowedPlace& windowed) { visit(windowed.array); },
            [&](StreamTarget& nested) { ForEachPlace(nested, visit); }},
        member.form);
  }
}

template <typename Visit>
void ForEachPlace(const StreamTarget& stream, Visit visit) {
  for (const StreamTargetMember& member : stream.members) {
    std::visit(
        Overloaded{
            [&](const StreamVariable& variable) {
              ForEachPlace(variable.target, visit);
            },
            [&](const WindowedPlace& windowed) { visit(windowed.array); },
            [&](const StreamTarget& nested) { ForEachPlace(nested, visit); }},
        member.form);
  }
}

// Each place `lvalue` is made of, in the order the source wrote them. A select
// of a join is made of the join's places.
template <typename Visit>
void ForEachPlace(Lvalue& lvalue, Visit visit) {
  std::visit(
      Overloaded{
          [&](AccessPath& place) { visit(place); },
          [&](LvalueJoin& join) {
            for (Lvalue& member : join.members) {
              ForEachPlace(member, visit);
            }
          },
          [&](LvalueSlice& slice) {
            for (Lvalue& member : slice.join.members) {
              ForEachPlace(member, visit);
            }
          },
          [&](StreamTarget& stream) { ForEachPlace(stream, visit); }},
      lvalue.form);
}

template <typename Visit>
void ForEachPlace(const Lvalue& lvalue, Visit visit) {
  std::visit(
      Overloaded{
          [&](const AccessPath& place) { visit(place); },
          [&](const LvalueJoin& join) {
            for (const Lvalue& member : join.members) {
              ForEachPlace(member, visit);
            }
          },
          [&](const LvalueSlice& slice) {
            for (const Lvalue& member : slice.join.members) {
              ForEachPlace(member, visit);
            }
          },
          [&](const StreamTarget& stream) { ForEachPlace(stream, visit); }},
      lvalue.form);
}

// Each place `slice` reaches, in the order of its join's members: the member
// at the select's start counted from where the member begins, as many bits as
// `type`, which some of them lie outside.
[[nodiscard]] auto SelectedParts(
    const mir::CompilationUnit& unit, mir::Builder block,
    const LvalueSlice& slice, mir::TypeId type) -> std::vector<AccessPath>;

// Each place a write of `lvalue` lands on, in the order its shares are cut: a
// select of a join lands on the members at the bits it reaches, where the
// places `lvalue` is made of are the members whole.
[[nodiscard]] auto PlacesWritten(
    const mir::CompilationUnit& unit, mir::Builder block, const Lvalue& lvalue)
    -> std::vector<AccessPath>;

// The share of `value` each place of `lvalue` takes, where `value` is a value
// or the place one lies in. One place takes the value, read where it is a
// place. A join or a stream target cuts it down to the places, taking each
// part where it lies, and holds a value that lies nowhere once, as a step of
// `frame`'s block; a select of a join gives every place the whole of it. The
// shares are cut in the order the source wrote the places.
[[nodiscard]] auto Shares(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    mir::ExprId value) -> diag::Result<std::vector<Share>>;

// Writes `value` to `lvalue` where `frame` stands: one store per place, each
// of its share, appended to the frame's block as the share is cut. Every place
// of a join, of a select of one or of a stream target is located before the
// first is written, so a member whose index another member's store would change
// names what it named when the write began; a `with` range in a stream target
// is evaluated at its turn, after the members before it are written
// (LRM 11.4.14.4).
[[nodiscard]] auto AppendStores(
    UnitLowerer& unit_lowerer, const WalkFrame& frame, const Lvalue& lvalue,
    mir::ExprId value) -> diag::Result<void>;

// `lvalue` as an operand that is read and then written (LRM 11.4.1, 13.5
// `inout`): every place settled here, and the value they hold going in, joined
// by kind and, for a select of a join, selected from that.
[[nodiscard]] auto ReadThenWrite(
    const mir::CompilationUnit& unit, const WalkFrame& frame, Lvalue lvalue)
    -> diag::Result<LvalueReadThenWritten>;

// Whether `expr` in target position names several places -- a join of lvalues,
// a name a view joins them under, a select of that name, or a stream target --
// rather than one.
template <ExprLowerer L>
[[nodiscard]] auto IsJoin(const L& lowerer, const hir::Expr& expr) -> bool;

}  // namespace lyra::lowering::hir_to_mir
