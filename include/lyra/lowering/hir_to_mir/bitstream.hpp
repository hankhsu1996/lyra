#pragma once

#include <cstdint>
#include <optional>
#include <span>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/lowering/hir_to_mir/walk_frame.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr_id.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::lowering::hir_to_mir {

// LRM 6.24.3 defines, for every bit-stream type, the one sequence of bits a
// value of it makes: the first item most significant, an array's elements in
// the order a `foreach` walks them, a structure's members in declaration order.
// Two operations stand on that sequence and one on the sequence alone, and the
// three of them carry every source construct written over it -- a bit-stream
// cast (LRM 6.24.3) and a streaming operator in either direction (LRM 11.4.14).
//
// Reading the bits out and reading them back is the value's own work, stated
// here as the calls that ask for it: which bits a value makes is the same
// question as how many it makes, which a value answers about itself. Where
// every part of a value has a width its type fixes, its stream is an integral
// value of that width; where some part's width is the running program's -- a
// dynamic array, a queue, an associative array, a string -- it is a run of
// bits whose length is its own (LRM 11.4.14.4).

// How wide a stream is, and whether its bits carry x or z (LRM 11.4.14: a pack
// over any 4-state part yields a 4-state stream).
struct StreamShape {
  std::uint64_t width;
  mir::IntegralStateKind state_kind;
};

// The shape of the stream a value of `type` makes where the type fixes its
// width, and nothing for a type whose bit count only the running program has,
// or that holds no bit stream at all.
[[nodiscard]] auto FixedStreamShapeOf(
    const mir::CompilationUnit& unit, mir::TypeId type)
    -> std::optional<StreamShape>;

// Whether a value of `type` holds a class handle anywhere in it, whose stream
// is the properties of the object it refers to (LRM 11.4.14.1).
[[nodiscard]] auto HoldsClassHandle(
    const mir::CompilationUnit& unit, mir::TypeId type) -> bool;

// Whether a stream is read back into a value of `type`: an associative array
// is illegal as a destination wherever in it one stands (LRM 6.24.3), and the
// object a class handle refers to is not yet read back (LRM 11.4.14.3).
[[nodiscard]] auto IsStreamDestination(
    const mir::CompilationUnit& unit, mir::TypeId type) -> bool;

// The shape of the stream a product of `parts` makes: each part's laid end to
// end, the first most significant.
[[nodiscard]] auto FixedStreamShapeOfParts(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> parts)
    -> std::optional<StreamShape>;

// How many bits a value of `type` read out of a stream takes (LRM 6.24.3,
// 11.4.14.4): `empty_width` where every part whose width the running program
// decides is empty; and of the first such part, which takes what the rest
// leave, `first_element` the bits its first element takes before any step where
// its elements' own widths are the program's, and `multiplier` the bits each
// step takes -- none where every part's width is its type's.
struct StreamFill {
  std::uint64_t empty_width;
  std::uint64_t first_element;
  std::uint64_t multiplier;
};
[[nodiscard]] auto StreamFillOf(
    const mir::CompilationUnit& unit, mir::TypeId type) -> StreamFill;

// The bits `value_id` makes: an integral value of its stream shape where its
// type fixes the width, and a run of bits otherwise. `value_id` is handed as it
// stands, a place or a value; a packed one already is its stream, and a run of
// bits is its own.
[[nodiscard]] auto BuildToBitstream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value_id,
    diag::SourceSpan span) -> diag::Result<mir::ExprId>;

// The stream `stream`, integral or a run of bits, as a run of bits.
[[nodiscard]] auto BuildRunOf(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId stream)
    -> mir::ExprId;

// A run of `width` bits, a machine count, every bit 0; and the run of bits
// `part` written over the one the local `run` holds from `at` positions from
// its most significant end, a machine count, which is how a run whose width is
// known before its parts are is built in one pass.
[[nodiscard]] auto BuildRunOfZeros(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId width)
    -> mir::ExprId;
void InsertIntoRun(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId run,
    mir::ExprId at, mir::ExprId part);

// The streams `parts` names, each an integral stream or a run of bits, laid
// end to end into a run of bits the local `run` is declared holding, the first
// most significant (LRM 11.4.14.1); answers a read of it.
[[nodiscard]] auto BuildRunJoin(
    const mir::CompilationUnit& unit, mir::Block& block, mir::LocalId run,
    std::span<const mir::ExprId> parts) -> mir::ExprId;

// LRM 11.4.14.2: `stream_id` divided into `block_bits`-wide blocks whose order
// is reversed. A `block_bits` of zero is `>>`, which re-orders nothing and
// ignores any slice size written beside it, so the stream stands unchanged --
// the operator states which of the two a program asked for and nothing below
// reads the operator itself.
[[nodiscard]] auto BuildReorderedStream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId stream_id,
    std::uint64_t block_bits) -> mir::ExprId;

// How many bits the run of bits `run` holds, as a machine count.
[[nodiscard]] auto BuildRunLength(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run)
    -> mir::ExprId;

// The `count` bits `at` positions from the most significant end of the run of
// bits `run`, both machine counts, as a run of their own; and the bits from
// `at` as an integral value of `shape`.
[[nodiscard]] auto BuildRunExtract(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run,
    mir::ExprId at, mir::ExprId count) -> mir::ExprId;
[[nodiscard]] auto BuildRunLoad(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run,
    mir::ExprId at, StreamShape shape) -> mir::ExprId;

// `bits_id`, an integral stream, read back as a value of `dst_type`, whose
// type fixes its width. A stream narrower than the destination is first
// widened by filling zero bits on its right, which is what LRM 11.4.14 says
// left-aligns a stream in a wider target -- the opposite end from where an
// ordinary assignment extends its right-hand side.
[[nodiscard]] auto BuildFromBitstream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId bits_id,
    mir::TypeId dst_type) -> mir::ExprId;

// LRM 11.4.14: a stream, integral or a run of bits, assigned to a value of
// `dst_type`. A destination whose type fixes its width narrower than the stream
// is an error and a wider one takes the stream left-aligned; one holding a part
// whose width the program decides takes the fewest elements as wide as the
// stream, zero bits filling the rest.
[[nodiscard]] auto BuildFromStreamAssigned(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::ExprId bits_id, mir::TypeId dst_type) -> mir::ExprId;

// LRM 6.24.3: a stream cast to `dst_type`, which takes exactly the bits the
// stream holds or is an error where it cannot.
[[nodiscard]] auto BuildFromBitstreamCast(
    const mir::CompilationUnit& unit, const WalkFrame& frame,
    mir::ExprId bits_id, mir::TypeId dst_type) -> mir::ExprId;

// The value of `dst_type`, a type holding a part whose width the program
// decides, that the run of bits `run` holds exactly.
[[nodiscard]] auto BuildFromBitVec(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId run,
    mir::TypeId dst_type) -> mir::ExprId;

}  // namespace lyra::lowering::hir_to_mir
