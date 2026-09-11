#pragma once

#include <cstdint>
#include <optional>

#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
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
// question as how many it makes, which a value answers about itself.

// How wide a stream is, and whether its bits carry x or z (LRM 11.4.14: a pack
// over any 4-state part yields a 4-state stream).
struct StreamShape {
  std::uint64_t width;
  mir::IntegralStateKind state_kind;
};

// The shape of the stream a value of `type` makes, and nothing for a type whose
// stream this cannot give: one whose bit count only the running program has
// (LRM 6.24.3 admits a dynamic array, a queue, an associative array and a
// string into a bit-stream type), and one the value layer carries no bit stream
// for at all. A stream is a value whose type states a width, so until such a
// type can be named, every operation over one refuses -- which is why this
// answers rather than asserts. Every construct written over a bit stream
// reaches it, so this is where that refusal is decided.
[[nodiscard]] auto FixedStreamShapeOf(
    const mir::TypePool& types, mir::TypeId type) -> std::optional<StreamShape>;

// The bits `value_id` makes, as a vector of that value's own stream shape.
[[nodiscard]] auto BuildToBitstream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId value_id,
    diag::SourceSpan span) -> diag::Result<mir::ExprId>;

// LRM 11.4.14.2: `stream_id` divided into `block_bits`-wide blocks whose order
// is reversed. A `block_bits` of zero is `>>`, which re-orders nothing and
// ignores any slice size written beside it, so the stream stands unchanged --
// the operator states which of the two a program asked for and nothing below
// reads the operator itself.
[[nodiscard]] auto BuildReorderedStream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId stream_id,
    std::uint64_t block_bits) -> mir::ExprId;

// `bits_id` read back as a value of `dst_type`. A stream narrower than the
// destination is first widened by filling zero bits on its right, which is what
// LRM 11.4.14 says left-aligns a stream in a wider target -- the opposite end
// from where an ordinary assignment extends its right-hand side.
[[nodiscard]] auto BuildFromBitstream(
    const mir::CompilationUnit& unit, mir::Block& block, mir::ExprId bits_id,
    mir::TypeId dst_type, diag::SourceSpan span) -> diag::Result<mir::Expr>;

}  // namespace lyra::lowering::hir_to_mir
