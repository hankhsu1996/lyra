#pragma once

#include <cstddef>
#include <cstdint>
#include <map>
#include <optional>
#include <span>
#include <utility>
#include <vector>

#include "lyra/hir/structural_scope.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::lowering::ast_to_hir {

// `scope` with every loop of one body, at any depth, listing no values its
// index stood at. Such a loop builds its body at every index it counts out, so
// where its blocks stood is what running the loop says, and two objects of one
// class may count differently; the values matter only to a loop choosing among
// several bodies. Asked once the scope is whole, because blocks of one name
// taken into one body are merged loop by loop first, and a loop of one body
// there may be one of several once merged.
[[nodiscard]] auto WithoutIndicesOfLoopsOfOneBody(hir::StructuralScope scope)
    -> hir::StructuralScope;

// The bodies of one loop generate's blocks. A block's body is named by the
// class it is an object of and by which body of that class its code is, so
// blocks of one name are taken into one body, which comes to hold what each of
// their constructions chose (LRM 27.5) and counted (LRM 27.4); each value of
// the loop's index stands under the one body its block was taken into.
class LoopBodies {
 public:
  // Takes `scope`, the block standing at `indices`, into the body of its name.
  // False where that body and `scope` are not one, or where one of `indices`
  // already stands under another body.
  [[nodiscard]] auto Take(
      hir::StructuralScope scope, std::optional<std::uint64_t> body_arguments,
      std::span<const std::int64_t> indices) -> bool;

  // `loop` building each body taken, at the values standing under it.
  [[nodiscard]] auto Build(hir::BlocksRepeat loop) && -> hir::Generate;

 private:
  std::vector<hir::StructuralScope> bodies_;
  std::vector<std::optional<std::uint64_t>> body_arguments_;
  std::map<
      std::pair<support::DefPath, std::optional<std::uint64_t>>, std::size_t>
      named_;
  std::map<std::int64_t, std::size_t> standing_at_;
};

}  // namespace lyra::lowering::ast_to_hir
