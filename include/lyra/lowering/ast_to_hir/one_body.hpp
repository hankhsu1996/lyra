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

// The one body two scopes of one name are, or nothing where they are not one.
// They are one where they differ in nothing but what their own constructions
// decide, at any depth beneath them, by an expression reading a value the
// construction is handed (LRM 27.4, 27.5): which alternative of a conditional
// generate stood, where the body holds every alternative either selected, and
// which blocks a loop counted out, where the body holds every body either
// counted one of. Two scopes of one name that are not one are a defect of how
// the name was worked out, and each caller reports it as one.
[[nodiscard]] auto OneBodyOf(
    const hir::StructuralScope& a, const hir::StructuralScope& b)
    -> std::optional<hir::StructuralScope>;

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
      hir::StructuralScope scope, std::optional<std::uint64_t> code_arguments,
      std::span<const std::int64_t> indices) -> bool;

  // `loop` building each body taken, at the values standing under it.
  [[nodiscard]] auto Build(hir::BlocksRepeat loop) && -> hir::Generate;

 private:
  std::vector<hir::StructuralScope> bodies_;
  std::vector<std::optional<std::uint64_t>> code_arguments_;
  std::map<
      std::pair<support::DefPath, std::optional<std::uint64_t>>, std::size_t>
      named_;
  std::map<std::int64_t, std::size_t> standing_at_;
};

}  // namespace lyra::lowering::ast_to_hir
