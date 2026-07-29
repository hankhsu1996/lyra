#include "lyra/lowering/hir_to_mir/flat_packed_type.hpp"

#include <cstdint>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

auto InternFlatPacked(
    mir::CompilationUnit& unit, std::uint64_t width, mir::BitAtom atom)
    -> mir::TypeId {
  return unit.types.Intern(
      mir::PackedArrayType{
          .atom = atom,
          .signedness = mir::Signedness::kUnsigned,
          .dims = {mir::PackedRange{
              .left = static_cast<std::int64_t>(width) - 1, .right = 0}},
          .form = mir::PackedArrayForm::kExplicit});
}

}  // namespace lyra::lowering::hir_to_mir
