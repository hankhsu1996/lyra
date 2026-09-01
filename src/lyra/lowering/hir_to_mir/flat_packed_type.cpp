#include "lyra/lowering/hir_to_mir/flat_packed_type.hpp"

#include <cstdint>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"

namespace lyra::lowering::hir_to_mir {

auto InternFlatPacked(
    const mir::CompilationUnit& unit, std::uint64_t width,
    mir::IntegralStateKind state_kind) -> mir::TypeId {
  return unit.types.Intern(
      mir::Type{mir::PackedArrayType{
          .state_kind = state_kind,
          .signedness = mir::Signedness::kUnsigned,
          .dims = {mir::PackedRange{
              .left = static_cast<std::int64_t>(width) - 1, .right = 0}}}});
}

}  // namespace lyra::lowering::hir_to_mir
