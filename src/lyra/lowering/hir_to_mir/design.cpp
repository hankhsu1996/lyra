#include "lyra/lowering/hir_to_mir/design.hpp"

#include <type_traits>
#include <variant>

#include "lyra/lowering/hir_to_mir/module.hpp"
#include "lyra/lowering/hir_to_mir/package.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {
template <class>
inline constexpr bool always_false_v = false;
}  // namespace

auto LowerDesign(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena, OriginMap* origin_map) -> mir::Design {
  mir::Design result;

  for (const auto& element : design.elements) {
    std::visit(
        [&](const auto& e) {
          using T = std::decay_t<decltype(e)>;
          if constexpr (std::is_same_v<T, hir::Module>) {
            result.elements.emplace_back(
                LowerModule(e, input, mir_arena, origin_map));
          } else if constexpr (std::is_same_v<T, hir::Package>) {
            result.elements.emplace_back(
                LowerPackage(e, input, mir_arena, origin_map));
          } else {
            static_assert(always_false_v<T>, "unhandled hir::DesignElement");
          }
        },
        element);
  }

  return result;
}

}  // namespace lyra::lowering::hir_to_mir
