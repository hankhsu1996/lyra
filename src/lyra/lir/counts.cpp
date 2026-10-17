#include "lyra/lir/counts.hpp"

#include <cstdint>
#include <vector>

#include "lyra/lir/compilation_unit.hpp"
#include "lyra/lir/function.hpp"
#include "lyra/lir/type.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::lir {

auto CountsOf(const CompilationUnit& unit) -> std::vector<support::Count> {
  // Every member of the unit is taken apart by name, so a member added to it
  // does not compile here until it is said what kind of thing it is, or that
  // it is none.
  const auto& [name, body_name, types, classes, closures, records, external_records, external_classes, functions, type_operations, static_storage, constants, enum_tables, integral_constants, root] =
      unit;

  std::uint64_t members = 0;
  for (const Class& cls : classes) {
    const auto& [path, linkage, base, own, dispatch, implements, conforming] =
        cls;
    members += own.size();
  }
  std::uint64_t captures = 0;
  for (const Closure& closure : closures) {
    const auto& [held, invoke] = closure;
    captures += held.size();
  }
  std::uint64_t coroutines = 0;
  for (const Function& function : functions) {
    coroutines += types.Get(function.result_type).Is<CoroutineType>() ? 1 : 0;
  }

  return {
      {.kind = "classes", .held = classes.size()},
      {.kind = "members", .held = members},
      {.kind = "functions", .held = functions.size()},
      {.kind = "coroutines", .held = coroutines},
      {.kind = "closures", .held = closures.size()},
      {.kind = "captures", .held = captures},
      {.kind = "records", .held = records.size()},
      {.kind = "type operations", .held = type_operations.size()},
      {.kind = "static storage", .held = static_storage.size()},
      {.kind = "constants", .held = constants.size()},
      {.kind = "integral constants", .held = integral_constants.size()},
      {.kind = "enum tables", .held = enum_tables.size()},
      {.kind = "types", .held = types.size()},
      {.kind = "external classes", .held = external_classes.size()},
      {.kind = "external records", .held = external_records.size()},
  };
}

}  // namespace lyra::lir
