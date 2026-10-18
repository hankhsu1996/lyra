#include "lyra/mir_transform/transform.hpp"

#include <utility>

#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/verify.hpp"
#include "lyra/mir_transform/box_large_values.hpp"
#include "lyra/mir_transform/inst_simplify.hpp"
#include "lyra/mir_transform/reaction.hpp"
#include "lyra/profiling/time_trace.hpp"

namespace lyra::mir_transform {

auto Transform(mir::CompilationUnit unit) -> mir::CompilationUnit {
  const profiling::TimeTraceScope span(
      "transform MIR", [&] { return unit.name; });
  mir::CompilationUnit transformed =
      SplitReactions(BoxLargeValues(InstSimplify(std::move(unit))));
  mir::Verify(transformed);
  return transformed;
}

}  // namespace lyra::mir_transform
