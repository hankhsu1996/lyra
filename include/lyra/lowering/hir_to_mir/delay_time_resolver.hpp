#pragma once

#include <cstdint>

#include "lyra/base/time.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/hir/primary.hpp"

namespace lyra::lowering::hir_to_mir {

class DelayTimeResolver {
 public:
  explicit DelayTimeResolver(TimeResolution resolution);

  // Takes the signed `hir::IntegerLiteral::value` payload; rejects negative.
  [[nodiscard]] auto ResolveIntegerDelay(
      std::int64_t value, diag::SourceSpan span) const
      -> diag::Result<SimDuration>;

  [[nodiscard]] auto ResolveTimeLiteral(
      double value, hir::TimeScale literal_unit, diag::SourceSpan span) const
      -> diag::Result<SimDuration>;

 private:
  TimeResolution resolution_;
};

}  // namespace lyra::lowering::hir_to_mir
