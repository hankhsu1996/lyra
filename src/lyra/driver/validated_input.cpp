#include "validated_input.hpp"

#include <utility>

#include "dpi_link_input.hpp"
#include "frontend.hpp"
#include "lyra/common/diagnostic/diagnostic.hpp"

namespace lyra::driver {

auto ValidateCompilationInput(CompilationInput raw)
    -> lyra::Result<ValidatedCompilationInput> {
  auto validated_dpi = ValidateDpiLinkInputs(raw.dpi_link_inputs);
  if (!validated_dpi) {
    return std::unexpected(validated_dpi.error());
  }

  raw.dpi_link_inputs = std::move(*validated_dpi);

  return ValidatedCompilationInput{
      .input = std::move(raw),
  };
}

}  // namespace lyra::driver
