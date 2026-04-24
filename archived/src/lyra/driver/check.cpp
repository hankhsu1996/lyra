#include "check.hpp"

#include <optional>

#include "compilation_output.hpp"
#include "driver_output_options.hpp"
#include "frontend.hpp"

namespace lyra::driver {

auto Check(const CompilationInput& input) -> int {
  CompilationOutput output(BuildCheckDriverOutputOptions(input));

  std::optional<ParseResult> parse_result;
  {
    PhaseTimer timer(output, Phase::kParse);
    parse_result = ParseFiles(input);
  }
  if (!parse_result) {
    output.Flush();
    return 1;
  }

  {
    PhaseTimer timer(output, Phase::kElaborate);
    if (!Elaborate(*parse_result, input)) {
      output.Flush();
      return 1;
    }
  }

  output.Flush();
  return 0;
}

}  // namespace lyra::driver
