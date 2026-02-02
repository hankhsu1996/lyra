#include "check.hpp"

#include "frontend.hpp"
#include "verbose_logger.hpp"

namespace lyra::driver {

auto Check(const CompilationInput& input) -> int {
  VerboseLogger vlog(input.verbose);

  std::optional<ParseResult> parse_result;
  {
    PhaseTimer timer(vlog, "parse");
    parse_result = ParseFiles(input);
  }
  if (!parse_result) {
    return 1;
  }

  {
    PhaseTimer timer(vlog, "elaborate");
    if (!Elaborate(*parse_result, input)) {
      return 1;
    }
  }

  return 0;
}

}  // namespace lyra::driver
