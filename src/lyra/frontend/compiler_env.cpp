#include "compiler_env.hpp"

namespace lyra::frontend {

namespace {
constexpr auto kLyraIdentityMacro = "__lyra__=1";
constexpr auto kPredefineSource = "<lyra>";
}  // namespace

auto BuildLyraPreprocessorOptions(std::span<const std::string> user_defines)
    -> slang::parsing::PreprocessorOptions {
  slang::parsing::PreprocessorOptions pp_options;
  pp_options.predefineSource = kPredefineSource;
  pp_options.predefines.emplace_back(kLyraIdentityMacro);
  pp_options.predefines.insert(
      pp_options.predefines.end(), user_defines.begin(), user_defines.end());
  return pp_options;
}

auto BuildLyraPreprocessorOptions() -> slang::parsing::PreprocessorOptions {
  return BuildLyraPreprocessorOptions({});
}

}  // namespace lyra::frontend
