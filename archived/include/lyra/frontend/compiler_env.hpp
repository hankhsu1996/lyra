#pragma once

#include <span>
#include <string>

#include <slang/parsing/Preprocessor.h>

namespace lyra::frontend {

// Single source of truth for Lyra's predefined compiler environment.
// All parsing entrypoints must route through this builder.
// Built-in macros precede user-supplied defines by policy.
auto BuildLyraPreprocessorOptions(std::span<const std::string> user_defines)
    -> slang::parsing::PreprocessorOptions;

auto BuildLyraPreprocessorOptions() -> slang::parsing::PreprocessorOptions;

}  // namespace lyra::frontend
