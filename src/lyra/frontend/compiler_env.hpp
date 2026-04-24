#pragma once

#include <span>
#include <string>

#include <slang/parsing/Preprocessor.h>

namespace lyra::frontend {

auto BuildLyraPreprocessorOptions(std::span<const std::string> user_defines)
    -> slang::parsing::PreprocessorOptions;

}  // namespace lyra::frontend
