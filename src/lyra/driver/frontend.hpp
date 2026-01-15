#pragma once

#include <memory>
#include <optional>
#include <string>

#include <slang/ast/Compilation.h>
#include <slang/text/SourceManager.h>

namespace lyra::driver {

struct ParseResult {
  std::shared_ptr<slang::SourceManager> source_manager;
  std::unique_ptr<slang::ast::Compilation> compilation;
};

auto LoadFile(const std::string& path) -> std::optional<ParseResult>;

}  // namespace lyra::driver
