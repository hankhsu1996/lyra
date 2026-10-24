#include "lyra/frontend/library_texts.hpp"

#include <array>
#include <optional>
#include <string_view>

#include <slang/ast/Compilation.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/text/SourceLocation.h>
#include <slang/text/SourceManager.h>

namespace lyra::frontend {

// The scope itself carries no source location, but every member read from one
// buffer shares it, so the first located member names it.
auto BufferOf(const slang::ast::CompilationUnitSymbol& unit)
    -> std::optional<slang::BufferID> {
  for (const auto& member : unit.members()) {
    if (member.location.valid()) {
      return member.location.buffer();
    }
  }
  return std::nullopt;
}

auto LibraryTextOf(const slang::ast::CompilationUnitSymbol& unit)
    -> const LibraryText* {
  const slang::SourceManager* sources =
      unit.getCompilation().getSourceManager();
  const std::optional<slang::BufferID> buffer = BufferOf(unit);
  if (sources == nullptr || !buffer.has_value()) {
    return nullptr;
  }
  const std::string_view name = sources->getRawFileName(*buffer);
  static constexpr std::array kTexts = {&kStdDefinitions, &kCoverageLibrary};
  for (const LibraryText* text : kTexts) {
    if (text->buffer == name) {
      return text;
    }
  }
  return nullptr;
}

}  // namespace lyra::frontend
