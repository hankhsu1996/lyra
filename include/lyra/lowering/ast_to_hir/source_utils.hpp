#pragma once

#include <filesystem>
#include <string>

#include <slang/ast/Symbol.h>
#include <slang/syntax/SyntaxNode.h>
#include <slang/text/SourceLocation.h>
#include <slang/text/SourceManager.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/source_manager.hpp"
#include "lyra/lowering/ast_to_hir/source_mapper.hpp"

namespace lyra::lowering::ast_to_hir {

// Recover the best possible source range from a slang symbol.
// Symbols are semantic objects and don't reliably carry full spans.
// The syntax node is the authoritative owner; fall back to location if absent.
inline auto GetSourceRange(const slang::ast::Symbol& sym)
    -> slang::SourceRange {
  if (const auto* syntax = sym.getSyntax()) {
    return syntax->sourceRange();
  }
  return {sym.location, sym.location};
}

// Register all source files from slang into Lyra's source infrastructure.
// Maps each slang BufferID to a Lyra FileId for span resolution.
inline void RegisterSourceFiles(
    const slang::SourceManager& slang_sm, SourceManager& source_manager,
    SourceMapper& source_mapper) {
  for (slang::BufferID buffer : slang_sm.getAllBuffers()) {
    if (!buffer) {
      continue;
    }
    if (source_mapper.Contains(buffer)) {
      throw common::InternalError(
          "source registration", "duplicate BufferID in slang SourceManager");
    }
    std::string path =
        std::filesystem::proximate(slang_sm.getFullPath(buffer)).string();
    std::string content{slang_sm.getSourceText(buffer)};
    FileId file_id =
        source_manager.AddFile(std::move(path), std::move(content));
    source_mapper.Register(buffer, file_id);
  }
}

}  // namespace lyra::lowering::ast_to_hir
