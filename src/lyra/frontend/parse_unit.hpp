#pragma once

#include <string>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/text/SourceManager.h>
#include <slang/util/Bag.h>

#include "lyra/diag/sink.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/frontend/slang_source_mapper.hpp"

namespace lyra::frontend {

enum class CompilationUnitMode {
  kPerFile,
  kSingleUnit,
};

struct PerFileUnit {
  std::string file;
};

struct SingleUnit {
  std::vector<std::string> files;
};

using ParseUnit = std::variant<PerFileUnit, SingleUnit>;

struct ParsePlan {
  std::vector<ParseUnit> units;
};

auto BuildParsePlan(
    const std::vector<std::string>& files, CompilationUnitMode mode)
    -> ParsePlan;

auto ExecuteParseUnit(
    const ParseUnit& unit, slang::SourceManager& source_manager,
    slang::ast::Compilation& compilation, const slang::Bag& options,
    diag::SourceManager& diag_sources, SlangSourceMapper& source_mapper,
    diag::DiagnosticSink& sink) -> bool;

}  // namespace lyra::frontend
