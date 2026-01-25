#pragma once

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

class ScopeLowerer;

auto LowerProcess(
    const slang::ast::ProceduralBlockSymbol& proc, ScopeLowerer& lowerer)
    -> hir::ProcessId;

auto LowerFunction(
    const slang::ast::SubroutineSymbol& func, ScopeLowerer& lowerer)
    -> hir::FunctionId;

auto LowerTask(const slang::ast::SubroutineSymbol& task, ScopeLowerer& lowerer)
    -> hir::TaskId;

}  // namespace lyra::lowering::ast_to_hir
