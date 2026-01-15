#pragma once

#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>

#include "lyra/hir/fwd.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

auto LowerProcess(
    const slang::ast::ProceduralBlockSymbol& proc, SymbolRegistrar& registrar,
    Context* ctx) -> hir::ProcessId;

auto LowerFunction(
    const slang::ast::SubroutineSymbol& func, SymbolRegistrar& registrar,
    Context* ctx) -> hir::FunctionId;

auto LowerTask(
    const slang::ast::SubroutineSymbol& task, SymbolRegistrar& registrar,
    Context* ctx) -> hir::TaskId;

}  // namespace lyra::lowering::ast_to_hir
