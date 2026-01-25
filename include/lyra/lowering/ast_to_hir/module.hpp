#pragma once

#include <span>

#include <slang/ast/symbols/InstanceSymbols.h>

#include "lyra/hir/module.hpp"
#include "lyra/lowering/ast_to_hir/port_binding.hpp"

namespace lyra::lowering::ast_to_hir {

struct Context;
class SymbolRegistrar;

// port_bindings is last with default empty span for call-site ergonomics
auto LowerModule(
    const slang::ast::InstanceSymbol& instance, SymbolRegistrar& registrar,
    Context* ctx, std::span<const InputPortBinding> port_bindings = {})
    -> hir::Module;

}  // namespace lyra::lowering::ast_to_hir
