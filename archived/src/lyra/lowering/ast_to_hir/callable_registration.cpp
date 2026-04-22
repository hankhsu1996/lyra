#include "lyra/lowering/ast_to_hir/callable_registration.hpp"

#include <format>

#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/parameter_direction.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/callable_signature.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto ConvertDirection(const slang::ast::FormalArgumentSymbol& arg)
    -> ParameterDirection {
  switch (arg.direction) {
    case slang::ast::ArgumentDirection::In:
      return ParameterDirection::kInput;
    case slang::ast::ArgumentDirection::Out:
      return ParameterDirection::kOutput;
    case slang::ast::ArgumentDirection::InOut:
      return ParameterDirection::kInOut;
    case slang::ast::ArgumentDirection::Ref:
      if (arg.flags.has(slang::ast::VariableFlags::Const)) {
        return ParameterDirection::kConstRef;
      }
      return ParameterDirection::kRef;
  }
  return ParameterDirection::kInput;
}

}  // namespace

auto RegisterCallableSymbol(
    const slang::ast::SubroutineSymbol& sub, SymbolKind kind, Context& ctx,
    SymbolRegistrar& registrar, SourceSpan span) -> SymbolId {
  // Check if already registered (idempotent for pre-registered symbols).
  SymbolId existing = registrar.Lookup(sub);
  if (existing) {
    if (ctx.callable_signatures == nullptr) {
      throw common::InternalError(
          "RegisterCallableSymbol",
          "no callable signature table for pre-registered callable");
    }
    if (ctx.callable_signatures->Lookup(existing) == nullptr) {
      throw common::InternalError(
          "RegisterCallableSymbol",
          std::format(
              "pre-registered callable symbol {} has no signature entry",
              existing.value));
    }
    return existing;
  }

  // Build full signature atomically before any registration.
  TypeId return_type = LowerType(sub.getReturnType(), span, &ctx);
  if (!return_type) return kInvalidSymbolId;

  hir::HirCallableSignature sig;
  sig.return_type = return_type;
  sig.params.reserve(sub.getArguments().size());

  for (const auto* arg : sub.getArguments()) {
    TypeId arg_type = LowerType(arg->getType(), span, &ctx);
    if (!arg_type) {
      return kInvalidSymbolId;
    }
    sig.params.push_back({
        .direction = ConvertDirection(*arg),
        .type = arg_type,
    });
  }

  // Signature fully built. Now register symbol and insert signature.
  SymbolId sym = registrar.Register(sub, kind, return_type);
  if (ctx.callable_signatures != nullptr) {
    ctx.callable_signatures->Insert(sym, std::move(sig));
  }

  return sym;
}

}  // namespace lyra::lowering::ast_to_hir
