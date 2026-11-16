#include "lyra/lowering/ast_to_hir/port_direction.hpp"

#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/support/port_direction.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The front end carries whether a `ref` port's writes are forbidden on the
// declaration it reaches rather than on the port.
auto IsConstRef(const slang::ast::Symbol* internal) -> bool {
  if (internal == nullptr) return false;
  const auto* variable = internal->as_if<slang::ast::VariableSymbol>();
  return variable != nullptr &&
         variable->flags.has(slang::ast::VariableFlags::Const);
}

}  // namespace

auto PortDirectionOf(const slang::ast::PortSymbol& port)
    -> support::PortDirection {
  switch (port.direction) {
    case slang::ast::ArgumentDirection::In:
      return support::PortDirection::kInput;
    case slang::ast::ArgumentDirection::Out:
      return support::PortDirection::kOutput;
    case slang::ast::ArgumentDirection::InOut:
      return support::PortDirection::kInOut;
    case slang::ast::ArgumentDirection::Ref:
      return IsConstRef(port.internalSymbol) ? support::PortDirection::kConstRef
                                             : support::PortDirection::kRef;
  }
  throw InternalError(
      "PortDirectionOf: a direction the language does not define");
}

}  // namespace lyra::lowering::ast_to_hir
