#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"

#include <format>
#include <string>
#include <string_view>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/parsing/LexerFacts.h>

#include "lyra/support/simple_identifier.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto IsKeyword(
    std::string_view text, const slang::ast::Compilation& compilation) -> bool {
  const auto* keywords = slang::parsing::LexerFacts::getKeywordTable(
      slang::parsing::LexerFacts::getDefaultKeywordVersion(
          compilation.languageVersion()));
  return keywords->contains(text);
}

}  // namespace

auto NameInAPath(
    std::string_view identifier, const slang::ast::Compilation& compilation)
    -> std::string {
  const bool written_as_it_is =
      identifier.empty() || (support::IsSimpleIdentifier(identifier) &&
                             !IsKeyword(identifier, compilation));
  if (written_as_it_is) {
    return std::string{identifier};
  }
  return std::format("\\{} ", identifier);
}

auto NameInAPath(const slang::ast::Symbol& named) -> std::string {
  // A constructor is declared under the keyword `new` rather than under an
  // identifier (LRM 8.7), so there is nothing to escape.
  if (const auto* subroutine = named.as_if<slang::ast::SubroutineSymbol>();
      subroutine != nullptr &&
      subroutine->flags.has(slang::ast::MethodFlags::Constructor)) {
    return std::string{named.name};
  }
  return NameInAPath(named.name, named.getParentScope()->getCompilation());
}

}  // namespace lyra::lowering::ast_to_hir
