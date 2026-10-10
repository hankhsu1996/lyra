#include "lyra/lowering/ast_to_hir/name_in_a_path.hpp"

#include <format>
#include <string>
#include <string_view>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
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
  return NameInAPath(named.name, named.getParentScope()->getCompilation());
}

}  // namespace lyra::lowering::ast_to_hir
