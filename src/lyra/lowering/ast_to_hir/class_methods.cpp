#include "lyra/lowering/ast_to_hir/class_methods.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <iterator>
#include <optional>
#include <string_view>
#include <vector>

#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CoverSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/types/AllTypes.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

[[noreturn]] void NotAMethodOf(std::string_view what) {
  throw InternalError(
      std::format(
          "ClassMethods::PositionOf: the {} is no method of the class -- "
          "please report this as a bug",
          what));
}

auto At(std::size_t position) -> hir::MethodId {
  return hir::MethodId{static_cast<std::uint32_t>(position)};
}

}  // namespace

auto ClassMethods::PositionOf(const slang::ast::SubroutineSymbol& method) const
    -> hir::MethodId {
  if (const auto at = std::ranges::find(defined, &method);
      at != defined.end()) {
    return At(static_cast<std::size_t>(std::distance(defined.begin(), at)));
  }
  const auto pure = std::ranges::find_if(
      pure_prototypes, [&](const slang::ast::MethodPrototypeSymbol* proto) {
        return proto->getSubroutine() == &method;
      });
  if (pure == pure_prototypes.end()) NotAMethodOf("subroutine");
  return At(
      defined.size() +
      static_cast<std::size_t>(std::distance(pure_prototypes.begin(), pure)));
}

auto ClassMethods::PositionOf(
    const slang::ast::FormalArgumentSymbol& formal) const -> hir::MethodId {
  const auto at = std::ranges::find_if(
      defaulted_formals,
      [&](const auto& defaulted) { return defaulted.second == &formal; });
  if (at == defaulted_formals.end()) NotAMethodOf("formal's default");
  return At(
      defined.size() + pure_prototypes.size() +
      static_cast<std::size_t>(std::distance(defaulted_formals.begin(), at)));
}

auto ClassMethods::DefaultsOf(const slang::ast::SubroutineSymbol& method) const
    -> std::vector<std::optional<hir::MethodId>> {
  std::vector<std::optional<hir::MethodId>> defaults;
  for (const slang::ast::FormalArgumentSymbol* formal : method.getArguments()) {
    const bool evaluated = std::ranges::any_of(
        defaulted_formals,
        [&](const auto& defaulted) { return defaulted.second == formal; });
    defaults.push_back(
        evaluated ? std::optional{PositionOf(*formal)} : std::nullopt);
  }
  return defaults;
}

auto ClassMethods::PositionOf(
    const slang::ast::ConstraintBlockSymbol& block) const -> hir::MethodId {
  const auto at = std::ranges::find(constraint_blocks, &block);
  if (at == constraint_blocks.end()) NotAMethodOf("constraint block");
  return At(
      defined.size() + pure_prototypes.size() + defaulted_formals.size() +
      static_cast<std::size_t>(std::distance(constraint_blocks.begin(), at)));
}

auto ClassMethods::PositionOf(
    const slang::ast::CovergroupType& covergroup) const -> hir::MethodId {
  const auto at = std::ranges::find(covergroups, &covergroup);
  if (at == covergroups.end()) NotAMethodOf("covergroup");
  return At(
      defined.size() + pure_prototypes.size() + defaulted_formals.size() +
      constraint_blocks.size() +
      static_cast<std::size_t>(std::distance(covergroups.begin(), at)));
}

auto ClassMethodsOf(const slang::ast::ClassType& cls) -> ClassMethods {
  ClassMethods methods;
  // SystemVerilog spells a definition two ways and they differ only in where
  // the body sits: written inline, the class scope holds the subroutine;
  // declared `extern`, the class scope holds a prototype and the definition
  // sits outside the class body (LRM 8.24). A pure virtual prototype (LRM
  // 8.21) is the one form with no definition behind it.
  for (const auto& method : cls.membersOfType<slang::ast::SubroutineSymbol>()) {
    if (method.getParentScope() != &cls) continue;
    // Every class carries methods the front end supplies itself, for its
    // generator and its constraints (LRM 18.6, 18.8, 18.9, 18.13.3 --
    // 18.13.5); none has a body to lower.
    if (method.flags.has(slang::ast::MethodFlags::BuiltIn)) continue;
    if (method.flags.has(slang::ast::MethodFlags::Constructor)) {
      methods.constructor = &method;
      continue;
    }
    methods.defined.push_back(&method);
  }
  for (const auto& proto :
       cls.membersOfType<slang::ast::MethodPrototypeSymbol>()) {
    if (proto.getParentScope() != &cls) continue;
    const auto* subroutine = proto.getSubroutine();
    if (subroutine == nullptr) {
      throw InternalError(
          "ClassMethodsOf: a class method prototype has no subroutine slang "
          "normally materializes");
    }
    if (proto.flags.has(slang::ast::MethodFlags::Constructor)) {
      methods.constructor = subroutine;
      continue;
    }
    if (proto.flags.has(slang::ast::MethodFlags::Pure)) {
      methods.pure_prototypes.push_back(&proto);
      continue;
    }
    methods.defined.push_back(subroutine);
  }

  // A formal's default is evaluated in the class each time a call omits it
  // (LRM 13.5.3), whatever object or scope the call is made from, so each is a
  // method of the class a caller asks for.
  const auto take_defaults = [&](const slang::ast::SubroutineSymbol& method) {
    if (!EvaluatesItsOwnDefaults(method)) return;
    for (const slang::ast::FormalArgumentSymbol* formal :
         method.getArguments()) {
      if (formal->getDefaultValue() != nullptr) {
        methods.defaulted_formals.emplace_back(&method, formal);
      }
    }
  };
  for (const auto* method : methods.defined) take_defaults(*method);
  for (const auto* proto : methods.pure_prototypes) {
    take_defaults(*proto->getSubroutine());
  }
  if (methods.constructor != nullptr) take_defaults(*methods.constructor);

  for (const auto& block :
       cls.membersOfType<slang::ast::ConstraintBlockSymbol>()) {
    if (block.getParentScope() != &cls) continue;
    methods.constraint_blocks.push_back(&block);
  }
  for (const auto& covergroup :
       cls.membersOfType<slang::ast::CovergroupType>()) {
    methods.covergroups.push_back(&covergroup);
  }
  return methods;
}

}  // namespace lyra::lowering::ast_to_hir
