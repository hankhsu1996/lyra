#include "lyra/lowering/ast_to_mir/package.hpp"

#include <cstdint>
#include <memory>
#include <string>
#include <vector>

#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/text/SourceLocation.h>

#include "lyra/lowering/ast_to_mir/type.hpp"
#include "lyra/mir/package.hpp"

namespace lyra::lowering::ast_to_mir {

auto LowerPackage(const slang::ast::PackageSymbol& pkg_symbol)
    -> std::unique_ptr<mir::Package> {
  auto package = std::make_unique<mir::Package>();
  package->name = std::string(pkg_symbol.name);

  for (const auto& member : pkg_symbol.members()) {
    if (member.kind == slang::ast::SymbolKind::TypeAlias) {
      const auto& alias = member.as<slang::ast::TypeAliasType>();
      const auto& target_type = alias.targetType.getType();

      // Lower the target type (the underlying type the alias refers to)
      auto type_result = LowerType(target_type, slang::SourceRange{});
      if (!type_result) {
        // Skip types we can't lower (e.g., unsupported types like structs)
        continue;
      }

      // Store the type without alias_name set - the name comes from
      // TypeDeclaration
      type_result->alias_name = std::nullopt;

      // Extract enum members if this is an enum type
      std::vector<mir::EnumMember> enum_members;
      const auto& canonical = target_type.getCanonicalType();
      if (canonical.isEnum()) {
        const auto& enum_type = canonical.as<slang::ast::EnumType>();
        for (const auto& value : enum_type.values()) {
          const auto& enum_val = value.as<slang::ast::EnumValueSymbol>();
          const auto& cv = enum_val.getValue();
          int64_t val = cv.integer().as<int64_t>().value_or(0);
          enum_members.push_back({std::string(enum_val.name), val});
        }
      }

      package->types.push_back(
          {std::string(alias.name), *type_result, std::move(enum_members)});
    }
  }

  return package;
}

}  // namespace lyra::lowering::ast_to_mir
