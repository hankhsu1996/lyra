#include "lyra/lowering/ast_to_hir/unit_definition.hpp"

#include <string>

#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/types/NetType.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/frontend/library_texts.hpp"
#include "lyra/hir/unit_definition.hpp"
#include "lyra/lowering/ast_to_hir/library_cell.hpp"
#include "lyra/support/scope_kind.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The net type `default_nettype` named; the front end reads `none` as a net
// type of no kind (LRM 22.8).
auto DefaultNetTypeOf(const slang::ast::NetType& net)
    -> support::DefaultNetType {
  switch (net.netKind) {
    case slang::ast::NetType::Wire:
      return support::DefaultNetType::kWire;
    case slang::ast::NetType::Tri:
      return support::DefaultNetType::kTri;
    case slang::ast::NetType::Tri0:
      return support::DefaultNetType::kTri0;
    case slang::ast::NetType::Tri1:
      return support::DefaultNetType::kTri1;
    case slang::ast::NetType::WAnd:
      return support::DefaultNetType::kWand;
    case slang::ast::NetType::TriAnd:
      return support::DefaultNetType::kTriand;
    case slang::ast::NetType::WOr:
      return support::DefaultNetType::kWor;
    case slang::ast::NetType::TriOr:
      return support::DefaultNetType::kTrior;
    case slang::ast::NetType::TriReg:
      return support::DefaultNetType::kTrireg;
    case slang::ast::NetType::UWire:
      return support::DefaultNetType::kUwire;
    case slang::ast::NetType::Unknown:
      return support::DefaultNetType::kNone;
    case slang::ast::NetType::Supply0:
    case slang::ast::NetType::Supply1:
    case slang::ast::NetType::Interconnect:
    case slang::ast::NetType::UserDefined:
      break;
  }
  throw InternalError(
      "DefaultNetTypeOf: `default_nettype names a net type LRM 22.8 does not "
      "admit");
}

auto UnconnectedDriveOf(slang::ast::UnconnectedDrive drive)
    -> support::UnconnectedDrive {
  switch (drive) {
    case slang::ast::UnconnectedDrive::None:
      return support::UnconnectedDrive::kNone;
    case slang::ast::UnconnectedDrive::Pull0:
      return support::UnconnectedDrive::kPull0;
    case slang::ast::UnconnectedDrive::Pull1:
      return support::UnconnectedDrive::kPull1;
  }
  throw InternalError("UnconnectedDriveOf: an unconnected drive of no kind");
}

// A compilation unit read from a text of Lyra's is the tool's, whatever it
// defines in the design's name: the classes of the built-in package `std` (LRM
// 26.7) are Lyra's definitions of what the front end declares. Anything else
// with no definition of its own is a class lowered alone.
auto NamespaceFormOf(const slang::ast::Symbol& home) -> hir::DefinitionForm {
  const auto* unit = home.as_if<slang::ast::CompilationUnitSymbol>();
  if (unit == nullptr) return hir::ClassSpecialization{};
  if (frontend::LibraryTextOf(*unit) != nullptr) return hir::ToolText{};
  return hir::CompilationUnitScope{};
}

// What the source states of the definition at `home` beside where it lies.
auto FormOf(const slang::ast::Symbol& home) -> hir::DefinitionForm {
  if (const auto* body = home.as_if<slang::ast::InstanceBodySymbol>()) {
    const slang::ast::DefinitionSymbol& definition = body->getDefinition();
    return hir::DesignElementDefinition{
        .kind = DesignElementKindOf(definition.definitionKind),
        .celldefine = definition.cellDefine,
        .default_net_type = DefaultNetTypeOf(definition.defaultNetType),
        .unconnected_drive = UnconnectedDriveOf(definition.unconnectedDrive),
        .automatic = definition.defaultLifetime ==
                     slang::ast::VariableLifetime::Automatic};
  }
  if (const auto* package = home.as_if<slang::ast::PackageSymbol>()) {
    return hir::PackageDefinition{
        .default_net_type = DefaultNetTypeOf(package->defaultNetType),
        .automatic = package->defaultLifetime ==
                     slang::ast::VariableLifetime::Automatic};
  }
  return NamespaceFormOf(home);
}

}  // namespace

auto DesignElementKindOf(slang::ast::DefinitionKind kind)
    -> hir::DesignElementKind {
  switch (kind) {
    case slang::ast::DefinitionKind::Module:
      return hir::DesignElementKind::kModule;
    case slang::ast::DefinitionKind::Interface:
      return hir::DesignElementKind::kInterface;
    case slang::ast::DefinitionKind::Program:
      return hir::DesignElementKind::kProgram;
  }
  throw InternalError("DesignElementKindOf: a design element of no kind");
}

auto UnitDefinitionOf(
    const slang::ast::Symbol& home, const slang::ast::Symbol& text_scope)
    -> hir::UnitDefinition {
  return hir::UnitDefinition{
      .library = std::string{LibraryOf(home)},
      .cell = std::string{CellOf(text_scope)},
      .form = FormOf(home)};
}

}  // namespace lyra::lowering::ast_to_hir
