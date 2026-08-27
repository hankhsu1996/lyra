#include <expected>
#include <optional>
#include <ranges>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/type_import.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Whether the declaration a `ref` port reaches forbids writing through it (LRM
// 23.3.3.2). The frontend carries this on the declaration rather than on the
// port, so the unit reads its own declaration to state what it publishes.
auto IsConstRef(const slang::ast::Symbol* internal) -> bool {
  if (internal == nullptr) return false;
  const auto* variable = internal->as_if<slang::ast::VariableSymbol>();
  return variable != nullptr &&
         variable->flags.has(slang::ast::VariableFlags::Const);
}

auto TranslateDirection(
    slang::ast::ArgumentDirection direction, const slang::ast::Symbol* internal)
    -> hir::PortDirection {
  switch (direction) {
    case slang::ast::ArgumentDirection::In:
      return hir::PortDirection::kInput;
    case slang::ast::ArgumentDirection::Out:
      return hir::PortDirection::kOutput;
    case slang::ast::ArgumentDirection::InOut:
      return hir::PortDirection::kInOut;
    case slang::ast::ArgumentDirection::Ref:
      return IsConstRef(internal) ? hir::PortDirection::kConstRef
                                  : hir::PortDirection::kRef;
  }
  throw InternalError(
      "PublishSignature: a port direction the language does not define");
}

}  // namespace

auto UnitLowerer::PublishSignature() -> diag::Result<void> {
  // Only a design element instantiated into the hierarchy has ports; a
  // namespace unit publishes its declarations by name and has no port list.
  const auto* body = scope_->asSymbol().as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) return {};

  hir::TypeImportMemo published;
  const auto publish_part =
      [&](const slang::ast::PortSymbol& port) -> diag::Result<hir::PortPart> {
    const auto span = SourceMapper().PointSpanOf(port.location);
    auto interned = InternType(port.getType(), span);
    if (!interned) return std::unexpected(std::move(interned.error()));
    hir::TypeImporter importer(
        unit_.types,
        hir::TypePoolOwner{.unit_name = unit_.name, .classes = &unit_.classes},
        signature_.types, published);
    return hir::PortPart{hir::DataPortPart{
        .direction = TranslateDirection(port.direction, port.internalSymbol),
        .type = importer.Import(*interned)}};
  };

  for (const auto* member : body->getPortList()) {
    if (const auto* port = member->as_if<slang::ast::PortSymbol>()) {
      auto part = publish_part(*port);
      if (!part) return std::unexpected(std::move(part.error()));
      signature_.ports.push_back(
          hir::PortDecl{
              .name = std::string{port->name}, .parts = {*std::move(part)}});
      continue;
    }
    if (const auto* multi = member->as_if<slang::ast::MultiPortSymbol>()) {
      // One external name over several bundled ones, each carrying data in its
      // own direction, so the port has a part per bundled name. LRM 23.2.2.1
      // gives the first name written the most significant bits, so a connection
      // reaches them least significant first.
      std::vector<hir::PortPart> parts;
      parts.reserve(multi->ports.size());
      for (const auto* bundled : std::views::reverse(multi->ports)) {
        auto part = publish_part(*bundled);
        if (!part) return std::unexpected(std::move(part.error()));
        parts.push_back(*std::move(part));
      }
      signature_.ports.push_back(
          hir::PortDecl{
              .name = std::string{multi->name}, .parts = std::move(parts)});
      continue;
    }
    // An interface port names a scope rather than carrying data (LRM 25.3). A
    // connection reaches it as one point like any other, so it has one part;
    // what the unit publishes about that part is that nothing crosses it.
    signature_.ports.push_back(
        hir::PortDecl{
            .name = std::string{member->name},
            .parts = {hir::PortPart{hir::InterfacePortPart{}}}});
  }
  return {};
}

auto UnitLowerer::ImportSignatureType(
    const hir::UnitSignature& signature, hir::TypeId published) -> hir::TypeId {
  hir::TypeImporter importer(
      signature.types, std::nullopt, unit_.types,
      signature_type_memos_[&signature]);
  return importer.Import(published);
}

}  // namespace lyra::lowering::ast_to_hir
