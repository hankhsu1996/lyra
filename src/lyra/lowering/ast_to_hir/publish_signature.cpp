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
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/type_import.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/net_type.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
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
  // Only a design element instantiated into the hierarchy has ports and an
  // object; a namespace unit publishes its declarations by name and roots
  // neither.
  const auto* body = scope_->asSymbol().as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) return {};

  auto& instance_class = signature_.instance_class.emplace(
      hir::InstanceClassSignature{
          .class_name = hir::InstanceClassName(unit_.name),
          .members = {},
          .callables = {}});

  hir::TypeImportMemo published;
  const auto publish_type = [&](hir::TypeId own) {
    hir::TypeImporter importer(
        unit_.types,
        hir::TypePoolOwner{.unit_name = unit_.name, .classes = &unit_.classes},
        signature_.types, published);
    return importer.Import(own);
  };

  // One member per internal declaration, however many ports reach it: two port
  // expressions may select disjoint parts of one name (LRM 23.2.2.2), and the
  // storage they share is one member.
  const auto publish_member = [&](const slang::ast::ValueSymbol& internal)
      -> diag::Result<hir::PublishedMemberId> {
    if (const auto it = published_member_ids_.find(&internal);
        it != published_member_ids_.end()) {
      return it->second;
    }
    const auto span = SourceMapper().PointSpanOf(internal.location);
    auto interned = InternType(internal.getType(), span);
    if (!interned) return std::unexpected(std::move(interned.error()));
    auto storage = DeclarationStorage(internal, span);
    if (!storage) return std::unexpected(std::move(storage.error()));
    const hir::PublishedMemberId id = instance_class.members.Add(
        hir::PublishedMember{
            .name = std::string{internal.name},
            .type = publish_type(*interned),
            .storage = *std::move(storage)});
    published_member_ids_.emplace(&internal, id);
    return id;
  };

  // A subroutine a caller enables on an instance of this unit (LRM 13.3, 25.7).
  // A DPI-C import is not one of them: its foreign symbol is program-global and
  // a caller reaches it by that name, through no instance at all (LRM 35.4).
  const auto publish_callable = [&](const slang::ast::SubroutineSymbol& sym)
      -> diag::Result<std::optional<hir::PublishedCallableId>> {
    if (sym.flags.has(slang::ast::MethodFlags::DPIImport)) {
      return std::nullopt;
    }
    const auto span = SourceMapper().PointSpanOf(sym.location);
    auto result_type = InternType(sym.getReturnType(), span);
    if (!result_type) return std::unexpected(std::move(result_type.error()));
    std::vector<hir::ExternalCalleeParam> params;
    params.reserve(sym.getArguments().size());
    for (const auto* formal : sym.getArguments()) {
      auto formal_type = InternType(formal->getType(), span);
      if (!formal_type) return std::unexpected(std::move(formal_type.error()));
      params.push_back(
          hir::ExternalCalleeParam{
              .direction = ParamDirectionOf(*formal),
              .type = publish_type(*formal_type)});
    }
    return instance_class.callables.Add(
        hir::PublishedCallable{
            .name = std::string{sym.name},
            .kind = ToHirSubroutineKind(sym.subroutineKind),
            .result_type = publish_type(*result_type),
            .params = std::move(params)});
  };

  // An interface port names an instance of another unit that this one neither
  // owns nor builds (LRM 25.3). What the unit publishes about it is a member
  // like any other: the position a connection binds, and a type naming the unit
  // whose instance belongs there -- which is what lets the parent's connection
  // be checked where the parent compiles rather than while the design
  // elaborates.
  const auto publish_interface_port =
      [&](const slang::ast::InterfacePortSymbol& port)
      -> diag::Result<hir::PublishedMemberId> {
    const auto span = SourceMapper().PointSpanOf(port.location);
    const auto refuse = [&](std::string message) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedStructuralMember,
          std::move(message));
    };
    const auto declared = port.getDeclaredRange();
    if (!declared.has_value()) {
      return refuse(
          "an interface port whose range is not constant is not yet "
          "supported");
    }
    // Which interface the port carries is settled during elaboration, so the
    // unit reads it here and publishes it; a header that leaves it unnamed
    // (LRM 25.3.3) is read the same way. A unit whose ports name different
    // interfaces is a different specialization and has its own name already.
    // A modport restricts which members a referrer may name and in which
    // direction (LRM 25.5), which is settled where that referrer compiles; what
    // a connection binds is the whole interface instance under any of its
    // views, so the port publishes the same member whichever one names it.
    const slang::ast::InstanceSymbol* instance =
        ConnectedInterfaceOf(port.getConnection()).instance;
    if (instance == nullptr) {
      return refuse("an unconnected interface port is not yet supported");
    }
    std::string interface_unit = SpecializationName(*instance);
    RecordReferencedUnit(interface_unit);
    hir::TypeId own = unit_.types.Intern(
        hir::Type{hir::UnitObjectType{.unit_name = interface_unit}});
    // A port carrying a range stands for as many instances as the range has
    // elements (LRM 25.3), which is a fact about what the member is and so
    // travels on its type. The innermost dimension is wrapped first, so the
    // range written leftmost ends up outermost.
    for (const slang::ConstantRange& dim : std::views::reverse(*declared)) {
      own = unit_.types.Intern(
          hir::Type{hir::UnpackedArrayType{
              .element_type = own,
              .dim =
                  hir::UnpackedRange{.left = dim.left, .right = dim.right}}});
    }
    interface_port_units_.emplace(&port, std::move(interface_unit));
    const hir::PublishedMemberId id = instance_class.members.Add(
        hir::PublishedMember{
            .name = std::string{port.name},
            .type = publish_type(own),
            .storage = hir::BorrowedObjectStorage{}});
    published_member_ids_.emplace(&port, id);
    return id;
  };

  const auto publish_part =
      [&](const slang::ast::PortSymbol& port) -> diag::Result<hir::PortPart> {
    const auto span = SourceMapper().PointSpanOf(port.location);
    auto interned = InternType(port.getType(), span);
    if (!interned) return std::unexpected(std::move(interned.error()));
    const hir::PortDirection direction =
        TranslateDirection(port.direction, port.internalSymbol);
    const auto* internal =
        port.internalSymbol == nullptr
            ? nullptr
            : port.internalSymbol->as_if<slang::ast::ValueSymbol>();
    std::optional<hir::PublishedMemberId> member;
    if (internal != nullptr) {
      // A `ref` port's direction is what makes its declaration a reference
      // (LRM 23.3.3.2), so the answer is taken here and read back wherever that
      // declaration is asked what it holds.
      if (direction == hir::PortDirection::kRef ||
          direction == hir::PortDirection::kConstRef) {
        ref_port_internals_.emplace(
            internal, direction == hir::PortDirection::kConstRef
                          ? hir::ReferenceBinding::kConstRef
                          : hir::ReferenceBinding::kRef);
      }
      auto id = publish_member(*internal);
      if (!id) return std::unexpected(std::move(id.error()));
      member = *id;
    }
    return hir::PortPart{hir::DataPortPart{
        .direction = direction,
        .type = publish_type(*interned),
        .member = member}};
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
    // A connection reaches an interface port as one point like any other, so it
    // has one part.
    auto published =
        publish_interface_port(member->as<slang::ast::InterfacePortSymbol>());
    if (!published) return std::unexpected(std::move(published.error()));
    signature_.ports.push_back(
        hir::PortDecl{
            .name = std::string{member->name},
            .parts = {
                hir::PortPart{hir::InterfacePortPart{.member = *published}}}});
  }

  // An interface port names the interface's scope rather than a point data
  // crosses (LRM 25.3), so every name the interface declares is reachable
  // through one. What a module promises is its ports; what an interface
  // promises is its whole declared surface, and it promises it here so a
  // referrer resolves a name on the port where it compiles. A subroutine the
  // interface declares is part of that surface: LRM 25.7 makes it callable
  // through a port, so a caller needs its call protocol, its result, and its
  // formals stated the same way a member's storage is.
  if (body->getDefinition().definitionKind ==
      slang::ast::DefinitionKind::Interface) {
    for (const auto& member : scope_->members()) {
      if (member.kind == slang::ast::SymbolKind::Subroutine) {
        auto published =
            publish_callable(member.as<slang::ast::SubroutineSymbol>());
        if (!published) return std::unexpected(std::move(published.error()));
        continue;
      }
      const auto* value = member.as_if<slang::ast::ValueSymbol>();
      if (value == nullptr ||
          (member.kind != slang::ast::SymbolKind::Variable &&
           member.kind != slang::ast::SymbolKind::Net)) {
        continue;
      }
      auto id = publish_member(*value);
      if (!id) return std::unexpected(std::move(id.error()));
    }
  }

  // One slot per member published, for the declarations to fill as this unit's
  // own walk reaches them.
  published_members_.resize(instance_class.members.size());
  return {};
}

auto UnitLowerer::DeclarationStorage(
    const slang::ast::ValueSymbol& value, diag::SourceSpan span) const
    -> diag::Result<hir::PublishedStorage> {
  if (const auto binding = ReferenceBindingOf(value)) {
    return hir::PublishedStorage{hir::ReferenceStorage{.binding = *binding}};
  }
  const auto* net = value.as_if<slang::ast::NetSymbol>();
  if (net == nullptr) {
    return hir::PublishedStorage{hir::VariableStorage{}};
  }
  const auto net_type = TranslateNetType(net->netType);
  if (!net_type.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedTypeKind,
        "this net type is not yet supported");
  }
  return hir::PublishedStorage{hir::NetStorage{.net_type = *net_type}};
}

auto UnitLowerer::ImportSignatureType(
    const hir::UnitSignature& signature, hir::TypeId published) -> hir::TypeId {
  hir::TypeImporter importer(
      signature.types, std::nullopt, unit_.types,
      signature_type_memos_[&signature]);
  return importer.Import(published);
}

auto UnitLowerer::ExternalUnitObjectOf(const std::string& unit_name)
    -> hir::ExternalUnitObjectId {
  if (const auto it = external_unit_objects_.find(unit_name);
      it != external_unit_objects_.end()) {
    return it->second;
  }
  const hir::ExternalUnitObjectId object_id = unit_.external_unit_objects.Add(
      hir::ImportExternalUnitObject(
          Signatures().Instantiated(unit_name), unit_.types));
  external_unit_objects_.emplace(unit_name, object_id);
  return object_id;
}

}  // namespace lyra::lowering::ast_to_hir
