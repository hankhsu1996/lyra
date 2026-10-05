#include <algorithm>
#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/EvalContext.h>
#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/ValuePath.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/OperatorExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/class_ref.hpp"
#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/external_class.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/type_import.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"
#include "lyra/lowering/ast_to_hir/declaration_scopes.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
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

// One coordinate a port expression wrote. LRM 23.2.2.1 gives a port reference a
// `constant_select`, so the language itself has fixed the coordinate before the
// program runs and the front end's answer is what is read. It crosses the
// boundary as a number because a number is what a descent step can carry: an
// expression names declarations of the unit that wrote it and would mean
// nothing where the signature is read.
auto SelectCoordinate(const slang::ast::Expression& bound)
    -> std::optional<std::int32_t> {
  const slang::ConstantValue* value = bound.getConstant();
  if (value == nullptr || !value->isInteger()) return std::nullopt;
  return value->integer().as<std::int32_t>();
}

// Whether a declaration holds a cell (LRM 6.5) -- what an expression over a
// unit's declarations can be waited on through.
auto HoldsCell(const slang::ast::Symbol& symbol) -> bool {
  return symbol.kind == slang::ast::SymbolKind::Variable ||
         symbol.kind == slang::ast::SymbolKind::Net;
}

auto SelectRange(const slang::ast::RangeSelectExpression& select)
    -> std::optional<hir::PublishedRange> {
  const auto left = SelectCoordinate(select.left());
  const auto right = SelectCoordinate(select.right());
  if (!left.has_value() || !right.has_value()) return std::nullopt;
  switch (select.getSelectionKind()) {
    case slang::ast::RangeSelectionKind::Simple:
      return hir::PublishedRange{
          hir::PublishedConstantRange{.left = *left, .right = *right}};
    case slang::ast::RangeSelectionKind::IndexedUp:
      return hir::PublishedRange{
          hir::PublishedIndexedUpRange{.base = *left, .width = *right}};
    case slang::ast::RangeSelectionKind::IndexedDown:
      return hir::PublishedRange{
          hir::PublishedIndexedDownRange{.base = *left, .width = *right}};
  }
  throw InternalError("SelectRange: unknown slang RangeSelectionKind");
}

// The declaration a port expression bottoms out at, and the selects standing
// between the two (LRM 23.2.2.1, 23.2.2.2). Selects peel from the outside in,
// so `steps` is leaf-first and a reader walks it in reverse to descend. Nothing
// when the expression takes a form no descent step spells, which the caller
// refuses as the port form it is.
struct PeeledPortExpression {
  const slang::ast::ValueSymbol* base;
  std::vector<const slang::ast::Expression*> steps;
};

auto PeelPortExpression(const slang::ast::Expression& expr)
    -> std::optional<PeeledPortExpression> {
  std::vector<const slang::ast::Expression*> steps;
  for (const slang::ast::Expression* step = &expr;;) {
    if (const auto* value = step->as_if<slang::ast::ValueExpressionBase>()) {
      return PeeledPortExpression{
          .base = &value->symbol, .steps = std::move(steps)};
    }
    if (const auto* select = step->as_if<slang::ast::ElementSelectExpression>();
        select != nullptr) {
      steps.push_back(step);
      step = &select->value();
      continue;
    }
    if (const auto* select = step->as_if<slang::ast::RangeSelectExpression>();
        select != nullptr) {
      steps.push_back(step);
      step = &select->value();
      continue;
    }
    return std::nullopt;
  }
}

// How many positions a declaration has, in the terms a connection lays
// positions over each other in: an integral declaration has one per bit (LRM
// 10.11 states an
// alias over "bits within a net"), and every other kind has one indivisible
// position, since nothing names a part of one.
auto PositionsOfDeclaration(const slang::ast::ValueSymbol& base)
    -> std::uint32_t {
  const slang::ast::Type& type = base.getType();
  if (!type.isIntegral()) {
    return 1;
  }
  return static_cast<std::uint32_t>(type.getBitWidth());
}

// Which of the declaration's own positions the part a port stands for covers.
// A port written as a plain name covers all of them; one written as a select
// covers the positions the front end folded that select to. A select into a
// declaration whose positions are not bits reaches a part that is none of
// them, which is what the absent answer says.
auto PositionsOfPortExpression(
    const slang::ast::ValueSymbol& base, const slang::ast::Expression* written)
    -> std::optional<hir::PublishedPositions> {
  if (written == nullptr) {
    return hir::PublishedPositions{
        .position = 0, .width = PositionsOfDeclaration(base)};
  }
  if (!base.getType().isIntegral()) {
    return std::nullopt;
  }
  slang::ast::EvalContext eval_context(base);
  const slang::ast::ValuePath path(*written, eval_context);
  if (path.lsp != written) {
    return std::nullopt;
  }
  return hir::PublishedPositions{
      .position = static_cast<std::uint32_t>(path.lspBounds.first),
      .width = static_cast<std::uint32_t>(
          path.lspBounds.second - path.lspBounds.first + 1)};
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

// The descent a port expression's own selects state, turned from the leaf-first
// steps a peel collects into the owner-to-leaf order a reader walks. Each step
// states the type it lands on, in this unit's own types, so a reader needs no
// knowledge of what selecting from a type produces.
auto PublishPath(
    UnitLowerer& lowerer, std::span<const slang::ast::Expression* const> steps,
    diag::SourceSpan span)
    -> diag::Result<std::vector<hir::PublishedSelector>> {
  std::vector<hir::PublishedSelector> path;
  path.reserve(steps.size());
  for (const auto* step : std::views::reverse(steps)) {
    auto projected = lowerer.InternType(*step->type, span);
    if (!projected) return std::unexpected(std::move(projected.error()));
    if (const auto* select =
            step->as_if<slang::ast::ElementSelectExpression>()) {
      const auto index = SelectCoordinate(select->selector());
      if (!index.has_value()) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedStructuralMember,
            "a port naming an element of an internal name at a coordinate "
            "the front end did not fix is not yet supported");
      }
      path.emplace_back(
          hir::PublishedElementSelector{
              .index = *index, .projected_type = *projected});
      continue;
    }
    const auto* select = step->as_if<slang::ast::RangeSelectExpression>();
    if (select == nullptr) {
      throw InternalError(
          "PublishSignature: a step that is neither an element nor a range "
          "select was collected as one");
    }
    auto range = SelectRange(*select);
    if (!range.has_value()) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedStructuralMember,
          "a port naming a window of an internal name at bounds the front "
          "end did not fix is not yet supported");
    }
    path.emplace_back(
        hir::PublishedSliceSelector{
            .range = *std::move(range), .projected_type = *projected});
  }
  return path;
}

}  // namespace

void UnitLowerer::PublishClassSignatures() {
  // Every class this unit declares is one another unit may name: a package's
  // by its declarations (LRM 26.2), and a design element's through a
  // hierarchical name reaching an object of it (LRM 23.6). One a design element
  // declares is a type of each instance (LRM 6.22), which every instance of
  // this unit declares alike, so what it publishes is one class.
  const auto publish_class = [&](const slang::ast::ClassType& cls) {
    const auto it = own_class_signatures_.find(&cls);
    if (it == own_class_signatures_.end()) {
      return;
    }
    hir::TypeImportMemo published;
    hir::TypeImporter importer(
        unit_.types,
        hir::TypePoolOwner{.unit_name = unit_.name, .classes = &unit_.classes},
        signature_.types, published);
    const hir::ClassSignature& own = it->second;
    // The order is part of what is published: a property's slot and a virtual
    // method's ordinal are counted out of these lists by the class that
    // declares them and by every unit that reaches one, and neither states a
    // position to the other.
    signature_.classes.push_back(
        hir::ClassSignature{
            .class_name = own.class_name,
            .base = own.base,
            .is_interface_class = own.is_interface_class,
            .implements = own.implements,
            .properties = hir::ImportProperties(importer, own.properties),
            .local_property_types =
                hir::ImportTypes(importer, own.local_property_types),
            .static_properties =
                hir::ImportStaticProperties(importer, own.static_properties),
            .constructor = own.constructor.transform(
                [&](const hir::ExternalCalleeInterface& stated) {
                  return hir::ImportCalleeInterface(importer, stated);
                }),
            .methods = hir::ImportMethods(importer, own.methods),
            .takes_declaring_instance = own.takes_declaring_instance});
  };
  // A parameterized class is one class per specialization the design uses
  // (LRM 8.25), each published on its own.
  const auto publish = [&](const slang::ast::Symbol& member) {
    if (const auto* cls = member.as_if<slang::ast::ClassType>()) {
      publish_class(*cls);
    } else if (
        const auto* generic =
            member.as_if<slang::ast::GenericClassDefSymbol>()) {
      for (const auto& spec : generic->specializations()) {
        publish_class(spec.getCanonicalType().as<slang::ast::ClassType>());
      }
    }
  };
  WalkDeclarationScopes(*scope_, publish);
}

auto UnitLowerer::PublishNamespaceSubroutines() -> diag::Result<void> {
  hir::TypeImportMemo published;
  hir::TypeImporter importer(
      unit_.types,
      hir::TypePoolOwner{.unit_name = unit_.name, .classes = &unit_.classes},
      signature_.types, published);
  auto& namespace_unit = signature_.unit.emplace<hir::PublishedNamespace>();
  for (const ScopePublicationRecord::Callable& callable :
       PublicationOf(*scope_).callables) {
    auto own = PublishedCallableOf(callable);
    if (!own) return std::unexpected(std::move(own.error()));
    namespace_unit.subroutines.push_back(
        hir::ImportCallable(importer, *std::move(own)));
  }
  return {};
}

auto UnitLowerer::PublishedCallableOf(
    const ScopePublicationRecord::Callable& callable)
    -> diag::Result<hir::PublishedCallable> {
  // An expression another unit asks for the value of is evaluated in a
  // function of this unit that takes nothing.
  const auto evaluator = [&](std::string name, const slang::ast::Type& type,
                             const slang::ast::Symbol& holder)
      -> diag::Result<hir::PublishedCallable> {
    auto result_type =
        InternType(type, SourceMapper().PointSpanOf(holder.location));
    if (!result_type) return std::unexpected(std::move(result_type.error()));
    return hir::PublishedCallable{
        .name = std::move(name),
        .interface =
            hir::ExternalCalleeInterface{
                .kind = hir::SubroutineKind::kFunction, .params = {}},
        .result_type = *result_type};
  };
  return std::visit(
      Overloaded{
          [&](const ScopePublicationRecord::Subroutine& declared)
              -> diag::Result<hir::PublishedCallable> {
            const slang::ast::SubroutineSymbol& sym = *declared.symbol;
            const auto span = SourceMapper().PointSpanOf(sym.location);
            auto interface = MakeExternalCalleeInterface(sym, span);
            if (!interface) {
              return std::unexpected(std::move(interface.error()));
            }
            auto result_type = InternType(sym.getReturnType(), span);
            if (!result_type) {
              return std::unexpected(std::move(result_type.error()));
            }
            return hir::PublishedCallable{
                .name = std::string{sym.name},
                .interface = *std::move(interface),
                .result_type = *result_type};
          },
          [&](const ScopePublicationRecord::PortDefault& port) {
            return evaluator(
                PortDefaultName(port.port->name), port.port->getType(),
                *port.port);
          },
          [&](const ScopePublicationRecord::ViewRead& read)
              -> diag::Result<hir::PublishedCallable> {
            const auto* connection = read.port->getConnectionExpr();
            if (connection == nullptr) {
              throw InternalError(
                  "PublishSignature: a name the view defines is the "
                  "expression it was written with");
            }
            return evaluator(
                ModportReadName(read.modport->name, read.port->name),
                *connection->type, *read.port);
          }},
      callable);
}

auto UnitLowerer::PublishSignature() -> diag::Result<void> {
  PublishClassSignatures();
  // Only a design element instantiated into the hierarchy has ports and an
  // object; a namespace unit publishes its declarations by name and roots
  // neither.
  const auto* body = scope_->asSymbol().as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) return PublishNamespaceSubroutines();

  auto& element = signature_.unit.emplace<hir::PublishedDesignElement>(
      hir::PublishedDesignElement{
          .ports = {}, .instance_class = {}, .blocks = {}});
  const ScopePublicationRecord& root = PublicationOf(*scope_);

  hir::TypeImportMemo published;
  hir::TypeImporter importer(
      unit_.types,
      hir::TypePoolOwner{.unit_name = unit_.name, .classes = &unit_.classes},
      signature_.types, published);

  const auto member_of = [&](const slang::ast::Symbol& declared) {
    const std::optional<hir::PublishedMemberId> id = root.MemberOf(declared);
    if (!id.has_value()) {
      throw InternalError(
          std::format(
              "PublishSignature: a port of '{}' reaches '{}', which its unit "
              "publishes no member for",
              unit_.name, declared.name));
    }
    return *id;
  };

  const auto publish_part =
      [&](const slang::ast::PortSymbol& port) -> diag::Result<hir::PortPart> {
    const auto span = SourceMapper().PointSpanOf(port.location);
    auto interned = InternType(port.getType(), span);
    if (!interned) return std::unexpected(std::move(interned.error()));
    const hir::PortDirection direction =
        TranslateDirection(port.direction, port.internalSymbol);
    // A port expression names the part of a declaration the port stands for
    // (LRM 23.2.2.2); a port written as a plain name has none, and the whole of
    // the declaration behind it is the same descent with no steps. A port with
    // neither reaches nothing inside the unit, which the same clause admits.
    const auto* written = port.getInternalExpr();
    std::optional<PeeledPortExpression> peeled;
    if (written != nullptr) {
      peeled = PeelPortExpression(*written);
    } else if (const auto* internal =
                   port.internalSymbol == nullptr
                       ? nullptr
                       : port.internalSymbol->as_if<slang::ast::ValueSymbol>();
               internal != nullptr) {
      peeled = PeeledPortExpression{.base = internal, .steps = {}};
    } else {
      return hir::PortPart{hir::DataPortPart{
          .direction = direction,
          .type = importer.Import(*interned),
          .target = hir::NoInternalTarget{}}};
    }
    if (!peeled.has_value()) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedStructuralMember,
          "a port naming this part of an internal name is not yet supported");
    }

    // A `ref` port's direction is what makes its declaration a reference (LRM
    // 23.3.3.2), so the answer is taken here and read back wherever that
    // declaration is asked what it holds.
    if (direction == hir::PortDirection::kRef ||
        direction == hir::PortDirection::kConstRef) {
      ref_port_internals_.emplace(
          peeled->base, direction == hir::PortDirection::kConstRef
                            ? hir::ReferenceBinding::kConstRef
                            : hir::ReferenceBinding::kRef);
    }
    auto path = PublishPath(*this, peeled->steps, span);
    if (!path) return std::unexpected(std::move(path.error()));
    return hir::PortPart{hir::DataPortPart{
        .direction = direction,
        .type = importer.Import(*interned),
        .target = hir::ImportProjection(
            importer,
            hir::MemberProjection{
                .member = member_of(*peeled->base),
                .path = *std::move(path),
                .positions =
                    PositionsOfPortExpression(*peeled->base, written)})}};
  };

  // The ports are read before the members are given their storage, because a
  // `ref` port is what makes the declaration it reaches a reference.
  for (const auto* member : body->getPortList()) {
    if (const auto* port = member->as_if<slang::ast::PortSymbol>()) {
      auto part = publish_part(*port);
      if (!part) return std::unexpected(std::move(part.error()));
      element.ports.push_back(
          hir::PortDecl{
              .name = std::string{port->name},
              .parts = {*std::move(part)},
              .default_value = root.CallableOf(*port)});
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
      element.ports.push_back(
          hir::PortDecl{
              .name = std::string{multi->name},
              .parts = std::move(parts),
              .default_value = std::nullopt});
      continue;
    }
    // A connection reaches an interface port as one point like any other, so it
    // has one part.
    element.ports.push_back(
        hir::PortDecl{
            .name = std::string{member->name},
            .parts = {hir::PortPart{
                hir::InterfacePortPart{.member = member_of(*member)}}},
            .default_value = std::nullopt});
  }

  // A hierarchical name reaches any named declaration of the element from
  // anywhere in the design (LRM 23.6), and an interface port reaches every name
  // the interface declares (LRM 25.3), so what a design element publishes is
  // every scope it holds -- its own and each generate block's (LRM 27). Each is
  // kept in this unit's own types as well, which is what the scope's own
  // published class is laid out from.
  for (const slang::ast::Scope* scope : publishing_scopes_) {
    auto own = PublishScopeClass(PublicationOf(*scope));
    if (!own) return std::unexpected(std::move(own.error()));
    hir::ScopeClassSignature stated = hir::ImportScopeClass(importer, *own);
    if (scope == scope_) {
      element.instance_class = std::move(stated);
    } else {
      element.blocks.push_back(std::move(stated));
    }
    scope_classes_.emplace(scope, *std::move(own));
  }
  return {};
}

auto UnitLowerer::PublishScopeClass(const ScopePublicationRecord& published)
    -> diag::Result<hir::ScopeClassSignature> {
  hir::ScopeClassSignature cls{
      .class_name = published.class_name,
      .members = {},
      .callables = {},
      .generates = {},
      .disable_targets = {},
      .modports = {}};

  // A declaration holding storage, published as the declaration states it.
  const auto cell = [&](const slang::ast::ValueSymbol& declared,
                        std::vector<std::string> within)
      -> diag::Result<hir::PublishedMember> {
    auto interned = InternType(
        declared.getType(), SourceMapper().PointSpanOf(declared.location));
    if (!interned) return std::unexpected(std::move(interned.error()));
    return hir::PublishedMember{
        .name = std::string{declared.name},
        .within = std::move(within),
        .type = *interned,
        .storage = DeclarationStorage(declared)};
  };

  // What a member standing for instances of another unit is, as this unit's
  // type: the set of the objects `elements` are, in row-major order of their
  // positions, under the dimensions `ranges` declares, each element's kind
  // read off that element (LRM 23.3.2, 23.10.1).
  const auto objects_type =
      [&](std::span<const slang::ast::InstanceSymbol* const> elements,
          std::span<const slang::ConstantRange> ranges) {
        std::vector<hir::UnitObjectType> per_position;
        per_position.reserve(elements.size());
        for (const slang::ast::InstanceSymbol* element : elements) {
          std::string instance_unit =
              SpecializationName(*element, Specialization());
          std::string class_name = hir::InstanceClassName(instance_unit);
          per_position.push_back(
              hir::UnitObjectType{
                  .unit_name = std::move(instance_unit),
                  .class_name = std::move(class_name)});
        }
        std::vector<hir::UnpackedRange> declared;
        declared.reserve(ranges.size());
        for (const slang::ConstantRange& dim : ranges) {
          declared.push_back(
              hir::UnpackedRange{.left = dim.left, .right = dim.right});
        }
        return unit_.types.Intern(
            hir::Type{hir::ObjectsOf(std::move(declared), per_position)});
      };

  // A member standing for instances of another unit: an interface port, whose
  // instances the parent binds (LRM 25.3), or an instance this scope builds,
  // which a hierarchical name steps onto (LRM 23.6). From a referrer's side the
  // two are one thing -- which kind of instance belongs at each position, how
  // many, and a borrowed pointer to each -- and which side builds the instance
  // is not a fact a referrer reads.
  const auto objects = [&](const slang::ast::Symbol& member, hir::TypeId own) {
    return hir::PublishedMember{
        .name = std::string{member.name},
        .within = {},
        .type = own,
        .storage = hir::BorrowedObjectStorage{}};
  };

  // An interface port names instances of another unit that this one neither
  // owns nor builds (LRM 25.3). What the unit publishes about it is a type
  // naming which kind of instance belongs at each of its positions -- which is
  // what lets the parent's connection be checked where the parent compiles
  // rather than while the design elaborates.
  const auto interface_port = [&](const slang::ast::InterfacePortSymbol& port)
      -> diag::Result<hir::PublishedMember> {
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
    const auto connected = ConnectedInterfaceOf(port.getConnection()).instances;
    if (connected.empty()) {
      return refuse("an unconnected interface port is not yet supported");
    }
    // A port carrying a range stands for as many instances as the range has
    // elements (LRM 25.3), each the one bound at its position, which is a fact
    // about what the member is and so travels on its type.
    const hir::TypeId own = objects_type(connected, *declared);
    interface_port_types_.emplace(&port, own);
    return objects(port, own);
  };

  for (const ScopePublicationRecord::Member& member : published.members) {
    auto made = std::visit(
        Overloaded{
            [&](const ScopePublicationRecord::DataObject& data) {
              return cell(*data.symbol, {});
            },
            [&](const ScopePublicationRecord::LocalStatic& local) {
              return cell(*local.symbol, local.within);
            },
            // Under the name the class was published as.
            [&](const ScopePublicationRecord::ClassStatic& property) {
              const auto own = own_class_signatures_.find(property.owner);
              if (own == own_class_signatures_.end()) {
                throw InternalError(
                    "PublishSignature: every class a scope declares is "
                    "interned before the scope's signature is published");
              }
              return cell(*property.symbol, {own->second.class_name});
            },
            [&](const ScopePublicationRecord::Instance& instance)
                -> diag::Result<hir::PublishedMember> {
              return objects(
                  *instance.symbol,
                  objects_type(instance.shape.elements, instance.shape.ranges));
            },
            [&](const ScopePublicationRecord::InterfacePort& port) {
              return interface_port(*port.symbol);
            }},
        member);
    if (!made) return std::unexpected(std::move(made.error()));
    cls.members.Add(*std::move(made));
  }

  // A subroutine is part of a scope's declared surface: a hierarchical name
  // enables one (LRM 23.6, 25.7), so a caller needs its call protocol, its
  // result, and its formals stated the same way a member's storage is.
  for (const ScopePublicationRecord::Callable& callable : published.callables) {
    auto own = PublishedCallableOf(callable);
    if (!own) return std::unexpected(std::move(own.error()));
    cls.callables.Add(*std::move(own));
  }

  for (const ScopePublicationRecord::Construct& construct :
       published.generates) {
    cls.generates.Add(
        std::visit(
            Overloaded{
                [&](const ScopePublicationRecord::Loop& loop)
                    -> hir::PublishedGenerate {
                  hir::PublishedLoop counted{
                      .name = std::string{loop.loop->name}, .blocks = {}};
                  for (const auto* entry : loop.loop->entries) {
                    counted.blocks.push_back(
                        hir::PublishedLoopBlock{
                            .index = LoopIndexOf(*entry),
                            .class_name = PublicationOf(*entry).class_name});
                  }
                  return counted;
                },
                [&](const ScopePublicationRecord::Choice& choice)
                    -> hir::PublishedGenerate {
                  hir::PublishedChoice chosen;
                  for (const auto* alternative : choice.built) {
                    chosen.blocks.push_back(
                        hir::PublishedAlternative{
                            .name = std::string{alternative->name},
                            .class_name =
                                PublicationOf(*alternative).class_name});
                  }
                  return chosen;
                }},
            construct));
  }

  for (const ScopePublicationRecord::DisableTarget& target :
       published.disable_targets) {
    cls.disable_targets.Add(hir::PublishedDisableTarget{.path = target.path});
  }

  // The member a declaration a view names stands for. LRM 25.5 confines those
  // names to the interface's own declarations.
  const auto viewed_member =
      [&](const slang::ast::Symbol& declared,
          diag::SourceSpan span) -> diag::Result<hir::PublishedMemberId> {
    const std::optional<hir::PublishedMemberId> id =
        published.MemberOf(declared);
    if (!id.has_value()) {
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedStructuralMember,
          std::format(
              "a view naming '{}', which its interface does not declare where "
              "the view is, is not yet supported",
              declared.name));
    }
    return *id;
  };

  // The storage a name the view admits a write to designates. LRM 25.5.4
  // sends what such a name may be to LRM 23.3.3, where a connection is a
  // continuous assignment and its sink is an lvalue, so the expression always
  // designates storage. A concatenation joins several declarations under one
  // name, which LRM 23.2.2.1 orders most significant first; a designator is
  // that shape with one part.
  const auto designated_parts = [&](const slang::ast::Expression& written,
                                    diag::SourceSpan span)
      -> diag::Result<std::vector<hir::MemberProjection>> {
    std::vector<const slang::ast::Expression*> written_parts;
    if (const auto* joined =
            written.as_if<slang::ast::ConcatenationExpression>()) {
      for (const auto* operand : joined->operands()) {
        written_parts.push_back(operand);
      }
    } else {
      written_parts.push_back(&written);
    }
    std::vector<hir::MemberProjection> parts;
    parts.reserve(written_parts.size());
    for (const auto* written_part : written_parts) {
      const auto peeled = PeelPortExpression(*written_part);
      if (!peeled.has_value()) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedStructuralMember,
            "a view naming this part of one of its interface's declarations "
            "is not yet supported");
      }
      auto id = viewed_member(*peeled->base, span);
      if (!id) return std::unexpected(std::move(id.error()));
      auto path = PublishPath(*this, peeled->steps, span);
      if (!path) return std::unexpected(std::move(path.error()));
      parts.push_back(
          hir::MemberProjection{
              .member = *id,
              .path = *std::move(path),
              .positions =
                  PositionsOfPortExpression(*peeled->base, written_part)});
    }
    return parts;
  };

  // A modport is a named view of what the interface publishes (LRM 25.5).
  // For a name the view wrote an expression for, what it publishes is decided
  // by the direction it declared, which is why no direction is on this
  // signature -- a referrer never asks which way the name runs, it asks what
  // the name is.
  const auto view_name = [&](const ScopePublicationRecord::ViewName& name)
      -> diag::Result<hir::PublishedModportPort> {
    const slang::ast::ModportPortSymbol& port =
        *std::visit([](const auto& defined) { return defined.port; }, name);
    const auto span = SourceMapper().PointSpanOf(port.location);
    const auto* connection = port.getConnectionExpr();
    if (connection == nullptr) {
      throw InternalError(
          "PublishSignature: a name the view defines is the expression it "
          "was written with");
    }
    return std::visit(
        Overloaded{
            [&](const ScopePublicationRecord::ViewPlace&)
                -> diag::Result<hir::PublishedModportPort> {
              auto interned = InternType(*connection->type, span);
              if (!interned) {
                return std::unexpected(std::move(interned.error()));
              }
              auto parts = designated_parts(*connection, span);
              if (!parts) return std::unexpected(std::move(parts.error()));
              return hir::PublishedModportPort{
                  .name = std::string{port.name},
                  .meaning = hir::ViewDefinedPlace{
                      .parts = *std::move(parts), .type = *interned}};
            },
            // Nothing bounds a name offered only for reading to an lvalue, so
            // what crosses is the subroutine this interface evaluates it in,
            // and what the expression reads, which is what a process waiting
            // on the name observes.
            [&](const ScopePublicationRecord::ViewComputed&)
                -> diag::Result<hir::PublishedModportPort> {
              const std::optional<hir::PublishedCallableId> evaluate =
                  published.CallableOf(port);
              if (!evaluate.has_value()) {
                throw InternalError(
                    "PublishSignature: a name a view offers only for reading "
                    "is evaluated by a callable its scope publishes");
              }
              std::vector<hir::PublishedMemberId> observes;
              diag::Result<void> read_failure;
              connection->visitSymbolReferences(
                  [&](const slang::ast::Expression&,
                      const slang::ast::Symbol& symbol) {
                    if (!read_failure || !HoldsCell(symbol)) return;
                    auto id = viewed_member(symbol, span);
                    if (!id) {
                      read_failure = std::unexpected(std::move(id.error()));
                      return;
                    }
                    if (!std::ranges::contains(observes, *id)) {
                      observes.push_back(*id);
                    }
                  });
              if (!read_failure) {
                return std::unexpected(std::move(read_failure.error()));
              }
              return hir::PublishedModportPort{
                  .name = std::string{port.name},
                  .meaning = hir::ViewComputedValue{
                      .evaluate = *evaluate, .observes = std::move(observes)}};
            }},
        name);
  };

  for (const ScopePublicationRecord::View& view : published.views) {
    hir::PublishedModport modport{
        .name = std::string{view.modport->name}, .ports = {}};
    for (const ScopePublicationRecord::ViewName& name : view.names) {
      auto port = view_name(name);
      if (!port) return std::unexpected(std::move(port.error()));
      modport.ports.push_back(*std::move(port));
    }
    cls.modports.push_back(std::move(modport));
  }
  return cls;
}

auto UnitLowerer::DeclarationStorage(const slang::ast::ValueSymbol& value) const
    -> hir::PublishedStorage {
  if (const auto binding = ReferenceBindingOf(value)) {
    return hir::PublishedStorage{hir::ReferenceStorage{.binding = *binding}};
  }
  if (value.as_if<slang::ast::NetSymbol>() != nullptr) {
    return hir::PublishedStorage{hir::NetStorage{}};
  }
  return hir::PublishedStorage{hir::VariableStorage{}};
}

auto UnitLowerer::ImportSignatureType(
    const hir::UnitSignature& signature, hir::TypeId published) -> hir::TypeId {
  hir::TypeImporter importer(
      signature.types, std::nullopt, unit_.types,
      signature_type_memos_[&signature]);
  return importer.Import(published);
}

auto UnitLowerer::ScopeClassOfInstance(
    const slang::ast::InstanceSymbol& instance) -> hir::ExternalScopeClassId {
  return ExternalScopeClassOf(SpecializationName(instance, Specialization()));
}

auto UnitLowerer::ExternalScopeClassOf(const std::string& unit_name)
    -> hir::ExternalScopeClassId {
  return ExternalScopeClassOf(
      unit_name, hir::DesignElementOf(Signatures().Instantiated(unit_name))
                     .instance_class.class_name);
}

auto UnitLowerer::ScopeClassTypeOf(hir::ExternalScopeClassId scope_class) const
    -> hir::TypeId {
  const hir::ExternalScopeClass& record =
      unit_.external_scope_classes.Get(scope_class);
  return unit_.types.Intern(
      hir::Type{hir::UnitObjectType{
          .unit_name = record.unit_name,
          .class_name = record.signature.class_name}});
}

auto UnitLowerer::ExternalScopeClassOf(
    const std::string& unit_name, const std::string& class_name)
    -> hir::ExternalScopeClassId {
  if (const auto it = external_scope_classes_.find({unit_name, class_name});
      it != external_scope_classes_.end()) {
    return it->second;
  }
  const hir::UnitSignature& signature = Signatures().Instantiated(unit_name);
  const hir::ScopeClassSignature* published =
      hir::DesignElementOf(signature).FindScopeClass(class_name);
  if (published == nullptr) {
    throw InternalError(
        std::format(
            "UnitLowerer::ExternalScopeClassOf: '{}' publishes no scope class "
            "'{}', and a name landed in one",
            unit_name, class_name));
  }
  const hir::ExternalScopeClassId scope_class =
      unit_.external_scope_classes.Add(
          hir::ImportExternalScopeClass(signature, *published, unit_.types));
  external_scope_classes_.emplace(
      std::pair{unit_name, class_name}, scope_class);
  return scope_class;
}

auto UnitLowerer::ExternalClassOf(
    const std::string& unit_name, const std::string& class_name)
    -> const hir::ExternalClass& {
  if (const hir::ExternalClass* held = hir::FindExternalClass(
          unit_.external_classes, unit_name, class_name)) {
    return *held;
  }
  const hir::UnitSignature* signature = Signatures().Find(unit_name);
  const hir::ClassSignature* published =
      signature == nullptr ? nullptr : signature->FindClass(class_name);
  if (published == nullptr) {
    throw InternalError(
        std::format(
            "UnitLowerer::ExternalClassOf: '{}' published no class '{}', and "
            "every class a unit declares is published",
            unit_name, class_name));
  }
  // A value of the class is laid out after the whole of what it extends, and
  // is also a value of each interface class it names and of what those extend,
  // so reading its signature reads the signature of every class it names, and
  // so on up: each states only what its own declaration says.
  hir::ExternalClass record =
      hir::ImportExternalClass(*signature, *published, unit_.types);
  for (const hir::ExternalClassRef& iface : record.implements) {
    ExternalClassOf(iface.unit_name, iface.class_name);
  }
  if (record.base.has_value()) {
    ExternalClassOf(record.base->unit_name, record.base->class_name);
    // The method an override replaces is named the way a call names it, by the
    // class that introduced it, found along the classes just read. Every class
    // a published one extends is itself published, so a name that resolves to
    // no introducer is a signature its own unit could not have made. A pure
    // override gives the method no body, so it replaces nothing.
    for (const hir::PublishedMethod& method : record.methods) {
      const auto* overriding =
          std::get_if<hir::OverridesVirtual>(&method.dispatch);
      if (overriding == nullptr || overriding->is_pure) {
        continue;
      }
      std::optional<hir::ExternalDispatchSlot> overridden =
          IntroducerOf(*record.base, method.prototype.name);
      if (!overridden.has_value()) {
        throw InternalError(
            std::format(
                "UnitLowerer::ExternalClassOf: '{}::{}' publishes '{}' as an "
                "override, which no class it extends introduces",
                unit_name, class_name, method.prototype.name));
      }
      record.overrides.push_back(
          hir::PublishedOverride{
              .method = method.prototype.name,
              .behavior = *std::move(overridden)});
    }
  }
  unit_.external_classes.push_back(std::move(record));
  return unit_.external_classes.back();
}

auto UnitLowerer::NamespaceCalleeInterface(
    const std::string& unit_name, const slang::ast::SubroutineSymbol& sym,
    diag::SourceSpan span) -> diag::Result<hir::ExternalCalleeInterface> {
  if (unit_name == unit_.name) {
    return MakeExternalCalleeInterface(sym, span);
  }
  const hir::UnitSignature* signature = Signatures().Find(unit_name);
  const auto* namespace_unit =
      signature == nullptr
          ? nullptr
          : std::get_if<hir::PublishedNamespace>(&signature->unit);
  const hir::PublishedCallable* published =
      namespace_unit == nullptr ? nullptr
                                : namespace_unit->FindSubroutine(sym.name);
  if (published == nullptr) {
    throw InternalError(
        std::format(
            "UnitLowerer::NamespaceCalleeInterface: '{}' declares '{}' and "
            "published no such subroutine",
            unit_name, sym.name));
  }
  hir::TypeImporter importer(
      signature->types, std::nullopt, unit_.types,
      signature_type_memos_[signature]);
  return hir::ImportCallable(importer, *published).interface;
}

}  // namespace lyra::lowering::ast_to_hir
