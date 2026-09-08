#include <algorithm>
#include <cstdint>
#include <expected>
#include <optional>
#include <ranges>
#include <span>
#include <string>
#include <utility>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/expressions/MiscExpressions.h>
#include <slang/ast/expressions/SelectExpressions.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/external_callee.hpp"
#include "lyra/hir/published_callable.hpp"
#include "lyra/hir/published_modport.hpp"
#include "lyra/hir/published_target.hpp"
#include "lyra/hir/type.hpp"
#include "lyra/hir/type_id.hpp"
#include "lyra/hir/type_import.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/lowering/ast_to_hir/connected_interface.hpp"
#include "lyra/lowering/ast_to_hir/expression/slang_atoms.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
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
// unit's declarations can be waited on through, and what a member of one is
// read and written as. A published member need not: an instance holds an
// object, and nothing waits on it.
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
          .callables = {},
          .modports = {}});

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
    RecordReferencedUnit(SpecializationName(*instance));
    hir::TypeId own = unit_.types.Intern(
        hir::Type{
            hir::UnitObjectType{.unit_name = SpecializationName(*instance)}});
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
    interface_port_types_.emplace(&port, own);
    const hir::PublishedMemberId id = instance_class.members.Add(
        hir::PublishedMember{
            .name = std::string{port.name},
            .type = publish_type(own),
            .storage = hir::BorrowedObjectStorage{}});
    published_member_ids_.emplace(&port, id);
    return id;
  };

  // A child this unit published takes the position its signature gave it. Its
  // declaration is already bound when this runs, since a unit walks its own
  // declarations before it publishes, so the pairing is stated here rather than
  // where the binding is made.
  std::vector<std::pair<hir::PublishedMemberId, hir::InstanceMemberId>>
      published_instances;

  // An interface an interface instantiates (LRM 25.3). Access to the objects an
  // interface declares is available through a port connection (LRM 25.10), so a
  // nested instance is on the surface the port reaches and is published like
  // any other member. What crosses is what an interface port's member carries
  // -- the unit whose instances belong there, its multiplicity, and that the
  // member holds a borrowed pointer -- because from the referrer's side the two
  // are one thing; that this scope builds this one and the parent binds that
  // one is not a fact a referrer reads.
  const auto publish_instance_member =
      [&](const slang::ast::Symbol& member,
          const slang::ast::InstanceSymbol& leaf,
          std::span<const slang::ConstantRange> ranges) {
        std::string instance_unit = SpecializationName(leaf);
        RecordReferencedUnit(instance_unit);
        hir::TypeId own = unit_.types.Intern(
            hir::Type{
                hir::UnitObjectType{.unit_name = std::move(instance_unit)}});
        // The innermost dimension is wrapped first, so the range written
        // leftmost ends up outermost.
        for (const slang::ConstantRange& dim : std::views::reverse(ranges)) {
          own = unit_.types.Intern(
              hir::Type{hir::UnpackedArrayType{
                  .element_type = own,
                  .dim = hir::UnpackedRange{
                      .left = dim.left, .right = dim.right}}});
        }
        const auto binding = LookupOwnedChildBinding(member);
        if (!binding.has_value()) {
          throw InternalError(
              "PublishSignature: an instance this unit publishes is a child "
              "its own declaration walk bound");
        }
        published_instances.emplace_back(
            instance_class.members.Add(
                hir::PublishedMember{
                    .name = std::string{member.name},
                    .type = publish_type(own),
                    .storage = hir::BorrowedObjectStorage{}}),
            std::get<hir::InstanceMemberId>(binding->child));
      };

  // The descent a port expression's own selects state, turned from the
  // leaf-first steps a peel collects into the owner-to-leaf order a reader
  // walks. Each step states the type it lands on, so a reader needs no
  // knowledge of what selecting from a type produces.
  const auto publish_path =
      [&](std::span<const slang::ast::Expression* const> steps,
          diag::SourceSpan span)
      -> diag::Result<std::vector<hir::PublishedSelector>> {
    std::vector<hir::PublishedSelector> path;
    path.reserve(steps.size());
    for (const auto* step : std::views::reverse(steps)) {
      auto step_type = InternType(*step->type, span);
      if (!step_type) return std::unexpected(std::move(step_type.error()));
      const hir::TypeId projected = publish_type(*step_type);
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
                .index = *index, .projected_type = projected});
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
              .range = *std::move(range), .projected_type = projected});
    }
    return path;
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
          .type = publish_type(*interned),
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
    auto id = publish_member(*peeled->base);
    if (!id) return std::unexpected(std::move(id.error()));
    auto path = publish_path(peeled->steps, span);
    if (!path) return std::unexpected(std::move(path.error()));
    return hir::PortPart{hir::DataPortPart{
        .direction = direction,
        .type = publish_type(*interned),
        .target =
            hir::MemberProjection{.member = *id, .path = *std::move(path)}}};
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
      if (member.kind == slang::ast::SymbolKind::Instance) {
        publish_instance_member(
            member, member.as<slang::ast::InstanceSymbol>(), {});
        continue;
      }
      if (member.kind == slang::ast::SymbolKind::InstanceArray) {
        // A dimension with no elements constructs nothing and names no unit,
        // so the array is no member at all -- the same answer the unit's own
        // walk reaches through the one predicate both read.
        const auto shape = ResolveInstanceArrayShape(
            member.as<slang::ast::InstanceArraySymbol>());
        if (!shape.has_value()) continue;
        publish_instance_member(member, *shape->leaf, shape->ranges);
        continue;
      }
      if (!HoldsCell(member)) continue;
      auto id = publish_member(member.as<slang::ast::ValueSymbol>());
      if (!id) return std::unexpected(std::move(id.error()));
    }

    // A modport is a named view of what the interface publishes (LRM 25.5), and
    // each name it offers stands for an expression this interface evaluates
    // (LRM 25.5.4) -- an item's own name where the view wrote none. Reading the
    // name is that expression evaluated and writing it is that expression
    // assigned to, so what the view promises is the pair of subroutines
    // carrying those out: the expression names declarations of this interface
    // and would mean nothing where the signature is read, while a callable
    // crosses as any other does.
    const auto publish_modport_port =
        [&](std::string_view modport_name,
            const slang::ast::ModportPortSymbol& port)
        -> diag::Result<hir::PublishedModportPort> {
      const auto span = SourceMapper().PointSpanOf(port.location);
      const auto* connection = port.getConnectionExpr();
      if (connection == nullptr) {
        return diag::Fail(
            span, diag::DiagCode::kUnsupportedStructuralMember,
            "a view offering a name that reaches nothing inside its interface "
            "is not yet supported");
      }
      auto interned = InternType(*connection->type, span);
      if (!interned) return std::unexpected(std::move(interned.error()));
      const hir::TypeId crossing = publish_type(*interned);
      const ModportAccessors accessors = ModportAccessorsOf(port);
      const hir::PublishedCallableId getter = instance_class.callables.Add(
          hir::PublishedCallable{
              .name = ModportReadName(modport_name, port.name),
              .kind = hir::SubroutineKind::kFunction,
              .result_type = crossing,
              .params = {}});
      std::optional<hir::PublishedCallableId> setter;
      if (accessors.setter.has_value()) {
        setter = instance_class.callables.Add(
            hir::PublishedCallable{
                .name = ModportWriteName(modport_name, port.name),
                .kind = hir::SubroutineKind::kFunction,
                .result_type = publish_type(
                    unit_.types.Intern(hir::Type{hir::VoidType{}})),
                .params = {hir::ExternalCalleeParam{
                    .direction = hir::ParamDirection::kInput,
                    .type = crossing}}});
      }
      // What the expression reads, which is what a process waiting on the name
      // observes. LRM 25.5 confines those names to this interface's own
      // declarations, so each is already a member it publishes.
      std::vector<hir::PublishedMemberId> reads;
      diag::Result<void> read_failure;
      connection->visitSymbolReferences(
          [&](const slang::ast::Expression&, const slang::ast::Symbol& symbol) {
            if (!read_failure || !HoldsCell(symbol)) return;
            auto id = publish_member(symbol.as<slang::ast::ValueSymbol>());
            if (!id) {
              read_failure = std::unexpected(std::move(id.error()));
              return;
            }
            if (!std::ranges::contains(reads, *id)) reads.push_back(*id);
          });
      if (!read_failure) {
        return std::unexpected(std::move(read_failure.error()));
      }
      return hir::PublishedModportPort{
          .name = std::string{port.name},
          .getter = getter,
          .setter = setter,
          .reads = std::move(reads)};
    };

    for (const auto& member : scope_->members()) {
      const auto* modport = member.as_if<slang::ast::ModportSymbol>();
      if (modport == nullptr) continue;
      hir::PublishedModport published{
          .name = std::string{modport->name}, .ports = {}};
      for (const auto& item : modport->members()) {
        const auto* port = item.as_if<slang::ast::ModportPortSymbol>();
        if (port == nullptr) continue;
        auto published_port = publish_modport_port(modport->name, *port);
        if (!published_port) {
          return std::unexpected(std::move(published_port.error()));
        }
        published.ports.push_back(*std::move(published_port));
      }
      instance_class.modports.push_back(std::move(published));
    }
  }

  // One slot per member published, for the declarations to fill as this unit's
  // own walk reaches them.
  published_members_.resize(instance_class.members.size());
  for (const auto& [slot, instance] : published_instances) {
    published_members_[slot.value] = instance;
  }
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
