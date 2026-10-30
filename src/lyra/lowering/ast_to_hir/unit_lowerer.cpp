#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"

#include <algorithm>
#include <cstdint>
#include <expected>
#include <format>
#include <iterator>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Expression.h>
#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/TimingControl.h>
#include <slang/ast/statements/MiscStatements.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/ClassSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/CoverSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/NetType.h>
#include <slang/ast/types/Type.h>
#include <slang/numeric/SVInt.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/diag/source_span.hpp"
#include "lyra/frontend/slang_source_span.hpp"
#include "lyra/hir/compilation_unit.hpp"
#include "lyra/hir/dump.hpp"
#include "lyra/hir/specialization.hpp"
#include "lyra/hir/specialization_ref.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/unit_signature.hpp"
#include "lyra/hir/unit_signatures.hpp"
#include "lyra/hir/verify.hpp"
#include "lyra/lowering/ast_to_hir/fixed_name.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/inferred_clocking.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/library_cell.hpp"
#include "lyra/lowering/ast_to_hir/statement/assertions.hpp"
#include "lyra/lowering/ast_to_hir/std_classes.hpp"
#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/profiling/time_trace.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/statistics.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

auto ScopeLoweredFor(const slang::ast::Symbol& home)
    -> const slang::ast::Scope& {
  const slang::ast::Symbol& lowered =
      home.kind == slang::ast::SymbolKind::ClassType
          ? DeclaringCompilationUnit(home)
          : home;
  if (const auto* body = lowered.as_if<slang::ast::InstanceBodySymbol>()) {
    return *body;
  }
  if (const auto* package = lowered.as_if<slang::ast::PackageSymbol>()) {
    return *package;
  }
  if (const auto* cu = lowered.as_if<slang::ast::CompilationUnitSymbol>()) {
    return *cu;
  }
  throw InternalError(
      "UnitLowerer: a unit is lowered from the scope of a package, a design "
      "element's body, or a `$unit` scope");
}

// The identifier the source declares `home` under. An instance body is one
// elaboration of a definition and carries the definition's; a `$unit` scope
// (LRM 3.12.1) was declared under none.
auto SourceNameOf(const slang::ast::Symbol& home) -> std::string {
  if (const auto* body = home.as_if<slang::ast::InstanceBodySymbol>()) {
    return std::string(body->getDefinition().name);
  }
  return std::string(home.name);
}

auto HomesOf(
    const slang::ast::Symbol& home,
    std::span<const slang::ast::InstanceBodySymbol* const> checked_against)
    -> std::vector<const slang::ast::Symbol*> {
  std::vector<const slang::ast::Symbol*> homes{&home};
  for (const slang::ast::InstanceBodySymbol* other : checked_against) {
    if (!std::ranges::contains(homes, other)) homes.push_back(other);
  }
  return homes;
}

auto ScopesLoweredFor(std::span<const slang::ast::Symbol* const> homes)
    -> std::vector<const slang::ast::Scope*> {
  std::vector<const slang::ast::Scope*> scopes;
  scopes.reserve(homes.size());
  for (const slang::ast::Symbol* home : homes) {
    scopes.push_back(&ScopeLoweredFor(*home));
  }
  return scopes;
}

}  // namespace

UnitLowerer::UnitLowerer(
    const LoweringFacts& facts, const slang::ast::Symbol& home,
    hir::SpecializationRef names, hir::UnitRole role,
    std::span<const slang::ast::InstanceBodySymbol* const> checked_against)
    : facts_(facts),
      homes_(HomesOf(home, checked_against)),
      applications_(ScopesLoweredFor(homes_)),
      scope_(applications_.front()),
      lowering_(scope_),
      unit_{std::move(names.unit), {}},
      specialization_(std::move(names.specialization)) {
  unit_.role = role;
  unit_.source_name = SourceNameOf(home);
  unit_.library_binding = LibraryBindingOf(scope_->asSymbol());
  unit_.fixed_name = FixedNameOf(*scope_);
  signature_.unit_name = unit_.name;
}

auto UnitLowerer::Owns(const slang::ast::Symbol& decl) const -> bool {
  return std::ranges::contains(homes_, &UnitHomeOf(decl));
}

auto UnitLowerer::Declare() -> diag::Result<void> {
  const profiling::TimeTraceScope span(
      "declare unit", [&] { return unit_.name; });
  return DeclareOwn();
}

auto UnitLowerer::DescribeForGrouping() -> diag::Result<void> {
  const profiling::TimeTraceScope span(
      "describe specialization", [&] { return specialization_; });
  return DeclareOwn();
}

auto UnitLowerer::DeclareOwn() -> diag::Result<void> {
  const auto in_unit = diag::FailureContext::InUnit(unit_.name);
  {
    const profiling::TimeTraceScope step("declare structural identities");
    for (const slang::ast::Scope* application : applications_) {
      DeclareStructuralIdentities(*application, support::DefPath{});
    }
  }
  {
    const profiling::TimeTraceScope step("declare classes");
    if (auto r = InternOwnClassDeclarations(); !r) {
      return std::unexpected(std::move(r.error()));
    }
  }
  {
    const profiling::TimeTraceScope step("declare types");
    if (auto r = InternOwnTypeDeclarations(); !r) {
      return std::unexpected(std::move(r.error()));
    }
  }
  const profiling::TimeTraceScope step("publish signature");
  return PublishSignature();
}

namespace {

// The type a declaration states, where what it states is, or holds as the
// elements of an array, an enumeration, an unpacked structure or an unpacked
// union: the types a declaration declares by being written (LRM 6.19, 7.2,
// 7.3), whether or not a typedef names them.
auto DeclaredTypeOf(const slang::ast::Symbol& member)
    -> const slang::ast::Type* {
  const slang::ast::Type* stated = nullptr;
  if (const auto* alias = member.as_if<slang::ast::TypeAliasType>()) {
    stated = &alias->targetType.getType();
  } else if (const auto* value = member.as_if<slang::ast::ValueSymbol>()) {
    stated = &value->getType();
  } else {
    return nullptr;
  }
  const slang::ast::Type* held = stated;
  while (const slang::ast::Type* element = held->getArrayElementType()) {
    held = element;
  }
  if (!held->isEnum() && !held->isUnpackedStruct() &&
      !held->isUnpackedUnion()) {
    return nullptr;
  }
  return stated;
}

}  // namespace

auto UnitLowerer::InternOwnTypeDeclarations() -> diag::Result<void> {
  // A declaration stating an enumeration, a structure or a union declares the
  // type, and the type brings what it declares and the operations the language
  // defines on its whole value (LRM 6.19, 7.2, 7.3), so the type exists in the
  // unit that declares it whether or not that unit ever holds a value of it --
  // another unit naming it takes its definition and reaches those operations
  // here. That holds of one a typedef names and of one a variable, a net or a
  // parameter states without a name, which another unit reaches through the
  // declaration holding it. It runs after the class walk, because a member may
  // be a handle to any class the unit declares.
  auto visit = [&](const slang::ast::Symbol& member) -> diag::Result<void> {
    const slang::ast::Type* declared = DeclaredTypeOf(member);
    if (declared == nullptr) return {};
    if (auto r = InternType(*declared, frontend::PointSpanOf(member.location));
        !r) {
      return std::unexpected(std::move(r.error()));
    }
    return {};
  };
  return WalkOwnDeclarations(visit);
}

auto UnitLowerer::TakeSignature() -> hir::UnitSignature {
  return std::move(signature_);
}

auto UnitLowerer::LowerBodies(
    const hir::UnitSignatures& signatures, const BuiltAsQuery& built_as,
    Implementations& implementations) -> diag::Result<hir::Specialization> {
  signatures_ = &signatures;
  built_as_ = &built_as;
  const auto in_unit = diag::FailureContext::InUnit(unit_.name);
  RecordPublishedScopeClasses();
  auto own = LowerScopeOf(*scope_);
  if (!own) return std::unexpected(std::move(own.error()));
  unit_.root_scope = std::move(own->scope);
  // Every application holds what the name it is built as holds, which includes
  // every value it is handed and every constant its names read where they
  // land, and the lowering reads a value only through that name or as one the
  // object is built with, so every one of them lowers to the same scope and
  // builds the same children.
  for (const slang::ast::Scope* application : applications_) {
    if (application == scope_) continue;
    auto built = LowerScopeOf(*application);
    if (!built) return std::unexpected(std::move(built.error()));
    if (built->scope != unit_.root_scope ||
        built->construction != own->construction) {
      throw InternalError(
          std::format(
              "UnitLowerer::LowerBodies: '{}' is built as '{}' and lowers to "
              "something else than '{}' does, so the lowering read a value "
              "the name it is built as does not hold -- please report this "
              "as a bug",
              application->asSymbol().getHierarchicalPath(), specialization_,
              scope_->asSymbol().getHierarchicalPath()));
    }
  }
  RequireEveryClassBodyLowered();
  ReadPendingDeclarations();
  ReadSignaturesOfNamedCovergroups();
  ReadTypeDefinitions();
  hir::Verify(unit_, own->construction);
  unit_.body_name = implementations.Intern(specialization_, unit_);
  return hir::Specialization{
      .name = std::move(specialization_),
      .construction = std::move(own->construction),
      .implementation = std::move(unit_)};
}

auto UnitLowerer::LowerScopeOf(const slang::ast::Scope& application)
    -> diag::Result<BuiltScope> {
  const profiling::TimeTraceScope span(
      "lower to HIR", [&] { return unit_.name; });
  support::RecordWork("bodies lowered");
  lowering_ = &application;
  WalkFrame frame;
  StructuralScopeLowerer root(*this, application);
  return root.Run(frame);
}

auto UnitLowerer::InternBlockBody(hir::StructuralScope scope)
    -> hir::BlockBodyId {
  std::vector<hir::BlockBodyId>& alike =
      block_bodies_by_text_[stable_hash::FingerprintOfText(
          hir::DumpScope(unit_, scope))];
  for (const hir::BlockBodyId body : alike) {
    if (unit_.block_bodies.Get(body).scope == scope) return body;
  }
  const hir::BlockBodyId body =
      unit_.block_bodies.Add(hir::BlockBody{.scope = std::move(scope)});
  alike.push_back(body);
  return body;
}

auto UnitLowerer::ApplicationHolding(const slang::ast::Scope& scope) const
    -> const slang::ast::Scope* {
  for (const slang::ast::Scope* level = &scope; level != nullptr;
       level = level->asSymbol().getParentScope()) {
    if (std::ranges::contains(applications_, level)) return level;
  }
  return nullptr;
}

auto UnitLowerer::ApplicationDeclaring(const slang::ast::ParameterSymbol& param)
    const -> const slang::ast::Scope& {
  const slang::ast::Scope* holding =
      ApplicationHolding(*param.getParentScope());
  return holding != nullptr ? *holding : *scope_;
}

auto UnitLowerer::InternOwnClassDeclarations() -> diag::Result<void> {
  // A class is held by the unit whose own source fixes it: a
  // non-parameterized one (LRM 8.3) by the unit declaring it, and each live
  // specialization of a parameterized one (LRM 8.25) by the unit its
  // parameters place it in, which the walk reaches it from either way. Minting
  // them before any body lowers keeps class identity queryable through the
  // unit's registry from the moment any body resolves a reference.
  //
  // The walk descends every structural scope, so which scope replicates a
  // class is settled before any body lowers rather than by whichever reference
  // reaches the class first (LRM 23.9 makes a class a scope of the name tree,
  // and a generate block declares its own).
  auto visit = [&](const slang::ast::Symbol& member) -> diag::Result<void> {
    const diag::SourceSpan span = frontend::PointSpanOf(member.location);
    // A covergroup is a type of its scope as a class is (LRM 19.3), so it is
    // minted on the same walk.
    if (const auto* covergroup = member.as_if<slang::ast::CovergroupType>()) {
      auto r = InternOwnCovergroup(*covergroup, span);
      if (!r) return std::unexpected(std::move(r.error()));
      return {};
    }
    if (member.kind != slang::ast::SymbolKind::ClassType) {
      return {};
    }
    if (auto r = InternOwnClass(member.as<slang::ast::ClassType>(), span); !r) {
      return std::unexpected(std::move(r.error()));
    }
    return {};
  };
  return WalkOwnDeclarations(visit);
}

auto UnitLowerer::NextScopeFrameId() -> ScopeFrameId {
  return ScopeFrameId{.value = next_scope_frame_++};
}

auto UnitLowerer::NextWithClauseId() -> hir::WithClauseId {
  return hir::WithClauseId{.value = next_with_clause_++};
}

// Each id minted here is the source-order position of what it names among its
// own kind in this scope, which is the arena index the body pass assigns, so a
// call or a hierarchical reference resolves regardless of source order
// (LRM 13.4.2, 23.9).
void UnitLowerer::DeclareStructuralIdentities(
    const slang::ast::Scope& scope, support::DefPath class_path) {
  const ScopeFrameId frame = NextScopeFrameId();
  scope_frames_.emplace(&scope, frame);
  ScopeDeclarations& decls = scope_declarations_[&scope];
  ScopePublicationRecord& published = scope_publications_[&scope];
  published.class_path = std::move(class_path);
  publishing_scopes_.push_back(&scope);
  for (const auto& member : scope.members()) {
    if (!Owns(member)) continue;
    DeclareMemberIdentities(member, decls, published, frame);
  }
  // A `disable` names a block or task by static identity (LRM 9.6.2), so it can
  // name one whose body lowers later, lives in another process entirely, or
  // stands in a block instance its body was not lowered from.
  DeclareProceduralScopes(scope, scope, *this, decls.procedural_scopes);
}

void UnitLowerer::DeclareBlockIdentities(
    const slang::ast::GenerateBlockSymbol& block,
    const ScopePublicationRecord& holder) {
  DeclareStructuralIdentities(
      block, support::Extended(holder.class_path, Naming().BlockStepOf(block)));
}

// Every slang symbol kind is listed and there is no `default`, so a kind a
// newer front end adds fails to compile here instead of silently getting no
// identity.
void UnitLowerer::DeclareMemberIdentities(
    const slang::ast::Symbol& member, ScopeDeclarations& decls,
    ScopePublicationRecord& published, ScopeFrameId frame) {
  using slang::ast::SymbolKind;
  // An instance is reached by a hierarchical name stepping onto it (LRM 23.6).
  const auto declare_instance = [&](InstanceArrayShape shape) {
    const hir::InstanceMemberId id = decls.instance_members.Declare();
    MapOwnedChildBinding(member, frame, id);
    published.members.emplace_back(
        ScopePublicationRecord::Instance{
            .symbol = &member, .id = id, .shape = std::move(shape)});
  };
  switch (member.kind) {
    case SymbolKind::GenerateBlock:
      DeclareConditionalGenerate(
          member.as<slang::ast::GenerateBlockSymbol>(), decls, published,
          frame);
      return;
    case SymbolKind::GenerateBlockArray:
      DeclareLoopGenerate(
          member.as<slang::ast::GenerateBlockArraySymbol>(), decls, published,
          frame);
      return;
    case SymbolKind::Instance:
      declare_instance(
          InstanceArrayShape{
              .ranges = {},
              .elements = {&member.as<slang::ast::InstanceSymbol>()}});
      return;
    // A zero-element array (LRM 23.3.2) constructs nothing, so it is no member
    // and takes no id.
    case SymbolKind::InstanceArray:
      if (auto shape = ResolveInstanceArrayShape(
              member.as<slang::ast::InstanceArraySymbol>())) {
        declare_instance(*std::move(shape));
      }
      return;
    case SymbolKind::Subroutine:
      DeclareSubroutine(
          member.as<slang::ast::SubroutineSymbol>(), decls, published, frame);
      return;
    // An anonymous program opens no scope: what it declares are members of
    // this one (LRM 24.6).
    case SymbolKind::AnonymousProgram:
      for (const auto& item :
           member.as<slang::ast::AnonymousProgramSymbol>().members()) {
        if (Owns(item)) DeclareMemberIdentities(item, decls, published, frame);
      }
      return;
    case SymbolKind::Modport:
      DeclareModportEvaluators(
          member.as<slang::ast::ModportSymbol>(), decls, published);
      return;
    // A port's default is an expression of this unit, evaluated in its scope
    // for an instance that leaves the port unconnected (LRM 23.2.2.4), so it
    // is a subroutine of this scope the instantiator asks for.
    case SymbolKind::Port:
      if (const auto& port = member.as<slang::ast::PortSymbol>();
          port.getInitializer() != nullptr) {
        const hir::StructuralSubroutineId id =
            decls.structural_subroutines.Declare();
        MapEvaluator(*port.getInitializer(), id);
        published.callables.emplace_back(
            ScopePublicationRecord::PortDefault{.port = &port, .id = id});
      }
      return;
    case SymbolKind::ProceduralBlock:
      DeclareProcess(
          member.as<slang::ast::ProceduralBlockSymbol>(), decls, published,
          frame);
      return;
    // A hierarchical name reaches storage of any scope (LRM 23.6), and a
    // sibling generate block lowers whole before this scope's own storage is
    // built, so a name from there holds the identity before its declaration.
    case SymbolKind::Variable:
    case SymbolKind::Net: {
      const auto& value = member.as<slang::ast::ValueSymbol>();
      const hir::StructuralDataObjectId id =
          decls.structural_data_objects.Declare();
      MapStructuralDataObjectBinding(value, frame, id);
      published.members.emplace_back(
          ScopePublicationRecord::DataObject{.symbol = &value, .id = id});
      return;
    }
    // A parameter whose value differs between the objects built from this
    // scope is held by each of them, so a hierarchical name reaches it there
    // as it reaches a variable (LRM 23.6). One the specialization fixes is a
    // constant of the specialization, which a name reaching it reads as one.
    case SymbolKind::Parameter: {
      const auto& param = member.as<slang::ast::ParameterSymbol>();
      if (!DiffersPerObject(param)) return;
      const hir::StructuralDataObjectId id =
          decls.structural_data_objects.Declare();
      MapStructuralDataObjectBinding(param, frame, id);
      published.members.emplace_back(
          ScopePublicationRecord::DataObject{.symbol = &param, .id = id});
      return;
    }
    // An interface port names an instance another unit builds and this one's
    // parent binds (LRM 25.3); its identity comes with the record of that
    // unit's object, which is reachable only once bodies lower.
    case SymbolKind::InterfacePort:
      published.members.emplace_back(
          ScopePublicationRecord::InterfacePort{
              .symbol = &member.as<slang::ast::InterfacePortSymbol>()});
      return;
    // Nothing a body names before it lowers: a connection takes its identity
    // where the member walk builds it, a type is interned where it is declared
    // or used -- a class publishing what it keeps for itself where it is
    // interned -- and the rest belongs to another scope or brings no structure
    // of its own.
    case SymbolKind::ClassType:
    case SymbolKind::GenericClassDef:
    case SymbolKind::ContinuousAssign:
    case SymbolKind::NetAlias:
    case SymbolKind::Sequence:
    case SymbolKind::Property:
    case SymbolKind::AssertionPort:
    case SymbolKind::LocalAssertionVar:
    case SymbolKind::Checker:
    case SymbolKind::CheckerInstance:
    case SymbolKind::CheckerInstanceBody:
    case SymbolKind::PrimitiveInstance:
    case SymbolKind::RandSeqProduction:
    case SymbolKind::UninstantiatedDef:
    case SymbolKind::PredefinedIntegerType:
    case SymbolKind::ScalarType:
    case SymbolKind::FloatingType:
    case SymbolKind::EnumType:
    case SymbolKind::EnumValue:
    case SymbolKind::PackedArrayType:
    case SymbolKind::FixedSizeUnpackedArrayType:
    case SymbolKind::DynamicArrayType:
    case SymbolKind::DPIOpenArrayType:
    case SymbolKind::AssociativeArrayType:
    case SymbolKind::QueueType:
    case SymbolKind::PackedStructType:
    case SymbolKind::UnpackedStructType:
    case SymbolKind::PackedUnionType:
    case SymbolKind::UnpackedUnionType:
    case SymbolKind::CovergroupType:
    case SymbolKind::VoidType:
    case SymbolKind::NullType:
    case SymbolKind::CHandleType:
    case SymbolKind::StringType:
    case SymbolKind::EventType:
    case SymbolKind::UnboundedType:
    case SymbolKind::TypeRefType:
    case SymbolKind::UntypedType:
    case SymbolKind::SequenceType:
    case SymbolKind::PropertyType:
    case SymbolKind::VirtualInterfaceType:
    case SymbolKind::TypeAlias:
    case SymbolKind::ErrorType:
    case SymbolKind::ForwardingTypedef:
    case SymbolKind::NetType:
    case SymbolKind::TypeParameter:
    case SymbolKind::Specparam:
    case SymbolKind::DefParam:
    case SymbolKind::Genvar:
    case SymbolKind::ExplicitImport:
    case SymbolKind::WildcardImport:
    case SymbolKind::Attribute:
    case SymbolKind::ConfigBlock:
    case SymbolKind::ElabSystemTask:
    case SymbolKind::MultiPort:
    case SymbolKind::ModportPort:
    case SymbolKind::ModportClocking:
    case SymbolKind::InstanceBody:
    case SymbolKind::Package:
    case SymbolKind::CompilationUnit:
    case SymbolKind::Root:
    case SymbolKind::Definition:
    case SymbolKind::Unknown:
    case SymbolKind::DeferredMember:
    case SymbolKind::TransparentMember:
    case SymbolKind::EmptyMember:
    case SymbolKind::StatementBlock:
    case SymbolKind::FormalArgument:
    case SymbolKind::Field:
    case SymbolKind::ClassProperty:
    case SymbolKind::MethodPrototype:
    case SymbolKind::Iterator:
    case SymbolKind::PatternVar:
    case SymbolKind::ConstraintBlock:
    case SymbolKind::CovergroupBody:
    case SymbolKind::Coverpoint:
    case SymbolKind::CoverCross:
    case SymbolKind::CoverCrossBody:
    case SymbolKind::CoverageBin:
    case SymbolKind::Primitive:
    case SymbolKind::PrimitivePort:
    case SymbolKind::SpecifyBlock:
    case SymbolKind::TimingPath:
    case SymbolKind::PulseStyle:
    case SymbolKind::SystemTimingCheck:
    case SymbolKind::ClockVar:
    case SymbolKind::LetDecl:
      return;
    case SymbolKind::ClockingBlock:
      DeclareClockingEvaluators(
          member.as<slang::ast::ClockingBlockSymbol>(), decls, published);
      return;
  }
  throw InternalError(
      "UnitLowerer::DeclareMemberIdentities: unknown slang SymbolKind");
}

// A conditional generate is one construct however many alternatives it holds
// and whichever of them this elaboration selected (LRM 27.5), so it takes one
// generate id, minted where the first alternative stands, and every
// alternative reads it back. An alternative this elaboration did not select
// carries no runtime object and so declares nothing of its own, but it is
// still one of the construct's and is named as such.
void UnitLowerer::DeclareConditionalGenerate(
    const slang::ast::GenerateBlockSymbol& block, ScopeDeclarations& decls,
    ScopePublicationRecord& published, ScopeFrameId frame) {
  if (!OpensItsConstruct(block)) return;
  const hir::GenerateId generate = decls.generates.Declare();
  for (const auto* arm : AlternativesOfConstruct(block)) {
    MapOwnedChildBinding(
        *arm, frame, DeclaredGenerateBlock{.generate = generate});
    if (arm->isUninstantiated) continue;
    DeclareBlockIdentities(*arm, published);
  }
  published.generates.emplace_back(
      ScopePublicationRecord::Choice{
          .id = generate, .construct = block.constructIndex});
}

// A loop generate elaborates each iteration into a block of its own
// (LRM 27.4), and a name reaching one means that block. How many scopes the
// construct compiles to belongs to where its bodies are built, so a name
// resolves here without it. The loop is one construct however many blocks it
// counted out: how many is a value of the construction that runs it, so a loop
// that counted none is declared like any other.
void UnitLowerer::DeclareLoopGenerate(
    const slang::ast::GenerateBlockArraySymbol& array, ScopeDeclarations& decls,
    ScopePublicationRecord& published, ScopeFrameId frame) {
  const hir::GenerateId generate = decls.generates.Declare();
  loop_generates_.emplace(&array, generate);
  published.generates.emplace_back(
      ScopePublicationRecord::Loop{.id = generate, .loop = &array});
  for (const auto* entry : array.entries) {
    MapOwnedChildBinding(
        *entry, frame, DeclaredGenerateBlock{.generate = generate});
    DeclareBlockIdentities(*entry, published);
  }
}

// A DPI-C import declares no body and takes no subroutine id; the unit interns
// its record on first sight from either side. What it does record here is the
// scope it is declared in, which a `context` import observes during its
// foreign call (LRM 35.5.3) -- and only an instantiated scope is one. A
// namespace is never instantiated, so its declarations name none and a call to
// one observes no scope, whether it is made from inside the namespace or from
// a unit that imported the name.
//
// Every other subroutine is one a caller enables on the scope (LRM 13.3, 23.6,
// 25.7) or on the namespace (LRM 26.3), and so one the scope publishes, along
// with what a `disable` of it ends (LRM 9.6.2) and what a name reaches through
// it (LRM 23.9).
void UnitLowerer::DeclareSubroutine(
    const slang::ast::SubroutineSymbol& sub, ScopeDeclarations& decls,
    ScopePublicationRecord& published, ScopeFrameId frame) {
  if (sub.flags.has(slang::ast::MethodFlags::DPIImport)) {
    if (hir::RootsAnObject(unit_.role)) {
      MapForeignImportScope(sub, frame);
    }
    return;
  }
  const hir::StructuralSubroutineId id = decls.structural_subroutines.Declare();
  MapSubroutineBinding(sub, frame, id);
  published.callables.emplace_back(
      ScopePublicationRecord::Subroutine{.symbol = &sub, .id = id});
  // A formal's default is evaluated in this scope each time a call omits it
  // (LRM 13.5.3), wherever the call is, so it is a subroutine of this scope
  // the caller asks for, as a port's default is.
  for (const slang::ast::FormalArgumentSymbol* formal : sub.getArguments()) {
    const slang::ast::Expression* default_value = formal->getDefaultValue();
    if (default_value == nullptr) continue;
    const hir::StructuralSubroutineId evaluator =
        decls.structural_subroutines.Declare();
    MapEvaluator(*default_value, evaluator);
    published.callables.emplace_back(
        ScopePublicationRecord::ArgumentDefault{
            .subroutine = &sub, .formal = formal, .id = evaluator});
  }
  support::DefPath path =
      support::Extended(published.class_path, ProceduralStepOf(sub));
  published.disable_targets.push_back(
      ScopePublicationRecord::DisableTarget{.symbol = &sub, .path = path});
  DeclareProceduralStatics(
      sub, sub, hir::ProceduralBodyRef{id}, frame, published, std::move(path));
}

// A name a view defines and offers only for reading stands for an expression
// this unit evaluates (LRM 25.5.4), which is a subroutine of this scope and
// takes its identity here with every other, so the signature can name it
// before any body is lowered. Every other name a view offers designates
// storage -- an item the view wrote no expression for is the interface's own,
// and every direction but `input` bounds the expression to an lvalue -- and
// storage is reached rather than asked for.
//
// What a view publishes is only the names it defines: an item written as a
// plain identifier is the interface's own item serving twice (LRM 25.5.4),
// already published as a member.
void UnitLowerer::DeclareModportEvaluators(
    const slang::ast::ModportSymbol& modport, ScopeDeclarations& decls,
    ScopePublicationRecord& published) {
  ScopePublicationRecord::View view{.modport = &modport, .names = {}};
  for (const auto& item : modport.members()) {
    const auto* port = item.as_if<slang::ast::ModportPortSymbol>();
    if (port == nullptr || !ViewDefinesTheName(*port)) continue;
    if (port->direction != slang::ast::ArgumentDirection::In) {
      view.names.emplace_back(ScopePublicationRecord::ViewPlace{.port = port});
      continue;
    }
    const hir::StructuralSubroutineId id =
        decls.structural_subroutines.Declare();
    MapEvaluator(ViewExpressionOf(*port), id);
    view.names.emplace_back(ScopePublicationRecord::ViewComputed{.port = port});
    published.callables.emplace_back(
        ScopePublicationRecord::ViewRead{
            .modport = &modport, .port = port, .id = id});
  }
  published.views.push_back(std::move(view));
}

// A named clocking block clocks a sequence or property it declares wherever the
// declaration is instantiated (LRM 14.13, 16.16), and another unit reaches the
// declaration through a name. The clocking event's operands and qualifiers name
// this scope's declarations, so each is an expression this unit evaluates for
// that unit, a subroutine of this scope as a view's computed name is. A block
// with no name is reached by no name, and is never another unit's clock.
void UnitLowerer::DeclareClockingEvaluators(
    const slang::ast::ClockingBlockSymbol& block, ScopeDeclarations& decls,
    ScopePublicationRecord& published) {
  if (block.name.empty()) return;
  const auto evaluator = [&](const slang::ast::Expression& expression,
                             std::string name) {
    const hir::StructuralSubroutineId id =
        decls.structural_subroutines.Declare();
    MapEvaluator(expression, id);
    const hir::PublishedCallableId position{
        static_cast<std::uint32_t>(published.callables.size())};
    published.callables.emplace_back(
        ScopePublicationRecord::ClockingExpression{
            .block = &block,
            .expression = &expression,
            .name = std::move(name),
            .id = id});
    return position;
  };
  ScopePublicationRecord::Clocking clocking{.block = &block, .events = {}};
  const std::vector<const slang::ast::SignalEventControl*> events =
      ClockingEventsOf(block);
  for (std::size_t at = 0; at < events.size(); ++at) {
    const slang::ast::SignalEventControl& event = *events[at];
    const hir::PublishedCallableId operand =
        evaluator(event.expr, ClockingEventName(block.name, at));
    std::optional<hir::PublishedCallableId> condition;
    if (event.iffCondition != nullptr) {
      condition =
          evaluator(*event.iffCondition, ClockingConditionName(block.name, at));
    }
    clocking.events.push_back(
        ScopePublicationRecord::ClockingEntry{
            .event = &event, .operand = operand, .condition = condition});
  }
  published.clockings.push_back(std::move(clocking));
}

// An assertion whose enabling condition is 1 is not a procedure the design
// runs (LRM 16.14.5): what starts its attempts is the clock. So it takes no
// process identity.
void UnitLowerer::DeclareProcess(
    const slang::ast::ProceduralBlockSymbol& proc, ScopeDeclarations& decls,
    ScopePublicationRecord& published, ScopeFrameId frame) {
  if (!Contains(proc)) return;
  if (StaticConcurrentAssertionOf(proc).assertion != nullptr) return;
  const hir::ProcessId id = decls.processes.Declare();
  MapProcessBinding(proc, id);
  // The frontend hoists a process's outermost blocks into this scope's member
  // list -- its body, or where the body opens no scope of its own, each block
  // written directly inside it -- so the process is the only place that says
  // which of those blocks are its own. A process is unnamed, so each of them
  // stands directly in this scope.
  for (const auto* block : proc.getBlocks()) {
    std::optional<support::DefPath> path;
    if (!block->name.empty()) {
      path = support::Extended(published.class_path, ProceduralStepOf(*block));
      published.disable_targets.push_back(
          ScopePublicationRecord::DisableTarget{
              .symbol = block, .path = *path});
    }
    DeclareProceduralStatics(
        *block, proc, hir::ProceduralBodyRef{id}, frame, published, path);
  }
}

void UnitLowerer::DeclareProceduralStatics(
    const slang::ast::Scope& block, const slang::ast::Symbol& body_symbol,
    hir::ProceduralBodyRef body, ScopeFrameId frame,
    ScopePublicationRecord& published,
    const std::optional<support::DefPath>& within) {
  for (const auto& member : block.members()) {
    if (member.kind == slang::ast::SymbolKind::StatementBlock) {
      const auto& nested = member.as<slang::ast::StatementBlockSymbol>();
      std::optional<support::DefPath> path;
      if (within.has_value() && !nested.name.empty()) {
        path = support::Extended(*within, ProceduralStepOf(nested));
        published.disable_targets.push_back(
            ScopePublicationRecord::DisableTarget{
                .symbol = &nested, .path = *path});
      }
      DeclareProceduralStatics(
          nested, body_symbol, body, frame, published, path);
      continue;
    }
    // A static-lifetime variable is storage of the object the body runs on
    // (LRM 6.21), and so is a constant whose value differs between the objects
    // built from this unit: it is read there rather than folded to the value
    // one elaboration gave it.
    const auto* var = member.as_if<slang::ast::VariableSymbol>();
    const auto* constant = member.as_if<slang::ast::ParameterSymbol>();
    const bool held = (var != nullptr &&
                       var->lifetime == slang::ast::VariableLifetime::Static) ||
                      (constant != nullptr && DiffersPerObject(*constant));
    if (!held) continue;

    const hir::ProceduralVarId id =
        procedural_static_vars_[&body_symbol].Declare();
    const auto [_, inserted] = procedural_static_bindings_.emplace(
        &member,
        ProceduralStaticBinding{.home_frame = frame, .body = body, .var = id});
    if (!inserted) {
      throw InternalError(
          "UnitLowerer::DeclareProceduralStatics: procedural static already "
          "mapped");
    }
    if (var != nullptr && within.has_value()) {
      published.members.emplace_back(
          ScopePublicationRecord::LocalStatic{
              .symbol = var, .body = body, .var = id, .within = *within});
    }
  }
}

void UnitLowerer::DeclareClassStatics(
    const slang::ast::ClassType& cls, const slang::ast::Scope& replicating) {
  const auto it = scope_publications_.find(&replicating);
  if (it == scope_publications_.end()) {
    throw InternalError(
        "UnitLowerer::DeclareClassStatics: a class this unit holds is "
        "replicated by a scope of this unit, which declared its identities "
        "first");
  }
  for (const auto& member : cls.members()) {
    const auto* property = member.as_if<slang::ast::ClassPropertySymbol>();
    if (property != nullptr &&
        property->lifetime == slang::ast::VariableLifetime::Static) {
      it->second.members.emplace_back(
          ScopePublicationRecord::ClassStatic{
              .symbol = property, .owner = &cls});
    }
  }
}

auto UnitLowerer::ValueSourceOf(const slang::ast::ParameterSymbol& param) const
    -> ParameterValueSource {
  const auto* body = ApplicationDeclaring(param)
                         .asSymbol()
                         .as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) return ParameterValueSource::kSettledAtElaboration;
  return Naming().Keys().ValueSourceOf(InstantiationOf(*body), param);
}

auto UnitLowerer::DiffersPerObject(
    const slang::ast::ParameterSymbol& param) const -> bool {
  switch (ValueSourceOf(param)) {
    case ParameterValueSource::kSettledAtElaboration:
      return false;
    case ParameterValueSource::kSuppliedAtConstruction:
    case ParameterValueSource::kComputedAtConstruction:
      return true;
  }
  throw InternalError(
      "UnitLowerer::DiffersPerObject: unknown ParameterValueSource");
}

auto UnitLowerer::MakeProceduralBody(const slang::ast::Symbol& body_symbol)
    -> hir::ProceduralBody {
  hir::ProceduralBody body;
  const auto it = procedural_static_vars_.find(&body_symbol);
  if (it == procedural_static_vars_.end()) return body;
  body.procedural_vars = std::move(it->second);
  return body;
}

auto UnitLowerer::TakeScopeDeclarations(const slang::ast::Scope& scope)
    -> ScopeDeclarations {
  const auto it = scope_declarations_.find(&scope);
  if (it == scope_declarations_.end()) return {};
  return std::move(it->second);
}

auto UnitLowerer::LookupScopeFrame(const slang::ast::Scope& scope) const
    -> ScopeFrameId {
  const auto it = scope_frames_.find(&scope);
  if (it == scope_frames_.end()) {
    throw InternalError(
        "UnitLowerer::LookupScopeFrame: scope frame was not declared before "
        "body lowering");
  }
  return it->second;
}

auto UnitLowerer::DeclaringScopeChain(const slang::ast::Scope& scope) const
    -> std::vector<ScopeFrameId> {
  // Walking outward and reversing, rather than descending, because the walk
  // starts from the declaration and the enclosing chain is what slang already
  // holds. A scope the declaration pass assigned no frame is not a structural
  // scope and contributes no level, which is the same reading the pass itself
  // applied when it chose which members to descend into.
  std::vector<ScopeFrameId> chain;
  for (const slang::ast::Scope* level = &scope; level != nullptr;
       level = level->asSymbol().getParentScope()) {
    if (const auto it = scope_frames_.find(level); it != scope_frames_.end()) {
      chain.push_back(it->second);
    }
  }
  std::ranges::reverse(chain);
  return chain;
}

auto UnitLowerer::InReadingOrder(std::vector<const slang::ast::Scope*> built)
    const -> std::vector<const slang::ast::Scope*> {
  if (Naming().Keys().ChecksItself()) std::ranges::reverse(built);
  return built;
}

auto UnitLowerer::HeldByAnotherDesignElement(
    const slang::ast::Symbol& type) const -> bool {
  return BelongsToAnInstance(type) && !Owns(type);
}

auto UnitLowerer::TakesDeclaringInstance(
    const slang::ast::ClassType& cls, diag::SourceSpan span)
    -> diag::Result<bool> {
  // A class the runtime library defines belongs to no instance, and no unit
  // declares it.
  if (ImportedRuntimeClassOf(cls).has_value()) {
    return false;
  }
  auto id = ResolveClass(cls, span);
  if (!id) return std::unexpected(std::move(id.error()));
  return DeclarationOf(*id).takes_declaring_instance;
}

auto UnitLowerer::DeclaringScopeHopsFrom(
    const slang::ast::Symbol& type, const WalkFrame& frame,
    diag::SourceSpan span) const -> diag::Result<hir::StructuralHops> {
  if (!BelongsToAnInstance(type) || HeldByAnotherDesignElement(type)) {
    throw InternalError(
        "UnitLowerer::DeclaringScopeHopsFrom: only a type one of this unit's "
        "structural scopes replicates is counted out of this unit's scopes");
  }
  const slang::ast::Scope& replicating = ReplicatingScope(type);
  const auto hops = frame.HopsTo(LookupScopeFrame(replicating));
  if (!hops.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedClassFeature,
        "a class whose static members belong to an instance not reachable "
        "from here is not yet supported");
  }
  return *hops;
}

auto UnitLowerer::TakeReplicatedClasses(const slang::ast::Scope& scope)
    -> std::vector<hir::ClassId> {
  const auto it = classes_by_scope_.find(&scope);
  if (it == classes_by_scope_.end()) return {};
  return std::move(it->second);
}

void UnitLowerer::MapStructuralDataObjectBinding(
    const slang::ast::ValueSymbol& var, ScopeFrameId home_frame,
    hir::StructuralDataObjectId local) {
  const auto [_, inserted] = structural_data_object_bindings_.emplace(
      &var,
      StructuralDataObjectBinding{.home_frame = home_frame, .var_id = local});
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapStructuralDataObjectBinding: structural data object "
        "already mapped");
  }
}

void UnitLowerer::MapInterfacePortBinding(
    const slang::ast::InterfacePortSymbol& port, ScopeFrameId home_frame,
    hir::InterfacePortId local) {
  const auto [_, inserted] = interface_port_bindings_.emplace(
      &port, InterfacePortBinding{.home_frame = home_frame, .port = local});
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapInterfacePortBinding: interface port already mapped");
  }
}

auto UnitLowerer::TakePublication(const slang::ast::Scope& scope)
    -> hir::ScopePublication {
  // A namespace roots no object, so no class lays out what it publishes;
  // another unit reaches its declarations by name (LRM 26.3).
  if (!hir::RootsAnObject(unit_.role)) return {};
  const ScopePublicationRecord& published = PublicationOf(scope);
  const auto unbound = [] [[noreturn]] (std::string_view what) {
    throw InternalError(
        std::format(
            "UnitLowerer::TakePublication: {} this unit published was given no "
            "identity by the time its scope finished lowering",
            what));
  };

  const auto stated = scope_classes_.find(&scope);
  if (stated == scope_classes_.end()) {
    throw InternalError(
        "UnitLowerer::TakePublication: the class a scope published is stated "
        "while the unit declares, and handed to its scope once");
  }
  const std::size_t published_loops = stated->second.loops.size();
  const std::size_t published_choices = stated->second.choices.size();
  const hir::ScopeClassId scope_class =
      unit_.scope_classes.DeclareOwn(stated->second.class_path);
  hir::ScopePublication publication{
      .scope_class = scope_class,
      .signature = std::move(stated->second),
      .members =
          base::Translation<hir::PublishedMemberId, hir::PublishedDecl>{
              published.members.size()},
      .loops =
          base::Translation<hir::PublishedLoopId, hir::GenerateId>{
              published_loops},
      .choices =
          base::Translation<hir::PublishedChoiceId, hir::GenerateId>{
              published_choices},
      .disable_targets =
          base::Translation<
              hir::PublishedDisableTargetId, hir::ProceduralScopeId>{
              published.disable_targets.size()},
      .callables = base::Translation<
          hir::PublishedCallableId, hir::StructuralSubroutineId>{
          published.callables.size()}};
  scope_classes_.erase(stated);
  for (const ScopePublicationRecord::Member& member : published.members) {
    publication.members.Append(
        std::visit(
            Overloaded{
                [](const ScopePublicationRecord::DataObject& data)
                    -> hir::PublishedDecl { return data.id; },
                [](const ScopePublicationRecord::LocalStatic& local)
                    -> hir::PublishedDecl {
                  return hir::PublishedStatic{
                      .body = local.body, .var = local.var};
                },
                [&](const ScopePublicationRecord::ClassStatic& property)
                    -> hir::PublishedDecl {
                  const auto owner = class_cache_.find(property.owner);
                  if (owner == class_cache_.end() ||
                      !unit_.classes.IsOwn(owner->second)) {
                    unbound("a class's static property");
                  }
                  return hir::ClassStaticPropertyTarget{
                      .owner = owner->second,
                      .prop = StaticPropertyPositionOf(
                          owner->second, property.symbol->name)};
                },
                [&](const ScopePublicationRecord::CovergroupEvent& event)
                    -> hir::PublishedDecl {
                  const auto minted = covergroup_cache_.find(event.symbol);
                  if (minted == covergroup_cache_.end()) {
                    unbound("a covergroup's block event");
                  }
                  return hir::CovergroupBlockEvent{
                      .covergroup = minted->second};
                },
                [](const ScopePublicationRecord::Instance& instance)
                    -> hir::PublishedDecl { return instance.id; },
                [&](const ScopePublicationRecord::InterfacePort& port)
                    -> hir::PublishedDecl {
                  const auto binding = LookupInterfacePortBinding(*port.symbol);
                  if (!binding.has_value()) unbound("an interface port");
                  return binding->port;
                }},
            member));
  }
  for (const ScopePublicationRecord::Construct& construct :
       published.generates) {
    std::visit(
        Overloaded{
            [&](const ScopePublicationRecord::Loop& loop) {
              publication.loops.Append(loop.id);
            },
            [&](const ScopePublicationRecord::Choice& choice) {
              publication.choices.Append(choice.id);
            }},
        construct);
  }
  for (const ScopePublicationRecord::DisableTarget& target :
       published.disable_targets) {
    publication.disable_targets.Append(LookupProceduralScope(*target.symbol));
  }
  for (const ScopePublicationRecord::Callable& callable : published.callables) {
    publication.callables.Append(
        std::visit([](const auto& entered) { return entered.id; }, callable));
  }
  return publication;
}

auto UnitLowerer::PublicationOf(const slang::ast::Scope& scope) const
    -> const ScopePublicationRecord& {
  const auto it = scope_publications_.find(&scope);
  if (it == scope_publications_.end()) {
    throw InternalError(
        "UnitLowerer::PublicationOf: the walk minting this unit's identities "
        "records what every scope it reaches publishes");
  }
  return it->second;
}

auto ScopePublicationRecord::MemberOf(const slang::ast::Symbol& declared) const
    -> std::optional<hir::PublishedMemberId> {
  const auto stands_for = [&](const Member& member) {
    return std::visit(
        [&](const auto& entry) -> bool {
          const slang::ast::Symbol* symbol = entry.symbol;
          return symbol == &declared;
        },
        member);
  };
  const auto it = std::ranges::find_if(members, stands_for);
  if (it == members.end()) return std::nullopt;
  return hir::PublishedMemberId{
      static_cast<std::uint32_t>(std::distance(members.begin(), it))};
}

auto ScopePublicationRecord::CallableOf(const slang::ast::Symbol& holder) const
    -> std::optional<hir::PublishedCallableId> {
  const auto holds = [&](const Callable& callable) {
    return std::visit(
        Overloaded{
            [&](const Subroutine& sub) -> bool {
              return sub.symbol == &holder;
            },
            [&](const PortDefault& port) -> bool {
              return port.port == &holder;
            },
            [&](const ViewRead& read) -> bool { return read.port == &holder; },
            [&](const ArgumentDefault& argument) -> bool {
              return argument.formal == &holder;
            },
            // A clocking block holds an expression per event of its clocking
            // event, so its callables are found through its events rather
            // than by the block.
            [](const ClockingExpression&) -> bool { return false; }},
        callable);
  };
  const auto it = std::ranges::find_if(callables, holds);
  if (it == callables.end()) return std::nullopt;
  return hir::PublishedCallableId{
      static_cast<std::uint32_t>(std::distance(callables.begin(), it))};
}

auto UnitLowerer::LookupInterfacePortBinding(const slang::ast::Symbol& port)
    const -> std::optional<InterfacePortBinding> {
  const auto it = interface_port_bindings_.find(&port);
  if (it == interface_port_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

auto UnitLowerer::ReservedDataObject(const slang::ast::ValueSymbol& declared)
    const -> hir::StructuralDataObjectId {
  const auto binding = LookupStructuralDataObjectBinding(declared);
  if (!binding.has_value()) {
    throw InternalError(
        "UnitLowerer::ReservedDataObject: the declaration pass reserves an "
        "identity for every variable and net a scope declares");
  }
  return binding->var_id;
}

auto UnitLowerer::LookupStructuralDataObjectBinding(
    const slang::ast::ValueSymbol& var) const
    -> std::optional<StructuralDataObjectBinding> {
  const auto it = structural_data_object_bindings_.find(&var);
  if (it == structural_data_object_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

void UnitLowerer::MapEvaluator(
    const slang::ast::Expression& evaluated,
    hir::StructuralSubroutineId evaluator) {
  const auto [_, inserted] = evaluators_.emplace(&evaluated, evaluator);
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapEvaluator: an expression is evaluated in one "
        "subroutine");
  }
}

auto UnitLowerer::FindEvaluator(const slang::ast::Expression& evaluated) const
    -> std::optional<hir::StructuralSubroutineId> {
  const auto it = evaluators_.find(&evaluated);
  if (it == evaluators_.end()) return std::nullopt;
  return it->second;
}

auto UnitLowerer::EvaluatorOf(const slang::ast::Expression& evaluated) const
    -> hir::StructuralSubroutineId {
  const std::optional<hir::StructuralSubroutineId> evaluator =
      FindEvaluator(evaluated);
  if (!evaluator.has_value()) {
    throw InternalError(
        "UnitLowerer::EvaluatorOf: an expression another unit asks for takes "
        "its identity with the unit's other structural declarations");
  }
  return *evaluator;
}

auto UnitLowerer::InternDeclaredType(const slang::ast::Symbol& holder)
    -> diag::Result<hir::TypeId> {
  const auto span = frontend::PointSpanOf(holder.location);
  if (const auto* declared = holder.as_if<slang::ast::PortSymbol>()) {
    return InternType(declared->getType(), span);
  }
  if (const auto* offered = holder.as_if<slang::ast::ModportPortSymbol>()) {
    return InternType(offered->getType(), span);
  }
  if (const auto* formal = holder.as_if<slang::ast::FormalArgumentSymbol>()) {
    return InternType(formal->getType(), span);
  }
  throw InternalError(
      "UnitLowerer::InternDeclaredType: only a port, a name a view offers and "
      "a formal hold an expression another unit asks this one to evaluate");
}

void UnitLowerer::MapSubroutineBinding(
    const slang::ast::SubroutineSymbol& sym, ScopeFrameId owner_frame,
    hir::StructuralSubroutineId local) {
  const auto [_, inserted] = subroutine_bindings_.emplace(
      &sym,
      SubroutineBinding{.owner_frame = owner_frame, .subroutine_id = local});
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapSubroutineBinding: subroutine symbol already "
        "mapped");
  }
}

auto UnitLowerer::LookupSubroutineBinding(
    const slang::ast::SubroutineSymbol& sym) const
    -> std::optional<SubroutineBinding> {
  const auto it = subroutine_bindings_.find(&sym);
  if (it == subroutine_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

void UnitLowerer::MapForeignImportScope(
    const slang::ast::SubroutineSymbol& sym, ScopeFrameId declaring_frame) {
  const auto [_, inserted] =
      foreign_import_scopes_.emplace(&sym, declaring_frame);
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapForeignImportScope: DPI import symbol already mapped");
  }
}

auto UnitLowerer::LookupForeignImportScope(
    const slang::ast::SubroutineSymbol& sym) const
    -> std::optional<ScopeFrameId> {
  const auto it = foreign_import_scopes_.find(&sym);
  if (it == foreign_import_scopes_.end()) {
    return std::nullopt;
  }
  return it->second;
}

auto UnitLowerer::EnsureForeignImport(const slang::ast::SubroutineSymbol& sym)
    -> diag::Result<hir::ForeignImportId> {
  if (const auto it = foreign_import_bindings_.find(&sym);
      it != foreign_import_bindings_.end()) {
    return it->second;
  }
  auto decl_or = LowerForeignImport(*this, sym);
  if (!decl_or) return std::unexpected(std::move(decl_or.error()));
  // One foreign symbol is one import however many declarations spell it: LRM
  // 35.5.4 requires every declaration of a C identifier to agree on the
  // signature, so two that agree are the same import and not two of them. The
  // record is therefore keyed by what it states rather than by which
  // declaration stated it -- two generate blocks each declaring the same
  // import is the ordinary case, and recording it twice makes two blocks that
  // state the same thing hold different ids for it.
  for (const hir::ForeignImportId id : unit_.foreign_imports.Ids()) {
    if (unit_.foreign_imports.Get(id) == *decl_or) {
      foreign_import_bindings_.emplace(&sym, id);
      return id;
    }
  }
  const hir::ForeignImportId id =
      unit_.foreign_imports.Add(*std::move(decl_or));
  foreign_import_bindings_.emplace(&sym, id);
  return id;
}

void UnitLowerer::MapPatternVar(
    const slang::ast::PatternVarSymbol& sym, hir::PatternId pattern) {
  const auto [_, inserted] = pattern_var_bindings_.emplace(&sym, pattern);
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapPatternVar: pattern-bound identifier already mapped; "
        "its id indexes one body's arena, so a second mapping would silently "
        "redirect the first body's references");
  }
}

auto UnitLowerer::LookupPatternVar(const slang::ast::PatternVarSymbol& sym)
    const -> std::optional<hir::PatternId> {
  const auto it = pattern_var_bindings_.find(&sym);
  if (it == pattern_var_bindings_.end()) return std::nullopt;
  return it->second;
}

void UnitLowerer::MapOwnedChildBinding(
    const slang::ast::Symbol& child, ScopeFrameId home_frame,
    DeclaredOwnedChild declared) {
  const auto [_, inserted] = owned_child_bindings_.emplace(
      &child, OwnedChildBinding{.home_frame = home_frame, .child = declared});
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapOwnedChildBinding: owned child already mapped");
  }
}

auto UnitLowerer::LookupOwnedChildBinding(const slang::ast::Symbol& child) const
    -> std::optional<OwnedChildBinding> {
  const auto it = owned_child_bindings_.find(&child);
  if (it == owned_child_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

auto UnitLowerer::GenerateIdOf(const slang::ast::Symbol& construct) const
    -> hir::GenerateId {
  const auto it = owned_child_bindings_.find(&construct);
  if (it == owned_child_bindings_.end()) {
    throw InternalError(
        "UnitLowerer::GenerateIdOf: a generate construct was given no "
        "identity by the declaration pass");
  }
  return std::visit(
      Overloaded{
          [](hir::InstanceMemberId) -> hir::GenerateId {
            throw InternalError(
                "UnitLowerer::GenerateIdOf: a generate construct was given an "
                "instance's identity");
          },
          [](DeclaredGenerateBlock block) { return block.generate; }},
      it->second.child);
}

auto UnitLowerer::GenerateIdOf(
    const slang::ast::GenerateBlockArraySymbol& loop) const -> hir::GenerateId {
  const auto it = loop_generates_.find(&loop);
  if (it == loop_generates_.end()) {
    throw InternalError(
        "UnitLowerer::GenerateIdOf: a loop generate was given no identity by "
        "the declaration pass");
  }
  return it->second;
}

auto UnitLowerer::InstanceMemberIdOf(const slang::ast::Symbol& instance) const
    -> hir::InstanceMemberId {
  const auto it = owned_child_bindings_.find(&instance);
  const auto* id = it == owned_child_bindings_.end()
                       ? nullptr
                       : std::get_if<hir::InstanceMemberId>(&it->second.child);
  if (id == nullptr) {
    throw InternalError(
        "UnitLowerer::InstanceMemberIdOf: an instance was given no instance "
        "identity by the declaration pass");
  }
  return *id;
}

void UnitLowerer::MapProcessBinding(
    const slang::ast::ProceduralBlockSymbol& proc, hir::ProcessId id) {
  const auto [_, inserted] = process_bindings_.emplace(&proc, id);
  if (!inserted) {
    throw InternalError(
        "UnitLowerer::MapProcessBinding: process symbol already mapped");
  }
}

auto UnitLowerer::InferredProcedureClock(const slang::ast::Symbol& containing)
    const -> const slang::ast::TimingControl* {
  const auto* proc = containing.as_if<slang::ast::ProceduralBlockSymbol>();
  if (proc == nullptr) {
    return nullptr;
  }
  return Sensitivity().AnalyzeProcedureClock(*proc);
}

auto UnitLowerer::Contains(const slang::ast::ProceduralBlockSymbol& proc) const
    -> bool {
  const StaticConcurrentAssertion found = StaticConcurrentAssertionOf(proc);
  if (found.assertion == nullptr) {
    return true;
  }
  return EvaluatedInSimulation(*found.assertion, AssertionPolicy());
}

auto UnitLowerer::LookupProcessBinding(
    const slang::ast::ProceduralBlockSymbol& proc) const
    -> std::optional<hir::ProcessId> {
  const auto it = process_bindings_.find(&proc);
  if (it == process_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

auto UnitLowerer::LookupProceduralStatic(const slang::ast::Symbol& var) const
    -> std::optional<ProceduralStaticBinding> {
  const auto it = procedural_static_bindings_.find(&var);
  if (it == procedural_static_bindings_.end()) {
    return std::nullopt;
  }
  return it->second;
}

namespace {

// The subroutine body a scope member declares, or nothing when it declares
// none. A scope holds the subroutine itself where the source wrote the body
// inline, and holds a prototype where the source put the body outside the class
// (LRM 8.24) -- one declaration, reached two ways. Three members declare no
// body at all: a pure virtual method (LRM 8.21) is a signature and nothing
// else, and a DPI-C import (LRM 35.4) and a compiler-generated class built-in
// (the randomize family, LRM 18.6) are provided rather than lowered from
// source.
auto DeclaredSubroutineBody(const slang::ast::Symbol& member)
    -> const slang::ast::SubroutineSymbol* {
  if (member.kind == slang::ast::SymbolKind::MethodPrototype) {
    const auto& proto = member.as<slang::ast::MethodPrototypeSymbol>();
    return proto.flags.has(slang::ast::MethodFlags::Pure)
               ? nullptr
               : proto.getSubroutine();
  }
  if (member.kind != slang::ast::SymbolKind::Subroutine) {
    return nullptr;
  }
  const auto& sub = member.as<slang::ast::SubroutineSymbol>();
  const bool provided = sub.flags.has(slang::ast::MethodFlags::DPIImport) ||
                        sub.flags.has(slang::ast::MethodFlags::BuiltIn);
  return provided ? nullptr : &sub;
}

// What one member of a declaration scope gives this pass: the symbol whose
// procedural-scope identity is minted now, and the scope the walk continues
// into. The two are independent questions and all four answers occur -- a
// subroutine gives both, an unnamed block only a scope to walk, a procedural
// block only an identity, and a variable or a type neither -- so they are
// answered as data rather than decided inside a branch that then acts.
struct ScopeContribution {
  const slang::ast::Symbol* minted = nullptr;
  const slang::ast::Scope* walked = nullptr;
  std::span<const slang::ast::StatementBlockSymbol* const> blocks = {};
};

auto ContributionOf(const slang::ast::Symbol& member, const UnitLowerer& owner)
    -> ScopeContribution {
  if (const auto* body = DeclaredSubroutineBody(member); body != nullptr) {
    return {.minted = body, .walked = body};
  }
  // A constraint block is a scope the source named (LRM 18.5), and the loop
  // variables of a `foreach` in it live in it (LRM 18.5.7).
  if (member.kind == slang::ast::SymbolKind::ConstraintBlock) {
    const auto& block = member.as<slang::ast::ConstraintBlockSymbol>();
    return {.minted = &member, .walked = &block};
  }
  if (member.kind == slang::ast::SymbolKind::ProceduralBlock) {
    const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
    // A statement label creates a named block around the statement it labels
    // (LRM 16.3), and the front end lists that block beside the process rather
    // than inside it. Whether the design has one is the process's answer, so
    // the process contributes its blocks and a process the design does not
    // contain carries them out of reach with it.
    if (!owner.Contains(proc)) {
      return {};
    }
    return {.minted = &member, .blocks = proc.getBlocks()};
  }
  if (member.kind == slang::ast::SymbolKind::StatementBlock) {
    const auto& block = member.as<slang::ast::StatementBlockSymbol>();
    // Only a block the source named can be named from elsewhere, so only one
    // needs an identity before the bodies lower. A block slang recorded for its
    // own reasons -- the implicit scope a pattern arm's bindings live in, the
    // one a loop's control variables live in -- is reached only by the walk
    // that lowers it, which mints its identity there.
    return {.minted = block.name.empty() ? nullptr : &member, .walked = &block};
  }
  // An anonymous program opens no scope, so its subroutines are this scope's
  // (LRM 24.6).
  if (member.kind == slang::ast::SymbolKind::AnonymousProgram) {
    return {.walked = &member.as<slang::ast::AnonymousProgramSymbol>()};
  }
  return {};
}

}  // namespace

void DeclareProceduralScopes(
    const slang::ast::Scope& declaring, const slang::ast::Scope& walked,
    UnitLowerer& owner,
    base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>& scopes) {
  // A process's own blocks are listed beside it here as well, so the pass
  // gathers them first and then leaves them to the process that answers for
  // them; reaching one from this loop would mint an identity for a block whose
  // process the design may not contain, and nothing would go on to fill it.
  std::unordered_set<const slang::ast::Symbol*> process_blocks;
  for (const auto& member : walked.members()) {
    if (member.kind != slang::ast::SymbolKind::ProceduralBlock) {
      continue;
    }
    for (const auto* block :
         member.as<slang::ast::ProceduralBlockSymbol>().getBlocks()) {
      process_blocks.insert(block);
    }
  }
  for (const auto& member : walked.members()) {
    // Slang lists a base class's members in the derived class's member list
    // too (LRM 8.13 inheritance), and this pass mints for one declaration
    // scope: a member declared elsewhere is that declaration's own to mint,
    // and minting a second identity for it would leave one of them unfilled.
    // So would minting one for a member another unit holds.
    if (member.getParentScope() != &walked ||
        process_blocks.contains(&member) || !owner.Owns(member)) {
      continue;
    }
    const ScopeContribution contribution = ContributionOf(member, owner);
    if (contribution.minted != nullptr) {
      owner.DeclareProceduralScope(
          *contribution.minted, declaring, scopes.Declare());
    }
    for (const auto* block : contribution.blocks) {
      const ScopeContribution owned = ContributionOf(*block, owner);
      if (owned.minted != nullptr) {
        owner.DeclareProceduralScope(
            *owned.minted, declaring, scopes.Declare());
      }
      DeclareProceduralScopes(declaring, *block, owner, scopes);
    }
    if (contribution.walked != nullptr) {
      DeclareProceduralScopes(declaring, *contribution.walked, owner, scopes);
    }
  }
}

}  // namespace lyra::lowering::ast_to_hir
