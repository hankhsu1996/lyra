#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

#include <algorithm>
#include <array>
#include <cstdint>
#include <expected>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/NetType.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/diag/failure_context.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/constant_value.hpp"
#include "lyra/lowering/ast_to_hir/event_handle.hpp"
#include "lyra/lowering/ast_to_hir/generate_construct.hpp"
#include "lyra/lowering/ast_to_hir/hierarchy_override.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/net_overlay.hpp"
#include "lyra/lowering/ast_to_hir/net_type.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/reads.hpp"
#include "lyra/lowering/ast_to_hir/statement/assertions.hpp"
#include "lyra/lowering/ast_to_hir/strength.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/time_resolution.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"
#include "lyra/profiling/time_trace.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Where one parameter an instance takes at construction gets its value: the
// expression its instantiation wrote (LRM 23.10.2), or none where the value
// was given elsewhere -- a defparam, a configuration (LRM 23.10.1, 33.4.3) --
// and is then handed as the constant it settled to.
struct ArgumentSource {
  const slang::ast::ParameterSymbol* parameter;
  const slang::ast::Expression* written;
};

// How one instance is built: the unit it is an instance of, and where each
// parameter it takes at construction gets its value, in the unit's own order.
struct InstanceConstruction {
  std::string unit;
  std::vector<ArgumentSource> arguments;
};

auto ConstructionOf(
    const slang::ast::InstanceSymbol& instance,
    const SpecializationPolicy& policy) -> InstanceConstruction {
  InstanceConstruction built{
      .unit = SpecializationName(instance, policy), .arguments = {}};
  for (const slang::ast::ParameterSymbol* param :
       policy.SuppliedParametersOf(instance)) {
    built.arguments.push_back(
        ArgumentSource{
            .parameter = param,
            .written = ValueWrittenAtInstantiation(instance, *param)});
  }
  return built;
}

// Whether two elements of one instantiation are built alike. The
// instantiation writes one assignment for every element (LRM 23.3.2), so two
// written arguments are the same expression, and a value given elsewhere is
// told apart by the constant it settled to.
auto BuiltAlike(const InstanceConstruction& a, const InstanceConstruction& b)
    -> bool {
  return a.unit == b.unit &&
         std::ranges::equal(
             a.arguments, b.arguments,
             [](const ArgumentSource& x, const ArgumentSource& y) {
               if ((x.written == nullptr) != (y.written == nullptr)) {
                 return false;
               }
               return x.written != nullptr ||
                      ValueIdentity(x.parameter->getValue()) ==
                          ValueIdentity(y.parameter->getValue());
             });
}

// The arguments `child`'s constructor is passed, built as `built` says. The
// front end binds the instantiation's own assignment where it is written (LRM
// 23.10.2), so that is lowered here, against this scope's names. A value given
// elsewhere was written in another scope and is part of what this unit is, so
// it is handed as the constant it settled to.
auto LowerConstructorArguments(
    StructuralScopeLowerer& lowerer, const InstanceConstruction& built,
    const slang::ast::InstanceSymbol& child, WalkFrame frame)
    -> diag::Result<std::vector<hir::Expr>> {
  UnitLowerer& owner = lowerer.Owner();
  std::vector<hir::Expr> arguments;
  for (const ArgumentSource& source : built.arguments) {
    if (source.written != nullptr) {
      auto lowered = lowerer.LowerExpr(*source.written, frame);
      if (!lowered) return std::unexpected(std::move(lowered.error()));
      arguments.push_back(*std::move(lowered));
      continue;
    }
    const auto span = owner.SourceMapper().PointSpanOf(child.location);
    auto type = owner.InternType(source.parameter->getType(), span);
    if (!type) return std::unexpected(std::move(type.error()));
    auto value = MakeConstantValueExpr(
        owner.Unit(), frame, source.parameter->getValue(), *type, span);
    if (!value) return std::unexpected(std::move(value.error()));
    arguments.push_back(*std::move(value));
  }
  return arguments;
}

}  // namespace

// The declaration of the child objects `elements` are, in row-major order of
// their positions; `dims` carries the element counts of an array, a scalar
// instance being the empty case rather than a shape of its own. What each is
// comes off this unit's record of the class its unit's instances are, never
// from the unit's own name.
//
// Every element is described by how it is built, and elements built alike are
// one alternative. Each alternative's arguments are lowered once and stored
// among this scope's expressions, which is where the construction reads them.
auto StructuralScopeLowerer::BuildInstanceMember(
    std::string_view instance_name,
    std::span<const slang::ast::InstanceSymbol* const> elements,
    std::vector<std::uint32_t> dims, WalkFrame frame)
    -> diag::Result<hir::InstanceMemberDecl> {
  hir::InstanceMemberDecl member{
      .instance_name = std::string{instance_name},
      .array_dims = std::move(dims),
      .alternatives = {},
      .taken = {}};
  member.taken.reserve(elements.size());
  std::vector<InstanceConstruction> distinct;
  for (const slang::ast::InstanceSymbol* element : elements) {
    InstanceConstruction built =
        ConstructionOf(*element, owner_->Specialization());
    const auto known =
        std::ranges::find_if(distinct, [&](const InstanceConstruction& other) {
          return BuiltAlike(other, built);
        });
    if (known != distinct.end()) {
      member.taken.push_back(
          static_cast<std::uint32_t>(known - distinct.begin()));
      continue;
    }
    auto arguments = LowerConstructorArguments(*this, built, *element, frame);
    if (!arguments) return std::unexpected(std::move(arguments.error()));
    std::vector<hir::ExprId> stored;
    stored.reserve(arguments->size());
    for (hir::Expr& value : *arguments) {
      stored.push_back(frame.Exprs().Add(std::move(value)));
    }
    member.taken.push_back(
        static_cast<std::uint32_t>(member.alternatives.size()));
    member.alternatives.push_back(
        hir::InstanceAlternative{
            .scope_class = owner_->ExternalScopeClassOf(built.unit),
            .arguments = std::move(stored)});
    distinct.push_back(std::move(built));
  }
  return member;
}

auto StructuralScopeLowerer::Run(WalkFrame parent_frame)
    -> diag::Result<hir::StructuralScope> {
  const profiling::TimeTraceScope span("lower scope", [this] {
    return slang_scope_->asSymbol().getHierarchicalPath();
  });
  hir::StructuralScope scope;
  // Filling a declaration is defining the identity a peer may already hold,
  // which is why this scope takes the pools rather than growing its own.
  ScopeDeclarations declarations = owner_->TakeScopeDeclarations(*slang_scope_);
  scope.structural_data_objects =
      std::move(declarations.structural_data_objects);
  scope.structural_subroutines = std::move(declarations.structural_subroutines);
  scope.processes = std::move(declarations.processes);
  scope.generates = std::move(declarations.generates);
  scope.instance_members = std::move(declarations.instance_members);
  scope.replicated_classes = owner_->TakeReplicatedClasses(*slang_scope_);
  const WalkFrame frame =
      parent_frame.WithStructuralFrame(frame_, slang_scope_, &scope)
          .WithProceduralScopeOwner(slang_scope_, &scope.procedural_scopes);
  scope.time_resolution = ResolveTimeResolution(slang_scope_->getTimeScale());

  // Declared before anything else so that a name reaching one during the walk
  // below resolves to the declaration rather than folding to what one
  // elaboration gave it.
  auto parameters = DeclareDifferingParameters(scope, frame);
  if (!parameters) return std::unexpected(std::move(parameters.error()));

  // A `disable` names a block or task by static identity (LRM 9.6.2), so it can
  // name one whose body lowers later, or lives in another process entirely.
  DeclareProceduralScopes(
      *slang_scope_, *slang_scope_, *owner_, scope.procedural_scopes);

  // Structural members (variables, instances, generates, subroutine bodies)
  // are lowered before the members that name them (processes, continuous
  // assigns, and the aliases that state which of this scope's nets are one
  // physical net), so such a member resolves a reference to a declaration it
  // textually precedes -- declarations are scope-wide (LRM 27).
  for (const auto& member : slang_scope_->members()) {
    if (!owner_->Owns(member) ||
        member.kind == slang::ast::SymbolKind::ProceduralBlock ||
        member.kind == slang::ast::SymbolKind::ContinuousAssign ||
        member.kind == slang::ast::SymbolKind::NetAlias) {
      continue;
    }
    auto r = PopulateMember(member, frame);
    if (!r) return std::unexpected(std::move(r.error()));
  }

  // The classes this scope replicates, lowered where a process of the scope
  // is: every declaration a class body may name is bound by now, and the
  // references it records against this scope are recorded before the scope
  // takes them.
  auto classes = owner_->PopulateClassBodiesReplicatedBy(*slang_scope_);
  if (!classes) return std::unexpected(std::move(classes.error()));

  for (const auto& member : slang_scope_->members()) {
    if (!owner_->Owns(member) ||
        (member.kind != slang::ast::SymbolKind::ProceduralBlock &&
         member.kind != slang::ast::SymbolKind::ContinuousAssign &&
         member.kind != slang::ast::SymbolKind::NetAlias)) {
      continue;
    }
    auto r = PopulateMember(member, frame);
    if (!r) return std::unexpected(std::move(r.error()));
  }

  // A variable port connection is an implied continuous assignment
  // (LRM 23.3.3), synthesized after every variable and instance binding
  // exists so its source and child-side endpoint resolve regardless of source
  // order.
  auto pc = PopulatePortConnections(*slang_scope_, frame);
  if (!pc) return std::unexpected(std::move(pc.error()));

  scope.routes = owner_->TakeRoutesForFrame(frame_);
  scope.published = owner_->TakePublication(*slang_scope_);
  return scope;
}

// The parameters of this scope whose values differ between the objects built
// from it, each a declaration of the scope. One supplied at construction -- a
// unit's overridden parameter, or the index a loop's block was built at -- is a
// value the construction fills, and the scope declares them in the order a
// construction supplies them. One computed from those holds the expression the
// source wrote for it. Every other parameter's value is fixed by the scope's
// specialization, so reading that value costs no second artifact.
//
// An expression naming the index resolves to the declaration the construction
// fills, so every block a loop counts out states the same thing whatever index
// it was built at.
auto StructuralScopeLowerer::DeclareDifferingParameters(
    hir::StructuralScope& scope, WalkFrame frame) -> diag::Result<void> {
  for (const auto& member : slang_scope_->members()) {
    const auto* parameter = member.as_if<slang::ast::ParameterSymbol>();
    if (parameter == nullptr) continue;
    switch (owner_->ValueSourceOf(*parameter)) {
      case ParameterValueSource::kFixedBySpecialization:
        break;
      case ParameterValueSource::kSuppliedAtConstruction: {
        auto declared = DeclareSettledValue(
            scope, *parameter, hir::StructuralConstructionValueDecl{});
        if (!declared) return std::unexpected(std::move(declared.error()));
        break;
      }
      case ParameterValueSource::kComputedAtConstruction: {
        // Only a parameter a declaration writes is computed, so there is
        // always the expression it was written with.
        const slang::ast::Expression* initializer = parameter->getInitializer();
        if (initializer == nullptr) {
          throw InternalError(
              "StructuralScopeLowerer::DeclareDifferingParameters: a parameter "
              "its declaration writes states no expression");
        }
        auto lowered = LowerExpr(*initializer, frame);
        if (!lowered) return std::unexpected(std::move(lowered.error()));
        auto declared = DeclareSettledValue(
            scope, *parameter,
            hir::StructuralParameterDecl{
                .initializer = frame.Exprs().Add(*std::move(lowered))});
        if (!declared) return std::unexpected(std::move(declared.error()));
        break;
      }
    }
  }
  return {};
}

// Declares a value the scope settles before its walk begins, and binds the
// symbol to it so every name reaching the symbol resolves to the declaration
// rather than to what one elaboration gave it.
auto StructuralScopeLowerer::DeclareSettledValue(
    hir::StructuralScope& scope, const slang::ast::ValueSymbol& value,
    hir::StructuralDataObjectKind kind) -> diag::Result<void> {
  auto type_or = owner_->InternType(
      value.getType(), owner_->SourceMapper().PointSpanOf(value.location));
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  const hir::StructuralDataObjectId declared =
      scope.structural_data_objects.Add(
          hir::StructuralDataObjectDecl{
              .name = std::string{value.name},
              .type = *type_or,
              .kind = std::move(kind)});
  owner_->MapStructuralDataObjectBinding(value, frame_, declared);
  return {};
}

// Total over slang's symbol kinds with no `default`: a member that carries
// behavior must not vanish into a catch-all -- the failure mode that let a
// checker's entire body disappear without a word. Listing every kind forces a
// deliberate classification of each -- lowered here, deliberately nothing, or
// reported -- and a kind added by a future slang release fails to compile
// until it is classified.
auto StructuralScopeLowerer::PopulateMember(
    const slang::ast::Symbol& member, WalkFrame frame) -> diag::Result<void> {
  using slang::ast::SymbolKind;
  const diag::FailureContext at(
      owner_->SourceMapper().PointSpanOf(member.location));
  switch (member.kind) {
    case SymbolKind::Variable:
      return PopulateVariableMember(
          member.as<slang::ast::VariableSymbol>(), frame);
    case SymbolKind::Net:
      return PopulateNetMember(member.as<slang::ast::NetSymbol>(), frame);
    case SymbolKind::Subroutine: {
      const auto& sub = member.as<slang::ast::SubroutineSymbol>();
      if (sub.flags.has(slang::ast::MethodFlags::DPIImport)) {
        return PopulateForeignImportMember(sub);
      }
      return PopulateSubroutineMember(sub, frame);
    }
    case SymbolKind::Modport:
      return PopulateModportMember(
          member.as<slang::ast::ModportSymbol>(), frame);
    case SymbolKind::ProceduralBlock:
      return PopulateProceduralBlockMember(
          member.as<slang::ast::ProceduralBlockSymbol>(), frame);
    case SymbolKind::ContinuousAssign:
      return PopulateContinuousAssignMember(
          member.as<slang::ast::ContinuousAssignSymbol>(), frame);
    case SymbolKind::NetAlias:
      return PopulateNetAliasMember(
          member.as<slang::ast::NetAliasSymbol>(), frame);
    case SymbolKind::GenerateBlockArray:
      return PopulateGenerateArrayMember(
          member.as<slang::ast::GenerateBlockArraySymbol>(), frame);
    case SymbolKind::GenerateBlock:
      return PopulateGenerateBlockMember(
          member.as<slang::ast::GenerateBlockSymbol>(), frame);

    case SymbolKind::Instance:
      return PopulateInstanceMember(
          member.as<slang::ast::InstanceSymbol>(), frame);
    case SymbolKind::InstanceArray:
      return PopulateInstanceArrayMember(
          member.as<slang::ast::InstanceArraySymbol>(), frame);

    // A named sequence or property declares no storage and no behavior: what
    // an instance of one stands for is its body with the actual arguments
    // substituted, and the front end hands over that expansion at the instance
    // (LRM 16.8, 16.12). So the declaration itself needs nothing here, and its
    // ports and local variables are members of its own scope rather than of
    // this one.
    case SymbolKind::Sequence:
    case SymbolKind::Property:
      return {};

    // LRM 17 checkers observe the design and never drive it, so a design with
    // them removed behaves identically and the policy may drop them whole.
    // Without it they are reported, so no design is quietly reduced to one that
    // checks nothing.
    case SymbolKind::AssertionPort:
    case SymbolKind::LocalAssertionVar:
    case SymbolKind::Checker:
    case SymbolKind::CheckerInstance:
    case SymbolKind::CheckerInstanceBody:
      if (support::ElidesAssertions(owner_->AssertionPolicy())) {
        return {};
      }
      return diag::Fail(
          owner_->SourceMapper().PointSpanOf(member.location),
          diag::DiagCode::kUnsupportedStructuralMember,
          "assertion and checker declarations are not supported; pass "
          "--assertions skip to elide them");

    // Behavior the design depends on: skipping one would hand the backend a
    // different design than the source describes.
    case SymbolKind::PrimitiveInstance:
    case SymbolKind::RandSeqProduction:
    case SymbolKind::AnonymousProgram:
    case SymbolKind::UninstantiatedDef:
      return diag::Fail(
          owner_->SourceMapper().PointSpanOf(member.location),
          diag::DiagCode::kUnsupportedStructuralMember,
          "this declaration form is not supported yet");

    // Naming a type creates no structure. Whatever declares an object of it
    // interns the type at its own declaration.
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
    case SymbolKind::ClassType:
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
    case SymbolKind::GenericClassDef:
      return {};

    // Nothing to build at the walk. A parameter that varies per block or per
    // instance is already a declaration, settled before this walk began so that
    // the members below reach it; any other is part of the scope's own
    // specialization, which makes reading its value cost nothing. A genvar and
    // an import contribute to the members that read them and nothing of their
    // own.
    case SymbolKind::Parameter:
    case SymbolKind::Specparam:
    case SymbolKind::DefParam:
    case SymbolKind::Genvar:
    case SymbolKind::ExplicitImport:
    case SymbolKind::WildcardImport:
    case SymbolKind::Attribute:
    case SymbolKind::ConfigBlock:
    case SymbolKind::ElabSystemTask:
      return {};

    // An interface port is its own declaration: no separate internal name
    // stands behind it the way one stands behind a data port, so the member
    // the scope holds for it is built here (LRM 25.3).
    case SymbolKind::InterfacePort:
      return PopulateInterfacePortMember(
          member.as<slang::ast::InterfacePortSymbol>(), frame);

    case SymbolKind::Port:
      return PopulatePortMember(member.as<slang::ast::PortSymbol>(), frame);

    // The scope's own boundary and its enclosing containers, reached as
    // members but describing where this scope sits rather than what it does.
    case SymbolKind::MultiPort:
    case SymbolKind::ModportPort:
    case SymbolKind::ModportClocking:
    case SymbolKind::InstanceBody:
    case SymbolKind::Package:
    case SymbolKind::CompilationUnit:
    case SymbolKind::Root:
    case SymbolKind::Definition:
      return {};

    // Owned by a different scope -- a subroutine's arguments, a class's
    // fields, a covergroup's bins -- and lowered with that scope if at all.
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
      return {};

    // These annotate or define; no behavior arises at the point of
    // declaration.
    case SymbolKind::Primitive:
    case SymbolKind::PrimitivePort:
    case SymbolKind::SpecifyBlock:
    case SymbolKind::TimingPath:
    case SymbolKind::PulseStyle:
    case SymbolKind::SystemTimingCheck:
      return {};

    // Declarations whose effect happens where they are used, not where they
    // are written, so the use site is what has to support them.
    case SymbolKind::ClockingBlock:
    case SymbolKind::ClockVar:
    case SymbolKind::LetDecl:
      return {};
  }
  throw InternalError(
      "StructuralScopeLowerer::PopulateMember: unknown slang "
      "SymbolKind");
}

auto StructuralScopeLowerer::PopulateVariableMember(
    const slang::ast::VariableSymbol& var, WalkFrame frame)
    -> diag::Result<void> {
  const auto& mapper = owner_->SourceMapper();
  if (var.lifetime != slang::ast::VariableLifetime::Static) {
    return diag::Fail(
        mapper.PointSpanOf(var.location),
        diag::DiagCode::kUnsupportedNonStaticVariableLifetime,
        "only static variables are supported");
  }
  auto type_id_or =
      owner_->InternType(var.getType(), mapper.PointSpanOf(var.location));
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  // Slang rejects `void` in any variable-declaration position before
  // elaboration, so a void-typed VariableSymbol can only reach this path
  // via a slang/Lyra integration bug.
  if (owner_->Unit().types.Get(*type_id_or).Is<hir::VoidType>()) {
    throw InternalError(
        "StructuralScopeLowerer::PopulateVariableMember: variable declaration "
        "produced "
        "void type");
  }
  hir::StructuralDataObjectKind kind = hir::StructuralVariableDecl{};
  if (const auto binding = owner_->ReferenceBindingOf(var)) {
    kind = hir::StructuralReferenceDecl{.binding = *binding};
  } else if (const auto* init = var.getInitializer(); init != nullptr) {
    if (auto refused = RefuseGivingAnEventAValue(
            var.getType(), mapper.PointSpanOf(var.location));
        !refused) {
      return std::unexpected(std::move(refused.error()));
    }
    auto init_or = LowerExpr(*init, frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    kind = hir::StructuralVariableDecl{
        .initializer = frame.Exprs().Add(*std::move(init_or))};
  }
  frame.current_structural_scope->structural_data_objects.Define(
      owner_->ReservedDataObject(var), hir::StructuralDataObjectDecl{
                                           .name = std::string{var.name},
                                           .type = *type_id_or,
                                           .kind = std::move(kind)});
  return {};
}

auto StructuralScopeLowerer::PopulateInterfacePortMember(
    const slang::ast::InterfacePortSymbol& port, WalkFrame frame)
    -> diag::Result<void> {
  // How many instances the port stands for is the range it declares (LRM 25.3),
  // outermost first; a port standing for one declares none, which is the same
  // answer with nothing in it.
  const auto declared = port.getDeclaredRange();
  if (!declared.has_value()) {
    throw InternalError(
        "PopulateInterfacePortMember: the port's range evaluated where the "
        "signature was published, so it evaluates here too");
  }
  std::vector<std::uint32_t> array_dims;
  array_dims.reserve(declared->size());
  for (const slang::ConstantRange& dim : *declared) {
    array_dims.push_back(dim.width());
  }
  const hir::InterfacePortId local =
      frame.current_structural_scope->interface_ports.Add(
          hir::InterfacePortDecl{
              .name = std::string{port.name},
              .array_dims = std::move(array_dims)});
  owner_->MapInterfacePortBinding(port, frame_, local);
  return {};
}

auto StructuralScopeLowerer::PopulateNetMember(
    const slang::ast::NetSymbol& net, WalkFrame frame) -> diag::Result<void> {
  const auto& mapper = owner_->SourceMapper();
  const auto span = mapper.PointSpanOf(net.location);
  auto type_id_or = owner_->InternType(net.getType(), span);
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  auto net_type = TranslateNetType(net.netType, span);
  if (!net_type) return std::unexpected(std::move(net_type.error()));

  // A delay on a net declaration is the time a value change takes to reach it
  // (LRM 10.3.3), and on a `trireg` its third component is how long a stored
  // charge lasts (LRM 6.6.4.2). Neither is modelled, and dropping one answers
  // at the wrong time rather than refusing.
  if (net.getDelay() != nullptr) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedTypeKind,
        "a delay on a net declaration (LRM 10.3.3) is not yet supported");
  }

  frame.current_structural_scope->structural_data_objects.Define(
      owner_->ReservedDataObject(net),
      hir::StructuralDataObjectDecl{
          .name = std::string{net.name},
          .type = *type_id_or,
          .kind = hir::StructuralNetDecl{
              .net_type = *net_type,
              .charge_strength =
                  TranslateChargeStrength(net.getChargeStrength())}});

  // A net-declaration assignment (`wire w = expr;`, LRM 6.5) is a single
  // continuous driver of the net. slang carries it as the net's initializer
  // rather than a separate continuous-assignment item, so synthesize the
  // equivalent continuous assignment here; it lands on the same driver path an
  // explicit `assign` does. The sensitivity is the read set of the driving
  // expression, analyzed with the net as the containing symbol.
  if (const auto* init = net.getInitializer(); init != nullptr) {
    auto strength = TranslateDriveStrength(net.getDriveStrength(), span);
    if (!strength) return std::unexpected(std::move(strength.error()));
    auto rhs_or = LowerExpr(*init, frame);
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    // The net is this scope's own, so the driver reaches it over the route
    // that climbs no edges.
    auto net_ref =
        owner_->MakeRoutedValueRef(net, frame_, ScopeRoute::Enclosing({}));
    if (!net_ref) return std::unexpected(std::move(net_ref.error()));
    const hir::ExprId lhs_id =
        frame.Exprs().Add(hir::MakeRefExpr(*net_ref, *type_id_or, span));
    const hir::ExprId rhs_id = frame.Exprs().Add(*std::move(rhs_or));
    const auto& reads = owner_->Sensitivity().AnalyzeReads(*init, net);
    auto sensitivity = owner_->SensitivityEntriesOf(*this, reads, frame);
    if (!sensitivity) return std::unexpected(std::move(sensitivity.error()));
    frame.current_structural_scope->continuous_assigns.Add(
        hir::ContinuousAssign{
            .span = span,
            .lhs = lhs_id,
            .rhs = rhs_id,
            .strength = *strength,
            .sensitivity_list = *std::move(sensitivity)});
  }
  return {};
}

// The subroutine this scope evaluates `holder`'s expression in, for another
// unit that asks for the value: the expression names declarations that would
// mean nothing where that unit's signature is read, so what crosses is the call
// and the expression is lowered here, in the scope that wrote it. It is one
// return of the expression and takes no formals.
auto StructuralScopeLowerer::DefineEvaluator(
    const slang::ast::Symbol& holder, std::string name,
    const slang::ast::Expression& expr, WalkFrame frame) -> diag::Result<void> {
  const auto span = owner_->SourceMapper().PointSpanOf(holder.location);
  auto result_type = owner_->InternType(*expr.type, span);
  if (!result_type) return std::unexpected(std::move(result_type.error()));

  hir::ProceduralBody body;
  OpenProceduralScope root{
      frame.ProceduralScopes().Declare(),
      hir::ProceduralScopeKind::kSubroutineRoot, name};
  const hir::ProceduralVarId result_var = body.procedural_vars.Declare();
  body.procedural_vars.Define(
      result_var,
      hir::ProceduralVarDecl{.name = std::nullopt, .type = *result_type});
  root.declarations.push_back(result_var);
  ProcessLowerer lowerer(*owner_, holder);
  const WalkFrame body_frame =
      frame.WithProceduralBody(&body).WithOpenScope(&root);
  auto value = lowerer.LowerExpr(expr, body_frame);
  if (!value) return std::unexpected(std::move(value.error()));
  // A wait on the value asks the evaluator what it reads (LRM 9.4.2), and an
  // expression a scope evaluates reads that scope's own storage.
  auto cells = CellsOfWaitedExpression(lowerer, body_frame, expr);
  if (!cells) return std::unexpected(std::move(cells.error()));
  hir::Reads reads;
  reads.leaves.assign(cells->begin(), cells->end());
  const hir::StmtId root_stmt = body.stmts.Add(
      hir::Stmt{
          .label = std::nullopt,
          .data = hir::ReturnStmt{.value = body.exprs.Add(*std::move(value))},
          .span = span});
  body.root_scope = frame.SealScope(std::move(root));
  frame.current_structural_scope->structural_subroutines.Define(
      owner_->EvaluatorOf(holder), hir::SubroutineDecl{
                                       .name = std::move(name),
                                       .kind = hir::SubroutineKind::kFunction,
                                       .result_type = *result_type,
                                       .params = {},
                                       .result_var = result_var,
                                       .body = std::move(body),
                                       .root_stmt = root_stmt,
                                       .is_virtual = false,
                                       .is_prototype = false,
                                       .is_static = false,
                                       .overrides = std::nullopt,
                                       .reads = std::move(reads)});
  return {};
}

// A name a view offers only for reading (LRM 25.5.4): nothing bounds it to an
// lvalue, so a module written against the view asks this interface for its
// value. Every other name a view offers designates storage the referrer
// reaches, so it has nothing to build here.
auto StructuralScopeLowerer::PopulateModportMember(
    const slang::ast::ModportSymbol& modport, WalkFrame frame)
    -> diag::Result<void> {
  for (const auto& item : modport.members()) {
    const auto* port = item.as_if<slang::ast::ModportPortSymbol>();
    if (port == nullptr || !ViewDefinesTheName(*port)) continue;
    if (port->direction != slang::ast::ArgumentDirection::In) continue;
    const auto* connection = port->getConnectionExpr();
    if (connection == nullptr) {
      throw InternalError(
          "PopulateModportMember: a name the view defines is the expression it "
          "was written with");
    }
    auto defined = DefineEvaluator(
        *port, ModportReadName(modport.name, port->name), *connection, frame);
    if (!defined) return std::unexpected(std::move(defined.error()));
  }
  return {};
}

// A port's default (LRM 23.2.2.4) is evaluated in this scope, not in the
// instantiator's, so an instance leaving the port unconnected asks this unit
// for it. A port with no default has nothing to build here.
auto StructuralScopeLowerer::PopulatePortMember(
    const slang::ast::PortSymbol& port, WalkFrame frame) -> diag::Result<void> {
  const slang::ast::Expression* initializer = port.getInitializer();
  if (initializer == nullptr) return {};
  return DefineEvaluator(port, PortDefaultName(port.name), *initializer, frame);
}

auto StructuralScopeLowerer::PopulateSubroutineMember(
    const slang::ast::SubroutineSymbol& sym, WalkFrame frame)
    -> diag::Result<void> {
  auto decl_or = LowerSubroutineDecl(*owner_, sym, frame);
  if (!decl_or) return std::unexpected(std::move(decl_or.error()));

  const auto binding = owner_->LookupSubroutineBinding(sym);
  if (!binding.has_value()) {
    throw InternalError(
        "StructuralScopeLowerer::PopulateSubroutineMember: the subroutine was "
        "not minted by the declaration pass");
  }
  frame.current_structural_scope->structural_subroutines.Define(
      binding->subroutine_id, *std::move(decl_or));

  // An `export "DPI-C"` (LRM 35.5) names a subroutine of its own scope, so the
  // exported subroutine reaches this ordinary body path and the export is
  // additionally recorded here to drive a foreign-linkage entry.
  if (const auto foreign_name = owner_->ForeignExportName(sym)) {
    auto export_or =
        LowerForeignExport(*owner_, sym, binding->subroutine_id, *foreign_name);
    if (!export_or) return std::unexpected(std::move(export_or.error()));
    frame.current_structural_scope->foreign_exports.push_back(
        *std::move(export_or));
  }
  return {};
}

auto StructuralScopeLowerer::PopulateForeignImportMember(
    const slang::ast::SubroutineSymbol& sym) -> diag::Result<void> {
  // The declaration is classified here even when nothing in this unit calls it,
  // so a signature outside the DPI-C type mapping (LRM 35.5.6) is reported
  // against the declaration that wrote it rather than against a call site far
  // away, or nowhere at all.
  auto id_or = owner_->EnsureForeignImport(sym);
  if (!id_or) return std::unexpected(std::move(id_or.error()));
  return {};
}

auto StructuralScopeLowerer::PopulateProceduralBlockMember(
    const slang::ast::ProceduralBlockSymbol& proc, WalkFrame frame)
    -> diag::Result<void> {
  if (!owner_->Contains(proc)) {
    return {};
  }
  if (const StaticConcurrentAssertion found = StaticConcurrentAssertionOf(proc);
      found.assertion != nullptr) {
    ProcessLowerer assertion_lowerer(*owner_, proc);
    auto decl = assertion_lowerer.RunConcurrentAssertion(
        proc, *found.assertion, found.named_block, frame);
    if (!decl) return std::unexpected(std::move(decl.error()));
    frame.current_structural_scope->concurrent_assertions.Add(*std::move(decl));
    return {};
  }
  ProcessLowerer proc_lowerer(*owner_, proc);
  auto p = proc_lowerer.Run(proc, frame);
  if (!p) return std::unexpected(std::move(p.error()));
  const auto reserved = owner_->LookupProcessBinding(proc);
  if (!reserved.has_value()) {
    throw InternalError(
        "StructuralScopeLowerer::PopulateProceduralBlockMember: the process "
        "was not minted by the declaration pass");
  }
  frame.current_structural_scope->processes.Define(*reserved, *std::move(p));
  return {};
}

auto StructuralScopeLowerer::PopulateContinuousAssignMember(
    const slang::ast::ContinuousAssignSymbol& sym, WalkFrame frame)
    -> diag::Result<void> {
  auto ca = LowerContinuousAssign(sym, frame);
  if (!ca) return std::unexpected(std::move(ca.error()));
  frame.current_structural_scope->continuous_assigns.Add(*std::move(ca));
  return {};
}

// An alias states that the bits of the signals it lists are the same physical
// nets (LRM 10.11). Each member is one side of the overlay, and the list is one
// statement about all of them.
//
// What the standard demands of the members is decided over the elaborated
// design and reported there: one net type across the list, sides of equal
// width, no variable and no hierarchical reference, and no pair stated twice.
auto StructuralScopeLowerer::PopulateNetAliasMember(
    const slang::ast::NetAliasSymbol& alias, WalkFrame frame)
    -> diag::Result<void> {
  const diag::SourceSpan span =
      owner_->SourceMapper().PointSpanOf(alias.location);
  std::vector<hir::NetSide> sides;
  for (const slang::ast::Expression* member : alias.getNetReferences()) {
    auto side = NetPositionsOfLvalue(
        *this, slang_scope_->asSymbol(), *member, span,
        diag::DiagCode::kUnsupportedStructuralMember, frame);
    if (!side) return std::unexpected(std::move(side.error()));
    sides.push_back(*std::move(side));
  }
  frame.current_structural_scope->net_joins.push_back(
      hir::NetJoin{.span = span, .sides = std::move(sides)});
  return {};
}

auto StructuralScopeLowerer::PopulateGenerateArrayMember(
    const slang::ast::GenerateBlockArraySymbol& array, WalkFrame frame)
    -> diag::Result<void> {
  // A loop generate whose range is empty elaborates no iteration, so it has no
  // runtime object and takes no generate id.
  if (array.entries.empty()) {
    return {};
  }
  auto g = BuildGenerateFromArray(array, frame);
  if (!g) return std::unexpected(std::move(g.error()));
  frame.current_structural_scope->generates.Define(
      owner_->GenerateIdOf(*array.entries.front()), *std::move(g));
  return {};
}

auto StructuralScopeLowerer::PopulateGenerateBlockMember(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<void> {
  // A conditional generate is one construct however many alternatives it holds
  // (LRM 27.5), so it is built once, where the first of them stands; a block
  // no conditional produced is its own construct and the same rule reaches it.
  if (!OpensItsConstruct(block)) return {};
  auto g = BuildGenerateFromBlock(block, frame);
  if (!g) return std::unexpected(std::move(g.error()));
  frame.current_structural_scope->generates.Define(
      owner_->GenerateIdOf(block), *std::move(g));
  return {};
}

auto StructuralScopeLowerer::PopulateInstanceMember(
    const slang::ast::InstanceSymbol& inst, WalkFrame frame)
    -> diag::Result<void> {
  const std::array<const slang::ast::InstanceSymbol*, 1> one{&inst};
  auto member = BuildInstanceMember(inst.name, one, {}, frame);
  if (!member) return std::unexpected(std::move(member.error()));
  frame.current_structural_scope->instance_members.Define(
      owner_->InstanceMemberIdOf(inst), *std::move(member));
  return {};
}

auto StructuralScopeLowerer::PopulateInstanceArrayMember(
    const slang::ast::InstanceArraySymbol& array, WalkFrame frame)
    -> diag::Result<void> {
  auto shape = ResolveInstanceArrayShape(array);
  if (!shape) {
    return {};
  }
  // The member holds one object per element, so what it is built from is how
  // many each dimension has; which element a name reaches is a position.
  std::vector<std::uint32_t> counts;
  counts.reserve(shape->ranges.size());
  for (const slang::ConstantRange& dim : shape->ranges) {
    counts.push_back(dim.width());
  }
  auto member = BuildInstanceMember(
      array.name, shape->elements, std::move(counts), frame);
  if (!member) return std::unexpected(std::move(member.error()));
  frame.current_structural_scope->instance_members.Define(
      owner_->InstanceMemberIdOf(array), *std::move(member));
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
