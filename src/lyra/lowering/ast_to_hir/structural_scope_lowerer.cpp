#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

#include <cstdint>
#include <expected>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include <slang/ast/Compilation.h>
#include <slang/ast/Scope.h>
#include <slang/ast/SemanticFacts.h>
#include <slang/ast/Statement.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/AllTypes.h>
#include <slang/ast/types/NetType.h>

#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diag_code.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/net_type.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/time_resolution.hpp"
#include "lyra/lowering/ast_to_hir/unit_identity.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// The identity the compilation unit's declaration pass minted for an owned
// child, as the kind of child `Ref` names it. Reaching this with the wrong
// kind, or with none, means the walk that mints and the walk that fills
// disagree about what the member even is.
template <typename Ref>
auto ReservedOwnedChild(
    const UnitLowerer& owner, const slang::ast::Symbol& child,
    std::string_view what) -> const Ref& {
  const auto binding = owner.LookupOwnedChildBinding(child);
  const auto* reserved =
      binding.has_value() ? std::get_if<Ref>(&binding->child) : nullptr;
  if (reserved == nullptr) {
    throw InternalError(
        std::format("{} was not minted by the declaration pass", what));
  }
  return *reserved;
}

auto ReservedGenerate(const UnitLowerer& owner, const slang::ast::Symbol& child)
    -> hir::GenerateId {
  return ReservedOwnedChild<hir::GenerateChildRef>(
             owner, child, "generate block")
      .generate;
}

auto ReservedInstanceMember(
    const UnitLowerer& owner, const slang::ast::Symbol& child)
    -> hir::InstanceMemberId {
  return ReservedOwnedChild<hir::InstanceMemberId>(
      owner, child, "instance member");
}

// The declaration of a child object, built from whichever unit `leaf` is an
// instance of. What the child is comes off this unit's record of that unit's
// object, never from the unit's own name; `dims` carries the element counts of
// an array, a scalar instance being the empty case rather than a shape of its
// own.
auto BuildInstanceMember(
    UnitLowerer& owner, std::string_view instance_name,
    const slang::ast::InstanceSymbol& leaf, std::vector<std::uint32_t> dims)
    -> hir::InstanceMemberDecl {
  return hir::InstanceMemberDecl{
      .instance_name = std::string{instance_name},
      .object = owner.ExternalUnitObjectOf(SpecializationName(leaf)),
      .array_dims = std::move(dims)};
}

}  // namespace

auto StructuralScopeLowerer::Run(WalkFrame parent_frame)
    -> diag::Result<hir::StructuralScope> {
  hir::StructuralScope scope;
  // Filling a declaration is defining the identity a peer may already hold,
  // which is why this scope takes the pools rather than growing its own.
  ScopeDeclarations declarations = owner_->TakeScopeDeclarations(*slang_scope_);
  scope.structural_subroutines = std::move(declarations.structural_subroutines);
  scope.processes = std::move(declarations.processes);
  scope.generates = std::move(declarations.generates);
  scope.instance_members = std::move(declarations.instance_members);
  const WalkFrame frame =
      parent_frame.WithStructuralFrame(frame_, slang_scope_, &scope)
          .WithProceduralScopeOwner(&scope.procedural_scopes);
  scope.time_resolution = ResolveTimeResolution(slang_scope_->getTimeScale());

  // A `disable` names a block or task by static identity (LRM 9.6.2), so it can
  // name one whose body lowers later, or lives in another process entirely.
  DeclareProceduralScopes(*slang_scope_, *owner_, scope.procedural_scopes);

  // Instance member decls are built ahead of the port-connection synthesis
  // below, which reads them to wire each connection. The owned-child binding a
  // reference resolves through is established earlier still, by the whole-unit
  // declaration pass, so this population order is a decl-availability concern,
  // not a reference-resolution one.
  for (const auto& member : slang_scope_->members()) {
    if (member.kind == slang::ast::SymbolKind::Instance) {
      auto r = PopulateInstanceMember(
          member.as<slang::ast::InstanceSymbol>(), frame);
      if (!r) return std::unexpected(std::move(r.error()));
    } else if (member.kind == slang::ast::SymbolKind::InstanceArray) {
      auto r = PopulateInstanceArrayMember(
          member.as<slang::ast::InstanceArraySymbol>(), frame);
      if (!r) return std::unexpected(std::move(r.error()));
    }
  }

  // Structural members (variables, generates, subroutine bodies) are lowered
  // before behavioral ones (processes, continuous assigns), so a process or
  // continuous assign resolves a downward reference into a generate block it
  // textually precedes -- declarations are scope-wide (LRM 27), the same
  // reason instances are bound in the pre-pass above.
  for (const auto& member : slang_scope_->members()) {
    if (member.kind == slang::ast::SymbolKind::Instance ||
        member.kind == slang::ast::SymbolKind::InstanceArray ||
        member.kind == slang::ast::SymbolKind::ProceduralBlock ||
        member.kind == slang::ast::SymbolKind::ContinuousAssign) {
      continue;
    }
    auto r = PopulateMember(member, frame);
    if (!r) return std::unexpected(std::move(r.error()));
  }
  for (const auto& member : slang_scope_->members()) {
    if (member.kind != slang::ast::SymbolKind::ProceduralBlock &&
        member.kind != slang::ast::SymbolKind::ContinuousAssign) {
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

  for (auto& ref : owner_->TakeRoutedRefsForFrame(frame_)) {
    scope.routed_refs.Add(std::move(ref));
  }
  return scope;
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
    case SymbolKind::ProceduralBlock:
      return PopulateProceduralBlockMember(
          member.as<slang::ast::ProceduralBlockSymbol>(), frame);
    case SymbolKind::ContinuousAssign:
      return PopulateContinuousAssignMember(
          member.as<slang::ast::ContinuousAssignSymbol>(), frame);
    case SymbolKind::GenerateBlockArray:
      return PopulateGenerateArrayMember(
          member.as<slang::ast::GenerateBlockArraySymbol>(), frame);
    case SymbolKind::GenerateBlock:
      return PopulateGenerateBlockMember(
          member.as<slang::ast::GenerateBlockSymbol>(), frame);

    case SymbolKind::Instance:
    case SymbolKind::InstanceArray:
      throw InternalError(
          "StructuralScopeLowerer::PopulateMember: an instance reached the "
          "general member walk, which runs after instance declarations are "
          "already bound");

    // LRM 16 assertion declarations and LRM 17 checkers observe the design
    // and never drive it, so a design with them removed behaves identically
    // and the policy may drop them whole. Without it they are reported, so
    // no design is quietly reduced to one that checks nothing.
    case SymbolKind::Sequence:
    case SymbolKind::Property:
    case SymbolKind::AssertionPort:
    case SymbolKind::LocalAssertionVar:
    case SymbolKind::Checker:
    case SymbolKind::CheckerInstance:
    case SymbolKind::CheckerInstanceBody:
      if (owner_->DisableAssertions()) {
        return {};
      }
      return diag::Fail(
          owner_->SourceMapper().PointSpanOf(member.location),
          diag::DiagCode::kUnsupportedStructuralMember,
          "assertion and checker declarations are not supported; pass "
          "--disable-assertions to skip them");

    // Behavior the design depends on: skipping one would hand the backend a
    // different design than the source describes.
    case SymbolKind::PrimitiveInstance:
    case SymbolKind::NetAlias:
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

    // Resolved before lowering begins: what a parameter, a genvar or an
    // import contributes is already folded into the members that read it.
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

    // The scope's own boundary and its enclosing containers, reached as
    // members but describing where this scope sits rather than what it does.
    case SymbolKind::Port:
    case SymbolKind::MultiPort:
    case SymbolKind::Modport:
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
    auto init_or = LowerExpr(*init, frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    kind = hir::StructuralVariableDecl{
        .initializer = frame.Exprs().Add(*std::move(init_or))};
  }
  const hir::StructuralDataObjectId local =
      frame.current_structural_scope->structural_data_objects.Add(
          hir::StructuralDataObjectDecl{
              .name = std::string{var.name},
              .type = *type_id_or,
              .kind = std::move(kind)});
  owner_->MapStructuralDataObjectBinding(var, frame_, local, *type_id_or);
  return {};
}

auto StructuralScopeLowerer::PopulateInterfacePortMember(
    const slang::ast::InterfacePortSymbol& port, WalkFrame frame)
    -> diag::Result<void> {
  const hir::ExternalUnitObjectId object =
      owner_->ExternalUnitObjectOf(owner_->InterfaceUnitOf(port));
  const hir::InterfacePortId local =
      frame.current_structural_scope->interface_ports.Add(
          hir::InterfacePortDecl{
              .name = std::string{port.name}, .object = object});
  owner_->MapInterfacePortBinding(port, frame_, local, object);
  return {};
}

auto StructuralScopeLowerer::PopulateNetMember(
    const slang::ast::NetSymbol& net, WalkFrame frame) -> diag::Result<void> {
  const auto& mapper = owner_->SourceMapper();
  const auto span = mapper.PointSpanOf(net.location);
  auto type_id_or = owner_->InternType(net.getType(), span);
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  const auto net_type = TranslateNetType(net.netType);
  if (!net_type.has_value()) {
    return diag::Fail(
        span, diag::DiagCode::kUnsupportedTypeKind,
        "this net type is not yet supported");
  }
  const hir::StructuralDataObjectId local =
      frame.current_structural_scope->structural_data_objects.Add(
          hir::StructuralDataObjectDecl{
              .name = std::string{net.name},
              .type = *type_id_or,
              .kind = hir::StructuralNetDecl{.net_type = *net_type}});
  owner_->MapStructuralDataObjectBinding(net, frame_, local, *type_id_or);

  // A net-declaration assignment (`wire w = expr;`, LRM 6.5) is a single
  // continuous driver of the net. slang carries it as the net's initializer
  // rather than a separate continuous-assignment item, so synthesize the
  // equivalent continuous assignment here; it lands on the same driver path an
  // explicit `assign` does. The sensitivity is the read set of the driving
  // expression, analyzed with the net as the containing symbol.
  if (const auto* init = net.getInitializer(); init != nullptr) {
    auto rhs_or = LowerExpr(*init, frame);
    if (!rhs_or) return std::unexpected(std::move(rhs_or.error()));
    const hir::ExprId lhs_id = frame.Exprs().Add(
        hir::MakeRefExpr(
            hir::DirectMemberRef{.var = local}, *type_id_or, span));
    const hir::ExprId rhs_id = frame.Exprs().Add(*std::move(rhs_or));
    const auto& reads = owner_->Sensitivity().AnalyzeReads(*init, net);
    auto sensitivity = owner_->TranslateSensitivityReads(
        reads, frame, support::EventEdge::kAnyChange);
    if (!sensitivity) return std::unexpected(std::move(sensitivity.error()));
    frame.current_structural_scope->continuous_assigns.Add(
        hir::ContinuousAssign{
            .span = span,
            .lhs = lhs_id,
            .rhs = rhs_id,
            .sensitivity_list = *std::move(sensitivity)});
  }
  return {};
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
      ReservedGenerate(*owner_, *array.entries.front()), *std::move(g));
  return {};
}

auto StructuralScopeLowerer::PopulateGenerateBlockMember(
    const slang::ast::GenerateBlockSymbol& block, WalkFrame frame)
    -> diag::Result<void> {
  // Every generate block is resolved at elaboration: an `if` / `case` arm not
  // selected for this scope carries no runtime object (LRM 27.5), so only an
  // instantiated block is lowered, as its own concrete scope.
  if (block.isUninstantiated) {
    return {};
  }
  auto g = BuildGenerateFromBlock(block, frame);
  if (!g) return std::unexpected(std::move(g.error()));
  frame.current_structural_scope->generates.Define(
      ReservedGenerate(*owner_, block), *std::move(g));
  return {};
}

auto StructuralScopeLowerer::PopulateInstanceMember(
    const slang::ast::InstanceSymbol& inst, WalkFrame frame)
    -> diag::Result<void> {
  frame.current_structural_scope->instance_members.Define(
      ReservedInstanceMember(*owner_, inst),
      BuildInstanceMember(*owner_, inst.name, inst, {}));
  return {};
}

auto StructuralScopeLowerer::PopulateInstanceArrayMember(
    const slang::ast::InstanceArraySymbol& array, WalkFrame frame)
    -> diag::Result<void> {
  auto shape = ResolveInstanceArrayShape(array);
  if (!shape) {
    return {};
  }
  frame.current_structural_scope->instance_members.Define(
      ReservedInstanceMember(*owner_, array),
      BuildInstanceMember(
          *owner_, array.name, *shape->leaf, std::move(shape->dims)));
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
