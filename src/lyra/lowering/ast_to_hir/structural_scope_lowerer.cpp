#include "lyra/lowering/ast_to_hir/structural_scope_lowerer.hpp"

#include <cstdint>
#include <expected>
#include <optional>
#include <string>
#include <utility>

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
#include "lyra/diag/diagnostic.hpp"
#include "lyra/hir/continuous_assign.hpp"
#include "lyra/hir/expr.hpp"
#include "lyra/hir/expr_builders.hpp"
#include "lyra/hir/structural_data_object.hpp"
#include "lyra/hir/structural_scope.hpp"
#include "lyra/hir/subroutine.hpp"
#include "lyra/lowering/ast_to_hir/instance_array_shape.hpp"
#include "lyra/lowering/ast_to_hir/process_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/specialization_name.hpp"
#include "lyra/lowering/ast_to_hir/subroutine_decl.hpp"
#include "lyra/lowering/ast_to_hir/time_resolution.hpp"
#include "lyra/lowering/ast_to_hir/unit_lowerer.hpp"
#include "lyra/lowering/ast_to_hir/walk_frame.hpp"

namespace lyra::lowering::ast_to_hir {

StructuralScopeLowerer::StructuralScopeLowerer(
    UnitLowerer& unit_lowerer, const slang::ast::Scope& slang_scope)
    : owner_(&unit_lowerer),
      slang_scope_(&slang_scope),
      frame_(unit_lowerer.LookupScopeFrame(slang_scope)) {
  // A `ref` / `const ref` port's internal variable aliases the connected
  // variable rather than owning storage (LRM 23.3.3.2); record which of this
  // body's variables those are so their members lower to a reference type. Only
  // a module body declares ports; a generate-block scope has none.
  const auto* body =
      slang_scope.asSymbol().as_if<slang::ast::InstanceBodySymbol>();
  if (body == nullptr) {
    return;
  }
  for (const auto* port_symbol : body->getPortList()) {
    const auto* port = port_symbol->as_if<slang::ast::PortSymbol>();
    if (port == nullptr ||
        port->direction != slang::ast::ArgumentDirection::Ref ||
        port->internalSymbol == nullptr) {
      continue;
    }
    const auto* internal_var =
        port->internalSymbol->as_if<slang::ast::VariableSymbol>();
    const bool is_const =
        internal_var != nullptr &&
        internal_var->flags.has(slang::ast::VariableFlags::Const);
    ref_port_internals_.emplace(
        port->internalSymbol, is_const ? hir::ReferenceBinding::kConstRef
                                       : hir::ReferenceBinding::kRef);
  }
}

auto StructuralScopeLowerer::ReferenceBindingFor(
    const slang::ast::VariableSymbol& var) const
    -> std::optional<hir::ReferenceBinding> {
  const auto it = ref_port_internals_.find(&var);
  if (it == ref_port_internals_.end()) {
    return std::nullopt;
  }
  return it->second;
}

auto StructuralScopeLowerer::Run(WalkFrame parent_frame)
    -> diag::Result<hir::StructuralScope> {
  hir::StructuralScope scope;
  const WalkFrame frame = parent_frame.WithStructuralFrame(
      frame_, slang_scope_, &scope, &scope.exprs);
  scope.time_resolution = ResolveTimeResolution(slang_scope_->getTimeScale());

  // Forward-declare every subroutine's binding before lowering any body so a
  // call resolves regardless of source order: direct self-recursion, mutual
  // recursion, and forward references (LRM 13.4.2). Ids are sequential in
  // source order and match the index `PopulateSubroutineMember` will write to.
  std::uint32_t reserved_subroutine_id = 0;
  std::uint32_t reserved_foreign_import_id = 0;
  for (const auto& member : slang_scope_->members()) {
    if (member.kind != slang::ast::SymbolKind::Subroutine) continue;
    const auto& sub = member.as<slang::ast::SubroutineSymbol>();
    if (sub.flags.has(slang::ast::MethodFlags::DPIImport)) {
      owner_->MapForeignImportBinding(
          sub, frame_, hir::ForeignImportId{reserved_foreign_import_id++});
    } else {
      owner_->MapSubroutineBinding(
          sub, frame_, hir::StructuralSubroutineId{reserved_subroutine_id++});
    }
  }

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

auto StructuralScopeLowerer::PopulateMember(
    const slang::ast::Symbol& member, WalkFrame frame) -> diag::Result<void> {
  switch (member.kind) {
    case slang::ast::SymbolKind::TypeAlias:
      return PopulateTypeAliasMember(
          member.as<slang::ast::TypeAliasType>(), frame);
    case slang::ast::SymbolKind::Variable:
      return PopulateVariableMember(
          member.as<slang::ast::VariableSymbol>(), frame);
    case slang::ast::SymbolKind::Net:
      return PopulateNetMember(member.as<slang::ast::NetSymbol>(), frame);
    case slang::ast::SymbolKind::Subroutine: {
      const auto& sub = member.as<slang::ast::SubroutineSymbol>();
      if (sub.flags.has(slang::ast::MethodFlags::DPIImport)) {
        return PopulateForeignImportMember(sub, frame);
      }
      return PopulateSubroutineMember(sub, frame);
    }
    case slang::ast::SymbolKind::ProceduralBlock:
      return PopulateProceduralBlockMember(
          member.as<slang::ast::ProceduralBlockSymbol>(), frame);
    case slang::ast::SymbolKind::ContinuousAssign:
      return PopulateContinuousAssignMember(
          member.as<slang::ast::ContinuousAssignSymbol>(), frame);
    case slang::ast::SymbolKind::GenerateBlockArray:
      return PopulateGenerateArrayMember(
          member.as<slang::ast::GenerateBlockArraySymbol>(), frame);
    case slang::ast::SymbolKind::GenerateBlock:
      return PopulateGenerateBlockMember(
          member.as<slang::ast::GenerateBlockSymbol>(), frame);
    default:
      return {};
  }
}

auto StructuralScopeLowerer::PopulateTypeAliasMember(
    const slang::ast::TypeAliasType& alias, WalkFrame frame)
    -> diag::Result<void> {
  const auto& mapper = owner_->SourceMapper();
  auto target_or = owner_->InternType(
      alias.targetType.getType(), mapper.PointSpanOf(alias.location));
  if (!target_or) return std::unexpected(std::move(target_or.error()));
  frame.current_structural_scope->AddTypeAlias(
      hir::TypeAliasDecl{
          .name = std::string{alias.name}, .target = *target_or});
  return {};
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
  if (std::holds_alternative<hir::VoidType>(
          owner_->Unit().types.Get(*type_id_or).data)) {
    throw InternalError(
        "StructuralScopeLowerer::PopulateVariableMember: variable declaration "
        "produced "
        "void type");
  }
  std::optional<hir::ExprId> initializer_id;
  if (const auto* init = var.getInitializer(); init != nullptr) {
    auto init_or = LowerExpr(*init, frame);
    if (!init_or) return std::unexpected(std::move(init_or.error()));
    initializer_id = frame.Exprs().Add(*std::move(init_or));
  }
  const hir::StructuralDataObjectId local =
      frame.current_structural_scope->structural_data_objects.Add(
          hir::StructuralDataObjectDecl{
              .name = std::string{var.name},
              .type = *type_id_or,
              .kind = hir::StructuralVariableDecl{
                  .initializer = initializer_id,
                  .reference = ReferenceBindingFor(var)}});
  owner_->MapStructuralDataObjectBinding(var, frame_, local, *type_id_or);
  return {};
}

auto StructuralScopeLowerer::PopulateNetMember(
    const slang::ast::NetSymbol& net, WalkFrame frame) -> diag::Result<void> {
  const auto& mapper = owner_->SourceMapper();
  const auto span = mapper.PointSpanOf(net.location);
  auto type_id_or = owner_->InternType(net.getType(), span);
  if (!type_id_or) return std::unexpected(std::move(type_id_or.error()));
  hir::NetType net_type{};
  switch (net.netType.netKind) {
    case slang::ast::NetType::Wire:
      net_type = hir::NetType::kWire;
      break;
    case slang::ast::NetType::Tri:
      net_type = hir::NetType::kTri;
      break;
    default:
      return diag::Fail(
          span, diag::DiagCode::kUnsupportedTypeKind,
          "this net type is not yet supported");
  }
  const hir::StructuralDataObjectId local =
      frame.current_structural_scope->structural_data_objects.Add(
          hir::StructuralDataObjectDecl{
              .name = std::string{net.name},
              .type = *type_id_or,
              .kind = hir::StructuralNetDecl{.net_type = net_type}});
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
    frame.current_structural_scope->continuous_assigns.Add(
        hir::ContinuousAssign{
            .span = span,
            .lhs = lhs_id,
            .rhs = rhs_id,
            .sensitivity_list =
                owner_->TranslateSensitivityReads(reads, frame)});
  }
  return {};
}

auto StructuralScopeLowerer::PopulateSubroutineMember(
    const slang::ast::SubroutineSymbol& sym, WalkFrame frame)
    -> diag::Result<void> {
  auto decl_or = LowerSubroutineDecl(*owner_, sym, frame);
  if (!decl_or) return std::unexpected(std::move(decl_or.error()));

  const auto binding = owner_->LookupSubroutineBinding(sym);
  if (!binding.has_value() ||
      binding->subroutine_id.value !=
          static_cast<std::uint32_t>(
              frame.current_structural_scope->structural_subroutines.size())) {
    throw InternalError(
        "StructuralScopeLowerer::PopulateSubroutineMember: subroutine added "
        "out of "
        "reserved order; ReserveSubroutineBinding must run first in the same "
        "order");
  }
  frame.current_structural_scope->structural_subroutines.Add(
      *std::move(decl_or));

  // slang associates an `export "DPI-C"` (LRM 35.5) with its subroutine only
  // through the design-wide export list, so the exported subroutine reaches
  // this ordinary body path and is additionally recorded here to drive a
  // foreign-linkage wrapper.
  const auto& compilation = sym.getParentScope()->getCompilation();
  for (const auto& dpi : compilation.getDPIExports()) {
    if (dpi.subroutine != &sym) continue;
    auto export_or = LowerForeignExport(*owner_, sym, dpi.cIdentifier);
    if (!export_or) return std::unexpected(std::move(export_or.error()));
    frame.current_structural_scope->foreign_exports.push_back(
        *std::move(export_or));
    break;
  }
  return {};
}

auto StructuralScopeLowerer::PopulateForeignImportMember(
    const slang::ast::SubroutineSymbol& sym, WalkFrame frame)
    -> diag::Result<void> {
  auto decl_or = LowerForeignImport(*owner_, sym);
  if (!decl_or) return std::unexpected(std::move(decl_or.error()));

  const auto binding = owner_->LookupForeignImportBinding(sym);
  if (!binding.has_value() ||
      binding->import_id.value !=
          static_cast<std::uint32_t>(
              frame.current_structural_scope->foreign_imports.size())) {
    throw InternalError(
        "StructuralScopeLowerer::PopulateForeignImportMember: DPI import added "
        "out of reserved order; the reserve pass must run first in the same "
        "order");
  }
  frame.current_structural_scope->foreign_imports.Add(*std::move(decl_or));
  return {};
}

auto StructuralScopeLowerer::PopulateProceduralBlockMember(
    const slang::ast::ProceduralBlockSymbol& proc, WalkFrame frame)
    -> diag::Result<void> {
  // A concurrent assertion declared as a module item becomes a process whose
  // entire body is the assertion. When assertions are disabled the process
  // contributes no behavior, so it is dropped at the source rather than emptied
  // -- an always block with no body and no timing control would be a zero-delay
  // infinite loop.
  if (owner_->DisableAssertions() && proc.isFromAssertion) {
    return {};
  }
  ProcessLowerer proc_lowerer(*owner_, proc);
  auto p = proc_lowerer.Run(proc, frame);
  if (!p) return std::unexpected(std::move(p.error()));
  frame.current_structural_scope->processes.Add(*std::move(p));
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
  auto g = BuildResolvedGenerateFromArray(array, frame);
  if (!g) return std::unexpected(std::move(g.error()));
  frame.current_structural_scope->generates.Add(*std::move(g));
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
  auto g = BuildResolvedGenerateFromBlock(block, frame);
  if (!g) return std::unexpected(std::move(g.error()));
  frame.current_structural_scope->generates.Add(*std::move(g));
  return {};
}

auto StructuralScopeLowerer::PopulateInstanceMember(
    const slang::ast::InstanceSymbol& inst, WalkFrame frame)
    -> diag::Result<void> {
  frame.current_structural_scope->instance_members.Add(
      hir::InstanceMemberDecl{
          .instance_name = std::string{inst.name},
          .target_unit = SpecializationName(inst),
          .array_dims = {}});
  return {};
}

auto StructuralScopeLowerer::PopulateInstanceArrayMember(
    const slang::ast::InstanceArraySymbol& array, WalkFrame frame)
    -> diag::Result<void> {
  auto shape = ResolveInstanceArrayShape(array);
  if (!shape) {
    return {};
  }
  frame.current_structural_scope->instance_members.Add(
      hir::InstanceMemberDecl{
          .instance_name = std::string{array.name},
          .target_unit = SpecializationName(*shape->leaf),
          .array_dims = std::move(shape->dims)});
  return {};
}

}  // namespace lyra::lowering::ast_to_hir
