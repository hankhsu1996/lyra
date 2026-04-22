#include "lyra/lowering/ast_to_hir/generate.hpp"

#include <cstddef>
#include <cstdint>
#include <format>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <utility>
#include <vector>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ParameterSymbols.h>
#include <slang/ast/symbols/PortSymbols.h>
#include <slang/ast/symbols/SubroutineSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/scope_types.hpp"
#include "lyra/common/source_span.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/hir/declaration.hpp"
#include "lyra/hir/generate.hpp"
#include "lyra/lowering/ast_to_hir/constant.hpp"
#include "lyra/lowering/ast_to_hir/context.hpp"
#include "lyra/lowering/ast_to_hir/source_utils.hpp"
#include "lyra/lowering/ast_to_hir/symbol_registrar.hpp"
#include "lyra/lowering/ast_to_hir/type.hpp"

namespace lyra::lowering::ast_to_hir {

// Legacy flat walk for Phase 0 per-instance SymbolId registration and
// per-spec-group behavioral-input preparation. Walks transparently
// through generate scopes; ownership modeling is the region tree's
// concern, not this walk's.
void CollectScopeMembers(
    const slang::ast::Scope& scope, SymbolRegistrar& registrar,
    CollectedMembers& out) {
  for (const auto& member : scope.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::Variable: {
        const auto& var = member.as<slang::ast::VariableSymbol>();
        out.variables.push_back(&var);
        break;
      }
      case slang::ast::SymbolKind::Net: {
        const auto& net = member.as<slang::ast::NetSymbol>();
        out.nets.push_back(&net);
        break;
      }
      case slang::ast::SymbolKind::Parameter: {
        const auto& param = member.as<slang::ast::ParameterSymbol>();
        out.parameters.push_back(&param);
        break;
      }
      case slang::ast::SymbolKind::Subroutine: {
        const auto& sub = member.as<slang::ast::SubroutineSymbol>();
        if (sub.subroutineKind == slang::ast::SubroutineKind::Function) {
          out.functions.push_back(&sub);
        } else {
          out.tasks.push_back(&sub);
        }
        break;
      }
      case slang::ast::SymbolKind::ProceduralBlock: {
        const auto& proc = member.as<slang::ast::ProceduralBlockSymbol>();
        out.processes.push_back(&proc);
        break;
      }
      case slang::ast::SymbolKind::ContinuousAssign: {
        const auto& ca = member.as<slang::ast::ContinuousAssignSymbol>();
        out.continuous_assigns.push_back(&ca);
        break;
      }
      case slang::ast::SymbolKind::GenerateBlock: {
        const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
        if (block.isUninstantiated) {
          break;
        }
        ScopeGuard guard(
            registrar, ScopeKind::kGenerate, std::string(block.name));
        CollectScopeMembers(block, registrar, out);
        break;
      }
      case slang::ast::SymbolKind::GenerateBlockArray: {
        const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
        for (size_t i = 0; i < array.entries.size(); ++i) {
          const auto* entry = array.entries[i];
          if (entry->isUninstantiated) {
            continue;
          }
          ScopeGuard guard(
              registrar, ScopeKind::kGenerate,
              std::format("{}[{}]", array.name, i));
          CollectScopeMembers(*entry, registrar, out);
        }
        break;
      }
      default:
        break;
    }
  }
}

namespace {

// Plain-child filter: same predicate used by LowerModuleBody's
// constructor synthesis so the region tree's kInstanceMember items
// and the synthesized kNewObject statements cover the same set.
auto IsPlainChildInstance(const slang::ast::InstanceSymbol& inst) -> bool {
  return inst.body.getPortList().empty();
}

void AllocateVariableInRegion(
    const slang::ast::VariableSymbol& var, hir::GenerateRegionId region_id,
    SymbolRegistrar& registrar, Context* ctx, hir::Arena& body_arena,
    std::unordered_map<SymbolId, hir::DeclRef, SymbolIdHash>& sym_to_decl) {
  SymbolId sym = registrar.Lookup(var);
  if (!sym) return;
  SourceSpan span = ctx->SpanOf(GetSourceRange(var));
  TypeId type = LowerType(var.getType(), span, ctx);
  if (!type) return;
  hir::VariableId vid = body_arena.AddVariable(
      hir::Variable{
          .name = std::string(var.name),
          .type = type,
          .span = span,
          .scope = registrar.CurrentScope(),
          .bridge_symbol = sym,
      });
  body_arena[region_id].items.push_back(
      hir::RegionItem{.kind = hir::RegionItemKind::kVariable, .payload = vid});
  sym_to_decl.emplace(sym, hir::DeclRef::FromVariable(vid));
}

void AllocateNetInRegion(
    const slang::ast::NetSymbol& net, hir::GenerateRegionId region_id,
    SymbolRegistrar& registrar, Context* ctx, hir::Arena& body_arena,
    std::unordered_map<SymbolId, hir::DeclRef, SymbolIdHash>& sym_to_decl) {
  SymbolId sym = registrar.Lookup(net);
  if (!sym) return;
  SourceSpan span = ctx->SpanOf(GetSourceRange(net));
  TypeId type = LowerType(net.getType(), span, ctx);
  if (!type) return;
  hir::NetId nid = body_arena.AddNet(
      hir::Net{
          .name = std::string(net.name),
          .type = type,
          .span = span,
          .scope = registrar.CurrentScope(),
          .bridge_symbol = sym,
      });
  body_arena[region_id].items.push_back(
      hir::RegionItem{.kind = hir::RegionItemKind::kNet, .payload = nid});
  sym_to_decl.emplace(sym, hir::DeclRef::FromNet(nid));
}

void AllocateParameterInRegion(
    const slang::ast::ParameterSymbol& param, hir::GenerateRegionId region_id,
    SymbolRegistrar& registrar, Context* ctx, hir::Arena& body_arena,
    std::unordered_map<SymbolId, hir::DeclRef, SymbolIdHash>& sym_to_decl) {
  SymbolId sym = registrar.Lookup(param);
  if (!sym) return;
  SourceSpan span = ctx->SpanOf(GetSourceRange(param));
  TypeId type = LowerType(param.getType(), span, ctx);
  if (!type) return;
  bool is_design_storage =
      (*ctx->symbol_table)[sym].storage_class == StorageClass::kDesignStorage;
  // init_value is read only for design-storage parameters (integral by
  // pipeline contract). Const-only parameters may carry non-integer
  // values (real, struct, unpacked); their init_value stays default.
  IntegralConstant init_value{};
  if (is_design_storage) {
    init_value = LowerSVIntToIntegralConstant(param.getValue().integer());
  }
  hir::ParameterId pid = body_arena.AddParameter(
      hir::Parameter{
          .name = std::string(param.name),
          .type = type,
          .span = span,
          .scope = registrar.CurrentScope(),
          .bridge_symbol = sym,
          .init_value = std::move(init_value),
          .is_design_storage = is_design_storage,
      });
  body_arena[region_id].items.push_back(
      hir::RegionItem{.kind = hir::RegionItemKind::kParameter, .payload = pid});
  sym_to_decl.emplace(sym, hir::DeclRef::FromParameter(pid));
}

void AllocateInstanceMemberInRegion(
    const slang::ast::InstanceSymbol& inst, hir::GenerateRegionId region_id,
    SymbolRegistrar& registrar, Context* ctx, hir::Arena& body_arena,
    std::unordered_map<SymbolId, hir::DeclRef, SymbolIdHash>& sym_to_decl) {
  SymbolId sym = registrar.Lookup(inst);
  if (!sym) return;
  SourceSpan span = ctx->SpanOf(GetSourceRange(inst));
  hir::InstanceMemberId mid = body_arena.AddInstanceMember(
      hir::InstanceMember{
          .name = std::string(inst.name),
          .type = ctx->ObjectHandleType(),
          .span = span,
          .scope = registrar.CurrentScope(),
          .bridge_symbol = sym,
      });
  body_arena[region_id].items.push_back(
      hir::RegionItem{
          .kind = hir::RegionItemKind::kInstanceMember, .payload = mid});
  sym_to_decl.emplace(sym, hir::DeclRef::FromInstanceMember(mid));
}

// Allocate a region for a single generate block. Uninstantiated blocks
// get a structurally-present but empty region; instantiated blocks
// recurse through BuildRegionTreeFromScope under a kGenerate scope
// guard.
auto BuildRegionForGenerateBlock(
    const slang::ast::GenerateBlockSymbol& block, const std::string& label,
    SymbolRegistrar& registrar, Context* body_ctx, hir::Arena& body_arena,
    std::unordered_map<SymbolId, hir::DeclRef, SymbolIdHash>& sym_to_decl,
    hir::GenerateRegionId parent_region_id) -> hir::GenerateRegionId {
  if (block.isUninstantiated) {
    return body_arena.AddGenerateRegion(
        hir::GenerateRegion{
            .items = {},
            .scope = registrar.CurrentScope(),
            .label = label,
            .parent_region = parent_region_id,
            .is_root = false,
        });
  }
  ScopeGuard guard(registrar, ScopeKind::kGenerate, label);
  hir::GenerateRegionId region_id = body_arena.AddGenerateRegion(
      hir::GenerateRegion{
          .items = {},
          .scope = registrar.CurrentScope(),
          .label = label,
          .parent_region = parent_region_id,
          .is_root = false,
      });
  BuildRegionTreeFromScope(
      block, region_id, registrar, body_ctx, body_arena, sym_to_decl);
  return region_id;
}

// One ConditionalGenerateConstruct per if/case-generate construct.
// `group` is the set of slang GenerateBlockSymbols sharing
// constructIndex at this scope, in source order. All branches (taken
// or not) contribute a region; nothing is dropped.
void EmitConditionalConstruct(
    const std::vector<const slang::ast::GenerateBlockSymbol*>& group,
    hir::GenerateRegionId parent_region_id, SymbolRegistrar& registrar,
    Context* body_ctx, hir::Arena& body_arena,
    std::unordered_map<SymbolId, hir::DeclRef, SymbolIdHash>& sym_to_decl) {
  std::vector<hir::GenerateRegionId> alternatives;
  alternatives.reserve(group.size());
  for (const auto* b : group) {
    alternatives.push_back(BuildRegionForGenerateBlock(
        *b, std::string(b->name), registrar, body_ctx, body_arena, sym_to_decl,
        parent_region_id));
  }
  hir::GenerateConstructId cid = body_arena.AddGenerateConstruct(
      hir::GenerateConstruct{
          .kind = hir::GenerateConstructKind::kConditional,
          .payload =
              hir::ConditionalGenerateConstruct{
                  .alternatives = std::move(alternatives)},
      });
  body_arena[parent_region_id].items.push_back(
      hir::RegionItem{
          .kind = hir::RegionItemKind::kGenerateConstruct, .payload = cid});
}

// One LoopGenerateConstruct per generate-for. Each live iteration
// carries its elaborated genvar value from slang's arrayIndex.
void EmitLoopConstruct(
    const slang::ast::GenerateBlockArraySymbol& array,
    hir::GenerateRegionId parent_region_id, SymbolRegistrar& registrar,
    Context* body_ctx, hir::Arena& body_arena,
    std::unordered_map<SymbolId, hir::DeclRef, SymbolIdHash>& sym_to_decl) {
  hir::LoopGenerateConstruct loop;
  for (size_t i = 0; i < array.entries.size(); ++i) {
    const auto* entry = array.entries[i];
    if (entry->isUninstantiated) continue;
    std::string iter_label = std::format("{}[{}]", array.name, i);
    hir::GenerateRegionId iter_region = BuildRegionForGenerateBlock(
        *entry, iter_label, registrar, body_ctx, body_arena, sym_to_decl,
        parent_region_id);
    if (entry->arrayIndex == nullptr) {
      throw common::InternalError(
          "EmitLoopConstruct",
          "elaborated generate-for iteration missing arrayIndex");
    }
    auto val = entry->arrayIndex->as<int64_t>();
    if (!val) {
      throw common::InternalError(
          "EmitLoopConstruct",
          "generate-for iteration arrayIndex does not fit in int64_t");
    }
    loop.iterations.push_back(
        hir::LoopGenerateIteration{
            .region = iter_region,
            .elaborated_index = *val,
            .label = std::move(iter_label),
        });
  }
  hir::GenerateConstructId cid = body_arena.AddGenerateConstruct(
      hir::GenerateConstruct{
          .kind = hir::GenerateConstructKind::kLoop,
          .payload = std::move(loop),
      });
  body_arena[parent_region_id].items.push_back(
      hir::RegionItem{
          .kind = hir::RegionItemKind::kGenerateConstruct, .payload = cid});
}

// Group sibling GenerateBlockSymbols at one scope by constructIndex,
// preserving source order within each group. `constructIndex` is
// scope-local in slang, which is exactly our iteration scope here.
auto GroupGenerateBlocksInScope(const slang::ast::Scope& scope)
    -> std::unordered_map<
        uint32_t, std::vector<const slang::ast::GenerateBlockSymbol*>> {
  std::unordered_map<
      uint32_t, std::vector<const slang::ast::GenerateBlockSymbol*>>
      groups;
  for (const auto& member : scope.members()) {
    if (member.kind != slang::ast::SymbolKind::GenerateBlock) continue;
    const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
    groups[block.constructIndex].push_back(&block);
  }
  return groups;
}

}  // namespace

// Region tree for one scope. Allocates body-owned declaration objects
// at their encounter point, emits one GenerateConstruct per grouped
// if/case construct, one per loop array. Declarations below
// (processes, functions, tasks, continuous assigns) are not
// RegionItems in this cut.
void BuildRegionTreeFromScope(
    const slang::ast::Scope& scope, hir::GenerateRegionId parent_region_id,
    SymbolRegistrar& registrar, Context* body_ctx, hir::Arena& body_arena,
    std::unordered_map<SymbolId, hir::DeclRef, SymbolIdHash>& sym_to_decl) {
  auto groups = GroupGenerateBlocksInScope(scope);
  std::unordered_set<uint32_t> emitted_groups;

  for (const auto& member : scope.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::Variable:
        AllocateVariableInRegion(
            member.as<slang::ast::VariableSymbol>(), parent_region_id,
            registrar, body_ctx, body_arena, sym_to_decl);
        break;
      case slang::ast::SymbolKind::Net:
        AllocateNetInRegion(
            member.as<slang::ast::NetSymbol>(), parent_region_id, registrar,
            body_ctx, body_arena, sym_to_decl);
        break;
      case slang::ast::SymbolKind::Parameter:
        AllocateParameterInRegion(
            member.as<slang::ast::ParameterSymbol>(), parent_region_id,
            registrar, body_ctx, body_arena, sym_to_decl);
        break;
      case slang::ast::SymbolKind::Instance: {
        const auto& inst = member.as<slang::ast::InstanceSymbol>();
        if (IsPlainChildInstance(inst)) {
          AllocateInstanceMemberInRegion(
              inst, parent_region_id, registrar, body_ctx, body_arena,
              sym_to_decl);
        }
        break;
      }
      case slang::ast::SymbolKind::GenerateBlock: {
        const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
        if (!emitted_groups.insert(block.constructIndex).second) break;
        EmitConditionalConstruct(
            groups.at(block.constructIndex), parent_region_id, registrar,
            body_ctx, body_arena, sym_to_decl);
        break;
      }
      case slang::ast::SymbolKind::GenerateBlockArray:
        EmitLoopConstruct(
            member.as<slang::ast::GenerateBlockArraySymbol>(), parent_region_id,
            registrar, body_ctx, body_arena, sym_to_decl);
        break;
      default:
        break;
    }
  }
}

}  // namespace lyra::lowering::ast_to_hir
