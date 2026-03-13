#include "lyra/lowering/ast_to_hir/generate_repertoire.hpp"

#include <format>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/VariableSymbols.h>

namespace lyra::lowering::ast_to_hir {

namespace {

// Traverses a slang scope, recording each compile-relevant artifact with
// the current availability path. Generate constructs extend the path;
// scopes that are not constructor-time selection points are transparent
// (traversed but not modeled -- their artifacts inherit the enclosing
// availability).
void VisitScope(
    const slang::ast::Scope& scope,
    const std::vector<GenerateSelection>& current_avail,
    ArtifactInventory& out) {
  for (const auto& member : scope.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::Variable:
      case slang::ast::SymbolKind::Net:
        out.decls.push_back({ArtifactKind::kDecl, &member, current_avail});
        break;

      case slang::ast::SymbolKind::ProceduralBlock:
        out.processes.push_back(
            {ArtifactKind::kProcess, &member, current_avail});
        break;

      case slang::ast::SymbolKind::Instance:
        out.child_instances.push_back(
            {ArtifactKind::kInstance, &member, current_avail});
        break;

      case slang::ast::SymbolKind::ContinuousAssign:
        out.continuous_assigns.push_back(
            {ArtifactKind::kContinuousAssign, &member, current_avail});
        break;

      case slang::ast::SymbolKind::GenerateBlock: {
        // Intentionally does NOT check block.isUninstantiated.
        // slang's body.members() includes both active and inactive generate
        // branches. We walk ALL branches so the type store is definition-
        // scoped (covers all constructor alternatives). The specialization
        // fingerprint depends on this full-definition view.
        const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
        auto next = current_avail;
        next.push_back(
            {SelectionKind::kBranch, &member, block.constructIndex, 0});
        VisitScope(block, next, out);
        break;
      }

      case slang::ast::SymbolKind::GenerateBlockArray: {
        const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
        for (uint32_t i = 0; i < array.entries.size(); ++i) {
          auto next = current_avail;
          next.push_back(
              {SelectionKind::kArrayEntry, array.entries[i],
               array.constructIndex, i});
          VisitScope(*array.entries[i], next, out);
        }
        break;
      }

      case slang::ast::SymbolKind::StatementBlock: {
        const auto& block = member.as<slang::ast::StatementBlockSymbol>();
        VisitScope(block, current_avail, out);
        break;
      }

      default:
        break;
    }
  }
}

void DumpSelection(const GenerateSelection& sel, std::string& out) {
  switch (sel.kind) {
    case SelectionKind::kBranch:
      out += std::format("branch({}@{})", sel.block->name, sel.construct_index);
      break;
    case SelectionKind::kArrayEntry: {
      // Entry blocks have empty names; the display name lives on the
      // parent GenerateBlockArraySymbol.
      const auto& array_name = sel.block->getParentScope()->asSymbol().name;
      out += std::format("entry({}[{}])", array_name, sel.entry_index);
      break;
    }
  }
}

void DumpHandles(
    std::string_view kind_label, const std::vector<ArtifactHandle>& handles,
    std::string& out) {
  for (const auto& h : handles) {
    out += std::format("  {} {}", kind_label, h.symbol->name);
    if (h.availability.empty()) {
      out += " avail=[]\n";
    } else {
      out += " avail=[";
      for (size_t i = 0; i < h.availability.size(); ++i) {
        if (i > 0) {
          out += ", ";
        }
        DumpSelection(h.availability[i], out);
      }
      out += "]\n";
    }
  }
}

}  // namespace

auto BuildArtifactInventory(const slang::ast::InstanceBodySymbol& body)
    -> ArtifactInventory {
  ArtifactInventory inventory;
  VisitScope(body, {}, inventory);
  return inventory;
}

auto DumpArtifactInventory(const ArtifactInventory& inventory) -> std::string {
  std::string result;
  DumpHandles("decl", inventory.decls, result);
  DumpHandles("proc", inventory.processes, result);
  DumpHandles("inst", inventory.child_instances, result);
  DumpHandles("cont", inventory.continuous_assigns, result);
  return result;
}

}  // namespace lyra::lowering::ast_to_hir
