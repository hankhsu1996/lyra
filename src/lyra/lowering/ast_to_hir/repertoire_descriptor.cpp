#include "lyra/lowering/ast_to_hir/repertoire_descriptor.hpp"

#include <algorithm>
#include <format>
#include <map>
#include <string>
#include <tuple>
#include <utility>

#include <slang/ast/Scope.h>
#include <slang/ast/Symbol.h>
#include <slang/ast/symbols/BlockSymbols.h>
#include <slang/ast/symbols/CompilationUnitSymbols.h>
#include <slang/ast/symbols/InstanceSymbols.h>
#include <slang/ast/symbols/MemberSymbols.h>
#include <slang/ast/symbols/ValueSymbol.h>
#include <slang/ast/symbols/VariableSymbols.h>
#include <slang/ast/types/Type.h>

#include "lyra/common/internal_error.hpp"
#include "lyra/lowering/ast_to_hir/compile_owned_type_desc.hpp"
#include "lyra/lowering/ast_to_hir/generate_repertoire.hpp"
#include "lyra/lowering/ast_to_hir/repertoire_descriptor_debug.hpp"

namespace lyra::lowering::ast_to_hir {

namespace {

// Lexicographic three-way comparison on coordinates.
auto CompareCoord(const RepertoireCoord& a, const RepertoireCoord& b) -> int {
  auto min_len = std::min(a.size(), b.size());
  for (size_t i = 0; i < min_len; ++i) {
    auto a_t = std::tie(a[i].kind, a[i].construct_index, a[i].alt_index);
    auto b_t = std::tie(b[i].kind, b[i].construct_index, b[i].alt_index);
    if (a_t < b_t) {
      return -1;
    }
    if (b_t < a_t) {
      return 1;
    }
  }
  if (a.size() < b.size()) {
    return -1;
  }
  if (a.size() > b.size()) {
    return 1;
  }
  return 0;
}

auto LowerProcessKind(slang::ast::ProceduralBlockKind kind) -> ProcessKindDesc {
  switch (kind) {
    case slang::ast::ProceduralBlockKind::Initial:
      return ProcessKindDesc::kInitial;
    case slang::ast::ProceduralBlockKind::Final:
      return ProcessKindDesc::kFinal;
    case slang::ast::ProceduralBlockKind::Always:
      return ProcessKindDesc::kAlways;
    case slang::ast::ProceduralBlockKind::AlwaysComb:
      return ProcessKindDesc::kAlwaysComb;
    case slang::ast::ProceduralBlockKind::AlwaysLatch:
      return ProcessKindDesc::kAlwaysLatch;
    case slang::ast::ProceduralBlockKind::AlwaysFF:
      return ProcessKindDesc::kAlwaysFF;
  }
  return ProcessKindDesc::kAlways;
}

auto ProcessKindLabel(ProcessKindDesc kind) -> std::string_view {
  switch (kind) {
    case ProcessKindDesc::kInitial:
      return "initial";
    case ProcessKindDesc::kFinal:
      return "final";
    case ProcessKindDesc::kAlways:
      return "always";
    case ProcessKindDesc::kAlwaysComb:
      return "always_comb";
    case ProcessKindDesc::kAlwaysLatch:
      return "always_latch";
    case ProcessKindDesc::kAlwaysFF:
      return "always_ff";
  }
  return "unknown";
}

// Branch ordinal assignment key: (parent coordinate prefix, construct_index).
// Siblings sharing a construct_index get ordinals in source order.
struct BranchOrdinalKey {
  RepertoireCoord parent_prefix;
  uint32_t construct_index;

  auto operator<(const BranchOrdinalKey& other) const -> bool {
    auto cmp = CompareCoord(parent_prefix, other.parent_prefix);
    if (cmp != 0) {
      return cmp < 0;
    }
    return construct_index < other.construct_index;
  }
};

// Maps each GenerateBlockSymbol to its assigned branch ordinal.
// Invariant: every kBranch selection in the M2c-2a inventory has a
// corresponding entry in this map, because BuildBranchOrdinals traverses
// the same scope structure as BuildArtifactInventory.
using BranchOrdinalMap =
    std::map<const slang::ast::GenerateBlockSymbol*, uint32_t>;

// Traverses the definition body to assign deterministic branch ordinals.
// For each (parent_prefix, construct_index) group, siblings get ordinals
// in slang member iteration order.
void BuildBranchOrdinals(
    const slang::ast::Scope& scope, const RepertoireCoord& parent_prefix,
    std::map<BranchOrdinalKey, uint32_t>& next_ordinal,
    BranchOrdinalMap& ordinal_map) {
  for (const auto& member : scope.members()) {
    switch (member.kind) {
      case slang::ast::SymbolKind::GenerateBlock: {
        const auto& block = member.as<slang::ast::GenerateBlockSymbol>();
        auto key = BranchOrdinalKey{parent_prefix, block.constructIndex};
        auto& next = next_ordinal[key];
        ordinal_map[&block] = next;
        uint32_t this_ordinal = next;
        ++next;

        auto child_prefix = parent_prefix;
        child_prefix.push_back(
            {SelectionStepKind::kBranch, block.constructIndex, this_ordinal});
        BuildBranchOrdinals(block, child_prefix, next_ordinal, ordinal_map);
        break;
      }

      case slang::ast::SymbolKind::GenerateBlockArray: {
        const auto& array = member.as<slang::ast::GenerateBlockArraySymbol>();
        for (uint32_t i = 0; i < array.entries.size(); ++i) {
          auto child_prefix = parent_prefix;
          child_prefix.push_back(
              {SelectionStepKind::kArrayEntry, array.constructIndex, i});
          BuildBranchOrdinals(
              *array.entries[i], child_prefix, next_ordinal, ordinal_map);
        }
        break;
      }

      case slang::ast::SymbolKind::StatementBlock: {
        const auto& block = member.as<slang::ast::StatementBlockSymbol>();
        BuildBranchOrdinals(block, parent_prefix, next_ordinal, ordinal_map);
        break;
      }

      default:
        break;
    }
  }
}

// Converts an extraction-layer availability path to a pointer-free coordinate.
// Invariant: every kBranch selection's block pointer must exist in ordinal_map.
// This holds because BuildBranchOrdinals and BuildArtifactInventory traverse
// the same scope structure.
auto LowerAvailability(
    const std::vector<GenerateSelection>& availability,
    const BranchOrdinalMap& ordinal_map) -> RepertoireCoord {
  RepertoireCoord coord;
  coord.reserve(availability.size());
  for (const auto& sel : availability) {
    switch (sel.kind) {
      case SelectionKind::kBranch: {
        const auto* block_sym =
            &sel.block->as<slang::ast::GenerateBlockSymbol>();
        auto it = ordinal_map.find(block_sym);
        if (it == ordinal_map.end()) {
          throw common::InternalError(
              "LowerAvailability",
              std::format(
                  "branch ordinal not found for kBranch selection "
                  "at constructIndex {}",
                  sel.construct_index));
        }
        coord.push_back(
            {SelectionStepKind::kBranch, sel.construct_index, it->second});
        break;
      }
      case SelectionKind::kArrayEntry:
        coord.push_back(
            {SelectionStepKind::kArrayEntry, sel.construct_index,
             sel.entry_index});
        break;
    }
  }
  return coord;
}

// Payload comparison for deterministic sorting.
auto ComparePayload(const RepertoirePayload& a, const RepertoirePayload& b)
    -> bool {
  if (a.index() != b.index()) {
    return a.index() < b.index();
  }
  return std::visit(
      [&](const auto& a_val) -> bool {
        using T = std::decay_t<decltype(a_val)>;
        const auto& b_val = std::get<T>(b);
        if constexpr (std::is_same_v<T, DeclArtifactDesc>) {
          return a_val.local_ordinal < b_val.local_ordinal;
        } else if constexpr (std::is_same_v<T, ProcessArtifactDesc>) {
          return std::tie(a_val.process_kind, a_val.local_ordinal) <
                 std::tie(b_val.process_kind, b_val.local_ordinal);
        } else if constexpr (std::is_same_v<T, ChildInstanceArtifactDesc>) {
          return std::tie(a_val.inst_name, a_val.target_def) <
                 std::tie(b_val.inst_name, b_val.target_def);
        } else {
          return a_val.local_ordinal < b_val.local_ordinal;
        }
      },
      a);
}

// Deterministic artifact ordering: coord, then kind, then payload.
auto LessArtifactDesc(
    const RepertoireArtifactDesc& a, const RepertoireArtifactDesc& b) -> bool {
  auto cmp = CompareCoord(a.coord, b.coord);
  if (cmp != 0) {
    return cmp < 0;
  }
  if (a.kind != b.kind) {
    return a.kind < b.kind;
  }
  return ComparePayload(a.payload, b.payload);
}

// Ordinal bucket key for processes: (coord, process_kind).
struct ProcessOrdinalKey {
  RepertoireCoord coord;
  ProcessKindDesc process_kind;

  auto operator<(const ProcessOrdinalKey& other) const -> bool {
    auto cmp = CompareCoord(coord, other.coord);
    if (cmp != 0) {
      return cmp < 0;
    }
    return process_kind < other.process_kind;
  }
};

// Ordinal bucket key for declarations: coord only.
// Declarations get encounter-order ordinals within each coordinate bucket.
struct DeclOrdinalKey {
  RepertoireCoord coord;

  auto operator<(const DeclOrdinalKey& other) const -> bool {
    return CompareCoord(coord, other.coord) < 0;
  }
};

// Ordinal bucket key for continuous assigns: coord only.
struct ContAssignOrdinalKey {
  RepertoireCoord coord;

  auto operator<(const ContAssignOrdinalKey& other) const -> bool {
    return CompareCoord(coord, other.coord) < 0;
  }
};

}  // namespace

auto RepertoireDebugView::Key::operator<(const Key& other) const -> bool {
  auto cmp = CompareCoord(coord, other.coord);
  if (cmp != 0) {
    return cmp < 0;
  }
  return local_ordinal < other.local_ordinal;
}

namespace {

// Internal builder that produces both semantic descriptor and debug view
// in one pass. No redundant inventory walks.
auto BuildDescInternal(const slang::ast::InstanceBodySymbol& body)
    -> std::pair<DefinitionRepertoireDesc, RepertoireDebugView> {
  // Step 1: Build artifact inventory using M2c-2a extraction.
  auto inventory = BuildArtifactInventory(body);

  // Step 2: Build branch ordinal map.
  std::map<BranchOrdinalKey, uint32_t> next_ordinal;
  BranchOrdinalMap ordinal_map;
  BuildBranchOrdinals(body, {}, next_ordinal, ordinal_map);

  // Step 3 & 4: Lower each artifact to pointer-free descriptor
  // and assign local ordinals. Collect debug names alongside.
  std::map<DeclOrdinalKey, uint32_t> decl_ordinals;
  std::map<ProcessOrdinalKey, uint32_t> process_ordinals;
  std::map<ContAssignOrdinalKey, uint32_t> cont_ordinals;

  DefinitionRepertoireDesc desc;
  RepertoireDebugView debug_view;

  // Decls: assign ordinals by encounter order within coord bucket,
  // intern type into compile-owned type store, collect debug names.
  for (const auto& handle : inventory.decls) {
    auto coord = LowerAvailability(handle.availability, ordinal_map);
    auto ordinal_key = DeclOrdinalKey{coord};
    auto ordinal = decl_ordinals[ordinal_key]++;

    // Inventory decl handles are guaranteed to be ValueSymbol-backed
    // (variable/net declarations only).
    const auto& value_sym = handle.symbol->as<slang::ast::ValueSymbol>();

    // Non-value-storage types (e.g., event) have no type descriptor.
    // Filter at the AST boundary before entering the Lyra type path.
    // This is the AST-facing equivalent of HasValueStorage(TypeKind).
    if (value_sym.getType().isEvent()) {
      continue;
    }

    auto type_id = InternCompileOwnedType(value_sym.getType(), desc.type_store);

    debug_view.decl_names[{coord, ordinal}] = std::string(handle.symbol->name);
    desc.artifacts.push_back(
        {RepertoireArtifactKind::kDecl, std::move(coord),
         DeclArtifactDesc{.local_ordinal = ordinal, .type_id = type_id}});
  }

  // Processes
  for (const auto& handle : inventory.processes) {
    auto coord = LowerAvailability(handle.availability, ordinal_map);
    const auto& proc = handle.symbol->as<slang::ast::ProceduralBlockSymbol>();
    auto kind = LowerProcessKind(proc.procedureKind);
    auto ordinal_key = ProcessOrdinalKey{coord, kind};
    auto ordinal = process_ordinals[ordinal_key]++;
    desc.artifacts.push_back(
        {RepertoireArtifactKind::kProcess, std::move(coord),
         ProcessArtifactDesc{kind, ordinal}});
  }

  // Child instances
  for (const auto& handle : inventory.child_instances) {
    auto coord = LowerAvailability(handle.availability, ordinal_map);
    const auto& inst = handle.symbol->as<slang::ast::InstanceSymbol>();
    desc.artifacts.push_back(
        {RepertoireArtifactKind::kChildInstance, std::move(coord),
         ChildInstanceArtifactDesc{
             std::string(inst.name), std::string(inst.getDefinition().name)}});
  }

  // Continuous assigns
  for (const auto& handle : inventory.continuous_assigns) {
    auto coord = LowerAvailability(handle.availability, ordinal_map);
    auto ordinal_key = ContAssignOrdinalKey{coord};
    auto ordinal = cont_ordinals[ordinal_key]++;
    desc.artifacts.push_back(
        {RepertoireArtifactKind::kContinuousAssign, std::move(coord),
         ContinuousAssignArtifactDesc{ordinal}});
  }

  // Step 5: Sort deterministically.
  std::sort(desc.artifacts.begin(), desc.artifacts.end(), LessArtifactDesc);

  return {std::move(desc), std::move(debug_view)};
}

auto FindDebugName(
    const RepertoireDebugView& debug_view, const RepertoireCoord& coord,
    uint32_t ordinal) -> std::string_view {
  auto it = debug_view.decl_names.find({coord, ordinal});
  if (it != debug_view.decl_names.end()) {
    return it->second;
  }
  return "?";
}

auto DumpCoord(const RepertoireCoord& coord) -> std::string {
  if (coord.empty()) {
    return "coord=[]";
  }
  std::string result = "coord=[";
  for (size_t i = 0; i < coord.size(); ++i) {
    if (i > 0) {
      result += ", ";
    }
    const auto& step = coord[i];
    switch (step.kind) {
      case SelectionStepKind::kBranch:
        result += std::format(
            "branch(ci={},alt={})", step.construct_index, step.alt_index);
        break;
      case SelectionStepKind::kArrayEntry:
        result += std::format(
            "entry(ci={},idx={})", step.construct_index, step.alt_index);
        break;
    }
  }
  result += "]";
  return result;
}

}  // namespace

auto BuildDefinitionRepertoireDesc(const slang::ast::InstanceBodySymbol& body)
    -> DefinitionRepertoireDesc {
  return BuildDescInternal(body).first;
}

auto BuildDefinitionRepertoireDescWithDebugView(
    const slang::ast::InstanceBodySymbol& body)
    -> std::pair<DefinitionRepertoireDesc, RepertoireDebugView> {
  return BuildDescInternal(body);
}

auto DumpDefinitionRepertoireDesc(
    const DefinitionRepertoireDesc& desc, const RepertoireDebugView& debug_view)
    -> std::string {
  std::string result;

  for (const auto& artifact : desc.artifacts) {
    std::visit(
        [&](const auto& payload) {
          using T = std::decay_t<decltype(payload)>;
          if constexpr (std::is_same_v<T, DeclArtifactDesc>) {
            auto debug_name = FindDebugName(
                debug_view, artifact.coord, payload.local_ordinal);
            auto type_str =
                DumpCompileOwnedType(desc.type_store, payload.type_id);
            result += std::format(
                "  decl #{} \"{}\" {} {}", payload.local_ordinal, debug_name,
                type_str, DumpCoord(artifact.coord));
          } else if constexpr (std::is_same_v<T, ProcessArtifactDesc>) {
            result += std::format(
                "  proc {}#{} {}", ProcessKindLabel(payload.process_kind),
                payload.local_ordinal, DumpCoord(artifact.coord));
          } else if constexpr (std::is_same_v<T, ChildInstanceArtifactDesc>) {
            result += std::format(
                "  inst \"{}\"->\"{}\" {}", payload.inst_name,
                payload.target_def, DumpCoord(artifact.coord));
          } else {
            result += std::format(
                "  cont #{} {}", payload.local_ordinal,
                DumpCoord(artifact.coord));
          }
        },
        artifact.payload);
    result += "\n";
  }

  return result;
}

}  // namespace lyra::lowering::ast_to_hir
