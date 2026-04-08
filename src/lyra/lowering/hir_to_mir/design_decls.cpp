#include <cstddef>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/common/internal_error.hpp"
#include "lyra/common/symbol.hpp"
#include "lyra/common/type.hpp"
#include "lyra/hir/design.hpp"
#include "lyra/hir/dpi.hpp"
#include "lyra/hir/fwd.hpp"
#include "lyra/hir/module.hpp"
#include "lyra/hir/module_body.hpp"
#include "lyra/hir/package.hpp"
#include "lyra/hir/routine.hpp"
#include "lyra/lowering/hir_to_mir/context.hpp"
#include "lyra/lowering/hir_to_mir/design.hpp"
#include "lyra/lowering/hir_to_mir/dpi_name_checks.hpp"
#include "lyra/lowering/hir_to_mir/dpi_registry.hpp"
#include "lyra/lowering/hir_to_mir/lower.hpp"
#include "lyra/lowering/hir_to_mir/routine.hpp"
#include "lyra/mir/arena.hpp"
#include "lyra/mir/call.hpp"
#include "lyra/mir/dpi_verify.hpp"
#include "lyra/mir/handle.hpp"
#include "lyra/mir/place.hpp"
#include "lyra/mir/routine.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// Conservative storage-shape classifier for direct unpacked array roots
// in module instances. Scoped to direct roots only -- does not attempt
// aggregate-root or transitive containment classification.
//
// Current approximation: if the module has any promoted parameters AND the
// slot type is a direct unpacked array root, classify as kOwnedContainer.
// This over-classifies literal-dimensioned arrays in parameterized modules
// (e.g., int arr[4] in a module with an unrelated value-only parameter).
// Direct unpacked array roots whose extent varies via promoted parameters
// will classify as kOwnedContainer under this rule. The cost of
// over-classification is 16 bytes of handle overhead per false positive.
//
// param_slots contains value-only/elaboration-time params that are
// transmitted per-instance. Structural-only params create different
// specializations and cannot cause within-spec variation, so modules
// with only structural params correctly return kInlineValue.
auto ClassifySlotStorageShape(
    const hir::Module& mod, TypeId type, const TypeArena& types)
    -> mir::StorageShape {
  if (mod.param_slots.empty()) return mir::StorageShape::kInlineValue;
  if (types[type].Kind() == TypeKind::kUnpackedArray) {
    return mir::StorageShape::kOwnedContainer;
  }
  return mir::StorageShape::kInlineValue;
}

}  // namespace

auto CollectBodyLocalDecls(
    const hir::Module& module, const SymbolTable& symbol_table,
    const TypeArena& type_arena, mir::Arena& mir_arena) -> BodyLocalDecls {
  BodyLocalDecls body_decls;
  uint32_t next_body_slot = 0;

  for (SymbolId var : module.variables) {
    const Symbol& sym = symbol_table[var];

    // Named events are synchronization primitives with no byte-level
    // storage. Allocate a dedicated EventId instead of a slot.
    if (type_arena[sym.type].Kind() == TypeKind::kEvent) {
      auto event_id =
          mir::EventId{static_cast<uint32_t>(body_decls.event_descs.size())};
      body_decls.events[var] = event_id;
      body_decls.event_descs.push_back(mir::EventDesc{.type = sym.type});
      continue;
    }

    mir::Place body_place{
        .root =
            mir::PlaceRoot{
                .kind = mir::PlaceRoot::Kind::kModuleSlot,
                .id = static_cast<int>(next_body_slot++),
                .type = sym.type,
            },
        .projections = {},
    };
    body_decls.places[var] = mir_arena.AddPlace(std::move(body_place));
    body_decls.slots.push_back(
        {sym.type, mir::SlotKind::kVariable,
         ClassifySlotStorageShape(module, sym.type, type_arena)});
  }

  for (SymbolId net : module.nets) {
    const Symbol& sym = symbol_table[net];
    mir::Place body_place{
        .root =
            mir::PlaceRoot{
                .kind = mir::PlaceRoot::Kind::kModuleSlot,
                .id = static_cast<int>(next_body_slot++),
                .type = sym.type,
            },
        .projections = {},
    };
    body_decls.places[net] = mir_arena.AddPlace(std::move(body_place));
    body_decls.slots.push_back(
        {sym.type, mir::SlotKind::kNet,
         ClassifySlotStorageShape(module, sym.type, type_arena)});
  }

  for (SymbolId param : module.param_slots) {
    const Symbol& sym = symbol_table[param];
    mir::Place body_place{
        .root =
            mir::PlaceRoot{
                .kind = mir::PlaceRoot::Kind::kModuleSlot,
                .id = static_cast<int>(next_body_slot++),
                .type = sym.type,
            },
        .projections = {},
    };
    body_decls.places[param] = mir_arena.AddPlace(std::move(body_place));
    body_decls.slots.push_back({sym.type, mir::SlotKind::kParamConst});
  }

  return body_decls;
}

namespace {

// Intern a string into the pool: deduplicates identical strings by returning
// the existing offset if the string was already interned.
class StringPoolIntern {
 public:
  auto Intern(std::vector<char>& pool, const std::string& name) -> uint32_t {
    if (name.empty()) return 0;
    auto [it, inserted] = index_.emplace(name, 0U);
    if (!inserted) return it->second;
    auto off = static_cast<uint32_t>(pool.size());
    pool.insert(pool.end(), name.begin(), name.end());
    pool.push_back('\0');
    it->second = off;
    return off;
  }

 private:
  std::unordered_map<std::string, uint32_t> index_;
};

auto ClassifyDpiReturnKind(DpiAbiTypeClass abi_type) -> mir::DpiReturnKind {
  if (abi_type == DpiAbiTypeClass::kVoid) return mir::DpiReturnKind::kVoid;
  if (IsPackedVecDpiType(abi_type)) return mir::DpiReturnKind::kIndirect;
  return mir::DpiReturnKind::kDirectValue;
}

auto BuildDpiReturnDesc(TypeId return_type, DpiAbiTypeClass abi_type)
    -> mir::DpiReturnDesc {
  return {
      .sv_type = return_type,
      .abi_type = abi_type,
      .kind = ClassifyDpiReturnKind(abi_type),
  };
}

// Result of attempting to register a DPI export.
enum class DpiExportRegResult {
  kOk,
  kDuplicateSymbol,
};

struct DpiExportRegOutcome {
  DpiExportRegResult result = DpiExportRegResult::kOk;
  std::string diagnostic;
  std::optional<mir::DpiSignature> signature;
};

auto BuildDpiSignatureFromCache(const hir::DpiExportSignature& sig_cache)
    -> DpiExportRegOutcome {
  mir::DpiSignature sig;
  sig.result =
      BuildDpiReturnDesc(sig_cache.return_type_id, sig_cache.return_dpi_type);

  for (const auto& p : sig_cache.params) {
    sig.params.push_back({
        .sv_type = p.type_id,
        .abi_type = p.dpi_type,
        .direction = p.direction,
        .passing = mir::GetDpiPassingMode(p.direction, p.dpi_type),
    });
  }

  mir::ValidateDpiSignatureContract(sig, "BuildDpiSignatureFromCache");

  return {
      .result = DpiExportRegResult::kOk,
      .diagnostic = {},
      .signature = std::move(sig),
  };
}

auto TryRegisterDpiExport(
    const hir::DpiExportDecl& decl, const hir::DpiExportSignature& sig_cache,
    DesignDpiExports& registry) -> DpiExportRegOutcome {
  auto build = BuildDpiSignatureFromCache(sig_cache);
  if (build.result != DpiExportRegResult::kOk) {
    return build;
  }

  DpiExportInfo info{
      .symbol = decl.symbol,
      .span = decl.span,
      .c_name = decl.c_name,
      .signature = std::move(*build.signature),
      .is_module_scoped = decl.is_module_scoped,
      .is_task = decl.is_task,
  };
  if (!registry.Insert(std::move(info))) {
    return {
        .result = DpiExportRegResult::kDuplicateSymbol,
        .diagnostic =
            std::format("duplicate DPI export symbol {}", decl.symbol.value),
        .signature = std::nullopt,
    };
  }
  return {};
}

void RegisterDpiImport(
    const hir::DpiImportDecl& dpi, DesignDpiImports& registry) {
  std::vector<DpiParamInfo> params;
  params.reserve(dpi.params.size());
  for (const auto& p : dpi.params) {
    params.push_back({
        .type_id = p.type_id,
        .abi_type = p.dpi_type,
        .direction = p.direction,
    });
  }
  DpiImportInfo info{
      .symbol = dpi.symbol,
      .span = dpi.span,
      .sv_name = dpi.sv_name,
      .c_name = dpi.c_name,
      .return_type_id = dpi.return_type_id,
      .return_abi_type = dpi.return_dpi_type,
      .params = std::move(params),
      .is_context = dpi.is_context,
  };
  if (!registry.Insert(std::move(info))) {
    throw common::InternalError(
        "CollectDesignDeclarations",
        std::format("duplicate DPI import symbol {}", dpi.symbol.value));
  }
}

}  // namespace

void CheckDpiVisibleNameCollisions(DesignDeclarations& decls) {
  // Map: c_name -> (symbol, source description for diagnostics)
  std::unordered_map<std::string, std::pair<SymbolId, std::string>>
      visible_names;

  // Register all exports.
  for (const auto& [sym, info] : decls.dpi_exports.Entries()) {
    visible_names[info.c_name] = {sym, "export"};
  }

  // Check all imports against the visible namespace.
  for (const auto& [sym, info] : decls.dpi_imports.Entries()) {
    auto [it, inserted] = visible_names.try_emplace(info.c_name, sym, "import");
    if (!inserted && it->second.first != sym) {
      decls.export_diagnostics.push_back(
          DesignDeclarations::ExportDiagnostic{
              .span = info.span,
              .message = std::format(
                  "DPI-C visible name '{}' collision: {} (symbol {}) "
                  "conflicts with {} (symbol {})",
                  info.c_name, "import", sym.value, it->second.second,
                  it->second.first.value),
          });
    }
  }
}

auto CollectDesignDeclarations(
    const hir::Design& design, const LoweringInput& input,
    mir::Arena& mir_arena) -> DesignDeclarations {
  DesignDeclarations decls;
  int next_slot = 0;

  // Initialize trace string pool with empty string at offset 0.
  decls.slot_trace_string_pool.push_back('\0');

  StringPoolIntern intern;

  // Ordering contract: packages first (in element order), then all module
  // instances (in BFS elaboration order from LowerDesign). This order is ABI -
  // do not change without updating all consumers (LLVM layout, dump).

  // Allocate package variable design places + pre-allocate function IDs
  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      const Symbol& pkg_sym = (*input.symbol_table)[pkg->symbol];
      auto pkg_name_off =
          intern.Intern(decls.slot_trace_string_pool, pkg_sym.name);

      for (SymbolId var : pkg->variables) {
        const Symbol& sym = (*input.symbol_table)[var];
        mir::Place place{
            .root =
                mir::PlaceRoot{
                    .kind = mir::PlaceRoot::Kind::kDesignGlobal,
                    .id = next_slot++,
                    .type = sym.type,
                },
            .projections = {},
        };
        decls.design_places[var] = mir_arena.AddPlace(std::move(place));
        decls.slots.push_back({sym.type, mir::SlotKind::kVariable});
        decls.slot_trace_provenance.push_back(
            {.local_name_str_off =
                 intern.Intern(decls.slot_trace_string_pool, sym.name),
             .scope_kind = mir::SlotScopeKind::kPackage,
             .scope_ref = pkg_name_off});
      }

      // Pre-allocate MIR function IDs with frozen signatures
      for (hir::FunctionId hir_func_id : pkg->functions) {
        const hir::Function& hir_func = (*input.hir_arena)[hir_func_id];
        mir::FunctionSignature sig = BuildFunctionSignature(
            hir_func, *input.symbol_table, *input.type_arena);
        mir::FunctionId mir_func_id =
            mir_arena.ReserveFunction(std::move(sig), hir_func.symbol);
        decls.functions[hir_func.symbol] = mir_func_id;
      }

      // Register package DPI imports in the design-level registry.
      for (const auto& dpi : pkg->dpi_imports) {
        RegisterDpiImport(dpi, decls.dpi_imports);
      }
    }
  }

  decls.num_package_slots = static_cast<uint32_t>(next_slot);

  // Module variables/nets/params do NOT get any design-global slots or
  // places in decls. decls.design_places is package-only.
  // Cross-instance references and connection compilation use a separate
  // cross_instance_places map built in LowerDesign.
  decls.num_design_slots = static_cast<size_t>(next_slot);

  // Build reverse lookup: instance symbol -> instance table index (for %m)
  if (input.instance_table != nullptr) {
    for (uint32_t i = 0; i < input.instance_table->entries.size(); ++i) {
      decls.instance_indices[input.instance_table->entries[i].instance_sym] = i;
    }
  }

  // Collect module-body DPI imports into the design-level registry.
  for (const auto& body : design.module_bodies) {
    for (const auto& dpi : body.dpi_imports) {
      RegisterDpiImport(dpi, decls.dpi_imports);
    }
  }

  // Collect DPI exports from packages and module bodies.
  // Exports with unsupported signatures produce user diagnostics, not
  // InternalError. They are skipped (not registered in the design registry).
  const auto& sig_cache = design.dpi_export_signatures;
  auto collect_exports = [&](const std::vector<hir::DpiExportDecl>& exports) {
    for (const auto& decl : exports) {
      auto it = sig_cache.find(decl.symbol);
      if (it == sig_cache.end()) {
        throw common::InternalError(
            "CollectDesignDeclarations",
            std::format(
                "no pre-classified signature for DPI export '{}'",
                decl.c_name));
      }
      auto outcome = TryRegisterDpiExport(decl, it->second, decls.dpi_exports);
      if (outcome.result == DpiExportRegResult::kDuplicateSymbol) {
        throw common::InternalError(
            "CollectDesignDeclarations", outcome.diagnostic);
      }
    }
  };

  for (const auto& element : design.elements) {
    if (const auto* pkg = std::get_if<hir::Package>(&element)) {
      collect_exports(pkg->dpi_exports);
    }
  }
  for (const auto& body : design.module_bodies) {
    collect_exports(body.dpi_exports);
  }

  CheckDpiVisibleNameCollisions(decls);

  return decls;
}

}  // namespace lyra::lowering::hir_to_mir
