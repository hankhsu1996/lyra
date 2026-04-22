#pragma once

#include <cstdint>
#include <unordered_map>
#include <vector>

#include "lyra/common/local_slot_id.hpp"
#include "lyra/common/module_identity.hpp"
#include "lyra/common/symbol_types.hpp"
#include "lyra/common/type.hpp"
#include "lyra/mir/child_binding_site_id.hpp"

namespace lyra::mir {

// SystemVerilog port direction for module ports (IEEE 1800-2023 23.2.2).
//
// This is the lowered port direction for the compile-time contract, not a
// source-level syntax category. No existing direction enum covers module
// ports (ParameterDirection is for subroutine arguments). This is the one
// canonical lowered direction type for the module-header contract.
enum class PortDirection : uint8_t {
  kInput,
  kOutput,
  kInOut,
};

// Port entry in the compiled module header.
// Each port symbol appears exactly once. Sorted by local slot value
// for deterministic iteration and hashing.
struct PortEntry {
  SymbolId sym;
  common::LocalSlotId slot;
  TypeId type;
  PortDirection dir = PortDirection::kInput;
};

// Handle into the header database.
//
// The value is the index into the database's append-only header store.
// Handles are stable for the lifetime of the database because entries
// are never erased or reordered.
//
// UINT32_MAX is the invalid sentinel. Default-constructed handles are
// invalid. Callers must not use an invalid handle for database lookup.
struct CompiledModuleHeaderId {
  uint32_t value = UINT32_MAX;
  auto operator==(const CompiledModuleHeaderId&) const -> bool = default;

  [[nodiscard]] constexpr auto IsValid() const -> bool {
    return value != UINT32_MAX;
  }
};

// Forward declarations for friend access.
struct ChildPortContract;
struct CompiledModuleBody;
class HeaderDatabase;

auto GetChildPortContract(
    const CompiledModuleBody& body, const HeaderDatabase& headers,
    ChildBindingSiteId site, SymbolId child_port_sym) -> ChildPortContract;

// Specialization-scoped lowered child contract artifact.
//
// This is the child-facing lowered contract for a specialization, not
// merely the source-level module header syntax. The port -> LocalSlotId
// mapping is a lowered identity stable within a specialization, not
// guaranteed definition-wide.
//
// Produced from elaboration/declaration before body compilation begins.
// The ONLY child-facing artifact a parent may consume during compilation.
//
// Parent-side child-port resolution must go through GetChildPortContract
// (the single canonical lookup path). Internal port data and lookup are
// private; only HeaderDatabase and GetChildPortContract have access.
//
// Invariant: each port symbol appears exactly once in ports_.
// The sorted vector enables deterministic iteration, hashing, debug
// output, and serialization. The type itself does not yet provide
// hash/serialization helpers; those will be added when needed.
class CompiledModuleHeader {
 public:
  [[nodiscard]] auto SpecId() const -> common::ModuleSpecId {
    return spec_id_;
  }
  [[nodiscard]] auto DefId() const -> common::ModuleDefId {
    return def_id_;
  }
  [[nodiscard]] auto PortCount() const -> size_t {
    return ports_.size();
  }

  // Factory to construct a header with the given ports.
  // Ports must be sorted by slot.value. Caller is responsible for
  // ensuring uniqueness (one entry per port symbol).
  static auto Create(
      common::ModuleSpecId spec_id, common::ModuleDefId def_id,
      std::vector<PortEntry> ports) -> CompiledModuleHeader;

 private:
  friend class HeaderDatabase;
  friend auto GetChildPortContract(
      const CompiledModuleBody& body, const HeaderDatabase& headers,
      ChildBindingSiteId site, SymbolId child_port_sym) -> ChildPortContract;

  // Symbol lookup primitive. Used by GetChildPortContract and HeaderDatabase.
  // Returns nullptr if the symbol is not a port of this module.
  [[nodiscard]] auto FindPort(SymbolId sym) const -> const PortEntry*;

  common::ModuleSpecId spec_id_ = {};
  common::ModuleDefId def_id_ = {};

  // Sorted by slot.value for deterministic ordering.
  // Each port symbol appears exactly once.
  std::vector<PortEntry> ports_;
};

// Child port contract: the narrow view a parent needs for one child port.
struct ChildPortContract {
  common::LocalSlotId slot;
  TypeId type;
  PortDirection dir = PortDirection::kInput;
};

// Separately owned compile-time artifact store for module headers.
//
// This is NOT nested ownership under CompiledSpecialization. Headers are
// a separately stored, independently queryable artifact set. The database
// owns all CompiledModuleHeader instances and provides stable handles.
//
// Invariants:
//   - Exactly one header per ModuleSpecId (Add rejects duplicates).
//   - Entries are append-only; never erased or reordered.
//   - CompiledModuleHeaderId.value is the vector index.
//   - GetHeader(ModuleSpecId) is total for registered specs.
//   - The database must outlive all CompiledSpecialization instances
//     and all code that holds CompiledModuleHeaderId handles.
class HeaderDatabase {
 public:
  // Register a header. Rejects duplicate ModuleSpecId (throws InternalError).
  // Returns the stable handle for the newly added header.
  auto Add(CompiledModuleHeader header) -> CompiledModuleHeaderId;

  // Retrieve by handle (index). Throws if handle is invalid or out of range.
  [[nodiscard]] auto GetHeader(CompiledModuleHeaderId id) const
      -> const CompiledModuleHeader&;

  // Retrieve by spec. Throws if the spec is not registered.
  // Uses internal ModuleSpecId -> handle index for lookup.
  [[nodiscard]] auto GetHeader(common::ModuleSpecId spec) const
      -> const CompiledModuleHeader&;

  // Look up a port's lowered contract by spec and port symbol.
  // Returns nullopt if the spec has no port with that symbol.
  // This is the header-database-level lookup for connection recipe building.
  [[nodiscard]] auto FindPortEntry(
      common::ModuleSpecId spec, SymbolId port_sym) const -> const PortEntry*;

 private:
  // Append-only header store. Index == CompiledModuleHeaderId.value.
  std::vector<CompiledModuleHeader> headers_;
  // Internal unique index: ModuleSpecId -> header handle.
  std::unordered_map<
      common::ModuleSpecId, CompiledModuleHeaderId, common::ModuleSpecIdHash>
      spec_index_;
};

}  // namespace lyra::mir
