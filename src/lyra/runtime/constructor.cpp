#include "lyra/runtime/constructor.hpp"

#include <cstdint>
#include <cstring>
#include <format>
#include <memory>
#include <span>
#include <utility>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/frame_allocator.hpp"
#include "lyra/runtime/process_frame.hpp"

namespace lyra::runtime {

ConstructionResult::~ConstructionResult() {
  FreePackedBuffer(packed_buffer);
}

ConstructionResult::ConstructionResult(ConstructionResult&& other) noexcept
    : states(std::move(other.states)),
      packed_buffer(std::exchange(other.packed_buffer, nullptr)),
      num_total(std::exchange(other.num_total, 0)),
      num_connection(std::exchange(other.num_connection, 0)) {
}

auto ConstructionResult::operator=(ConstructionResult&& other) noexcept
    -> ConstructionResult& {
  if (this != &other) {
    FreePackedBuffer(packed_buffer);
    states = std::move(other.states);
    packed_buffer = std::exchange(other.packed_buffer, nullptr);
    num_total = std::exchange(other.num_total, 0);
    num_connection = std::exchange(other.num_connection, 0);
  }
  return *this;
}

Constructor::Constructor(
    std::span<const ProcessStateSchema> schemas,
    std::span<const uint64_t> slot_byte_offsets, uint32_t num_package_slots,
    std::span<std::byte> design_state)
    : schemas_(schemas),
      slot_byte_offsets_(slot_byte_offsets),
      design_state_(design_state),
      next_slot_base_(num_package_slots) {
}

void Constructor::CheckNotFinalized(const char* caller) const {
  if (finalized_) {
    throw common::InternalError(caller, "constructor already finalized");
  }
}

void Constructor::AddConnection(const ConnectionRealizationDesc& desc) {
  CheckNotFinalized("Constructor::AddConnection");
  if (connections_finalized_) {
    throw common::InternalError(
        "Constructor::AddConnection",
        "connections must be added before any body instances");
  }
  if (desc.schema_index >= schemas_.size()) {
    throw common::InternalError(
        "Constructor::AddConnection", "schema_index out of range");
  }
  staged_.push_back(
      StagedProcess{
          .schema_index = desc.schema_index,
          .body = nullptr,
          .this_ptr = nullptr,
          .instance_id = 0,
          .signal_id_offset = 0,
          .is_module = false,
      });
  ++num_connection_;
}

// Safe for repeated body switching in instance-major order.
// BeginBody only sets the active body descriptor fields (slot_count,
// entries). No body-local state accumulates between BeginBody calls.
// Running counters (next_instance_id_, next_slot_base_) and the flat
// staged_ vector are global across all bodies.
void Constructor::BeginBody(
    const BodyRealizationDesc& desc,
    std::span<const BodyProcessEntry> entries) {
  CheckNotFinalized("Constructor::BeginBody");
  connections_finalized_ = true;
  if (entries.size() != desc.num_processes) {
    throw common::InternalError(
        "Constructor::BeginBody",
        std::format(
            "entry count {} != descriptor num_processes {}", entries.size(),
            desc.num_processes));
  }
  for (const auto& entry : entries) {
    if (entry.schema_index >= schemas_.size()) {
      throw common::InternalError(
          "Constructor::BeginBody", "schema_index out of range");
    }
  }
  has_body_ = true;
  current_slot_count_ = desc.slot_count;
  current_entries_ = entries;
}

void Constructor::AddInstance() {
  CheckNotFinalized("Constructor::AddInstance");
  if (!has_body_) {
    throw common::InternalError(
        "Constructor::AddInstance", "no active body (call BeginBody first)");
  }
  connections_finalized_ = true;

  uint32_t instance_id = next_instance_id_;
  uint32_t signal_id_offset = next_slot_base_;

  // Resolve this_ptr from the layout oracle. Zero-slot bodies (no module-
  // level variables) skip the oracle lookup -- they have no state region.
  void* this_ptr = design_state_.data();
  if (current_slot_count_ > 0) {
    if (next_slot_base_ >= slot_byte_offsets_.size()) {
      throw common::InternalError(
          "Constructor::AddInstance",
          std::format(
              "slot base {} exceeds layout oracle size {}", next_slot_base_,
              slot_byte_offsets_.size()));
    }
    uint64_t base_byte_offset = slot_byte_offsets_[next_slot_base_];
    this_ptr = design_state_.subspan(base_byte_offset).data();
  }

  for (const auto& entry : current_entries_) {
    SharedBodyFn body{};
    std::memcpy(&body, &entry.shared_body_fn, sizeof(body));

    staged_.push_back(
        StagedProcess{
            .schema_index = entry.schema_index,
            .body = body,
            .this_ptr = this_ptr,
            .instance_id = instance_id,
            .signal_id_offset = signal_id_offset,
            .is_module = true,
        });
  }

  uint32_t new_slot_base = next_slot_base_ + current_slot_count_;
  if (new_slot_base < next_slot_base_) {
    throw common::InternalError(
        "Constructor::AddInstance", "slot base overflow");
  }
  // The constructor maintains a valid slot-base progression over the
  // layout oracle domain. Zero-slot bodies do not advance the counter
  // and do not require an oracle lookup.
  if (current_slot_count_ > 0 && new_slot_base > slot_byte_offsets_.size()) {
    throw common::InternalError(
        "Constructor::AddInstance",
        std::format(
            "post-increment slot base {} exceeds layout oracle size {}",
            new_slot_base, slot_byte_offsets_.size()));
  }
  next_instance_id_ += 1;
  next_slot_base_ = new_slot_base;
}

auto Constructor::Finalize() -> ConstructionResult {
  CheckNotFinalized("Constructor::Finalize");
  finalized_ = true;

  auto num_total = static_cast<uint32_t>(staged_.size());
  if (num_total == 0) {
    return ConstructionResult{};
  }

  // Build schema index sequence for the neutral frame allocator.
  std::vector<uint32_t> schema_indices;
  schema_indices.reserve(num_total);
  for (const auto& proc : staged_) {
    schema_indices.push_back(proc.schema_index);
  }

  // Allocate packed buffer and distribute state pointers.
  std::vector<void*> states(num_total);
  void* packed_buffer = AllocateProcessFrames(
      std::span(states), std::span(schema_indices), schemas_);

  // Bind frame headers from pre-resolved staging data.
  for (uint32_t i = 0; i < num_total; ++i) {
    auto* header = static_cast<ProcessFrameHeader*>(states[i]);
    header->design_ptr = design_state_.data();

    const auto& proc = staged_[i];
    if (proc.is_module) {
      header->body = proc.body;
      header->this_ptr = proc.this_ptr;
      header->instance_id = proc.instance_id;
      header->signal_id_offset = proc.signal_id_offset;
    }
  }

  ConstructionResult result;
  result.states = std::move(states);
  result.packed_buffer = packed_buffer;
  result.num_total = num_total;
  result.num_connection = num_connection_;
  return result;
}

}  // namespace lyra::runtime

// C ABI wrappers for emitted LLVM IR constructor function.
// These are the boundary between emitted LLVM code and the C++ Constructor
// class. Raw pointer/opaque handle patterns are inherent to this boundary.

auto LyraConstructorCreate(
    const lyra::runtime::ProcessStateSchema* schemas, uint32_t num_schemas,
    const uint64_t* slot_byte_offsets, uint32_t num_slots,
    uint32_t num_package_slots, void* design_state, uint64_t design_state_size)
    -> void* {
  auto ctor = std::make_unique<lyra::runtime::Constructor>(
      std::span(schemas, num_schemas), std::span(slot_byte_offsets, num_slots),
      num_package_slots,
      std::span(static_cast<std::byte*>(design_state), design_state_size));
  return ctor.release();
}

namespace {

void ValidateHandle(const void* handle, const char* caller) {
  if (handle == nullptr) {
    throw lyra::common::InternalError(caller, "null handle");
  }
}

}  // namespace

void LyraConstructorAddConnection(
    void* ctor, const lyra::runtime::ConnectionRealizationDesc* desc) {
  ValidateHandle(ctor, "LyraConstructorAddConnection");
  ValidateHandle(desc, "LyraConstructorAddConnection");
  static_cast<lyra::runtime::Constructor*>(ctor)->AddConnection(*desc);
}

void LyraConstructorBeginBody(
    void* ctor, const lyra::runtime::BodyRealizationDesc* desc,
    const lyra::runtime::BodyProcessEntry* entries, uint32_t num_entries) {
  ValidateHandle(ctor, "LyraConstructorBeginBody");
  ValidateHandle(desc, "LyraConstructorBeginBody");
  static_cast<lyra::runtime::Constructor*>(ctor)->BeginBody(
      *desc, std::span(entries, num_entries));
}

void LyraConstructorAddInstance(void* ctor) {
  ValidateHandle(ctor, "LyraConstructorAddInstance");
  static_cast<lyra::runtime::Constructor*>(ctor)->AddInstance();
}

auto LyraConstructorFinalize(void* ctor_raw) -> void* {
  ValidateHandle(ctor_raw, "LyraConstructorFinalize");
  std::unique_ptr<lyra::runtime::Constructor> ctor(
      static_cast<lyra::runtime::Constructor*>(ctor_raw));
  auto result =
      std::make_unique<lyra::runtime::ConstructionResult>(ctor->Finalize());
  return result.release();
}

auto LyraConstructionResultGetStates(void* result_raw) -> void** {
  ValidateHandle(result_raw, "LyraConstructionResultGetStates");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)
      ->states.data();
}

auto LyraConstructionResultGetNumTotal(void* result_raw) -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetNumTotal");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)->num_total;
}

auto LyraConstructionResultGetNumConnection(void* result_raw) -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetNumConnection");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)
      ->num_connection;
}

void LyraConstructionResultDestroy(void* result_raw) {
  if (result_raw == nullptr) return;
  std::unique_ptr<lyra::runtime::ConstructionResult> result(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw));
}
