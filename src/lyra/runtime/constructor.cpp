#include "lyra/runtime/constructor.hpp"

#include <cstdint>
#include <cstring>
#include <format>
#include <memory>
#include <span>
#include <string_view>
#include <utility>

#include "lyra/common/internal_error.hpp"
#include "lyra/runtime/frame_allocator.hpp"
#include "lyra/runtime/process_frame.hpp"
#include "lyra/runtime/process_meta_abi.hpp"

namespace lyra::runtime {

ConstructionResult::~ConstructionResult() {
  FreePackedBuffer(packed_buffer);
}

ConstructionResult::ConstructionResult(ConstructionResult&& other) noexcept
    : states(std::move(other.states)),
      packed_buffer(std::exchange(other.packed_buffer, nullptr)),
      num_total(std::exchange(other.num_total, 0)),
      num_connection(std::exchange(other.num_connection, 0)),
      process_meta(std::move(other.process_meta)) {
}

auto ConstructionResult::operator=(ConstructionResult&& other) noexcept
    -> ConstructionResult& {
  if (this != &other) {
    FreePackedBuffer(packed_buffer);
    states = std::move(other.states);
    packed_buffer = std::exchange(other.packed_buffer, nullptr);
    num_total = std::exchange(other.num_total, 0);
    num_connection = std::exchange(other.num_connection, 0);
    process_meta = std::move(other.process_meta);
  }
  return *this;
}

namespace {

// Validate a process metadata template view: pool sentinel, and that
// every entry's file_pool_off is within pool bounds and points to a
// NUL-terminated string before pool_size.
void ValidateMetaTemplate(
    const ProcessMetaTemplateView& tmpl, const char* caller) {
  if (tmpl.entries.empty() && tmpl.pool == nullptr && tmpl.pool_size == 0) {
    return;
  }
  if (tmpl.pool == nullptr || tmpl.pool_size == 0) {
    throw common::InternalError(caller, "meta pool null/size mismatch");
  }
  auto pool = std::span(tmpl.pool, tmpl.pool_size);
  if (pool[0] != '\0') {
    throw common::InternalError(caller, "meta pool missing '\\0' sentinel");
  }
  for (uint32_t i = 0; i < tmpl.entries.size(); ++i) {
    uint32_t off = tmpl.entries[i].file_pool_off;
    if (off == 0) continue;
    if (off >= tmpl.pool_size) {
      throw common::InternalError(
          caller, std::format(
                      "entry {} file_pool_off {} >= pool_size {}", i, off,
                      tmpl.pool_size));
    }
    bool found_nul = false;
    for (uint32_t j = off; j < tmpl.pool_size; ++j) {
      if (pool[j] == '\0') {
        found_nul = true;
        break;
      }
    }
    if (!found_nul) {
      throw common::InternalError(
          caller,
          std::format(
              "entry {} file string at offset {} not NUL-terminated", i, off));
    }
  }
}

}  // namespace

Constructor::Constructor(
    std::span<const ProcessStateSchema> schemas,
    std::span<const uint64_t> slot_byte_offsets, uint32_t num_package_slots,
    std::span<std::byte> design_state, ProcessMetaTemplateView conn_meta)
    : schemas_(schemas),
      slot_byte_offsets_(slot_byte_offsets),
      design_state_(design_state),
      next_slot_base_(num_package_slots),
      conn_meta_(conn_meta) {
  ValidateMetaTemplate(conn_meta_, "Constructor");
  realized_meta_.pool.push_back('\0');
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
  if (conn_meta_index_ >= conn_meta_.entries.size()) {
    throw common::InternalError(
        "Constructor::AddConnection",
        std::format(
            "connection metadata template exhausted (index {} >= count {})",
            conn_meta_index_, conn_meta_.entries.size()));
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

  const auto& meta = conn_meta_.entries[conn_meta_index_];
  auto conn_pool = std::span(conn_meta_.pool, conn_meta_.pool_size);
  uint32_t file_off = InternString(conn_pool, meta.file_pool_off);
  realized_meta_.words.push_back(0);
  realized_meta_.words.push_back(meta.kind_packed);
  realized_meta_.words.push_back(file_off);
  realized_meta_.words.push_back(meta.line);
  realized_meta_.words.push_back(meta.col);

  ++conn_meta_index_;
  ++num_connection_;
}

void Constructor::BeginBody(const BodyDescriptorPackage& package) {
  CheckNotFinalized("Constructor::BeginBody");
  connections_finalized_ = true;

  if (package.desc == nullptr) {
    throw common::InternalError("Constructor::BeginBody", "null descriptor");
  }
  if (package.entries.size() != package.desc->num_processes) {
    throw common::InternalError(
        "Constructor::BeginBody",
        std::format(
            "entry count {} != descriptor num_processes {}",
            package.entries.size(), package.desc->num_processes));
  }
  if (package.entries.size() != package.meta.entries.size()) {
    throw common::InternalError(
        "Constructor::BeginBody",
        std::format(
            "process entry count {} != meta entry count {}",
            package.entries.size(), package.meta.entries.size()));
  }
  ValidateMetaTemplate(package.meta, "Constructor::BeginBody");
  for (const auto& entry : package.entries) {
    if (entry.schema_index >= schemas_.size()) {
      throw common::InternalError(
          "Constructor::BeginBody", "schema_index out of range");
    }
  }

  body_ = ActiveBodyDescriptor{
      .slot_count = package.desc->slot_count,
      .entries = package.entries,
      .meta = package.meta,
      .active = true,
  };
}

void Constructor::AddInstance(const char* instance_path) {
  CheckNotFinalized("Constructor::AddInstance");
  if (instance_path == nullptr) {
    throw common::InternalError(
        "Constructor::AddInstance", "null instance_path");
  }
  if (!body_.active) {
    throw common::InternalError(
        "Constructor::AddInstance", "no active body (call BeginBody first)");
  }
  connections_finalized_ = true;

  uint32_t instance_id = next_instance_id_;
  uint32_t signal_id_offset = next_slot_base_;

  void* this_ptr = design_state_.data();
  if (body_.slot_count > 0) {
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

  for (const auto& entry : body_.entries) {
    SharedBodyFn body_fn{};
    std::memcpy(&body_fn, &entry.shared_body_fn, sizeof(body_fn));

    staged_.push_back(
        StagedProcess{
            .schema_index = entry.schema_index,
            .body = body_fn,
            .this_ptr = this_ptr,
            .instance_id = instance_id,
            .signal_id_offset = signal_id_offset,
            .is_module = true,
        });
  }

  // Append metadata entries for all body processes of this instance.
  uint32_t inst_path_off = AppendString(instance_path);
  for (const auto& meta : body_.meta.entries) {
    auto body_pool = std::span(body_.meta.pool, body_.meta.pool_size);
    uint32_t file_off = InternString(body_pool, meta.file_pool_off);
    realized_meta_.words.push_back(inst_path_off);
    realized_meta_.words.push_back(meta.kind_packed);
    realized_meta_.words.push_back(file_off);
    realized_meta_.words.push_back(meta.line);
    realized_meta_.words.push_back(meta.col);
  }

  uint32_t new_slot_base = next_slot_base_ + body_.slot_count;
  if (new_slot_base < next_slot_base_) {
    throw common::InternalError(
        "Constructor::AddInstance", "slot base overflow");
  }
  if (body_.slot_count > 0 && new_slot_base > slot_byte_offsets_.size()) {
    throw common::InternalError(
        "Constructor::AddInstance",
        std::format(
            "post-increment slot base {} exceeds layout oracle size {}",
            new_slot_base, slot_byte_offsets_.size()));
  }
  next_instance_id_ += 1;
  next_slot_base_ = new_slot_base;
}

auto Constructor::InternString(std::span<const char> pool, uint32_t pool_off)
    -> uint32_t {
  if (pool.empty() || pool_off == 0) return 0;
  std::string_view sv(pool.subspan(pool_off).data());
  if (sv.empty()) return 0;
  auto it = string_intern_.find(sv);
  if (it != string_intern_.end()) return it->second;
  auto off = AppendString(sv);
  interned_strings_.emplace_back(sv);
  string_intern_[std::string_view(interned_strings_.back())] = off;
  return off;
}

auto Constructor::AppendString(std::string_view s) -> uint32_t {
  if (s.empty()) return 0;
  auto off = static_cast<uint32_t>(realized_meta_.pool.size());
  realized_meta_.pool.insert(realized_meta_.pool.end(), s.begin(), s.end());
  realized_meta_.pool.push_back('\0');
  return off;
}

auto Constructor::Finalize() -> ConstructionResult {
  CheckNotFinalized("Constructor::Finalize");
  finalized_ = true;

  if (conn_meta_index_ != conn_meta_.entries.size()) {
    throw common::InternalError(
        "Constructor::Finalize",
        std::format(
            "connection metadata not fully consumed ({} of {})",
            conn_meta_index_, conn_meta_.entries.size()));
  }

  auto num_total = static_cast<uint32_t>(staged_.size());

  // Validate metadata-process ordering alignment.
  auto expected_meta_words =
      static_cast<size_t>(num_total) * process_meta_abi::kStride;
  if (realized_meta_.words.size() != expected_meta_words) {
    throw common::InternalError(
        "Constructor::Finalize",
        std::format(
            "metadata word count {} != expected {} (num_total={} * stride={})",
            realized_meta_.words.size(), expected_meta_words, num_total,
            process_meta_abi::kStride));
  }

  if (num_total == 0) {
    ConstructionResult result;
    result.process_meta = std::move(realized_meta_);
    return result;
  }

  std::vector<uint32_t> schema_indices;
  schema_indices.reserve(num_total);
  for (const auto& proc : staged_) {
    schema_indices.push_back(proc.schema_index);
  }

  std::vector<void*> states(num_total);
  void* packed_buffer = AllocateProcessFrames(
      std::span(states), std::span(schema_indices), schemas_);

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
  result.process_meta = std::move(realized_meta_);
  return result;
}

}  // namespace lyra::runtime

// C ABI wrappers for emitted LLVM IR constructor function.

namespace {

void ValidateHandle(const void* handle, const char* caller) {
  if (handle == nullptr) {
    throw lyra::common::InternalError(caller, "null handle");
  }
}

// Validate meta template ABI inputs at the C boundary: pointer/count
// coherence, pool sentinel, and all entry file_pool_off offsets in range
// with NUL termination. This catches malformed emitted templates before
// span construction or downstream constructor logic.
void ValidateMetaAbiInputs(
    const lyra::runtime::ProcessMetaTemplateEntry* entries, uint32_t count,
    const char* pool, uint32_t pool_size, const char* caller) {
  if (count > 0 && entries == nullptr) {
    throw lyra::common::InternalError(
        caller, "non-zero meta entry count with null entries pointer");
  }
  if (pool_size > 0 && pool == nullptr) {
    throw lyra::common::InternalError(
        caller, "non-zero meta pool size with null pool pointer");
  }
  if (count > 0 && pool_size == 0) {
    throw lyra::common::InternalError(
        caller, "non-empty meta entries with zero pool size");
  }
  if (pool != nullptr && pool_size > 0) {
    auto pool_span = std::span(pool, pool_size);
    if (pool_span[0] != '\0') {
      throw lyra::common::InternalError(
          caller, "meta pool missing '\\0' sentinel at offset 0");
    }
    auto entries_span = std::span(entries, count);
    for (uint32_t i = 0; i < count; ++i) {
      uint32_t off = entries_span[i].file_pool_off;
      if (off == 0) continue;
      if (off >= pool_size) {
        throw lyra::common::InternalError(
            caller, std::format(
                        "entry {} file_pool_off {} >= pool_size {}", i, off,
                        pool_size));
      }
      bool found_nul = false;
      for (uint32_t j = off; j < pool_size; ++j) {
        if (pool_span[j] == '\0') {
          found_nul = true;
          break;
        }
      }
      if (!found_nul) {
        throw lyra::common::InternalError(
            caller, std::format(
                        "entry {} file string at offset {} not NUL-terminated",
                        i, off));
      }
    }
  }
}

}  // namespace

auto LyraConstructorCreate(
    const lyra::runtime::ProcessStateSchema* schemas, uint32_t num_schemas,
    const uint64_t* slot_byte_offsets, uint32_t num_slots,
    uint32_t num_package_slots, void* design_state, uint64_t design_state_size,
    const lyra::runtime::ProcessMetaTemplateEntry* conn_meta_entries,
    uint32_t num_conn_meta, const char* conn_meta_pool,
    uint32_t conn_meta_pool_size) -> void* {
  ValidateMetaAbiInputs(
      conn_meta_entries, num_conn_meta, conn_meta_pool, conn_meta_pool_size,
      "LyraConstructorCreate");
  lyra::runtime::ProcessMetaTemplateView conn_meta{
      .entries = std::span(conn_meta_entries, num_conn_meta),
      .pool = conn_meta_pool,
      .pool_size = conn_meta_pool_size,
  };
  auto ctor = std::make_unique<lyra::runtime::Constructor>(
      std::span(schemas, num_schemas), std::span(slot_byte_offsets, num_slots),
      num_package_slots,
      std::span(static_cast<std::byte*>(design_state), design_state_size),
      conn_meta);
  return ctor.release();
}

void LyraConstructorAddConnection(
    void* ctor, const lyra::runtime::ConnectionRealizationDesc* desc) {
  ValidateHandle(ctor, "LyraConstructorAddConnection");
  ValidateHandle(desc, "LyraConstructorAddConnection");
  static_cast<lyra::runtime::Constructor*>(ctor)->AddConnection(*desc);
}

void LyraConstructorBeginBody(
    void* ctor, const lyra::runtime::BodyRealizationDesc* desc,
    const lyra::runtime::BodyProcessEntry* entries, uint32_t num_entries,
    const lyra::runtime::ProcessMetaTemplateEntry* meta_entries,
    uint32_t num_meta, const char* meta_pool, uint32_t meta_pool_size) {
  ValidateHandle(ctor, "LyraConstructorBeginBody");
  ValidateHandle(desc, "LyraConstructorBeginBody");
  ValidateMetaAbiInputs(
      meta_entries, num_meta, meta_pool, meta_pool_size,
      "LyraConstructorBeginBody");
  lyra::runtime::BodyDescriptorPackage package{
      .desc = desc,
      .entries = std::span(entries, num_entries),
      .meta =
          lyra::runtime::ProcessMetaTemplateView{
              .entries = std::span(meta_entries, num_meta),
              .pool = meta_pool,
              .pool_size = meta_pool_size,
          },
  };
  static_cast<lyra::runtime::Constructor*>(ctor)->BeginBody(package);
}

void LyraConstructorAddInstance(void* ctor, const char* instance_path) {
  ValidateHandle(ctor, "LyraConstructorAddInstance");
  static_cast<lyra::runtime::Constructor*>(ctor)->AddInstance(instance_path);
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

auto LyraConstructionResultGetProcessMetaWords(void* result_raw)
    -> const uint32_t* {
  ValidateHandle(result_raw, "LyraConstructionResultGetProcessMetaWords");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)
      ->process_meta.words.data();
}

auto LyraConstructionResultGetProcessMetaWordCount(void* result_raw)
    -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetProcessMetaWordCount");
  auto& words = static_cast<lyra::runtime::ConstructionResult*>(result_raw)
                    ->process_meta.words;
  return static_cast<uint32_t>(
      words.size() / lyra::runtime::process_meta_abi::kStride);
}

auto LyraConstructionResultGetProcessMetaPool(void* result_raw) -> const char* {
  ValidateHandle(result_raw, "LyraConstructionResultGetProcessMetaPool");
  return static_cast<lyra::runtime::ConstructionResult*>(result_raw)
      ->process_meta.pool.data();
}

auto LyraConstructionResultGetProcessMetaPoolSize(void* result_raw)
    -> uint32_t {
  ValidateHandle(result_raw, "LyraConstructionResultGetProcessMetaPoolSize");
  return static_cast<uint32_t>(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw)
          ->process_meta.pool.size());
}

void LyraConstructionResultDestroy(void* result_raw) {
  if (result_raw == nullptr) return;
  std::unique_ptr<lyra::runtime::ConstructionResult> result(
      static_cast<lyra::runtime::ConstructionResult*>(result_raw));
}
