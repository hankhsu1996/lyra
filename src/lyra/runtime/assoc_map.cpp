#include "lyra/runtime/assoc_map.hpp"

#include <algorithm>
#include <cstdint>
#include <cstring>
#include <memory>
#include <optional>
#include <string>
#include <vector>

#include "lyra/runtime/assoc_array.hpp"
#include "lyra/runtime/dyn_array.hpp"
#include "lyra/runtime/string.hpp"

namespace lyra::runtime {

// --- Entry helpers ---

auto AssocMap::Entry::data() -> uint8_t* {
  return on_heap ? heap_data : inline_data;
}

auto AssocMap::Entry::data() const -> const uint8_t* {
  return on_heap ? heap_data : inline_data;
}

void AssocMap::AllocEntry(Entry& e) {
  if (val_ops_.value_size <= kInlineSize) {
    e.on_heap = false;
  } else {
    e.heap_data = new uint8_t[val_ops_.value_size];
    e.on_heap = true;
  }
}

void AssocMap::DestroyEntry(Entry& e) {
  val_ops_.destroy(e.data(), val_ops_.ctx);
  if (e.on_heap) {
    delete[] e.heap_data;
    e.on_heap = false;
  }
}

void AssocMap::CopyInitEntry(Entry& dst, const Entry& src) {
  AllocEntry(dst);
  val_ops_.copy_init(dst.data(), src.data(), val_ops_.ctx);
}

// --- AssocMap ---

AssocMap::AssocMap(KeySpec key_spec, ValueOps val_ops)
    : key_spec_(key_spec),
      val_ops_(val_ops),
      entries_(CanonKeyLess{.spec = key_spec_}) {
}

AssocMap::~AssocMap() {
  for (auto& [key, entry] : entries_) {
    DestroyEntry(entry);
  }
}

auto AssocMap::Get(const CanonKeyPayload& key, void* out_buf) -> bool {
  auto it = entries_.find(key);
  if (it != entries_.end()) {
    val_ops_.copy_init(out_buf, it->second.data(), val_ops_.ctx);
    return true;
  }
  val_ops_.default_init(out_buf, val_ops_.ctx);
  return false;
}

void AssocMap::Set(const CanonKeyPayload& key, const void* value_buf) {
  auto it = entries_.find(key);
  if (it != entries_.end()) {
    val_ops_.copy_assign(it->second.data(), value_buf, val_ops_.ctx);
  } else {
    Entry e{};
    AllocEntry(e);
    val_ops_.copy_init(e.data(), value_buf, val_ops_.ctx);
    entries_.emplace(key, std::move(e));
  }
}

auto AssocMap::Exists(const CanonKeyPayload& key) const -> bool {
  return entries_.contains(key);
}

void AssocMap::DeleteAll() {
  for (auto& [key, entry] : entries_) {
    DestroyEntry(entry);
  }
  entries_.clear();
}

void AssocMap::DeleteKey(const CanonKeyPayload& key) {
  auto it = entries_.find(key);
  if (it != entries_.end()) {
    DestroyEntry(it->second);
    entries_.erase(it);
  }
}

auto AssocMap::Size() const -> int64_t {
  return static_cast<int64_t>(entries_.size());
}

auto AssocMap::First() const -> std::optional<CanonKeyPayload> {
  if (entries_.empty()) return std::nullopt;
  return entries_.begin()->first;
}

auto AssocMap::Last() const -> std::optional<CanonKeyPayload> {
  if (entries_.empty()) return std::nullopt;
  return entries_.rbegin()->first;
}

auto AssocMap::Next(const CanonKeyPayload& current) const
    -> std::optional<CanonKeyPayload> {
  auto it = entries_.upper_bound(current);
  if (it == entries_.end()) return std::nullopt;
  return it->first;
}

auto AssocMap::Prev(const CanonKeyPayload& current) const
    -> std::optional<CanonKeyPayload> {
  auto it = entries_.lower_bound(current);
  if (it == entries_.begin()) return std::nullopt;
  --it;
  return it->first;
}

auto AssocMap::SnapshotKeys() const -> std::vector<CanonKeyPayload> {
  std::vector<CanonKeyPayload> keys;
  keys.reserve(entries_.size());
  for (const auto& [key, entry] : entries_) {
    keys.push_back(key);
  }
  return keys;
}

auto AssocMap::Clone() const -> AssocMap* {
  auto* copy = new AssocMap(key_spec_, val_ops_);
  for (const auto& [key, entry] : entries_) {
    Entry e{};
    copy->CopyInitEntry(e, entry);
    copy->entries_.emplace(key, std::move(e));
  }
  return copy;
}

}  // namespace lyra::runtime

// --- ValueOps factories ---

namespace {

// POD: memcpy/memset(0)/nop
void PodDefaultInit(void* dst, const void* ctx) {
  auto size = static_cast<uint32_t>(reinterpret_cast<uintptr_t>(ctx));
  std::memset(dst, 0, size);
}

void PodCopyInit(void* dst, const void* src, const void* ctx) {
  auto size = static_cast<uint32_t>(reinterpret_cast<uintptr_t>(ctx));
  std::memcpy(dst, src, size);
}

void PodCopyAssign(void* dst, const void* src, const void* ctx) {
  PodCopyInit(dst, src, ctx);
}

void PodDestroy(void* /*val*/, const void* /*ctx*/) {
}

// POD 4-state: value plane = 0, unknown plane = 0xFF (X encoding).
// ctx encodes total size in low 16 bits, value plane size in high 16 bits.
void Pod4StateDefaultInit(void* dst, const void* ctx) {
  auto encoded = static_cast<uint32_t>(reinterpret_cast<uintptr_t>(ctx));
  auto value_plane_size = encoded >> 16;
  auto total_size = encoded & 0xFFFF;
  std::memset(dst, 0, value_plane_size);
  std::memset(
      static_cast<uint8_t*>(dst) + value_plane_size, 0xFF,
      total_size - value_plane_size);
}

// String handle: retain/release
void StringDefaultInit(void* dst, const void* /*ctx*/) {
  *static_cast<void**>(dst) = nullptr;
}

void StringCopyInit(void* dst, const void* src, const void* /*ctx*/) {
  auto* handle = *static_cast<void* const*>(src);
  LyraStringRetain(handle);
  *static_cast<void**>(dst) = handle;
}

void StringCopyAssign(void* dst, const void* src, const void* /*ctx*/) {
  auto* old_handle = *static_cast<void**>(dst);
  auto* new_handle = *static_cast<void* const*>(src);
  LyraStringRetain(new_handle);
  *static_cast<void**>(dst) = new_handle;
  LyraStringRelease(old_handle);
}

void StringDestroy(void* val, const void* /*ctx*/) {
  auto* handle = *static_cast<void**>(val);
  LyraStringRelease(handle);
}

// DynArray/Queue handle: clone/release
void DynArrayDefaultInit(void* dst, const void* /*ctx*/) {
  *static_cast<void**>(dst) = nullptr;
}

void DynArrayCopyInit(void* dst, const void* src, const void* /*ctx*/) {
  auto* handle = *static_cast<void* const*>(src);
  *static_cast<void**>(dst) = LyraDynArrayClone(handle);
}

void DynArrayCopyAssign(void* dst, const void* src, const void* /*ctx*/) {
  auto* old_handle = *static_cast<void**>(dst);
  auto* new_handle = *static_cast<void* const*>(src);
  *static_cast<void**>(dst) = LyraDynArrayClone(new_handle);
  LyraDynArrayRelease(old_handle);
}

void DynArrayDestroy(void* val, const void* /*ctx*/) {
  auto* handle = *static_cast<void**>(val);
  LyraDynArrayRelease(handle);
}

// Assoc handle: clone/release
void AssocDefaultInit(void* dst, const void* /*ctx*/) {
  *static_cast<void**>(dst) = nullptr;
}

void AssocCopyInit(void* dst, const void* src, const void* /*ctx*/) {
  auto* handle = *static_cast<void* const*>(src);
  *static_cast<void**>(dst) = LyraAssocClone(handle);
}

void AssocCopyAssign(void* dst, const void* src, const void* /*ctx*/) {
  auto* old_handle = *static_cast<void**>(dst);
  auto* new_handle = *static_cast<void* const*>(src);
  *static_cast<void**>(dst) = LyraAssocClone(new_handle);
  LyraAssocRelease(old_handle);
}

void AssocDestroy(void* val, const void* /*ctx*/) {
  auto* handle = *static_cast<void**>(val);
  LyraAssocRelease(handle);
}

auto MakeValueOps(LyraAssocElemKind kind, uint32_t elem_size)
    -> lyra::runtime::ValueOps {
  switch (kind) {
    case kLyraAssocElemPod:
      return lyra::runtime::ValueOps{
          .value_size = elem_size,
          .value_align = std::min(elem_size, 8u),
          .default_init = PodDefaultInit,
          .copy_init = PodCopyInit,
          .copy_assign = PodCopyAssign,
          .destroy = PodDestroy,
          .ctx =
              reinterpret_cast<const void*>(static_cast<uintptr_t>(elem_size)),
      };
    case kLyraAssocElemString:
      return lyra::runtime::ValueOps{
          .value_size = sizeof(void*),
          .value_align = alignof(void*),
          .default_init = StringDefaultInit,
          .copy_init = StringCopyInit,
          .copy_assign = StringCopyAssign,
          .destroy = StringDestroy,
          .ctx = nullptr,
      };
    case kLyraAssocElemDynArray:
    case kLyraAssocElemQueue:
      return lyra::runtime::ValueOps{
          .value_size = sizeof(void*),
          .value_align = alignof(void*),
          .default_init = DynArrayDefaultInit,
          .copy_init = DynArrayCopyInit,
          .copy_assign = DynArrayCopyAssign,
          .destroy = DynArrayDestroy,
          .ctx = nullptr,
      };
    case kLyraAssocElemAssocArray:
      return lyra::runtime::ValueOps{
          .value_size = sizeof(void*),
          .value_align = alignof(void*),
          .default_init = AssocDefaultInit,
          .copy_init = AssocCopyInit,
          .copy_assign = AssocCopyAssign,
          .destroy = AssocDestroy,
          .ctx = nullptr,
      };
    case kLyraAssocElemPod4State: {
      // Encode value_plane_size in high 16 bits, total_size in low 16 bits.
      uint32_t value_plane_size = elem_size / 2;
      uint32_t encoded = (value_plane_size << 16) | elem_size;
      return lyra::runtime::ValueOps{
          .value_size = elem_size,
          .value_align = std::min(elem_size, 8u),
          .default_init = Pod4StateDefaultInit,
          .copy_init = PodCopyInit,
          .copy_assign = PodCopyAssign,
          .destroy = PodDestroy,
          .ctx = reinterpret_cast<const void*>(static_cast<uintptr_t>(encoded)),
      };
    }
  }
  return {};
}

// Snapshot: opaque handle owning vector of keys.
struct AssocSnapshot {
  std::vector<lyra::runtime::CanonKeyPayload> keys;
  lyra::runtime::KeySpec key_spec;
};

// Canonicalize key from uniform ABI parameters.
auto CanonFromAbi(
    lyra::runtime::AssocMap* map, void* key_a, void* key_b, uint32_t key_c)
    -> lyra::runtime::CanonResult {
  const auto& spec = map->GetKeySpec();
  if (spec.kind == lyra::runtime::KeySpec::Kind::kString) {
    return lyra::runtime::CanonicalizeKeyString(
        static_cast<const char*>(key_a), key_c);
  }
  return lyra::runtime::CanonicalizeKeyRaw(
      static_cast<const uint64_t*>(key_a), static_cast<const uint64_t*>(key_b),
      key_c, spec);
}

// Write key payload to output buffer for iteration.
void WriteKeyToOutput(
    const lyra::runtime::CanonKeyPayload& payload,
    const lyra::runtime::KeySpec& spec, void* out_key) {
  if (spec.kind == lyra::runtime::KeySpec::Kind::kString) {
    // Create a string handle and store it.
    auto* handle = LyraStringFromLiteral(
        payload.str_value.data(),
        static_cast<int64_t>(payload.str_value.size()));
    *static_cast<void**>(out_key) = handle;
  } else {
    uint32_t num_words = (spec.bit_width + 63) / 64;
    lyra::runtime::KeyPayloadToRaw(
        payload, spec, static_cast<uint64_t*>(out_key), num_words);
  }
}

}  // namespace

// --- Extern "C" ABI ---

auto LyraAssocNew(
    uint32_t key_kind, uint32_t key_bw, uint32_t key_signed,
    uint32_t elem_ops_kind, uint32_t elem_size) -> void* {
  lyra::runtime::KeySpec spec{
      .kind = static_cast<lyra::runtime::KeySpec::Kind>(key_kind),
      .bit_width = key_bw,
      .is_signed = key_signed != 0,
  };
  auto val_ops =
      MakeValueOps(static_cast<LyraAssocElemKind>(elem_ops_kind), elem_size);
  return new lyra::runtime::AssocMap(spec, val_ops);
}

void LyraAssocRelease(void* aa) {
  if (aa == nullptr) return;
  delete static_cast<lyra::runtime::AssocMap*>(aa);
}

auto LyraAssocClone(void* aa) -> void* {
  if (aa == nullptr) return nullptr;
  return static_cast<lyra::runtime::AssocMap*>(aa)->Clone();
}

int32_t LyraAssocGet(
    void* aa, void* key_a, void* key_b, uint32_t key_c, void* out_value,
    void* had_xz) {
  if (aa == nullptr) {
    // Null AA = miss. Caller must provide zeroed or uninitialized buffer.
    // We can't default-init without ValueOps. Return 0 (miss).
    return 0;
  }
  auto* map = static_cast<lyra::runtime::AssocMap*>(aa);
  auto result = CanonFromAbi(map, key_a, key_b, key_c);
  if (result.status == lyra::runtime::KeyStatus::kHasXZ) {
    if (had_xz != nullptr) *static_cast<uint8_t*>(had_xz) = 1;
    map->GetValueOps().default_init(out_value, map->GetValueOps().ctx);
    return 0;
  }
  return map->Get(result.payload, out_value) ? 1 : 0;
}

int32_t LyraAssocSet(
    void* aa, void* key_a, void* key_b, uint32_t key_c, void* value,
    void* had_xz) {
  if (aa == nullptr) return 0;
  auto* map = static_cast<lyra::runtime::AssocMap*>(aa);
  auto result = CanonFromAbi(map, key_a, key_b, key_c);
  if (result.status == lyra::runtime::KeyStatus::kHasXZ) {
    if (had_xz != nullptr) *static_cast<uint8_t*>(had_xz) = 1;
    return 0;
  }
  map->Set(result.payload, value);
  return 1;
}

int32_t LyraAssocExists(
    void* aa, void* key_a, void* key_b, uint32_t key_c, void* had_xz) {
  if (aa == nullptr) return 0;
  auto* map = static_cast<lyra::runtime::AssocMap*>(aa);
  auto result = CanonFromAbi(map, key_a, key_b, key_c);
  if (result.status == lyra::runtime::KeyStatus::kHasXZ) {
    if (had_xz != nullptr) *static_cast<uint8_t*>(had_xz) = 1;
    return 0;
  }
  return map->Exists(result.payload) ? 1 : 0;
}

int32_t LyraAssocDeleteKey(
    void* aa, void* key_a, void* key_b, uint32_t key_c, void* had_xz) {
  if (aa == nullptr) return 0;
  auto* map = static_cast<lyra::runtime::AssocMap*>(aa);
  auto result = CanonFromAbi(map, key_a, key_b, key_c);
  if (result.status == lyra::runtime::KeyStatus::kHasXZ) {
    if (had_xz != nullptr) *static_cast<uint8_t*>(had_xz) = 1;
    return 0;
  }
  map->DeleteKey(result.payload);
  return 1;
}

void LyraAssocDeleteAll(void* aa) {
  if (aa == nullptr) return;
  static_cast<lyra::runtime::AssocMap*>(aa)->DeleteAll();
}

auto LyraAssocSize(void* aa) -> int64_t {
  if (aa == nullptr) return 0;
  return static_cast<lyra::runtime::AssocMap*>(aa)->Size();
}

int32_t LyraAssocFirst(void* aa, void* out_key) {
  if (aa == nullptr) return 0;
  auto* map = static_cast<lyra::runtime::AssocMap*>(aa);
  auto key = map->First();
  if (!key) return 0;
  WriteKeyToOutput(*key, map->GetKeySpec(), out_key);
  return 1;
}

int32_t LyraAssocLast(void* aa, void* out_key) {
  if (aa == nullptr) return 0;
  auto* map = static_cast<lyra::runtime::AssocMap*>(aa);
  auto key = map->Last();
  if (!key) return 0;
  WriteKeyToOutput(*key, map->GetKeySpec(), out_key);
  return 1;
}

int32_t LyraAssocNext(void* aa, void* key_inout) {
  if (aa == nullptr) return 0;
  auto* map = static_cast<lyra::runtime::AssocMap*>(aa);
  const auto& spec = map->GetKeySpec();

  // Read current key from buffer
  lyra::runtime::CanonKeyPayload current;
  if (spec.kind == lyra::runtime::KeySpec::Kind::kString) {
    auto* handle = *static_cast<void**>(key_inout);
    const char* ptr = nullptr;
    uint64_t len = 0;
    LyraStringGetView(handle, &ptr, &len);
    current.str_value = std::string(ptr, static_cast<size_t>(len));
  } else {
    uint32_t num_words = (spec.bit_width + 63) / 64;
    auto* words = static_cast<uint64_t*>(key_inout);
    current.int_words.assign(words, words + num_words);
  }

  auto next = map->Next(current);
  if (!next) return 0;
  // Caller handles releasing old string handle via CommitValue ownership.
  WriteKeyToOutput(*next, spec, key_inout);
  return 1;
}

int32_t LyraAssocPrev(void* aa, void* key_inout) {
  if (aa == nullptr) return 0;
  auto* map = static_cast<lyra::runtime::AssocMap*>(aa);
  const auto& spec = map->GetKeySpec();

  lyra::runtime::CanonKeyPayload current;
  if (spec.kind == lyra::runtime::KeySpec::Kind::kString) {
    auto* handle = *static_cast<void**>(key_inout);
    const char* ptr = nullptr;
    uint64_t len = 0;
    LyraStringGetView(handle, &ptr, &len);
    current.str_value = std::string(ptr, static_cast<size_t>(len));
  } else {
    uint32_t num_words = (spec.bit_width + 63) / 64;
    auto* words = static_cast<uint64_t*>(key_inout);
    current.int_words.assign(words, words + num_words);
  }

  auto prev = map->Prev(current);
  if (!prev) return 0;
  // Caller handles releasing old string handle via CommitValue ownership.
  WriteKeyToOutput(*prev, spec, key_inout);
  return 1;
}

auto LyraAssocSnapshotCreate(void* aa) -> void* {
  if (aa == nullptr) {
    return new AssocSnapshot{{}, {}};
  }
  auto* map = static_cast<lyra::runtime::AssocMap*>(aa);
  return new AssocSnapshot{map->SnapshotKeys(), map->GetKeySpec()};
}

auto LyraAssocSnapshotSize(void* snap) -> int64_t {
  if (snap == nullptr) return 0;
  return static_cast<int64_t>(static_cast<AssocSnapshot*>(snap)->keys.size());
}

void LyraAssocSnapshotKeyAt(void* snap, int64_t index, void* out_key) {
  auto* s = static_cast<AssocSnapshot*>(snap);
  WriteKeyToOutput(s->keys[static_cast<size_t>(index)], s->key_spec, out_key);
}

void LyraAssocSnapshotRelease(void* snap) {
  delete static_cast<AssocSnapshot*>(snap);
}

void LyraAssocCloneElem(void* dst, const void* src) {
  auto* handle = *static_cast<void* const*>(src);
  *static_cast<void**>(dst) = LyraAssocClone(handle);
}

void LyraAssocDestroyElem(void* elem) {
  auto* handle = *static_cast<void**>(elem);
  LyraAssocRelease(handle);
}
