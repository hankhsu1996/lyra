#pragma once

#include <cstdint>
#include <map>
#include <optional>
#include <vector>

#include "lyra/runtime/assoc_array.hpp"

namespace lyra::runtime {

// Element operations for type-erased value storage.
// All callbacks receive an opaque ctx pointer for type-specific behavior.
struct ValueOps {
  uint32_t value_size;
  uint32_t value_align;
  void (*default_init)(void* dst, const void* ctx);
  void (*copy_init)(void* dst, const void* src, const void* ctx);
  void (*copy_assign)(void* dst, const void* src, const void* ctx);
  void (*destroy)(void* val, const void* ctx);
  const void* ctx;
};

class AssocMap {
 public:
  AssocMap(KeySpec key_spec, ValueOps val_ops);
  ~AssocMap();

  AssocMap(const AssocMap&) = delete;
  auto operator=(const AssocMap&) -> AssocMap& = delete;

  // Contract: out_buf MUST point to uninitialized storage.
  // On hit: copy_init(out_buf, entry). On miss: default_init(out_buf).
  // Returns true if key was found.
  auto Get(const CanonKeyPayload& key, void* out_buf) -> bool;

  // copy_assign if key exists, copy_init if new entry.
  void Set(const CanonKeyPayload& key, const void* value_buf);

  [[nodiscard]] auto Exists(const CanonKeyPayload& key) const -> bool;
  void DeleteAll();
  void DeleteKey(const CanonKeyPayload& key);
  [[nodiscard]] auto Size() const -> int64_t;

  [[nodiscard]] auto First() const -> std::optional<CanonKeyPayload>;
  [[nodiscard]] auto Last() const -> std::optional<CanonKeyPayload>;
  [[nodiscard]] auto Next(const CanonKeyPayload& current) const
      -> std::optional<CanonKeyPayload>;
  [[nodiscard]] auto Prev(const CanonKeyPayload& current) const
      -> std::optional<CanonKeyPayload>;

  [[nodiscard]] auto SnapshotKeys() const -> std::vector<CanonKeyPayload>;
  [[nodiscard]] auto Clone() const -> AssocMap*;

  [[nodiscard]] auto GetKeySpec() const -> const KeySpec& {
    return key_spec_;
  }
  [[nodiscard]] auto GetValueOps() const -> const ValueOps& {
    return val_ops_;
  }

  // SBO threshold for inline storage.
  static constexpr uint32_t kInlineSize = 24;

 private:
  KeySpec key_spec_;
  ValueOps val_ops_;

  struct Entry {
    alignas(8) union {
      uint8_t inline_data[kInlineSize];
      uint8_t* heap_data;
    };
    bool on_heap;
    auto data() -> uint8_t*;
    auto data() const -> const uint8_t*;
  };
  using Map = std::map<CanonKeyPayload, Entry, CanonKeyLess>;
  Map entries_;

  void AllocEntry(Entry& e);
  void DestroyEntry(Entry& e);
  void CopyInitEntry(Entry& dst, const Entry& src);
};

}  // namespace lyra::runtime

// Element ops kind enum for extern "C" ABI.
// Runtime constructs ValueOps internally based on this + elem_size.
enum LyraAssocElemKind : uint32_t {
  kLyraAssocElemPod = 0,
  kLyraAssocElemString = 1,
  kLyraAssocElemDynArray = 2,
  kLyraAssocElemQueue = 3,
  kLyraAssocElemAssocArray = 4,
  kLyraAssocElemPod4State = 5,
};

extern "C" {

auto LyraAssocNew(
    uint32_t key_kind, uint32_t key_bw, uint32_t key_signed,
    uint32_t elem_ops_kind, uint32_t elem_size) -> void*;
void LyraAssocRelease(void* aa);
auto LyraAssocClone(void* aa) -> void*;

int32_t LyraAssocGet(
    void* aa, void* key_a, void* key_b, uint32_t key_c, void* out_value,
    void* had_xz);
int32_t LyraAssocSet(
    void* aa, void* key_a, void* key_b, uint32_t key_c, void* value,
    void* had_xz);
int32_t LyraAssocExists(
    void* aa, void* key_a, void* key_b, uint32_t key_c, void* had_xz);
int32_t LyraAssocDeleteKey(
    void* aa, void* key_a, void* key_b, uint32_t key_c, void* had_xz);
void LyraAssocDeleteAll(void* aa);
auto LyraAssocSize(void* aa) -> int64_t;

int32_t LyraAssocFirst(void* aa, void* out_key);
int32_t LyraAssocLast(void* aa, void* out_key);
int32_t LyraAssocNext(void* aa, void* key_inout);
int32_t LyraAssocPrev(void* aa, void* key_inout);

auto LyraAssocSnapshotCreate(void* aa) -> void*;
auto LyraAssocSnapshotSize(void* snap) -> int64_t;
void LyraAssocSnapshotKeyAt(void* snap, int64_t index, void* out_key);
void LyraAssocSnapshotRelease(void* snap);

void LyraAssocCloneElem(void* dst, const void* src);
void LyraAssocDestroyElem(void* elem);
}
