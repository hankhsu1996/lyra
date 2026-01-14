#pragma once

#include <cstddef>
#include <unordered_map>
#include <vector>

#include "lyra/common/type.hpp"

namespace lyra::common {

/// Arena allocator for Type instances with structural interning.
///
/// Provides bump allocation semantics: types are allocated from contiguous
/// memory blocks, never individually freed, and bulk-freed when the arena
/// is destroyed. Type destructors are not called - the arena assumes types
/// are immutable and live for the entire compilation.
///
/// Structurally identical types are deduplicated via interning, enabling
/// pointer-based type identity comparison.
///
/// Lifetime: The arena must outlive all pointers returned by Intern().
class TypeArena {
 public:
  explicit TypeArena(size_t chunk_size = 16 * 1024);
  ~TypeArena();

  TypeArena(const TypeArena&) = delete;
  auto operator=(const TypeArena&) -> TypeArena& = delete;
  TypeArena(TypeArena&&) noexcept = default;
  auto operator=(TypeArena&&) noexcept -> TypeArena& = default;

  /// Intern a type, returning a stable pointer to the canonical instance.
  /// If a structurally identical type already exists, returns that pointer.
  /// Otherwise, allocates and stores the type in the arena.
  auto Intern(Type type) -> const Type*;

 private:
  struct Chunk {
    std::byte* mem;
    size_t size;
  };

  std::vector<Chunk> chunks_;
  size_t chunk_size_;
  size_t current_ = 0;
  size_t offset_ = 0;

  // Interning: hash -> list of types with that hash (for collision handling)
  std::unordered_map<size_t, std::vector<const Type*>> interned_;

  auto Allocate(size_t size, size_t alignment) -> void*;
  void AddChunk(size_t size);
};

}  // namespace lyra::common
