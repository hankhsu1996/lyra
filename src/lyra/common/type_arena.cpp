#include "lyra/common/type_arena.hpp"

#include <algorithm>
#include <cstddef>
#include <new>
#include <utility>

#include "lyra/common/type.hpp"

namespace lyra::common {

TypeArena::TypeArena(size_t chunk_size) : chunk_size_(chunk_size) {
  AddChunk(chunk_size_);
}

TypeArena::~TypeArena() {
  // Bulk free all chunks. Type destructors are intentionally not called -
  // types are immutable and the arena lives for the entire compilation.
  for (auto& chunk : chunks_) {
    // NOLINTNEXTLINE(cppcoreguidelines-no-malloc,cppcoreguidelines-owning-memory)
    ::operator delete[](chunk.mem, std::align_val_t{alignof(std::max_align_t)});
  }
}

auto TypeArena::Intern(Type type) -> const Type* {
  size_t hash = type.Hash();

  // Check for existing type with same hash
  auto it = interned_.find(hash);
  if (it != interned_.end()) {
    for (const Type* existing : it->second) {
      if (*existing == type) {
        return existing;
      }
    }
  }

  // Allocate new type in arena
  void* mem = Allocate(sizeof(Type), alignof(Type));
  // NOLINTNEXTLINE(cppcoreguidelines-owning-memory)
  auto* ptr = new (mem) Type(std::move(type));

  interned_[hash].push_back(ptr);
  return ptr;
}

auto TypeArena::Allocate(size_t size, size_t alignment) -> void* {
  // Align the current position
  size_t aligned = (offset_ + alignment - 1) & ~(alignment - 1);

  if (aligned + size > chunks_[current_].size) {
    // Need new chunk
    size_t next_size = std::max(chunk_size_, size);
    AddChunk(next_size);
    aligned = 0;
  }

  // NOLINTNEXTLINE(cppcoreguidelines-pro-bounds-pointer-arithmetic)
  void* ptr = chunks_[current_].mem + aligned;
  offset_ = aligned + size;
  return ptr;
}

void TypeArena::AddChunk(size_t size) {
  // NOLINTNEXTLINE(cppcoreguidelines-no-malloc,cppcoreguidelines-owning-memory)
  auto* mem = static_cast<std::byte*>(
      ::operator new[](size, std::align_val_t{alignof(std::max_align_t)}));
  chunks_.push_back({mem, size});
  current_ = chunks_.size() - 1;
  offset_ = 0;
}

}  // namespace lyra::common
