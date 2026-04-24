#include "lyra/runtime/small_byte_buffer.hpp"

#include <cstring>

namespace lyra::runtime {

// NOLINTBEGIN(cppcoreguidelines-pro-type-union-access,cppcoreguidelines-owning-memory)

SmallByteBuffer::~SmallByteBuffer() {
  if (on_heap_) {
    delete[] storage_.heap;
  }
}

SmallByteBuffer::SmallByteBuffer(SmallByteBuffer&& other) noexcept
    : size_(other.size_), on_heap_(other.on_heap_) {
  if (on_heap_) {
    storage_.heap = other.storage_.heap;
  } else {
    std::memcpy(
        storage_.inline_buf.data(), other.storage_.inline_buf.data(), size_);
  }
  other.size_ = 0;
  other.on_heap_ = false;
}

auto SmallByteBuffer::operator=(SmallByteBuffer&& other) noexcept
    -> SmallByteBuffer& {
  if (this == &other) return *this;

  if (on_heap_) {
    delete[] storage_.heap;
  }

  size_ = other.size_;
  on_heap_ = other.on_heap_;
  if (on_heap_) {
    storage_.heap = other.storage_.heap;
  } else {
    std::memcpy(
        storage_.inline_buf.data(), other.storage_.inline_buf.data(), size_);
  }
  other.size_ = 0;
  other.on_heap_ = false;
  return *this;
}

void SmallByteBuffer::AssignCopy(const void* src, uint32_t size) {
  if (on_heap_) {
    delete[] storage_.heap;
    on_heap_ = false;
  }

  size_ = size;
  if (size <= kInlineCap) {
    std::memcpy(storage_.inline_buf.data(), src, size);
  } else {
    storage_.heap = new uint8_t[size];
    std::memcpy(storage_.heap, src, size);
    on_heap_ = true;
  }
}

void SmallByteBuffer::Clear() noexcept {
  if (on_heap_) {
    delete[] storage_.heap;
    on_heap_ = false;
  }
  size_ = 0;
}

auto SmallByteBuffer::Data() const -> const uint8_t* {
  return on_heap_ ? storage_.heap : storage_.inline_buf.data();
}

auto SmallByteBuffer::Data() -> uint8_t* {
  return on_heap_ ? storage_.heap : storage_.inline_buf.data();
}

// NOLINTEND(cppcoreguidelines-pro-type-union-access,cppcoreguidelines-owning-memory)

}  // namespace lyra::runtime
