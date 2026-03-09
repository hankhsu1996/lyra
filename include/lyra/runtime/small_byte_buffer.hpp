#pragma once

#include <cstdint>

namespace lyra::runtime {

// Move-only owned byte blob with small-buffer optimization.
// Inline storage avoids heap allocation for typical small NBA payloads (<=16
// bytes). Not a container -- no reserve/growth API. Just a fixed owned blob.
class SmallByteBuffer {
  static constexpr uint32_t kInlineCap = 16;

  uint32_t size_ = 0;
  bool on_heap_ = false;

  // NOLINTBEGIN(cppcoreguidelines-avoid-c-arrays)
  union Storage {
    uint8_t inline_buf[kInlineCap];
    uint8_t* heap;
  };
  // NOLINTEND(cppcoreguidelines-avoid-c-arrays)
  Storage storage_{};

 public:
  SmallByteBuffer() noexcept = default;
  ~SmallByteBuffer();

  SmallByteBuffer(SmallByteBuffer&& other) noexcept;
  auto operator=(SmallByteBuffer&& other) noexcept -> SmallByteBuffer&;

  SmallByteBuffer(const SmallByteBuffer&) = delete;
  auto operator=(const SmallByteBuffer&) -> SmallByteBuffer& = delete;

  // Copy size bytes from src into the buffer, replacing any previous content.
  void AssignCopy(const void* src, uint32_t size);

  // Release storage and reset to empty.
  void Clear() noexcept;

  [[nodiscard]] auto Data() const -> const uint8_t*;
  [[nodiscard]] auto Data() -> uint8_t*;
  [[nodiscard]] auto Size() const -> uint32_t {
    return size_;
  }
  [[nodiscard]] auto Empty() const -> bool {
    return size_ == 0;
  }
};

}  // namespace lyra::runtime
