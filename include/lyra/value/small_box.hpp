#pragma once

#include <concepts>
#include <memory>
#include <type_traits>
#include <utility>

#include "lyra/value/frame_bound.hpp"

namespace lyra::value {

// One value of `T`, kept inline in whatever holds the box where a value of `T`
// fits a frame and in storage the box owns past that, so a holder that lies in
// a frame never takes a value too large for one, and one that holds a small
// value pays no allocation for it. It holds its value from the moment it is
// built, is moved and never copied, and what was moved from is good only for
// being ended.
template <typename T>
class SmallBox {
 public:
  template <typename... Args>
    requires std::constructible_from<T, Args...>
  explicit SmallBox(Args&&... args)
      : held_([&] {
          if constexpr (kInline) {
            return T(std::forward<Args>(args)...);
          } else {
            return std::make_unique<T>(std::forward<Args>(args)...);
          }
        }()) {
  }
  SmallBox(const SmallBox&) = delete;
  auto operator=(const SmallBox&) -> SmallBox& = delete;
  SmallBox(SmallBox&&) noexcept = default;
  auto operator=(SmallBox&&) noexcept -> SmallBox& = default;
  ~SmallBox() = default;

  [[nodiscard]] auto operator*() const -> const T& {
    if constexpr (kInline) {
      return held_;
    } else {
      return *held_;
    }
  }
  [[nodiscard]] auto operator*() -> T& {
    if constexpr (kInline) {
      return held_;
    } else {
      return *held_;
    }
  }
  [[nodiscard]] auto operator->() const -> const T* {
    return &**this;
  }
  [[nodiscard]] auto operator->() -> T* {
    return &**this;
  }

 private:
  static constexpr bool kInline = sizeof(T) <= kMaxInlineBytes;

  std::conditional_t<kInline, T, std::unique_ptr<T>> held_;
};

}  // namespace lyra::value
