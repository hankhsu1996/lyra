#pragma once

#include <concepts>
#include <memory>
#include <type_traits>
#include <utility>

#include "lyra/support/layout.hpp"
#include "lyra/value/frame_bound.hpp"

namespace lyra::value {

// One value of `T`, kept inline in whatever holds the box where a value of `T`
// lies inline by the rule every holder reads and in storage the box owns past
// that, so a holder that lies in a frame never takes a value too large for
// one, and one that holds a small value pays no allocation for it. It holds its
// value from the moment it is built, is moved and never copied, and what was
// moved from is good only for being ended.
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
  // A box holding what `make` answers, made where the box keeps it.
  template <typename Make>
  [[nodiscard]] static auto BuiltBy(Make make) -> SmallBox {
    return SmallBox(MadeBy<Make>{.make = &make});
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
  static constexpr bool kInline =
      LiesInline(support::Layout{.size = sizeof(T), .align = alignof(T)});

  // What builds the value a box made by its factory holds.
  template <typename Make>
  struct MadeBy {
    Make* make;
  };
  template <typename Make>
  explicit SmallBox(MadeBy<Make> made)
      : held_([&] {
          if constexpr (kInline) {
            return (*made.make)();
          } else {
            return std::unique_ptr<T>(new T((*made.make)()));
          }
        }()) {
  }

  std::conditional_t<kInline, T, std::unique_ptr<T>> held_;
};

}  // namespace lyra::value
