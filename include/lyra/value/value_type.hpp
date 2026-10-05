#pragma once

#include <array>
#include <bit>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <new>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

template <typename Host>
class RealValue;
class Chandle;

// The five ways LRM 7.12.3 folds an array's values into one.
enum class Reduction : std::uint8_t { kSum, kProduct, kAnd, kOr, kXor };

// Two values of `T` folded by `reduction`. A reduction the type does not define
// -- a bitwise one over a real -- is one the front end does not admit.
template <typename T>
[[nodiscard]] auto Reduced(Reduction reduction, const T& a, const T& b) -> T {
  switch (reduction) {
    case Reduction::kSum:
      if constexpr (requires {
                      { a + b } -> std::same_as<T>;
                    }) {
        return a + b;
      }
      break;
    case Reduction::kProduct:
      if constexpr (requires {
                      { a * b } -> std::same_as<T>;
                    }) {
        return a * b;
      }
      break;
    case Reduction::kAnd:
      if constexpr (requires {
                      { a & b } -> std::same_as<T>;
                    }) {
        return a & b;
      }
      break;
    case Reduction::kOr:
      if constexpr (requires {
                      { a | b } -> std::same_as<T>;
                    }) {
        return a | b;
      }
      break;
    case Reduction::kXor:
      if constexpr (requires {
                      { a ^ b } -> std::same_as<T>;
                    }) {
        return a ^ b;
      }
      break;
  }
  throw InternalError(
      "value type: an LRM 7.12.3 reduction is asked of a type that does not "
      "define it -- please report this as a bug");
}

// What code compiled without knowing a type needs of a value of it: how much
// storage the value takes, what the storage's own lifecycle is, and the
// operations the language defines on the whole value (LRM 11.4.5, 20.6.2, 20.9,
// 6.24.3, 6.6.1, 28.12.1) or that a container's algorithms ask of an element
// or of what a `with` clause answers (LRM 7.8, 7.12, 12.4). A library compiled
// once, before any design existed, holds a value of a type the design declares
// as its bytes together with this, the way Swift's generic code holds a value
// with its type's value witness table and Rust's `dyn` reference holds one with
// its vtable.
//
// A value's type is exact, since nothing in the language makes the type a value
// has differ from the type it is held as, so whoever holds the value states its
// type once and the value carries none of its own. The one exception is a
// tuple, whose bytes open with its type, because the cells, references and
// nets that hold one do not state it.
//
// Every value is passed as its address. Copying and moving build in `out` a
// value the source still has to end; `Assign` writes into a value already
// there, which goes on being that value. An operation answering a value builds
// it in `out`. One the language does not define for the type is never asked of
// it, since the front end rejects the program asking.
class ValueType {
 public:
  ValueType(const ValueType&) = delete;
  auto operator=(const ValueType&) -> ValueType& = delete;
  ValueType(ValueType&&) = delete;
  auto operator=(ValueType&&) -> ValueType& = delete;
  virtual ~ValueType();

  [[nodiscard]] auto Size() const -> std::size_t {
    return size_;
  }
  [[nodiscard]] auto Align() const -> std::size_t {
    return align_;
  }

  virtual void Copy(const void* value, void* out) const = 0;
  virtual void Move(void* value, void* out) const noexcept = 0;
  virtual void Destroy(void* value) const noexcept = 0;
  virtual void Assign(void* storage, const void* value) const = 0;

  virtual void Equal(const void* lhs, const void* rhs, void* out) const = 0;
  virtual void CaseEqual(const void* lhs, const void* rhs, void* out) const = 0;
  [[nodiscard]] virtual auto BitIdentical(
      const void* lhs, const void* rhs) const -> bool = 0;
  [[nodiscard]] virtual auto HasUnknown(const void* value) const -> bool = 0;

  virtual void BitstreamWidth(const void* value, void* out) const = 0;
  virtual void CountBits(
      const void* value, const void* control_bits, void* out) const = 0;
  virtual void ToBitstream(const void* value, void* out) const = 0;
  virtual void FromBitstream(
      const void* bits, const void* prototype, void* out) const = 0;

  virtual void ResolveTriState(
      const void* lhs, const void* rhs, void* out) const = 0;
  virtual void ResolveWiredAnd(
      const void* lhs, const void* rhs, void* out) const = 0;
  virtual void ResolveWiredOr(
      const void* lhs, const void* rhs, void* out) const = 0;
  virtual void Dominating(
      const void* stronger, const void* weaker, void* out) const = 0;
  virtual void FilledLike(
      const void* prototype, const void* fill, void* out) const = 0;

  // The order an associative array keeps its indices in (LRM 7.8.2, 7.8.4)
  // and the one `min`, `max` and `sort` read (LRM 7.12).
  [[nodiscard]] virtual auto OrderBefore(const void* lhs, const void* rhs) const
      -> bool = 0;
  // Whether a value holds as a condition (LRM 12.4).
  [[nodiscard]] virtual auto IsTrue(const void* value) const -> bool = 0;
  virtual void Reduce(
      Reduction reduction, const void* lhs, const void* rhs,
      void* out) const = 0;

  // A value whose parts are ordered by position -- a fixed-size array, a
  // dynamic array, a queue (LRM 7.4, 7.5, 7.10): how many it holds, the type
  // they are, and where the one at storage position `position` lies, for
  // reading and as storage a write lands in. What walks a value down to its
  // leaves -- imaging it for a foreign call (LRM H.12), loading a memory file
  // into it (LRM 21.4) -- asks these of the type at each level.
  [[nodiscard]] virtual auto PartCount(const void* value) const
      -> std::size_t = 0;
  [[nodiscard]] virtual auto PartType(const void* value) const
      -> const ValueType& = 0;
  [[nodiscard]] virtual auto PartAt(
      const void* value, std::size_t position) const -> const void* = 0;
  [[nodiscard]] virtual auto PartRefAt(void* value, std::size_t position) const
      -> void* = 0;

 protected:
  constexpr ValueType(std::uint32_t size, std::uint32_t align)
      : size_(size), align_(align) {
  }

 private:
  std::uint32_t size_;
  std::uint32_t align_;
};

// The value of `T` that `build` lays out in the storage it is handed, taken out
// of that storage: how code holding a `T` reads what an operation answers in
// storage it was given.
template <typename T, typename Build>
[[nodiscard]] auto TakeBuilt(Build build) -> T {
  alignas(T) std::array<std::byte, sizeof(T)> storage{};
  build(static_cast<void*>(storage.data()));
  T* built = std::launder(std::bit_cast<T*>(storage.data()));
  T answer = std::move(*built);
  std::destroy_at(built);
  return answer;
}

namespace detail {

[[noreturn]] inline void LacksOperation() {
  throw InternalError(
      "value type: an operation is asked of a type the language does not "
      "define it for -- please report this as a bug");
}

// LRM 6.24.3: a real and a chandle are the kinds that hold no stream of bits,
// so a streaming conversion of one is a program the front end rejected. Every
// other kind is one a program may stream.
template <typename T>
inline constexpr bool kIsBitStreamType = true;
template <typename Host>
inline constexpr bool kIsBitStreamType<RealValue<Host>> = false;
template <>
inline constexpr bool kIsBitStreamType<Chandle> = false;

// A value of `T` holds parts ordered by position, each a value of a type the
// value states and handed out where it lies.
template <typename T>
concept HoldsParts = requires(const T& c, T& m, std::size_t position) {
  { c.Count() } -> std::same_as<std::size_t>;
  { c.ElementType() } -> std::same_as<const ValueType&>;
  { c.ElementAt(position) } -> std::same_as<const void*>;
  { m.ElementAt(position) } -> std::same_as<void*>;
};

// A question about a value's stream of bits the language defines for a kind
// and Lyra does not yet answer, which a legal program can ask.
[[noreturn]] inline void StreamNotYetSupported() {
  throw SimulationError(
      "reading this value as a stream of bits, or building one from it, is "
      "not yet supported on this backend; please open an issue asking for "
      "support");
}

}  // namespace detail

// The type `T` is, answering each operation with what `T` itself does: C++
// states a type's operations as its own members, so the table is generated
// from them rather than written.
template <LyraValue T>
class ValueTypeOf final : public ValueType {
 public:
  constexpr ValueTypeOf()
      : ValueType(
            static_cast<std::uint32_t>(sizeof(T)),
            static_cast<std::uint32_t>(alignof(T))) {
  }

  // Defined beside the library's instances of this class, which are the only
  // ones: the order of an index type is stated with the associative array.
  [[nodiscard]] auto OrderBefore(const void* lhs, const void* rhs) const
      -> bool override;
  [[nodiscard]] auto IsTrue(const void* value) const -> bool override {
    if constexpr (requires { Of(value).IsTruthy(); }) {
      return Of(value).IsTruthy();
    } else {
      detail::LacksOperation();
    }
  }
  void Reduce(Reduction reduction, const void* lhs, const void* rhs, void* out)
      const override {
    Answer(out, Reduced(reduction, Of(lhs), Of(rhs)));
  }

  [[nodiscard]] auto PartCount(const void* value) const
      -> std::size_t override {
    if constexpr (detail::HoldsParts<T>) {
      return Of(value).Count();
    } else {
      detail::LacksOperation();
    }
  }
  [[nodiscard]] auto PartType(const void* value) const
      -> const ValueType& override {
    if constexpr (detail::HoldsParts<T>) {
      return Of(value).ElementType();
    } else {
      detail::LacksOperation();
    }
  }
  [[nodiscard]] auto PartAt(const void* value, std::size_t position) const
      -> const void* override {
    if constexpr (detail::HoldsParts<T>) {
      return Of(value).ElementAt(position);
    } else {
      detail::LacksOperation();
    }
  }
  [[nodiscard]] auto PartRefAt(void* value, std::size_t position) const
      -> void* override {
    if constexpr (detail::HoldsParts<T>) {
      return static_cast<T*>(value)->ElementAt(position);
    } else {
      detail::LacksOperation();
    }
  }

  void Copy(const void* value, void* out) const override {
    std::construct_at(static_cast<T*>(out), Of(value));
  }
  void Move(void* value, void* out) const noexcept override {
    std::construct_at(static_cast<T*>(out), std::move(*static_cast<T*>(value)));
  }
  void Destroy(void* value) const noexcept override {
    std::destroy_at(static_cast<T*>(value));
  }
  void Assign(void* storage, const void* value) const override {
    *static_cast<T*>(storage) = Of(value);
  }

  void Equal(const void* lhs, const void* rhs, void* out) const override {
    Answer(out, Of(lhs) == Of(rhs));
  }
  void CaseEqual(const void* lhs, const void* rhs, void* out) const override {
    if constexpr (CaseEqualComparable<T>) {
      Answer(out, Of(lhs).CaseEqual(Of(rhs)));
    } else {
      detail::LacksOperation();
    }
  }
  [[nodiscard]] auto BitIdentical(const void* lhs, const void* rhs) const
      -> bool override {
    return Of(lhs).IsBitIdentical(Of(rhs));
  }
  [[nodiscard]] auto HasUnknown(const void* value) const -> bool override {
    return Of(value).HasUnknown();
  }

  void BitstreamWidth(const void* value, void* out) const override {
    if constexpr (BitstreamSizable<T>) {
      Answer(out, Of(value).BitstreamWidth());
    } else if constexpr (detail::kIsBitStreamType<T>) {
      detail::StreamNotYetSupported();
    } else {
      detail::LacksOperation();
    }
  }
  void CountBits(
      const void* value, const void* control_bits, void* out) const override {
    if constexpr (BitstreamSizable<T>) {
      Answer(
          out,
          Of(value).CountBits(*static_cast<const PackedArray*>(control_bits)));
    } else if constexpr (detail::kIsBitStreamType<T>) {
      detail::StreamNotYetSupported();
    } else {
      detail::LacksOperation();
    }
  }
  void ToBitstream(const void* value, void* out) const override {
    if constexpr (BitstreamConvertible<T>) {
      Answer(out, Of(value).ToBitstream());
    } else if constexpr (detail::kIsBitStreamType<T>) {
      detail::StreamNotYetSupported();
    } else {
      detail::LacksOperation();
    }
  }
  void FromBitstream(
      const void* bits, const void* prototype, void* out) const override {
    if constexpr (BitstreamConvertible<T>) {
      Answer(
          out, T::FromBitstream(
                   *static_cast<const PackedArray*>(bits), Of(prototype)));
    } else if constexpr (detail::kIsBitStreamType<T>) {
      detail::StreamNotYetSupported();
    } else {
      detail::LacksOperation();
    }
  }

  void ResolveTriState(
      const void* lhs, const void* rhs, void* out) const override {
    if constexpr (NetResolvable<T>) {
      Answer(out, Of(lhs).ResolveTriState(Of(rhs)));
    } else {
      detail::LacksOperation();
    }
  }
  void ResolveWiredAnd(
      const void* lhs, const void* rhs, void* out) const override {
    if constexpr (NetResolvable<T>) {
      Answer(out, Of(lhs).ResolveWiredAnd(Of(rhs)));
    } else {
      detail::LacksOperation();
    }
  }
  void ResolveWiredOr(
      const void* lhs, const void* rhs, void* out) const override {
    if constexpr (NetResolvable<T>) {
      Answer(out, Of(lhs).ResolveWiredOr(Of(rhs)));
    } else {
      detail::LacksOperation();
    }
  }
  void Dominating(
      const void* stronger, const void* weaker, void* out) const override {
    if constexpr (NetResolvable<T>) {
      Answer(out, Of(stronger).Dominating(Of(weaker)));
    } else {
      detail::LacksOperation();
    }
  }
  void FilledLike(
      const void* prototype, const void* fill, void* out) const override {
    if constexpr (NetResolvable<T>) {
      Answer(
          out,
          T::FilledLike(Of(prototype), *static_cast<const PackedArray*>(fill)));
    } else {
      detail::LacksOperation();
    }
  }

 protected:
  [[nodiscard]] static auto Of(const void* value) -> const T& {
    return *static_cast<const T*>(value);
  }

  template <typename Answered>
  static void Answer(void* out, Answered answered) {
    std::construct_at(static_cast<Answered*>(out), std::move(answered));
  }
};

}  // namespace lyra::value
