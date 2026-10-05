#pragma once

#include <cstddef>
#include <cstdint>
#include <memory>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"

namespace lyra::value {

// What code compiled without knowing a type needs of a value of it: how much
// storage the value takes, what the storage's own lifecycle is, and the
// operations the language defines on the whole value (LRM 11.4.5, 20.6.2, 20.9,
// 6.24.3, 6.6.1, 28.12.1). A library compiled once, before any design existed,
// holds a value of a type the design declares as its bytes together with this,
// the way Swift's generic code holds a value with its type's value witness
// table and Rust's `dyn` reference holds one with its vtable.
//
// The value never carries it: a value's type is exact, since nothing in the
// language makes the type a value has differ from the type it is held as, so
// whoever holds the value states its type once.
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

 protected:
  constexpr ValueType(std::uint32_t size, std::uint32_t align)
      : size_(size), align_(align) {
  }

 private:
  std::uint32_t size_;
  std::uint32_t align_;
};

namespace detail {

[[noreturn]] inline void LacksOperation() {
  throw InternalError(
      "value type: an operation is asked of a type the language does not "
      "define it for -- please report this as a bug");
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
    } else {
      detail::LacksOperation();
    }
  }
  void ToBitstream(const void* value, void* out) const override {
    if constexpr (BitstreamConvertible<T>) {
      Answer(out, Of(value).ToBitstream());
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

 private:
  [[nodiscard]] static auto Of(const void* value) -> const T& {
    return *static_cast<const T*>(value);
  }

  template <typename Answered>
  static void Answer(void* out, Answered answered) {
    std::construct_at(static_cast<Answered*>(out), std::move(answered));
  }
};

}  // namespace lyra::value
