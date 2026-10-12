#pragma once

#include <bit>
#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>

#include "lyra/value/integral_words.hpp"
#include "lyra/value/reduction.hpp"

namespace lyra::value {

class FixedArrayType;
class IntegralValueType;
class ValueType;

// What a value whose parts are ordered by position -- a fixed-size array, a
// dynamic array, a queue (LRM 7.4, 7.5, 7.10) -- answers about them: how many
// it holds, the type they are, and where the one at storage position
// `position` lies, for reading and as storage a write lands in. What walks a
// value down to its leaves -- imaging it for a foreign call (LRM H.12), loading
// a memory file into it (LRM 21.4) -- asks these of the type at each level.
class PartsByPosition {
 public:
  PartsByPosition(const PartsByPosition&) = delete;
  auto operator=(const PartsByPosition&) -> PartsByPosition& = delete;
  PartsByPosition(PartsByPosition&&) = delete;
  auto operator=(PartsByPosition&&) -> PartsByPosition& = delete;

  [[nodiscard]] virtual auto Count(const void* value) const -> std::size_t = 0;
  [[nodiscard]] virtual auto Type(const void* value) const
      -> const ValueType& = 0;
  [[nodiscard]] virtual auto At(const void* value, std::size_t position) const
      -> const void* = 0;
  [[nodiscard]] virtual auto RefAt(void* value, std::size_t position) const
      -> void* = 0;

 protected:
  constexpr PartsByPosition() = default;
  ~PartsByPosition() = default;
};

// Where an object states one of its numbers: how far into the object the
// number lies, and how many bytes it takes.
struct StatedAt {
  std::size_t offset;
  std::size_t bytes;
};

// Where `whole` states its member `member`.
template <typename Whole, typename Member>
[[nodiscard]] auto StatedAtIn(const Whole* whole, const Member& member)
    -> StatedAt {
  return StatedAt{
      .offset = static_cast<std::size_t>(
          std::bit_cast<std::uintptr_t>(std::addressof(member)) -
          std::bit_cast<std::uintptr_t>(whole)),
      .bytes = sizeof(Member)};
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
// type once and the value carries none of its own. A reference and a part
// designated within a write name bytes lying in something else and hold no
// type, so an access through one of those is told the type of what it
// reaches.
//
// Every value is passed as its address. Copying and moving build in `out` a
// value the source still has to end; `Assign` writes into a value already
// there, which goes on being that value. An operation answering a value builds
// it in `out`. One the language does not define for the type is never asked of
// it, since the front end rejects the program asking.
//
// An integral answer is built as the bytes of its type where the language fixes
// that type -- a width's and a count's `int`. An equality answers its 0, 1 or
// x as a scalar, and a case equality, which is never unknown (LRM 11.4.5),
// whether it holds; whoever knows the type the comparison has holds either as
// a value of it. A stream of bits is an integral value, which a value writes
// its own bits into and is read back out of (LRM 6.24.3), and the control bits
// a count is told of are one too. A fill is the bytes of a `logic`.
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

  // Where the type states its size and its alignment, which code stating a
  // type as constant data places them by.
  [[nodiscard]] auto SizeStatedAt() const -> StatedAt;
  [[nodiscard]] auto AlignStatedAt() const -> StatedAt;

  virtual void Copy(const void* value, void* out) const = 0;
  virtual void Move(void* value, void* out) const noexcept = 0;
  virtual void Destroy(void* value) const noexcept = 0;
  virtual void Assign(void* storage, const void* value) const = 0;

  [[nodiscard]] virtual auto Equal(const void* lhs, const void* rhs) const
      -> FourStateBit = 0;
  [[nodiscard]] virtual auto CaseEqual(const void* lhs, const void* rhs) const
      -> bool = 0;
  [[nodiscard]] virtual auto BitIdentical(
      const void* lhs, const void* rhs) const -> bool = 0;
  [[nodiscard]] virtual auto HasUnknown(const void* value) const -> bool = 0;

  // The value storage of the type holds before anything writes it, which the
  // type alone fixes: a singular type's default (LRM 6.4, Table 6-7), an empty
  // container, and an aggregate's parts each at what its own type builds. What
  // a declaration states of its members (LRM 7.2.2) is written by its
  // initialization after this. An untagged union's type states none of its
  // members, so it builds one holding none, short of the first member's
  // default LRM 7.3 gives it.
  virtual void BuildDefault(void* out) const = 0;

  virtual void BitstreamWidth(const void* value, void* out) const = 0;
  virtual void CountBits(
      const void* value, const ConstIntegralView& control_bits,
      void* out) const = 0;
  // The value's bits written into a stream below its `filled` most
  // significant positions, and the value of this type the stream holds below
  // its `taken` most significant ones, built in `out`. The type fixes how
  // many positions a value of it takes and what each part of it holds, which
  // is everything a sequence of bits carries nothing of. Each answers how many
  // positions are filled, or taken, after it.
  virtual auto WriteToStream(
      const void* value, const IntegralView& stream, std::uint64_t filled) const
      -> std::uint64_t = 0;
  virtual auto ReadFromStream(
      const ConstIntegralView& stream, std::uint64_t taken, void* out) const
      -> std::uint64_t = 0;

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

  // What a value of this type answers about its parts, where they are ordered
  // by position, and nothing for a type whose parts are not.
  [[nodiscard]] virtual auto Parts() const -> const PartsByPosition* = 0;

  // This type as an integral one (LRM 6.11), where it is one: what a facility
  // compiled for every type reads a value's bits through, and nothing for a
  // type whose values are no integral.
  [[nodiscard]] virtual auto AsIntegral() const -> const IntegralValueType* = 0;

  // This type as a fixed-size unpacked array type (LRM 7.4.2), where it is
  // one, and nothing otherwise. Each unit naming an array type states it with
  // a constant of its own, so two of them are told to be one type by what
  // they state.
  [[nodiscard]] virtual auto AsFixedArray() const -> const FixedArrayType* = 0;

 protected:
  constexpr ValueType(std::uint32_t size, std::uint32_t align)
      : size_(size), align_(align) {
  }

 private:
  std::uint32_t size_;
  std::uint32_t align_;
};

// A value the library holds of a type it was compiled without and is handed as
// bytes laid out by that type, which state nothing of it -- a tuple, a
// fixed-size unpacked array: kept with its type, and compared with and taking
// the bytes of a value of that type.
template <class T>
concept LaidOutByType = requires(T& held, const T& read, const void* bytes) {
  { read.Type() } -> std::convertible_to<const ValueType&>;
  { read.IsBitIdenticalTo(bytes) } -> std::same_as<bool>;
  held.AssignBytes(bytes);
};

}  // namespace lyra::value
