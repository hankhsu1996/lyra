#pragma once

#include <algorithm>
#include <array>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string_view>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/element_policy.hpp"
#include "lyra/value/formation.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/value_type_kind.hpp"

namespace lyra::value {

class String;

// The C++ class of a fixed-size unpacked array type (LRM 7.4.2): `N` elements
// of the C++ type `T`, one after another, which is all an array is, and every
// operation of the language on its whole value a body the compiler builds for
// the type. `Impl` is the struct the C++ backend writes for the type's
// implementation of the library's traits, each item a static function whose
// body is the type's own; each question a library template asks of the array
// is asked of the item answering it, under the name the template asks by, so
// this class states no algorithm over its elements. Its steps are its
// elements', in order, as C++ copies, moves and destroys an array member.
//
// One C++ layer per declared unpacked dimension; multi-dim composes as
// `UnpackedArray<UnpackedArray<...>, N, ...>`, each inner array lying in its
// place in the outer. The elements lie in the array, and an element keeps its
// address for as long as the array holds it.
//
// An access names an element by its ordinal, counted from the left (LRM 7.6),
// because the declared range is a fact of the static type the select was
// written against and is read there.
template <typename T, std::size_t N, typename Impl>
class UnpackedArray {
  static_assert(N >= 1, "a fixed-size unpacked array has at least one element");

 public:
  static constexpr ValueTypeKind kKind = ValueTypeKind::kFixedArray;

  UnpackedArray() = default;

  // An array at the value every element starts at, then written by `fill`,
  // which is handed it: made where whoever asked for it keeps it, since what
  // this answers is the array being made and never a copy of one made here
  // (C++17 guaranteed copy elision), so an array too large for a frame is
  // never one in the frame of the operation computing it.
  template <typename Fill>
  [[nodiscard]] static auto Built(Fill fill) -> UnpackedArray {
    return UnpackedArray(WrittenBy<Fill>{.fill = &fill});
  }

  // The array `unit` fills when laid down again and again from the left:
  // element `i` is a copy of `unit[i % unit.size()]`. Every element stated
  // once (LRM 10.9), one element default (LRM Table 7-1) and the items of a
  // `'{n{...}}` pattern (LRM 10.9.1) are each such a unit. How many elements
  // the array has is the type's, so a unit that is empty, or of which no whole
  // number fills the array, is a lowering defect.
  explicit UnpackedArray(std::span<const T> unit) {
    if (unit.empty() || N % unit.size() != 0) {
      throw InternalError(
          std::format(
              "an unpacked array of {} elements was built from a unit of {}", N,
              unit.size()));
    }
    for (std::size_t i = 0; i < N; ++i) {
      items_[i] = unit[i % unit.size()];
    }
  }

  // LRM 5.9 / 21.3.3: a string value assigned to an unpacked array of bytes is
  // left-justified -- the first character lands at the array's left bound and
  // runs toward the right bound, an element past the end of the text keeps the
  // element type's default, and text beyond the array's last element is
  // dropped. The clause admits this form for an array of bytes alone, so the
  // element is integral.
  [[nodiscard]] static auto FromString(const String& text) -> UnpackedArray;

  // LRM 5.9: a string literal assigned to an unpacked array of bytes, under the
  // same left justification. A literal is a packed bit-vector constant, not a
  // string value, so its bytes arrive whole -- a NUL among them is a byte like
  // any other, where building a string value would have removed it (LRM 6.16).
  template <IntegralValue Bits>
  [[nodiscard]] static auto FromIntegral(const Bits& bits) -> UnpackedArray {
    return OfBytes(BytesOf(bits.AsWords().View()));
  }

  // An array whose element at ordinal `i` is a copy of what `element_at(i)`
  // names.
  template <typename ElementAt>
  [[nodiscard]] static auto Gathered(ElementAt element_at) -> UnpackedArray {
    return Built([&](UnpackedArray& out) {
      for (std::size_t i = 0; i < N; ++i) {
        out.items_[i] = element_at(i);
      }
    });
  }

  // LRM 7.4.2: size() yields an SV int.
  [[nodiscard]] auto Size() const -> Int {
    return Int::FromInt(static_cast<std::int64_t>(N));
  }

  [[nodiscard]] static constexpr auto Count() -> std::size_t {
    return N;
  }

  // The element at the index `i`, which names one the array holds, as Rust's
  // `get_unchecked` reaches one: nothing checks it, so only a caller that
  // counts below `N` asks; a position the program computed goes through the
  // checked element access, which answers the element default where it names
  // none (LRM 7.4.5).
  [[nodiscard]] auto GetUnchecked(std::size_t i) const -> const T& {
    return items_[i];
  }
  [[nodiscard]] auto GetUnchecked(std::size_t i) -> T& {
    return items_[i];
  }

  // LRM 7.4.5: an invalid-index write lands where no read reaches, which is no
  // element of the array.
  [[nodiscard]] auto ElementMut(const Position& position, Formation& formed)
      -> T& {
    const auto ordinal = ElementOrdinal(position, N);
    formed = ordinal ? Formation::kExisting : Formation::kNowhere;
    return ordinal ? items_[*ordinal] : detail::DiscardedWriteOf<T>();
  }
  [[nodiscard]] auto ElementMut(const Position& position) -> T& {
    Formation formed{};
    return ElementMut(position, formed);
  }

  // LRM 7.4.5: an invalid-index read returns the element default (LRM Table
  // 7-1).
  [[nodiscard]] auto Element(const Position& position) const -> const T& {
    const auto ordinal = ElementOrdinal(position, N);
    return ordinal ? items_[*ordinal] : DefaultOf<T>();
  }

  [[nodiscard]] static auto DeclaredDefault() -> UnpackedArray {
    return Impl::DeclaredDefault();
  }
  [[nodiscard]] auto IsBitIdentical(const UnpackedArray& other) const
      -> decltype(auto) {
    return Impl::IsBitIdentical(*this, other);
  }
  [[nodiscard]] auto operator==(const UnpackedArray& other) const
      -> decltype(auto) {
    return Impl::Equal(*this, other);
  }
  [[nodiscard]] auto operator!=(const UnpackedArray& other) const
      -> decltype(auto) {
    return Impl::Unequal(*this, other);
  }
  [[nodiscard]] auto CaseEqual(const UnpackedArray& other) const
      -> decltype(auto) {
    return Impl::CaseEqual(*this, other);
  }
  [[nodiscard]] auto HasUnknown() const -> decltype(auto) {
    return Impl::HasUnknown(*this);
  }

  template <typename... Args>
    requires requires(const UnpackedArray& a, Args&&... args) {
      Impl::BitstreamWidth(a, std::forward<Args>(args)...);
    }
  [[nodiscard]] auto BitstreamWidth(Args&&... args) const -> decltype(auto) {
    return Impl::BitstreamWidth(*this, std::forward<Args>(args)...);
  }
  template <typename... Args>
    requires requires(const UnpackedArray& a, Args&&... args) {
      Impl::CountBits(a, std::forward<Args>(args)...);
    }
  [[nodiscard]] auto CountBits(Args&&... args) const -> decltype(auto) {
    return Impl::CountBits(*this, std::forward<Args>(args)...);
  }
  template <typename... Args>
    requires requires(const UnpackedArray& a, Args&&... args) {
      Impl::ToBitstream(a, std::forward<Args>(args)...);
    }
  [[nodiscard]] auto ToBitstream(Args&&... args) const -> decltype(auto) {
    return Impl::ToBitstream(*this, std::forward<Args>(args)...);
  }
  template <typename... Args>
    requires requires(Args&&... args) {
      Impl::FromBitstream(std::forward<Args>(args)...);
    }
  [[nodiscard]] static auto FromBitstream(Args&&... args) -> decltype(auto) {
    return Impl::FromBitstream(std::forward<Args>(args)...);
  }
  template <typename... Args>
    requires requires(const UnpackedArray& a, Args&&... args) {
      Impl::ToBitVec(a, std::forward<Args>(args)...);
    }
  [[nodiscard]] auto ToBitVec(Args&&... args) const -> decltype(auto) {
    return Impl::ToBitVec(*this, std::forward<Args>(args)...);
  }
  template <typename... Args>
    requires requires(Args&&... args) {
      Impl::FromBitVec(std::forward<Args>(args)...);
    }
  [[nodiscard]] static auto FromBitVec(Args&&... args) -> decltype(auto) {
    return Impl::FromBitVec(std::forward<Args>(args)...);
  }

  [[nodiscard]] static auto PositionCount() -> std::int64_t
    requires requires { Impl::PositionCount(); }
  {
    return Impl::PositionCount();
  }
  template <typename... Args>
    requires requires(const UnpackedArray& a, Args&&... args) {
      Impl::TakePositions(a, std::forward<Args>(args)...);
    }
  [[nodiscard]] auto TakePositions(Args&&... args) const -> decltype(auto) {
    return Impl::TakePositions(*this, std::forward<Args>(args)...);
  }
  template <typename... Args>
    requires requires(UnpackedArray& a, Args&&... args) {
      Impl::WritePositions(a, std::forward<Args>(args)...);
    }
  auto WritePositions(Args&&... args) -> decltype(auto) {
    return Impl::WritePositions(*this, std::forward<Args>(args)...);
  }
  [[nodiscard]] auto ToPositions() const -> decltype(auto)
    requires requires(const UnpackedArray& a) { Impl::ToPositions(a); }
  {
    return Impl::ToPositions(*this);
  }

 private:
  // What writes an array `Built` makes.
  template <typename Fill>
  struct WrittenBy {
    Fill* fill;
  };
  template <typename Fill>
  explicit UnpackedArray(WrittenBy<Fill> written) : UnpackedArray() {
    (*written.fill)(*this);
  }

  // Left-justifies a byte sequence: the first byte lands at the array's left
  // bound and runs toward the right, an element past the end of the sequence
  // keeps the element type's default, and bytes beyond the last element are
  // dropped.
  [[nodiscard]] static auto OfBytes(std::string_view bytes) -> UnpackedArray {
    return Built([&](UnpackedArray& out) {
      for (std::size_t i = 0; i < std::min(N, bytes.size()); ++i) {
        out.items_[i] = T::FromInt(static_cast<unsigned char>(bytes[i]));
      }
    });
  }

  std::array<T, N> items_{};
};

}  // namespace lyra::value
