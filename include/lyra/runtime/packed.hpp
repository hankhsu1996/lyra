#pragma once

#include <array>
#include <cstddef>
#include <cstdint>
#include <limits>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/internal_error.hpp"

namespace lyra::runtime {

enum class Signedness : std::uint8_t { kSigned, kUnsigned };
enum class TwoStateBit : std::uint8_t { kZero, kOne };
enum class FourStateBit : std::uint8_t {
  kZero,
  kOne,
  kHighImpedance,
  kUnknown
};

struct PackedDim {
  std::int64_t left{};
  std::int64_t right{};

  [[nodiscard]] constexpr auto Size() const -> std::uint64_t {
    const std::int64_t span = (left >= right) ? (left - right) : (right - left);
    return static_cast<std::uint64_t>(span) + 1U;
  }
  [[nodiscard]] constexpr auto Contains(std::int64_t index) const -> bool {
    const std::int64_t lo = (left < right) ? left : right;
    const std::int64_t hi = (left < right) ? right : left;
    return index >= lo && index <= hi;
  }
  [[nodiscard]] auto LinearOffset(std::int64_t index) const -> std::uint64_t {
    if (!Contains(index)) {
      throw InternalError("PackedDim::LinearOffset: index out of range");
    }
    return left >= right ? static_cast<std::uint64_t>(left - index)
                         : static_cast<std::uint64_t>(index - left);
  }

  auto operator==(const PackedDim&) const -> bool = default;
};

// Rank-parametric structural shape used as a class-template NTTP. Rank lives
// in the type so scalar (PackedShape<0>) and one-bit-range
// (PackedShape<1>{{0,0}}) are distinct declared shapes even though both have
// width 1, and there is no artificial maximum rank. std::array<T,0> is a
// valid empty array, so PackedShape<0>{} works without a special case.
//
// TotalWidth() is constexpr (not consteval) so semantic queries on shape
// metadata work in both compile-time and runtime contexts -- e.g. a runtime
// `kShape.TotalWidth()` call from tests or future runtime code. Overflow and
// zero-width are guarded with `throw InternalError(...)`: at runtime the
// exception propagates normally, in constant evaluation the call simply
// stops being a constant expression and the compiler reports the error.
//
// StorageWidth() stays consteval because it drives the BitPlane<> template
// argument and must produce a usable std::size_t at compile time.
template <std::size_t Rank>
struct PackedShape {
  std::array<PackedDim, Rank> dims{};

  [[nodiscard]] static constexpr auto RankValue() -> std::size_t {
    return Rank;
  }

  [[nodiscard]] constexpr auto TotalWidth() const -> std::uint64_t {
    std::uint64_t width = 1;
    for (const auto& dim : dims) {
      const std::uint64_t size = dim.Size();
      if (size != 0U &&
          width > std::numeric_limits<std::uint64_t>::max() / size) {
        throw InternalError(
            "PackedShape::TotalWidth: width overflows uint64_t");
      }
      width *= size;
    }
    if (width == 0U) {
      throw InternalError("PackedShape::TotalWidth: zero width");
    }
    return width;
  }

  [[nodiscard]] consteval auto StorageWidth() const -> std::size_t {
    const std::uint64_t width = TotalWidth();
    if (width >
        static_cast<std::uint64_t>(std::numeric_limits<std::size_t>::max())) {
      throw InternalError("PackedShape::StorageWidth: width overflows size_t");
    }
    return static_cast<std::size_t>(width);
  }

  [[nodiscard]] auto Dim(std::size_t index) const -> PackedDim {
    if (index >= Rank) {
      throw InternalError("PackedShape::Dim: index out of range");
    }
    return dims[index];
  }

  auto operator==(const PackedShape&) const -> bool = default;
};

// Forward declarations so detail::BitPlane can grant them friendship for
// raw-words access.
template <PackedShape Shape, Signedness Signed = Signedness::kUnsigned>
class Bit;
template <PackedShape Shape, Signedness Signed = Signedness::kUnsigned>
class Logic;

namespace detail {
struct PlaneAccess;  // defined in convert.hpp
}  // namespace detail

namespace detail {

inline auto ValidateBitRange(
    std::size_t word_count, std::uint64_t bit_offset, std::uint64_t width,
    std::string_view where) -> void {
  if (width == 0U) {
    throw InternalError(std::string(where) + ": zero-width view");
  }
  if (static_cast<std::uint64_t>(word_count) >
      std::numeric_limits<std::uint64_t>::max() / 64U) {
    throw InternalError(std::string(where) + ": plane bit count overflows");
  }
  const auto plane_bits = static_cast<std::uint64_t>(word_count) * 64U;
  if (bit_offset > plane_bits || width > plane_bits - bit_offset) {
    throw InternalError(std::string(where) + ": range exceeds plane");
  }
}

inline auto ValidateLogicRange(
    std::size_t value_word_count, std::size_t state_word_count,
    std::uint64_t bit_offset, std::uint64_t width, std::string_view where)
    -> void {
  if (value_word_count != state_word_count) {
    throw InternalError(
        std::string(where) + ": value and state span sizes differ");
  }
  ValidateBitRange(value_word_count, bit_offset, width, where);
}

// Internal storage primitive. Lives in detail because raw words are an
// implementation detail of the Bit/Logic value types -- public runtime API
// is Bit/Logic + views + typed operations. Bit/Logic are friended so they
// can build views over their owned plane(s) without exposing words_.
template <std::size_t Width>
class BitPlane {
 public:
  static_assert(Width > 0, "BitPlane width must be positive");
  static constexpr std::size_t kWordCount = (Width + 63U) / 64U;

  BitPlane() = default;

  auto SetZero() -> void {
    words_.fill(0U);
  }

  auto SetOne() -> void {
    words_.fill(~std::uint64_t{0});
    constexpr std::uint64_t kTailBits = Width % 64U;
    if constexpr (kTailBits != 0U) {
      const std::uint64_t mask = (std::uint64_t{1} << kTailBits) - 1U;
      words_[kWordCount - 1U] &= mask;
    }
  }

  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> bool {
    if (offset >= Width) {
      throw InternalError("BitPlane::GetBit: offset out of range");
    }
    return ((words_[offset / 64U] >> (offset % 64U)) & 1U) != 0U;
  }

  auto SetBit(std::uint64_t offset, bool value) -> void {
    if (offset >= Width) {
      throw InternalError("BitPlane::SetBit: offset out of range");
    }
    const std::uint64_t mask = std::uint64_t{1} << (offset % 64U);
    if (value) {
      words_[offset / 64U] |= mask;
    } else {
      words_[offset / 64U] &= ~mask;
    }
  }

  [[nodiscard]] static constexpr auto BitWidth() -> std::uint64_t {
    return Width;
  }

 private:
  template <PackedShape S, Signedness Sg>
  friend class lyra::runtime::Bit;
  template <PackedShape S, Signedness Sg>
  friend class lyra::runtime::Logic;

  [[nodiscard]] auto MutableWordsForView() -> std::span<std::uint64_t> {
    return words_;
  }
  [[nodiscard]] auto WordsForView() const -> std::span<const std::uint64_t> {
    return words_;
  }

  std::array<std::uint64_t, kWordCount> words_{};
};

}  // namespace detail

class ConstBitView {
 public:
  ConstBitView(
      std::span<const std::uint64_t> words, std::uint64_t bit_offset,
      std::uint64_t width)
      : words_(words), bit_offset_(bit_offset), width_(width) {
    detail::ValidateBitRange(
        words_.size(), bit_offset_, width_, "ConstBitView");
  }

  [[nodiscard]] auto Width() const -> std::uint64_t {
    return width_;
  }

  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> TwoStateBit {
    if (offset >= width_) {
      throw InternalError("ConstBitView::GetBit: offset out of range");
    }
    const std::uint64_t bp = bit_offset_ + offset;
    const bool bit = ((words_[bp / 64U] >> (bp % 64U)) & 1U) != 0U;
    return bit ? TwoStateBit::kOne : TwoStateBit::kZero;
  }

  // Pack the view's logical bits 0..width_-1 into a single uint64 (LSB at bit
  // 0). Walks bits so non-zero bit_offset_ is honored.
  [[nodiscard]] auto PackLowWord() const -> std::uint64_t {
    if (width_ > 64U) {
      throw InternalError("ConstBitView::PackLowWord: width > 64");
    }
    std::uint64_t out = 0;
    for (std::uint64_t i = 0; i < width_; ++i) {
      if (GetBit(i) == TwoStateBit::kOne) {
        out |= std::uint64_t{1} << i;
      }
    }
    return out;
  }

 private:
  friend struct detail::PlaneAccess;

  [[nodiscard]] auto WordsForConvert() const -> std::span<const std::uint64_t> {
    return words_;
  }
  [[nodiscard]] auto BitOffsetForConvert() const -> std::uint64_t {
    return bit_offset_;
  }

  std::span<const std::uint64_t> words_;
  std::uint64_t bit_offset_;
  std::uint64_t width_;
};

class BitView {
 public:
  BitView(
      std::span<std::uint64_t> words, std::uint64_t bit_offset,
      std::uint64_t width)
      : words_(words), bit_offset_(bit_offset), width_(width) {
    detail::ValidateBitRange(words_.size(), bit_offset_, width_, "BitView");
  }

  [[nodiscard]] auto Width() const -> std::uint64_t {
    return width_;
  }

  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> TwoStateBit {
    return AsConst().GetBit(offset);
  }

  auto SetBit(std::uint64_t offset, TwoStateBit value) -> void {
    if (offset >= width_) {
      throw InternalError("BitView::SetBit: offset out of range");
    }
    const std::uint64_t bp = bit_offset_ + offset;
    const std::uint64_t mask = std::uint64_t{1} << (bp % 64U);
    if (value == TwoStateBit::kOne) {
      words_[bp / 64U] |= mask;
    } else {
      words_[bp / 64U] &= ~mask;
    }
  }

  auto SetZero() -> void {
    for (std::uint64_t i = 0; i < width_; ++i) {
      SetBit(i, TwoStateBit::kZero);
    }
  }

  auto SetOne() -> void {
    for (std::uint64_t i = 0; i < width_; ++i) {
      SetBit(i, TwoStateBit::kOne);
    }
  }

  // Overlap-safe: SystemVerilog assignment semantics evaluate the RHS fully
  // before updating the LHS. Reading bits straight into a fresh buffer and
  // then writing handles the same-owner overlap case correctly.
  auto CopyFromSameWidth(ConstBitView src) -> void {
    if (src.Width() != width_) {
      throw InternalError("BitView::CopyFromSameWidth: width mismatch");
    }
    std::vector<TwoStateBit> tmp;
    tmp.reserve(width_);
    for (std::uint64_t i = 0; i < width_; ++i) {
      tmp.push_back(src.GetBit(i));
    }
    for (std::uint64_t i = 0; i < width_; ++i) {
      SetBit(i, tmp[i]);
    }
  }

  [[nodiscard]] auto AsConst() const -> ConstBitView {
    return ConstBitView{words_, bit_offset_, width_};
  }
  [[nodiscard]] auto PackLowWord() const -> std::uint64_t {
    return AsConst().PackLowWord();
  }

 private:
  std::span<std::uint64_t> words_;
  std::uint64_t bit_offset_;
  std::uint64_t width_;
};

class ConstLogicView {
 public:
  ConstLogicView(
      std::span<const std::uint64_t> value_words,
      std::span<const std::uint64_t> state_words, std::uint64_t bit_offset,
      std::uint64_t width)
      : value_words_(value_words),
        state_words_(state_words),
        bit_offset_(bit_offset),
        width_(width) {
    detail::ValidateLogicRange(
        value_words_.size(), state_words_.size(), bit_offset_, width_,
        "ConstLogicView");
  }

  [[nodiscard]] auto Width() const -> std::uint64_t {
    return width_;
  }

  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> FourStateBit {
    if (offset >= width_) {
      throw InternalError("ConstLogicView::GetBit: offset out of range");
    }
    const std::uint64_t bp = bit_offset_ + offset;
    const bool v = ((value_words_[bp / 64U] >> (bp % 64U)) & 1U) != 0U;
    const bool s = ((state_words_[bp / 64U] >> (bp % 64U)) & 1U) != 0U;
    if (!s) {
      return v ? FourStateBit::kOne : FourStateBit::kZero;
    }
    return v ? FourStateBit::kUnknown : FourStateBit::kHighImpedance;
  }

  // Pack the value plane's view bits into a uint64 (LSB at bit 0). Walks bits
  // so non-zero bit_offset_ is honored.
  [[nodiscard]] auto PackValueLowWord() const -> std::uint64_t {
    if (width_ > 64U) {
      throw InternalError("ConstLogicView::PackValueLowWord: width > 64");
    }
    std::uint64_t out = 0;
    for (std::uint64_t i = 0; i < width_; ++i) {
      const std::uint64_t bp = bit_offset_ + i;
      if (((value_words_[bp / 64U] >> (bp % 64U)) & 1U) != 0U) {
        out |= std::uint64_t{1} << i;
      }
    }
    return out;
  }

  [[nodiscard]] auto PackStateLowWord() const -> std::uint64_t {
    if (width_ > 64U) {
      throw InternalError("ConstLogicView::PackStateLowWord: width > 64");
    }
    std::uint64_t out = 0;
    for (std::uint64_t i = 0; i < width_; ++i) {
      const std::uint64_t bp = bit_offset_ + i;
      if (((state_words_[bp / 64U] >> (bp % 64U)) & 1U) != 0U) {
        out |= std::uint64_t{1} << i;
      }
    }
    return out;
  }

 private:
  friend struct detail::PlaneAccess;

  [[nodiscard]] auto ValueWordsForConvert() const
      -> std::span<const std::uint64_t> {
    return value_words_;
  }
  [[nodiscard]] auto StateWordsForConvert() const
      -> std::span<const std::uint64_t> {
    return state_words_;
  }
  [[nodiscard]] auto BitOffsetForConvert() const -> std::uint64_t {
    return bit_offset_;
  }

  std::span<const std::uint64_t> value_words_;
  std::span<const std::uint64_t> state_words_;
  std::uint64_t bit_offset_;
  std::uint64_t width_;
};

class LogicView {
 public:
  LogicView(
      std::span<std::uint64_t> value_words,
      std::span<std::uint64_t> state_words, std::uint64_t bit_offset,
      std::uint64_t width)
      : value_words_(value_words),
        state_words_(state_words),
        bit_offset_(bit_offset),
        width_(width) {
    detail::ValidateLogicRange(
        value_words_.size(), state_words_.size(), bit_offset_, width_,
        "LogicView");
  }

  [[nodiscard]] auto Width() const -> std::uint64_t {
    return width_;
  }

  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> FourStateBit {
    return AsConst().GetBit(offset);
  }

  auto SetBit(std::uint64_t offset, FourStateBit value) -> void {
    if (offset >= width_) {
      throw InternalError("LogicView::SetBit: offset out of range");
    }
    const std::uint64_t bp = bit_offset_ + offset;
    const std::uint64_t mask = std::uint64_t{1} << (bp % 64U);
    const std::size_t widx = bp / 64U;
    const auto write = [&](std::span<std::uint64_t> arr, bool bit) {
      if (bit) {
        arr[widx] |= mask;
      } else {
        arr[widx] &= ~mask;
      }
    };
    switch (value) {
      case FourStateBit::kZero:
        write(value_words_, false);
        write(state_words_, false);
        break;
      case FourStateBit::kOne:
        write(value_words_, true);
        write(state_words_, false);
        break;
      case FourStateBit::kHighImpedance:
        write(value_words_, false);
        write(state_words_, true);
        break;
      case FourStateBit::kUnknown:
        write(value_words_, true);
        write(state_words_, true);
        break;
    }
  }

  auto SetZero() -> void {
    for (std::uint64_t i = 0; i < width_; ++i) {
      SetBit(i, FourStateBit::kZero);
    }
  }
  auto SetOne() -> void {
    for (std::uint64_t i = 0; i < width_; ++i) {
      SetBit(i, FourStateBit::kOne);
    }
  }
  auto SetUnknown() -> void {
    for (std::uint64_t i = 0; i < width_; ++i) {
      SetBit(i, FourStateBit::kUnknown);
    }
  }
  auto SetHighImpedance() -> void {
    for (std::uint64_t i = 0; i < width_; ++i) {
      SetBit(i, FourStateBit::kHighImpedance);
    }
  }

  // Overlap-safe: see BitView::CopyFromSameWidth.
  auto CopyFromSameWidth(ConstLogicView src) -> void {
    if (src.Width() != width_) {
      throw InternalError("LogicView::CopyFromSameWidth: width mismatch");
    }
    std::vector<FourStateBit> tmp;
    tmp.reserve(width_);
    for (std::uint64_t i = 0; i < width_; ++i) {
      tmp.push_back(src.GetBit(i));
    }
    for (std::uint64_t i = 0; i < width_; ++i) {
      SetBit(i, tmp[i]);
    }
  }

  [[nodiscard]] auto AsConst() const -> ConstLogicView {
    return ConstLogicView{value_words_, state_words_, bit_offset_, width_};
  }
  [[nodiscard]] auto PackValueLowWord() const -> std::uint64_t {
    return AsConst().PackValueLowWord();
  }
  [[nodiscard]] auto PackStateLowWord() const -> std::uint64_t {
    return AsConst().PackStateLowWord();
  }

 private:
  std::span<std::uint64_t> value_words_;
  std::span<std::uint64_t> state_words_;
  std::uint64_t bit_offset_;
  std::uint64_t width_;
};

template <PackedShape Shape, Signedness Signed>
class Bit {
 public:
  static constexpr auto kShape = Shape;
  static constexpr Signedness kSignedness = Signed;
  static constexpr std::size_t kWidth = Shape.StorageWidth();

  Bit() = default;

  [[nodiscard]] auto View() -> BitView {
    return BitView{bits_.MutableWordsForView(), 0U, kWidth};
  }
  [[nodiscard]] auto View() const -> ConstBitView {
    return ConstBitView{bits_.WordsForView(), 0U, kWidth};
  }
  [[nodiscard]] auto View(std::uint64_t offset, std::uint64_t width)
      -> BitView {
    const auto kw = static_cast<std::uint64_t>(kWidth);
    if (width == 0U || offset > kw || width > kw - offset) {
      throw InternalError("Bit::View: invalid view range");
    }
    return BitView{bits_.MutableWordsForView(), offset, width};
  }
  [[nodiscard]] auto View(std::uint64_t offset, std::uint64_t width) const
      -> ConstBitView {
    const auto kw = static_cast<std::uint64_t>(kWidth);
    if (width == 0U || offset > kw || width > kw - offset) {
      throw InternalError("Bit::View: invalid view range");
    }
    return ConstBitView{bits_.WordsForView(), offset, width};
  }

  auto SetZero() -> void {
    bits_.SetZero();
  }
  auto SetOne() -> void {
    bits_.SetOne();
  }

  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> TwoStateBit {
    return bits_.GetBit(offset) ? TwoStateBit::kOne : TwoStateBit::kZero;
  }
  auto SetBit(std::uint64_t offset, TwoStateBit value) -> void {
    bits_.SetBit(offset, value == TwoStateBit::kOne);
  }

  // Same-width copy; conversion goes through `ConvertToBit`/`ConvertToLogic`.
  auto Assign(ConstBitView rhs) -> void {
    View().CopyFromSameWidth(rhs);
  }
  auto Assign(BitView rhs) -> void {
    Assign(rhs.AsConst());
  }

 private:
  friend struct detail::PlaneAccess;

  [[nodiscard]] auto MutableValueWordsForConvert() -> std::span<std::uint64_t> {
    return bits_.MutableWordsForView();
  }

  detail::BitPlane<kWidth> bits_{};
};

template <PackedShape Shape, Signedness Signed>
class Logic {
 public:
  static constexpr auto kShape = Shape;
  static constexpr Signedness kSignedness = Signed;
  static constexpr std::size_t kWidth = Shape.StorageWidth();

  Logic() {
    SetUnknown();
  }

  [[nodiscard]] auto View() -> LogicView {
    return LogicView{
        value_.MutableWordsForView(), state_.MutableWordsForView(), 0U, kWidth};
  }
  [[nodiscard]] auto View() const -> ConstLogicView {
    return ConstLogicView{
        value_.WordsForView(), state_.WordsForView(), 0U, kWidth};
  }
  [[nodiscard]] auto View(std::uint64_t offset, std::uint64_t width)
      -> LogicView {
    const auto kw = static_cast<std::uint64_t>(kWidth);
    if (width == 0U || offset > kw || width > kw - offset) {
      throw InternalError("Logic::View: invalid view range");
    }
    return LogicView{
        value_.MutableWordsForView(), state_.MutableWordsForView(), offset,
        width};
  }
  [[nodiscard]] auto View(std::uint64_t offset, std::uint64_t width) const
      -> ConstLogicView {
    const auto kw = static_cast<std::uint64_t>(kWidth);
    if (width == 0U || offset > kw || width > kw - offset) {
      throw InternalError("Logic::View: invalid view range");
    }
    return ConstLogicView{
        value_.WordsForView(), state_.WordsForView(), offset, width};
  }

  auto SetZero() -> void {
    value_.SetZero();
    state_.SetZero();
  }
  auto SetOne() -> void {
    value_.SetOne();
    state_.SetZero();
  }
  auto SetHighImpedance() -> void {
    value_.SetZero();
    state_.SetOne();
  }
  auto SetUnknown() -> void {
    value_.SetOne();
    state_.SetOne();
  }

  [[nodiscard]] auto GetBit(std::uint64_t offset) const -> FourStateBit {
    const bool v = value_.GetBit(offset);
    const bool s = state_.GetBit(offset);
    if (!s) {
      return v ? FourStateBit::kOne : FourStateBit::kZero;
    }
    return v ? FourStateBit::kUnknown : FourStateBit::kHighImpedance;
  }
  auto SetBit(std::uint64_t offset, FourStateBit value) -> void {
    switch (value) {
      case FourStateBit::kZero:
        value_.SetBit(offset, false);
        state_.SetBit(offset, false);
        break;
      case FourStateBit::kOne:
        value_.SetBit(offset, true);
        state_.SetBit(offset, false);
        break;
      case FourStateBit::kHighImpedance:
        value_.SetBit(offset, false);
        state_.SetBit(offset, true);
        break;
      case FourStateBit::kUnknown:
        value_.SetBit(offset, true);
        state_.SetBit(offset, true);
        break;
    }
  }

  // Same-width copy; conversion goes through `ConvertToLogic`.
  auto Assign(ConstLogicView rhs) -> void {
    View().CopyFromSameWidth(rhs);
  }
  auto Assign(LogicView rhs) -> void {
    Assign(rhs.AsConst());
  }

 private:
  friend struct detail::PlaneAccess;

  [[nodiscard]] auto MutableValueWordsForConvert() -> std::span<std::uint64_t> {
    return value_.MutableWordsForView();
  }
  [[nodiscard]] auto MutableStateWordsForConvert() -> std::span<std::uint64_t> {
    return state_.MutableWordsForView();
  }

  detail::BitPlane<kWidth> value_{};
  detail::BitPlane<kWidth> state_{};
};

template <PackedShape Shape, Signedness Signed = Signedness::kUnsigned>
using Reg = Logic<Shape, Signed>;

}  // namespace lyra::runtime
