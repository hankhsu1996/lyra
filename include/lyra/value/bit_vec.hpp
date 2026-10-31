#pragma once

#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::value {

// A run of bits whose length is the value's, the first most significant (LRM
// 6.24.3, 11.4.14.1): the stream made of a value whose bit count only the
// running program has, as Rust's `BitVec` and LLVM's `BitVector` are runs
// whose length their value holds. A run of no bits is a value, the stream of
// an empty queue.
//
// It always holds an unknown plane. Whether a stream is 4-state (LRM 11.4.14)
// shows only once it is read back, and every value it is read into takes the
// states its own type holds (LRM 6.24.3), so a run of 2-state bits holding no
// x or z reads back exactly as one that cannot.
class BitVec {
 public:
  BitVec() = default;

  // A run `width` bits wide, every bit 0.
  [[nodiscard]] static auto Zeros(std::uint64_t width) -> BitVec {
    return BitVec(width);
  }

  // The bits `bits` holds, as many as it is wide.
  [[nodiscard]] static auto Of(AnyConstIntegralView auto bits) -> BitVec {
    BitVec run(bits.width);
    Extract(run.MutableView(), bits, 0);
    return run;
  }
  template <IntegralValue T>
  [[nodiscard]] static auto Of(const T& bits) -> BitVec {
    return Of(bits.View());
  }
  // The bits of `bits` from `at` positions below its most significant end up
  // to `until`, as a run of their own.
  [[nodiscard]] static auto Of(
      AnyConstIntegralView auto bits, std::uint64_t at, std::uint64_t until)
      -> BitVec {
    BitVec run(until - at);
    Extract(
        run.MutableView(), bits, static_cast<std::int64_t>(bits.width - until));
    return run;
  }

  // LRM 20.6.2: how many bits the run holds, as a machine count.
  [[nodiscard]] auto Len() const -> std::int64_t {
    return static_cast<std::int64_t>(width_);
  }
  [[nodiscard]] auto View() const -> ConstIntegralView {
    return ViewOfWords(words_.data(), Shape(width_));
  }
  [[nodiscard]] auto MutableView() -> IntegralView {
    return ViewOfWords(words_.data(), Shape(width_));
  }

  // LRM 11.4.14.1: `part` laid after the bits this holds, so it is the least
  // significant of the run that results.
  void Extend(AnyConstIntegralView auto part) {
    BitVec longer(width_ + part.width);
    Insert(longer.MutableView(), View(), static_cast<std::int64_t>(part.width));
    Insert(longer.MutableView(), part, 0);
    *this = std::move(longer);
  }
  template <IntegralValue T>
  void Extend(const T& part) {
    Extend(part.View());
  }
  void Append(const BitVec& part) {
    Extend(part.View());
  }
  // `part` written over the bits starting `at` positions from the most
  // significant end, which it fits inside.
  void InsertBits(std::int64_t at, const BitVec& part) {
    Insert(
        MutableView(), part.View(),
        StartOf(static_cast<std::uint64_t>(at), part.width_));
  }

  // LRM 11.4.14.2: the run's `block`-wide blocks in reversed order, counted
  // from its least significant bit.
  [[nodiscard]] auto ReverseBlocks(std::int64_t block) const -> BitVec {
    BitVec reversed(width_);
    value::ReverseBlocks(
        reversed.MutableView(), View(), static_cast<std::uint64_t>(block));
    return reversed;
  }

  // The `width` bits starting `at` positions from the most significant end.
  [[nodiscard]] auto ExtractBits(std::int64_t at, std::int64_t width) const
      -> BitVec {
    return Of(
        View(), static_cast<std::uint64_t>(at),
        static_cast<std::uint64_t>(at + width));
  }

  // The bits starting `at` positions from the most significant end, as a
  // value of `R`, as many as it is wide.
  template <IntegralValue R>
  [[nodiscard]] auto Load(std::int64_t at) const -> R {
    R out = R::Zero();
    Extract(
        out.AsMutableWords().MutableView(), View(),
        StartOf(static_cast<std::uint64_t>(at), R::kWidth));
    return out;
  }

  // Those bits read into `out`, as many as it is wide.
  void LoadInto(std::uint64_t at, IntegralView out) const {
    Extract(out, View(), StartOf(at, out.width));
  }

  // LRM 11.4.14: the run widened to `width` by zero bits on its right, which
  // left-aligns it in a target that wide. `width` is never narrower.
  [[nodiscard]] auto PaddedTo(std::int64_t width) const -> BitVec {
    BitVec padded(static_cast<std::uint64_t>(width));
    Insert(
        padded.MutableView(), View(),
        width - static_cast<std::int64_t>(width_));
    return padded;
  }

  [[nodiscard]] auto operator==(const BitVec& other) const -> FourStateBit {
    if (width_ != other.width_) {
      return FourStateBit::kZero;
    }
    return Equal(View(), other.View());
  }
  [[nodiscard]] auto operator!=(const BitVec& other) const -> FourStateBit {
    return Inverted(*this == other);
  }
  [[nodiscard]] auto IsBitIdentical(const BitVec& other) const -> bool {
    return width_ == other.width_ && words_ == other.words_;
  }
  [[nodiscard]] auto HasUnknown() const -> bool {
    return value::HasUnknown(View());
  }

 private:
  [[nodiscard]] static constexpr auto Shape(std::uint64_t width)
      -> IntegralShape {
    return IntegralShape{
        .width = width,
        .signedness = Signedness::kUnsigned,
        .domain = StateDomain::kFourState};
  }

  explicit BitVec(std::uint64_t width)
      : width_(width), words_(WordCountOf(Shape(width)), std::uint64_t{0}) {
  }

  // Where a part `width` bits wide `at` positions from the most significant
  // end starts, counted from the least significant bit.
  [[nodiscard]] auto StartOf(std::uint64_t at, std::uint64_t width) const
      -> std::int64_t {
    return static_cast<std::int64_t>(width_) - static_cast<std::int64_t>(at) -
           static_cast<std::int64_t>(width);
  }

  std::uint64_t width_ = 0;
  // The value plane's words, then the unknown plane's.
  std::vector<std::uint64_t> words_;
};

static_assert(LyraValue<BitVec>);

// The run of bits `value` makes (LRM 6.24.3), as many as it holds, each part
// writing its own. The value states its width and writes its bits by two
// different means, so a write filling other than the whole run is a defect of
// that value's type. A container the library holds of an element type it was
// compiled without is handed, in `element`, the operations it streams one by.
template <typename V, typename... Element>
[[nodiscard]] auto StreamOf(const V& value, const Element&... element)
    -> BitVec {
  BitVec run = BitVec::Zeros(
      static_cast<std::uint64_t>(value.BitstreamWidth(element...).ToInt64()));
  const std::uint64_t filled =
      value.WriteToStream(run.MutableView(), 0, element...);
  if (filled != run.View().width) {
    throw InternalError(
        "StreamOf: a value wrote a stream of another width than it states");
  }
  return run;
}

// `value` replaced by what `run` holds, which it takes every bit of.
template <typename V, typename... Element>
void ReadWholeStream(V& value, const BitVec& run, const Element&... element) {
  value.ReadFromStream(run.View(), 0, run.View().width, element...);
}

}  // namespace lyra::value
