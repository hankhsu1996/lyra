#pragma once

#include <algorithm>
#include <cctype>
#include <cstddef>
#include <cstdint>
#include <cstdlib>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <vector>

#include "lyra/value/concepts.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/position.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::value {

class StringCharRef;

// Runtime representation of the SystemVerilog `string` type (LRM 6.16). The
// SV semantics -- equality on contents, lexicographic compare, and the method
// family in LRM 6.16.1 through 6.16.15 -- are exposed as member operators and
// methods so emitted code dispatches `receiver.Method(args)` exactly the way
// it would on any other class. Each answers with the type the clause gives it,
// and takes an index or a character of whatever integral type names one.
class String {
 public:
  String() = default;
  explicit String(const char* s) : impl_(s) {
  }
  explicit String(std::string s) : impl_(std::move(s)) {
  }
  explicit String(std::string_view s) : impl_(s) {
  }

  // LRM 21.3.4.3: $sscanf accepts an unpacked array of byte as input,
  // viewed as a contiguous character sequence in element order. The low
  // byte of each element becomes one byte of the result, embedded NULs
  // included -- the scan itself decides what a NUL means there.
  template <IntegralValue Element>
  [[nodiscard]] static auto FromByteArray(const UnpackedArray<Element>& bytes)
      -> String {
    std::string out;
    out.reserve(bytes.RawSize());
    for (std::size_t i = 0; i < bytes.RawSize(); ++i) {
      const auto byte =
          static_cast<unsigned char>(bytes.RawAt(i).ToInt64() & 0xFF);
      out.push_back(static_cast<char>(byte));
    }
    return String{std::move(out)};
  }

  // LRM 6.16: a string value holds no NUL, so building one from bits strips
  // every NUL byte -- which is also why the empty `""` (the packed `8'h00`
  // of LRM 11.10.3) strips to the empty string.
  [[nodiscard]] static auto FromIntegral(const ConstIntegralView& bits)
      -> String {
    std::string out;
    for (const char c : BytesOf(bits)) {
      if (c != '\0') out.push_back(c);
    }
    return String{std::move(out)};
  }
  template <IntegralValue Bits>
  [[nodiscard]] static auto FromIntegral(const Bits& bits) -> String {
    return FromIntegral(bits.Load().View());
  }

  // LRM 11.4.5 `==` / `!=`: string equality is exact content equality (strings
  // are 2-state, no x/z to propagate), so the answer is a bit.
  [[nodiscard]] auto operator==(const String& o) const -> Bit {
    return Bit::FromBool(impl_ == o.impl_);
  }
  [[nodiscard]] auto operator!=(const String& o) const -> Bit {
    return Bit::FromBool(impl_ != o.impl_);
  }

  // LRM 11.4.5 `===`: a string has no unknown plane, so case equality is exact
  // content equality and matches `==`.
  [[nodiscard]] auto CaseEqual(const String& o) const -> Bit {
    return Bit::FromBool(impl_ == o.impl_);
  }

  // LRM 9.4.2 update event predicate (engine change-detection hook): host bool
  // form of bit-pattern identity. Distinct from `CaseEqual` only in role.
  [[nodiscard]] auto IsBitIdentical(const String& o) const -> bool {
    return impl_ == o.impl_;
  }

  // LRM 6.16 strings have no X/Z plane.
  [[nodiscard]] static auto HasUnknown() -> bool {
    return false;
  }

  [[nodiscard]] static auto IsUnknown() -> Bit {
    return Bit::FromBool(false);
  }

  // LRM 11.4.4 relational operators on `String` (LRM 6.16).
  [[nodiscard]] auto operator<(const String& o) const -> Bit {
    return Bit::FromBool(impl_ < o.impl_);
  }
  [[nodiscard]] auto operator<=(const String& o) const -> Bit {
    return Bit::FromBool(impl_ <= o.impl_);
  }
  [[nodiscard]] auto operator>(const String& o) const -> Bit {
    return Bit::FromBool(impl_ > o.impl_);
  }
  [[nodiscard]] auto operator>=(const String& o) const -> Bit {
    return Bit::FromBool(impl_ >= o.impl_);
  }

  // LRM 11.4.12 over string operands, Table 6-9: the parts join contents in the
  // order written, and the result grows to hold them rather than truncating.
  [[nodiscard]] auto Concat(const String& o) const -> String {
    return String{impl_ + o.impl_};
  }

  [[nodiscard]] auto operator+(const String& o) const -> String {
    return Concat(o);
  }

  // LRM 11.4.12.2: when at least one inner operand is string-typed or the
  // multiplier is non-constant, `{count{*this}}` yields count concatenated
  // copies. The multiplier is unsigned in SystemVerilog and may be computed at
  // run time here, so a count that is not positive yields the empty string
  // rather than reporting a bug the program did not commit.
  [[nodiscard]] auto Replicate(std::int64_t count) const -> String {
    if (count <= 0) return {};
    std::string out;
    out.reserve(impl_.size() * static_cast<std::size_t>(count));
    for (std::int64_t i = 0; i < count; ++i) {
      out.append(impl_);
    }
    return String{std::move(out)};
  }

  // LRM 20.6.2 `$bits`: a string occupies 8 bits per character (LRM 6.16, a
  // string element is a byte).
  [[nodiscard]] auto BitstreamWidth() const -> Int {
    return Int::FromInt(static_cast<std::int64_t>(8 * impl_.size()));
  }

  // LRM 20.9 `$countbits`: a string's bit stream is its characters, each an
  // LRM 6.16 byte, so the count over it is the sum of the characters' own.
  template <IntegralValue Control>
  [[nodiscard]] auto CountBits(const Control& control_bits) const -> Int {
    return CountBits(control_bits.Load().View());
  }
  [[nodiscard]] auto CountBits(const ConstIntegralView& control_bits) const
      -> Int {
    std::int64_t total = 0;
    for (const char c : impl_) {
      const std::uint64_t word = static_cast<unsigned char>(c);
      total += lyra::value::CountBits(
          ConstPlanes{.value = std::span(&word, 1), .unknown = {}}, 8,
          control_bits.planes, control_bits.width);
    }
    return Int::FromInt(total);
  }

  // LRM 6.16.1: len() yields an SV int.
  [[nodiscard]] auto Len() const -> Int {
    return Int::FromInt(static_cast<std::int64_t>(impl_.size()));
  }

  // LRM 6.16.2. Out-of-range index or zero byte -- no change.
  template <IntegralValue C>
  void Putc(const Position& position, const C& c_arg) {
    PutCharacter(position, c_arg.ToInt64());
  }

  // LRM 6.16.3: getc() yields an SV byte. Out-of-range index returns 0.
  [[nodiscard]] auto Getc(const Position& position) const -> Byte {
    const std::optional<std::size_t> at =
        ElementOrdinal(position, impl_.size());
    if (!at) {
      return Byte::FromInt(0);
    }
    return Byte::FromInt(static_cast<std::int8_t>(impl_[*at]));
  }

  // The read side of indexed character access `s[i]`: the character value, with
  // the same out-of-range default as `getc`. Distinct from `Getc` only in role
  // (the indexing form versus the LRM 6.16.3 method), so it shares the query.
  [[nodiscard]] auto Element(const Position& position) const -> Byte {
    return Getc(position);
  }

  // The write side of indexed character access `s[i]`: a write-back reference
  // to the character. A string exposes no in-place character reference --
  // writes go through `putc` (LRM 6.16.2) with its out-of-range / NUL rules --
  // so the reference is a proxy that captures the index once, routes
  // `operator=` through `putc`, and reads-modifies-writes for each compound
  // operator. The read/write pair `Element` / `ElementRef` mirrors a packed
  // array element. Defined after `StringCharRef` below.
  [[nodiscard]] auto ElementRef(const Position& position) -> StringCharRef;

  // LRM 6.16.4. Receiver unchanged.
  [[nodiscard]] auto Toupper() const -> String {
    std::string out = impl_;
    std::ranges::transform(out, out.begin(), [](unsigned char ch) {
      return static_cast<char>(std::toupper(ch));
    });
    return String{std::move(out)};
  }

  // LRM 6.16.5. Receiver unchanged.
  [[nodiscard]] auto Tolower() const -> String {
    std::string out = impl_;
    std::ranges::transform(out, out.begin(), [](unsigned char ch) {
      return static_cast<char>(std::tolower(ch));
    });
    return String{std::move(out)};
  }

  // LRM 6.16.6: compare() yields an SV int. ANSI C strcmp semantics:
  // negative / zero / positive.
  [[nodiscard]] auto Compare(const String& s) const -> Int {
    const int r = impl_.compare(s.impl_);
    if (r < 0) return Int::FromInt(-1);
    if (r > 0) return Int::FromInt(1);
    return Int::FromInt(0);
  }

  // LRM 6.16.7: icompare() yields an SV int. Case-insensitive strcmp.
  [[nodiscard]] auto Icompare(const String& s) const -> Int {
    const std::size_t n = std::min(impl_.size(), s.impl_.size());
    for (std::size_t k = 0; k < n; ++k) {
      const int a = std::tolower(static_cast<unsigned char>(impl_[k]));
      const int b = std::tolower(static_cast<unsigned char>(s.impl_[k]));
      if (a != b) return Int::FromInt((a < b) ? -1 : 1);
    }
    if (impl_.size() == s.impl_.size()) return Int::FromInt(0);
    return Int::FromInt((impl_.size() < s.impl_.size()) ? -1 : 1);
  }

  // LRM 6.16.8. i..j inclusive. Returns "" if i<0, j<i, or j>=len.
  [[nodiscard]] auto Substr(const Position& first, const Position& last) const
      -> String {
    const std::optional<std::size_t> from = ElementOrdinal(first, impl_.size());
    const std::optional<std::size_t> to = ElementOrdinal(last, impl_.size());
    if (!from || !to || *to < *from) {
      return String{};
    }
    return String{impl_.substr(*from, *to - *from + 1)};
  }

  // LRM 6.16.9: the ato* family yields an SV integer (4-state). Parse leading
  // optional sign and digits (with `_` skipped); 0 if no digits were consumed.
  [[nodiscard]] auto Atoi() const -> Integer {
    return Integer::FromInt(ParseInt(10));
  }
  [[nodiscard]] auto Atohex() const -> Integer {
    return Integer::FromInt(ParseInt(16));
  }
  [[nodiscard]] auto Atooct() const -> Integer {
    return Integer::FromInt(ParseInt(8));
  }
  [[nodiscard]] auto Atobin() const -> Integer {
    return Integer::FromInt(ParseInt(2));
  }

  // LRM 6.16.10. Parse leading real-number syntax; 0.0 if none.
  [[nodiscard]] auto Atoreal() const -> Real {
    if (impl_.empty()) return Real{0.0};
    char* end_ptr = nullptr;
    const double v = std::strtod(impl_.c_str(), &end_ptr);
    if (end_ptr == impl_.c_str()) return Real{0.0};
    return Real{v};
  }

  // LRM 6.16.11 through 6.16.15. Each replaces receiver with the ASCII
  // representation of the argument in the corresponding base / format, the
  // argument read as the number it holds.
  template <IntegralValue I>
  void Itoa(const I& i) {
    SetDecimal(i.ToInt64());
  }
  template <IntegralValue I>
  void Hextoa(const I& i) {
    SetHex(i.ToInt64());
  }
  template <IntegralValue I>
  void Octtoa(const I& i) {
    SetOctal(i.ToInt64());
  }
  template <IntegralValue I>
  void Bintoa(const I& i) {
    SetBinary(i.ToInt64());
  }
  void Realtoa(const Real& r);

  [[nodiscard]] auto View() const -> std::string_view {
    return impl_;
  }

  // Borrows the string as a NUL-terminated C string. The pointer is owned by
  // this `String` and stays valid until the string is mutated or destroyed;
  // it is the carrier a DPI-C `const char*` argument crosses the boundary as
  // (LRM 35.5.6). A caller that needs the string to outlive this object copies.
  [[nodiscard]] auto CStr() const -> const char* {
    return impl_.c_str();
  }

  // The write `putc` makes at a position, the character read as the number
  // its value holds.
  void PutCharacter(const Position& position, std::int64_t character) {
    const auto c = static_cast<std::int8_t>(character);
    if (c == 0) return;
    if (const std::optional<std::size_t> at =
            ElementOrdinal(position, impl_.size())) {
      impl_[*at] = static_cast<char>(c);
    }
  }

  // Out-of-line so std::format does not leak into every translation unit that
  // includes this header.
  void SetDecimal(std::int64_t i);
  void SetHex(std::int64_t i);
  void SetOctal(std::int64_t i);
  void SetBinary(std::int64_t i);

 private:
  // Hand-rolled to avoid std::from_chars's pointer-pair API; accumulates one
  // digit at a time. Underscores are skipped per the LRM atoi family rules.
  [[nodiscard]] auto ParseInt(std::uint64_t base) const -> std::int32_t {
    std::size_t k = 0;
    bool negative = false;
    if (k < impl_.size() && (impl_[k] == '+' || impl_[k] == '-')) {
      negative = (impl_[k] == '-');
      ++k;
    }
    std::uint64_t value = 0;
    bool any_digit = false;
    for (; k < impl_.size(); ++k) {
      const char ch = impl_[k];
      if (ch == '_') continue;
      const std::optional<std::uint64_t> digit = DigitOf(ch);
      if (!digit || *digit >= base) break;
      value = (value * base) + *digit;
      any_digit = true;
    }
    if (!any_digit) return 0;
    if (negative) value = std::uint64_t{0} - value;
    return static_cast<std::int32_t>(value);
  }

  std::string impl_;
};

// The write-back location for a string character (the write side of `s[i]`). A
// string exposes no in-place character reference -- writes go through `putc`
// (LRM 6.16.2) -- so the proxy captures the receiver and index once and routes
// every write through it: `operator=` is `putc`.
class StringCharRef {
 public:
  StringCharRef(String& target, const Position& position)
      : target_(&target), position_(position) {
  }

  StringCharRef(const StringCharRef&) = delete;
  auto operator=(const StringCharRef&) -> StringCharRef& = delete;
  StringCharRef(StringCharRef&&) noexcept = default;
  auto operator=(StringCharRef&&) noexcept -> StringCharRef& = default;
  ~StringCharRef() = default;

  auto operator=(const Byte& value) -> StringCharRef& {
    target_->PutCharacter(position_, value.ToInt64());
    return *this;
  }

 private:
  String* target_;
  Position position_;
};

inline auto String::ElementRef(const Position& position) -> StringCharRef {
  return StringCharRef{*this, position};
}

// LRM 5.9: a string value assigned to an integral variable is right-justified
// -- a destination wider than the text pads its leftmost bits with zeros, and a
// narrower one truncates the leftmost characters.
template <IntegralValue R>
[[nodiscard]] auto FromString(const String& text) -> R {
  return FromBytes<R>(text.View());
}

static_assert(LyraValue<String>);
static_assert(CaseEqualComparable<String>);
static_assert(Ordered<String>);
static_assert(Lengthable<String>);
static_assert(BitstreamSizable<String>);
static_assert(Indexable<String>);

// Defined here rather than alongside the rest of `UnpackedArray` because it is
// the one member that reads a `String`, whose definition depends on the array.
template <typename T>
auto UnpackedArray<T>::FromString(const String& text, std::int64_t count)
    -> UnpackedArray<T> {
  return OfBytes(text.View(), count);
}

}  // namespace lyra::value
