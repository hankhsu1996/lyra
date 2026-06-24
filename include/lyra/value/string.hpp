#pragma once

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <string>
#include <string_view>
#include <utility>

#include "lyra/value/concepts.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/unpacked_array.hpp"

namespace lyra::value {

// Runtime representation of the SystemVerilog `string` type (LRM 6.16). The
// SV semantics -- equality on contents, lexicographic compare, and the method
// family in LRM 6.16.1 through 6.16.15 -- are exposed as member operators and
// methods so emitted code dispatches `receiver.Method(args)` exactly the way
// it would on any other class.
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
  // byte of each element becomes one std::string byte (embedded NULs
  // preserved); StringScanSource::IsWhitespace handles the sscanf-only
  // NUL-as-whitespace rule.
  [[nodiscard]] static auto FromByteArray(
      const UnpackedArray<PackedArray>& bytes) -> String {
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
  [[nodiscard]] static auto FromPackedArray(const PackedArray& bits) -> String {
    std::string out;
    for (char c : bits.ByteString()) {
      if (c != '\0') out.push_back(c);
    }
    return String{std::move(out)};
  }

  // LRM 11.4.5 `==` / `!=`: string equality is exact content equality (strings
  // are 2-state, no x/z to propagate). The result is a 1-bit 2-state
  // `PackedArray` so callers do not branch on the operand type.
  [[nodiscard]] auto operator==(const String& o) const -> PackedArray {
    return PackedArray::FromInt(impl_ == o.impl_ ? 1 : 0, 1, false, false);
  }
  [[nodiscard]] auto operator!=(const String& o) const -> PackedArray {
    return PackedArray::FromInt(impl_ != o.impl_ ? 1 : 0, 1, false, false);
  }

  // LRM 11.4.5 `===`: a string has no unknown plane, so case equality is exact
  // content equality and matches `==`. Returned as a 1-bit `PackedArray` for
  // uniform handling at SV expression sites.
  [[nodiscard]] auto CaseEqual(const String& o) const -> PackedArray {
    return PackedArray::FromInt(impl_ == o.impl_ ? 1 : 0, 1, false, false);
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

  [[nodiscard]] static auto IsUnknown() -> PackedArray {
    return PackedArray::Bit(false);
  }

  // LRM Table 6-7: the string default is the empty string. Satisfies the
  // container OOB-shield contract shared with the other value types, so a
  // `string` can be an unpacked-array element.
  auto ResetToDefault() -> void {
    impl_.clear();
  }

  // LRM 11.4.4 relational operators on `String` (LRM 6.16). The result is
  // 2-state.
  [[nodiscard]] auto operator<(const String& o) const -> PackedArray {
    return PackedArray::FromInt(impl_ < o.impl_ ? 1 : 0, 1, false, false);
  }
  [[nodiscard]] auto operator<=(const String& o) const -> PackedArray {
    return PackedArray::FromInt(impl_ <= o.impl_ ? 1 : 0, 1, false, false);
  }
  [[nodiscard]] auto operator>(const String& o) const -> PackedArray {
    return PackedArray::FromInt(impl_ > o.impl_ ? 1 : 0, 1, false, false);
  }
  [[nodiscard]] auto operator>=(const String& o) const -> PackedArray {
    return PackedArray::FromInt(impl_ >= o.impl_ ? 1 : 0, 1, false, false);
  }

  [[nodiscard]] auto operator+(const String& o) const -> String {
    return String{impl_ + o.impl_};
  }

  // LRM 6.16.1: len() yields an SV int.
  [[nodiscard]] auto Len() const -> PackedArray {
    return PackedArray::Int(static_cast<std::int32_t>(impl_.size()));
  }

  // LRM 6.16.2. Out-of-range index or zero byte -- no change.
  void Putc(const PackedArray& i_arg, const PackedArray& c_arg) {
    const auto c = static_cast<std::int8_t>(c_arg.ToInt64());
    if (c == 0) return;
    const auto i = i_arg.ToInt64();
    if (i < 0 || static_cast<std::size_t>(i) >= impl_.size()) return;
    impl_[static_cast<std::size_t>(i)] = static_cast<char>(c);
  }

  // LRM 6.16.3: getc() yields an SV byte. Out-of-range index returns 0.
  [[nodiscard]] auto Getc(const PackedArray& i_arg) const -> PackedArray {
    const auto i = i_arg.ToInt64();
    if (i < 0 || static_cast<std::size_t>(i) >= impl_.size()) {
      return PackedArray::Byte(0);
    }
    return PackedArray::Byte(
        static_cast<std::int8_t>(impl_[static_cast<std::size_t>(i)]));
  }

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
  [[nodiscard]] auto Compare(const String& s) const -> PackedArray {
    const int r = impl_.compare(s.impl_);
    if (r < 0) return PackedArray::Int(-1);
    if (r > 0) return PackedArray::Int(1);
    return PackedArray::Int(0);
  }

  // LRM 6.16.7: icompare() yields an SV int. Case-insensitive strcmp.
  [[nodiscard]] auto Icompare(const String& s) const -> PackedArray {
    const std::size_t n = std::min(impl_.size(), s.impl_.size());
    for (std::size_t k = 0; k < n; ++k) {
      const int a = std::tolower(static_cast<unsigned char>(impl_[k]));
      const int b = std::tolower(static_cast<unsigned char>(s.impl_[k]));
      if (a != b) return PackedArray::Int((a < b) ? -1 : 1);
    }
    if (impl_.size() == s.impl_.size()) return PackedArray::Int(0);
    return PackedArray::Int((impl_.size() < s.impl_.size()) ? -1 : 1);
  }

  // LRM 6.16.8. i..j inclusive. Returns "" if i<0, j<i, or j>=len.
  [[nodiscard]] auto Substr(
      const PackedArray& i_arg, const PackedArray& j_arg) const -> String {
    const auto i = static_cast<std::int32_t>(i_arg.ToInt64());
    const auto j = static_cast<std::int32_t>(j_arg.ToInt64());
    const auto n = static_cast<std::int32_t>(impl_.size());
    if (i < 0 || j < i || j >= n) return String{};
    return String{impl_.substr(
        static_cast<std::size_t>(i), static_cast<std::size_t>(j - i + 1))};
  }

  // LRM 6.16.9: the ato* family yields an SV integer (4-state). Parse leading
  // optional sign and digits (with `_` skipped); 0 if no digits were consumed.
  [[nodiscard]] auto Atoi() const -> PackedArray {
    return PackedArray::Integer(ParseInt(10));
  }
  [[nodiscard]] auto Atohex() const -> PackedArray {
    return PackedArray::Integer(ParseInt(16));
  }
  [[nodiscard]] auto Atooct() const -> PackedArray {
    return PackedArray::Integer(ParseInt(8));
  }
  [[nodiscard]] auto Atobin() const -> PackedArray {
    return PackedArray::Integer(ParseInt(2));
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
  // representation of the argument in the corresponding base / format.
  // Out-of-line so std::format does not leak into every translation unit
  // that includes this header.
  void Itoa(const PackedArray& i);
  void Hextoa(const PackedArray& i);
  void Octtoa(const PackedArray& i);
  void Bintoa(const PackedArray& i);
  void Realtoa(const Real& r);

  [[nodiscard]] auto View() const -> std::string_view {
    return impl_;
  }

 private:
  // Hand-rolled to avoid std::from_chars's pointer-pair API; accumulates one
  // digit at a time. Underscores are skipped per the LRM atoi family rules.
  [[nodiscard]] auto ParseInt(int base) const -> std::int32_t {
    std::size_t k = 0;
    bool negative = false;
    if (k < impl_.size() && (impl_[k] == '+' || impl_[k] == '-')) {
      negative = (impl_[k] == '-');
      ++k;
    }
    std::int64_t value = 0;
    bool any_digit = false;
    for (; k < impl_.size(); ++k) {
      const char ch = impl_[k];
      if (ch == '_') continue;
      const int digit = DigitValue(ch, base);
      if (digit < 0) break;
      value = (value * base) + digit;
      any_digit = true;
    }
    if (!any_digit) return 0;
    if (negative) value = -value;
    return static_cast<std::int32_t>(value);
  }

  [[nodiscard]] static auto DigitValue(char ch, int base) -> int {
    int v = -1;
    if (ch >= '0' && ch <= '9')
      v = ch - '0';
    else if (ch >= 'a' && ch <= 'f')
      v = (ch - 'a') + 10;
    else if (ch >= 'A' && ch <= 'F')
      v = (ch - 'A') + 10;
    if (v < 0 || v >= base) return -1;
    return v;
  }

  std::string impl_;
};

static_assert(LyraValue<String>);
static_assert(Lengthable<String>);

}  // namespace lyra::value
