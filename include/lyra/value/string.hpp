#pragma once

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <cstdlib>
#include <format>
#include <string>
#include <string_view>
#include <utility>

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

  [[nodiscard]] auto operator==(const String& o) const -> bool {
    return impl_ == o.impl_;
  }
  [[nodiscard]] auto operator!=(const String& o) const -> bool {
    return impl_ != o.impl_;
  }
  [[nodiscard]] auto operator<(const String& o) const -> bool {
    return impl_ < o.impl_;
  }
  [[nodiscard]] auto operator<=(const String& o) const -> bool {
    return impl_ <= o.impl_;
  }
  [[nodiscard]] auto operator>(const String& o) const -> bool {
    return impl_ > o.impl_;
  }
  [[nodiscard]] auto operator>=(const String& o) const -> bool {
    return impl_ >= o.impl_;
  }

  [[nodiscard]] auto operator+(const String& o) const -> String {
    return String{impl_ + o.impl_};
  }

  // LRM 6.16.1
  [[nodiscard]] auto Len() const -> std::int32_t {
    return static_cast<std::int32_t>(impl_.size());
  }

  // LRM 6.16.2. Out-of-range index or zero byte -- no change.
  void Putc(std::int32_t i, std::int8_t c) {
    if (c == 0) return;
    if (i < 0 || static_cast<std::size_t>(i) >= impl_.size()) return;
    impl_[static_cast<std::size_t>(i)] = static_cast<char>(c);
  }

  // LRM 6.16.3. Out-of-range index returns 0.
  [[nodiscard]] auto Getc(std::int32_t i) const -> std::int8_t {
    if (i < 0 || static_cast<std::size_t>(i) >= impl_.size()) return 0;
    return static_cast<std::int8_t>(impl_[static_cast<std::size_t>(i)]);
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

  // LRM 6.16.6. ANSI C strcmp semantics: negative / zero / positive.
  [[nodiscard]] auto Compare(const String& s) const -> std::int32_t {
    const int r = impl_.compare(s.impl_);
    if (r < 0) return -1;
    if (r > 0) return 1;
    return 0;
  }

  // LRM 6.16.7. Case-insensitive strcmp.
  [[nodiscard]] auto Icompare(const String& s) const -> std::int32_t {
    const std::size_t n = std::min(impl_.size(), s.impl_.size());
    for (std::size_t k = 0; k < n; ++k) {
      const int a = std::tolower(static_cast<unsigned char>(impl_[k]));
      const int b = std::tolower(static_cast<unsigned char>(s.impl_[k]));
      if (a != b) return (a < b) ? -1 : 1;
    }
    if (impl_.size() == s.impl_.size()) return 0;
    return (impl_.size() < s.impl_.size()) ? -1 : 1;
  }

  // LRM 6.16.8. i..j inclusive. Returns "" if i<0, j<i, or j>=len.
  [[nodiscard]] auto Substr(std::int32_t i, std::int32_t j) const -> String {
    const auto n = static_cast<std::int32_t>(impl_.size());
    if (i < 0 || j < i || j >= n) return String{};
    return String{impl_.substr(
        static_cast<std::size_t>(i), static_cast<std::size_t>(j - i + 1))};
  }

  // LRM 6.16.9. Parse leading optional sign and digits (with `_` skipped).
  // Returns 0 if no digits were consumed.
  [[nodiscard]] auto Atoi() const -> std::int32_t {
    return ParseInt(10);
  }
  [[nodiscard]] auto Atohex() const -> std::int32_t {
    return ParseInt(16);
  }
  [[nodiscard]] auto Atooct() const -> std::int32_t {
    return ParseInt(8);
  }
  [[nodiscard]] auto Atobin() const -> std::int32_t {
    return ParseInt(2);
  }

  // LRM 6.16.10. Parse leading real-number syntax; 0.0 if none.
  [[nodiscard]] auto Atoreal() const -> double {
    if (impl_.empty()) return 0.0;
    char* end_ptr = nullptr;
    const double v = std::strtod(impl_.c_str(), &end_ptr);
    if (end_ptr == impl_.c_str()) return 0.0;
    return v;
  }

  // LRM 6.16.11 through 6.16.15. Each replaces receiver with the ASCII
  // representation of the argument in the corresponding base / format.
  void Itoa(std::int32_t i) {
    impl_ = std::format("{}", i);
  }
  void Hextoa(std::int32_t i) {
    impl_ = std::format("{:x}", static_cast<std::uint32_t>(i));
  }
  void Octtoa(std::int32_t i) {
    impl_ = std::format("{:o}", static_cast<std::uint32_t>(i));
  }
  void Bintoa(std::int32_t i) {
    impl_ = std::format("{:b}", static_cast<std::uint32_t>(i));
  }
  void Realtoa(double r) {
    impl_ = std::format("{}", r);
  }

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

}  // namespace lyra::value
