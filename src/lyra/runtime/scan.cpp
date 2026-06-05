#include "lyra/runtime/scan.hpp"

#include <algorithm>
#include <cerrno>
#include <cstddef>
#include <cstdint>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/runtime_services.hpp"
#include "lyra/runtime/scan_source.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

// One scanned bit as it leaves a per-spec parser. The val/unk pair mirrors
// PackedArray's storage planes: 0=(0,0), 1=(1,0), X=(1,1), Z=(0,1).
struct ScannedBit {
  bool val;
  bool unk;
};

[[nodiscard]] auto IsAsciiWhitespace(int ch) -> bool {
  return ch == ' ' || ch == '\t' || ch == '\n' || ch == '\r' || ch == '\f' ||
         ch == '\v';
}

[[nodiscard]] auto IsDecDigit(int ch) -> bool {
  return ch >= '0' && ch <= '9';
}

[[nodiscard]] auto HexDigit(int ch) -> int {
  if (ch >= '0' && ch <= '9') {
    return ch - '0';
  }
  if (ch >= 'a' && ch <= 'f') {
    return (ch - 'a') + 10;
  }
  if (ch >= 'A' && ch <= 'F') {
    return (ch - 'A') + 10;
  }
  return -1;
}

[[nodiscard]] auto OctDigit(int ch) -> int {
  if (ch >= '0' && ch <= '7') {
    return ch - '0';
  }
  return -1;
}

void SkipSourceWhitespace(ScanSource& src) {
  while (true) {
    const int ch = src.Peek();
    if (ch == -1 || !IsAsciiWhitespace(ch)) {
      return;
    }
    src.Consume();
  }
}

// Write `bits` (MSB-first, i.e. bits[0] is the leftmost scanned digit /
// bit) into `dest`. If `bits.size() < dest.BitWidth()`, MSBs of dest fill
// with zero. If `bits.size() > dest.BitWidth()`, the high (leading) bits
// are dropped -- only the trailing dest_width bits land. For a 2-state
// dest, X/Z scanned bits collapse to 0 per the LRM 4-state -> 2-state
// convention.
void AssignBitsMsbFirst(
    value::PackedArray& dest, std::span<const ScannedBit> bits) {
  const std::uint64_t width = dest.BitWidth();
  const auto word_count = static_cast<std::size_t>((width + 63U) / 64U);
  std::vector<std::uint64_t> val_words(word_count, 0U);
  std::vector<std::uint64_t> unk_words(word_count, 0U);

  const std::size_t bits_to_use = std::min<std::size_t>(bits.size(), width);
  const std::size_t skip = bits.size() - bits_to_use;
  for (std::size_t i = 0; i < bits_to_use; ++i) {
    const std::size_t bit_pos = bits_to_use - 1U - i;
    const ScannedBit b = bits[skip + i];
    const std::size_t word_ix = bit_pos / 64U;
    const std::size_t bit_ix = bit_pos % 64U;
    if (b.val) {
      val_words[word_ix] |= (std::uint64_t{1} << bit_ix);
    }
    if (b.unk) {
      unk_words[word_ix] |= (std::uint64_t{1} << bit_ix);
    }
  }

  const bool four_state = dest.IsFourState();
  if (!four_state) {
    for (std::size_t w = 0; w < word_count; ++w) {
      val_words[w] &= ~unk_words[w];
    }
    dest = value::PackedArray::FromWords(
        std::span<const std::uint64_t>{val_words},
        std::span<const std::uint64_t>{}, width, dest.IsSigned(), false);
    return;
  }
  dest = value::PackedArray::FromWords(
      std::span<const std::uint64_t>{val_words},
      std::span<const std::uint64_t>{unk_words}, width, dest.IsSigned(), true);
}

void FillAllX(value::PackedArray& dest) {
  const std::uint64_t width = dest.BitWidth();
  const bool four_state = dest.IsFourState();
  if (!four_state) {
    dest = value::PackedArray::FromInt(0, width, dest.IsSigned(), false);
    return;
  }
  const auto word_count = static_cast<std::size_t>((width + 63U) / 64U);
  std::vector<std::uint64_t> val_words(word_count, ~std::uint64_t{0});
  std::vector<std::uint64_t> unk_words(word_count, ~std::uint64_t{0});
  dest = value::PackedArray::FromWords(
      std::span<const std::uint64_t>{val_words},
      std::span<const std::uint64_t>{unk_words}, width, dest.IsSigned(), true);
}

void FillAllZ(value::PackedArray& dest) {
  const std::uint64_t width = dest.BitWidth();
  const bool four_state = dest.IsFourState();
  if (!four_state) {
    dest = value::PackedArray::FromInt(0, width, dest.IsSigned(), false);
    return;
  }
  const auto word_count = static_cast<std::size_t>((width + 63U) / 64U);
  std::vector<std::uint64_t> val_words(word_count, 0U);
  std::vector<std::uint64_t> unk_words(word_count, ~std::uint64_t{0});
  dest = value::PackedArray::FromWords(
      std::span<const std::uint64_t>{val_words},
      std::span<const std::uint64_t>{unk_words}, width, dest.IsSigned(), true);
}

void AssignFromI64(value::PackedArray& dest, std::int64_t value) {
  dest = value::PackedArray::FromInt(
      value, dest.BitWidth(), dest.IsSigned(), dest.IsFourState());
}

[[nodiscard]] auto RequireIntegralSlot(
    const ScanSlot& slot, std::string_view spec) -> value::PackedArray& {
  value::PackedArray* dest = slot.AsIntegral();
  if (dest == nullptr) {
    throw InternalError(
        std::format(
            "$sscanf/$fscanf: format spec '%{}' expects an integral output "
            "argument, but the corresponding actual is not integral",
            spec));
  }
  return *dest;
}

[[nodiscard]] auto RequireStringSlot(
    const ScanSlot& slot, std::string_view spec) -> value::String& {
  value::String* dest = slot.AsString();
  if (dest == nullptr) {
    throw InternalError(
        std::format(
            "$sscanf/$fscanf: format spec '%{}' expects a string output "
            "argument, but the corresponding actual is not a string",
            spec));
  }
  return *dest;
}

// LRM 21.3.4.3 Table 21-7 `%d`. Either a sign-prefixed decimal digit run
// (with `_` separators) or a single x/X/z/Z/? that fills the entire dest.
// Mixed (`12x`) stops at the first non-digit -- the `x` is not consumed
// here, so a following `%h` could still match it.
[[nodiscard]] auto ScanDecimal(ScanSource& src, const ScanSlot& slot) -> bool {
  SkipSourceWhitespace(src);
  int ch = src.Peek();
  if (ch == -1) {
    return false;
  }

  value::PackedArray& dest = RequireIntegralSlot(slot, "d");

  if (ch == 'x' || ch == 'X') {
    src.Consume();
    FillAllX(dest);
    return true;
  }
  if (ch == 'z' || ch == 'Z' || ch == '?') {
    src.Consume();
    FillAllZ(dest);
    return true;
  }

  bool negative = false;
  bool had_sign = false;
  if (ch == '+' || ch == '-') {
    negative = (ch == '-');
    had_sign = true;
    src.Consume();
    ch = src.Peek();
  }

  if (!IsDecDigit(ch)) {
    if (had_sign) {
      src.Unget(negative ? '-' : '+');
    }
    return false;
  }

  std::int64_t acc = 0;
  bool consumed_digit = false;
  while (ch != -1 && (IsDecDigit(ch) || ch == '_')) {
    if (ch != '_') {
      acc = (acc * 10) + (ch - '0');
      consumed_digit = true;
    }
    src.Consume();
    ch = src.Peek();
  }
  if (!consumed_digit) {
    return false;
  }
  if (negative) {
    acc = -acc;
  }
  AssignFromI64(dest, acc);
  return true;
}

// LRM 21.3.4.3 Table 21-7 `%h` / `%x`. Each character -> one 4-bit nibble.
// Hex digits 0..9 a..f A..F land as (val=digit, unk=0); xX as
// (val=1111, unk=1111); zZ? as (val=0000, unk=1111); `_` is a separator.
[[nodiscard]] auto ScanHex(ScanSource& src, const ScanSlot& slot) -> bool {
  SkipSourceWhitespace(src);
  value::PackedArray& dest = RequireIntegralSlot(slot, "h");
  std::vector<ScannedBit> bits;
  bool consumed_digit = false;
  while (true) {
    const int ch = src.Peek();
    if (ch == -1) {
      break;
    }
    const int hd = HexDigit(ch);
    if (hd >= 0) {
      for (int b = 3; b >= 0; --b) {
        bits.push_back(
            {.val = ((static_cast<unsigned>(hd) >> static_cast<unsigned>(b)) &
                     1U) != 0U,
             .unk = false});
      }
      consumed_digit = true;
    } else if (ch == 'x' || ch == 'X') {
      for (int b = 0; b < 4; ++b) {
        bits.push_back({.val = true, .unk = true});
      }
      consumed_digit = true;
    } else if (ch == 'z' || ch == 'Z' || ch == '?') {
      for (int b = 0; b < 4; ++b) {
        bits.push_back({.val = false, .unk = true});
      }
      consumed_digit = true;
    } else if (ch == '_') {
      // separator, no bit
    } else {
      break;
    }
    src.Consume();
  }
  if (!consumed_digit) {
    return false;
  }
  AssignBitsMsbFirst(dest, std::span<const ScannedBit>{bits});
  return true;
}

// LRM 21.3.4.3 Table 21-7 `%o`. 3 bits per octal digit; xX -> (111,111),
// zZ? -> (000,111); `_` is a separator.
[[nodiscard]] auto ScanOctal(ScanSource& src, const ScanSlot& slot) -> bool {
  SkipSourceWhitespace(src);
  value::PackedArray& dest = RequireIntegralSlot(slot, "o");
  std::vector<ScannedBit> bits;
  bool consumed_digit = false;
  while (true) {
    const int ch = src.Peek();
    if (ch == -1) {
      break;
    }
    const int od = OctDigit(ch);
    if (od >= 0) {
      for (int b = 2; b >= 0; --b) {
        bits.push_back(
            {.val = ((static_cast<unsigned>(od) >> static_cast<unsigned>(b)) &
                     1U) != 0U,
             .unk = false});
      }
      consumed_digit = true;
    } else if (ch == 'x' || ch == 'X') {
      for (int b = 0; b < 3; ++b) {
        bits.push_back({.val = true, .unk = true});
      }
      consumed_digit = true;
    } else if (ch == 'z' || ch == 'Z' || ch == '?') {
      for (int b = 0; b < 3; ++b) {
        bits.push_back({.val = false, .unk = true});
      }
      consumed_digit = true;
    } else if (ch == '_') {
      // separator
    } else {
      break;
    }
    src.Consume();
  }
  if (!consumed_digit) {
    return false;
  }
  AssignBitsMsbFirst(dest, std::span<const ScannedBit>{bits});
  return true;
}

// LRM 21.3.4.3 Table 21-7 `%b`. Per-character to one bit: 0/1 -> (val,0),
// xX -> (1,1), zZ? -> (0,1), `_` separator.
[[nodiscard]] auto ScanBinary(ScanSource& src, const ScanSlot& slot) -> bool {
  SkipSourceWhitespace(src);
  value::PackedArray& dest = RequireIntegralSlot(slot, "b");
  std::vector<ScannedBit> bits;
  bool consumed_digit = false;
  while (true) {
    const int ch = src.Peek();
    if (ch == -1) {
      break;
    }
    if (ch == '0') {
      bits.push_back({.val = false, .unk = false});
      consumed_digit = true;
    } else if (ch == '1') {
      bits.push_back({.val = true, .unk = false});
      consumed_digit = true;
    } else if (ch == 'x' || ch == 'X') {
      bits.push_back({.val = true, .unk = true});
      consumed_digit = true;
    } else if (ch == 'z' || ch == 'Z' || ch == '?') {
      bits.push_back({.val = false, .unk = true});
      consumed_digit = true;
    } else if (ch == '_') {
      // separator
    } else {
      break;
    }
    src.Consume();
  }
  if (!consumed_digit) {
    return false;
  }
  AssignBitsMsbFirst(dest, std::span<const ScannedBit>{bits});
  return true;
}

// Standard scanf `%s`: skip leading whitespace, then read non-whitespace
// chars until whitespace or EOF.
[[nodiscard]] auto ScanString(ScanSource& src, const ScanSlot& slot) -> bool {
  SkipSourceWhitespace(src);
  std::string buf;
  while (true) {
    const int ch = src.Peek();
    if (ch == -1 || IsAsciiWhitespace(ch)) {
      break;
    }
    buf.push_back(static_cast<char>(ch));
    src.Consume();
  }
  if (buf.empty()) {
    return false;
  }
  value::String& dest = RequireStringSlot(slot, "s");
  dest = value::String(std::move(buf));
  return true;
}

// Standard scanf `%c`: no whitespace skip, one byte stored as int8.
[[nodiscard]] auto ScanChar(ScanSource& src, const ScanSlot& slot) -> bool {
  const int ch = src.Consume();
  if (ch == -1) {
    return false;
  }
  value::PackedArray& dest = RequireIntegralSlot(slot, "c");
  AssignFromI64(dest, static_cast<std::int64_t>(ch & 0xFF));
  return true;
}

[[nodiscard]] auto ScanFromSource(
    ScanSource& src, std::string_view fmt, std::span<const ScanSlot> slots)
    -> std::int32_t {
  std::int32_t items = 0;
  std::size_t slot_ix = 0;
  bool first_conversion = true;
  std::size_t fmt_ix = 0;

  while (fmt_ix < fmt.size()) {
    const char fc = fmt[fmt_ix];

    if (IsAsciiWhitespace(static_cast<unsigned char>(fc))) {
      // Any whitespace in the format matches zero-or-more whitespace in
      // the input.
      SkipSourceWhitespace(src);
      ++fmt_ix;
      continue;
    }

    if (fc != '%') {
      const int ch = src.Peek();
      if (ch == -1) {
        return first_conversion ? -1 : items;
      }
      if (ch != static_cast<unsigned char>(fc)) {
        return items;
      }
      src.Consume();
      ++fmt_ix;
      continue;
    }

    ++fmt_ix;
    if (fmt_ix >= fmt.size()) {
      throw InternalError(
          "$sscanf/$fscanf: format string ended after '%' with no conversion "
          "specifier");
    }
    const char spec = fmt[fmt_ix];
    ++fmt_ix;

    if (spec == '%') {
      const int ch = src.Peek();
      if (ch == -1) {
        return first_conversion ? -1 : items;
      }
      if (ch != '%') {
        return items;
      }
      src.Consume();
      continue;
    }

    // Field width (`%5d`) and assignment suppression (`%*d`) are deferred;
    // detect explicitly so a future-supported format does not silently
    // miscount items.
    if (spec == '*' || IsDecDigit(static_cast<unsigned char>(spec))) {
      throw InternalError(
          "$sscanf/$fscanf: field width and assignment suppression are not "
          "yet supported (LRM 21.3.4.3)");
    }

    if (slot_ix >= slots.size()) {
      throw InternalError(
          "$sscanf/$fscanf: format string has more conversion specifiers "
          "than output arguments");
    }
    const ScanSlot slot = slots[slot_ix];

    bool ok = false;
    switch (spec) {
      case 'd':
        ok = ScanDecimal(src, slot);
        break;
      case 'h':
      case 'x':
        ok = ScanHex(src, slot);
        break;
      case 'b':
        ok = ScanBinary(src, slot);
        break;
      case 'o':
        ok = ScanOctal(src, slot);
        break;
      case 's':
        ok = ScanString(src, slot);
        break;
      case 'c':
        ok = ScanChar(src, slot);
        break;
      default:
        throw InternalError(
            std::format(
                "$sscanf/$fscanf: unsupported conversion specifier '%{}'",
                spec));
    }

    if (!ok) {
      // Mismatch / EOF mid-conversion. The LRM distinguishes "EOF before
      // any conversion" (-1) from "match failure after some" (return the
      // count so far).
      if (first_conversion && src.Peek() == -1) {
        return -1;
      }
      return items;
    }
    first_conversion = false;
    ++items;
    ++slot_ix;
  }
  return items;
}

}  // namespace

auto LyraSScanf(
    const value::String& input, const value::String& format,
    std::initializer_list<ScanSlot> slots) -> value::PackedArray {
  StringScanSource src(input.View());
  const std::int32_t items = ScanFromSource(
      src, format.View(),
      std::span<const ScanSlot>{slots.begin(), slots.size()});
  return value::PackedArray::Int(items);
}

auto LyraFScanf(
    RuntimeServices& services, const value::PackedArray& fd_pa,
    const value::String& format, std::initializer_list<ScanSlot> slots)
    -> value::PackedArray {
  const auto fd = static_cast<std::int32_t>(fd_pa.ToInt64());
  auto* slot = services.Files().ResolveSlot(fd);
  if (slot == nullptr) {
    // MCD descriptors, closed FDs, and unmapped FDs all land here. LRM
    // 21.3.4.3 specifies EOF (-1) when the input ends before the first
    // matching failure or conversion; we treat "no readable stream" as
    // that condition and additionally stamp $ferror so an SV caller
    // querying it sees EBADF.
    services.Files().SetError(
        fd, EBADF, "$fscanf: not an open file descriptor");
    return value::PackedArray::Int(-1);
  }
  if (!slot->permits_read) {
    services.Files().SetError(fd, EBADF, "$fscanf: file not open for reading");
    return value::PackedArray::Int(-1);
  }
  FileScanSource src(*slot);
  const std::int32_t items = ScanFromSource(
      src, format.View(),
      std::span<const ScanSlot>{slots.begin(), slots.size()});
  src.FlushPushback();
  return value::PackedArray::Int(items);
}

}  // namespace lyra::runtime
