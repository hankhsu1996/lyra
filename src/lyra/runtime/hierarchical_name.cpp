#include "lyra/runtime/hierarchical_name.hpp"

#include <algorithm>
#include <cctype>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <system_error>
#include <variant>
#include <vector>

#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/support/scope_kind.hpp"
#include "lyra/support/simple_identifier.hpp"

namespace lyra::runtime {

namespace {

auto IsWhiteSpace(char c) -> bool {
  return std::isspace(static_cast<unsigned char>(c)) != 0;
}

// Reads the hierarchical name of `text` from its start.
class NameReader {
 public:
  explicit NameReader(std::string_view text) : text_(text) {
  }

  auto Read() -> std::optional<HierarchicalName> {
    HierarchicalName name;
    while (true) {
      std::optional<NamePart> part = ReadPart();
      if (!part.has_value()) return std::nullopt;
      if (name.parts.empty() && !name.package.has_value() &&
          Rest().starts_with("::")) {
        if (!part->indices.empty()) return std::nullopt;
        name.package = part->identifier;
        at_ += 2;
        if (at_ == text_.size()) return name;
        continue;
      }
      name.parts.push_back(*std::move(part));
      if (at_ == text_.size()) return name;
      if (text_[at_] != '.') return std::nullopt;
      ++at_;
    }
  }

 private:
  [[nodiscard]] auto Rest() const -> std::string_view {
    return text_.substr(at_);
  }

  // One identifier, escaped or simple, and the selects after it.
  auto ReadPart() -> std::optional<NamePart> {
    NamePart part{.identifier = {}, .indices = {}};
    if (at_ < text_.size() && text_[at_] == '\\') {
      const std::size_t from = at_ + 1;
      std::size_t to = from;
      while (to < text_.size() && !IsWhiteSpace(text_[to])) ++to;
      part.identifier = text_.substr(from, to - from);
      at_ = std::min(to + 1, text_.size());
    } else {
      const std::size_t from = at_;
      if (at_ < text_.size() && support::IsSimpleIdentifierStart(text_[at_])) {
        ++at_;
        while (at_ < text_.size() &&
               support::IsSimpleIdentifierContinue(text_[at_])) {
          ++at_;
        }
      }
      part.identifier = text_.substr(from, at_ - from);
    }
    if (part.identifier.empty()) return std::nullopt;
    while (at_ < text_.size() && text_[at_] == '[') {
      std::optional<NameIndex> index = ReadIndex();
      if (!index.has_value()) return std::nullopt;
      part.indices.push_back(*std::move(index));
    }
    return part;
  }

  // `[`, a decimal integer or a string literal, `]`, white space allowed
  // around what it selects by. A decimal no 64-bit integer holds is its bits
  // as a signed number, four to a digit and one for the sign being room enough
  // for any.
  auto ReadIndex() -> std::optional<NameIndex> {
    ++at_;
    SkipWhiteSpace();
    std::optional<NameIndex> index;
    if (at_ < text_.size() && text_[at_] == '"') {
      index = ReadStringLiteral();
    } else {
      const std::string_view digits = ReadDecimal();
      std::int64_t number = 0;
      const char* const begin = std::to_address(digits.begin());
      const char* const end = std::to_address(digits.end());
      const auto [stopped, error] = std::from_chars(begin, end, number);
      if (error == std::errc::result_out_of_range) {
        index = value::BitVec::OfDecimal(digits, (digits.size() * 4) + 1);
      } else if (error == std::errc{} && stopped == end) {
        index = number;
      }
    }
    if (!index.has_value()) return std::nullopt;
    SkipWhiteSpace();
    if (at_ == text_.size() || text_[at_] != ']') return std::nullopt;
    ++at_;
    return index;
  }

  // A decimal number's text, a sign and the digits after it, read past.
  auto ReadDecimal() -> std::string_view {
    const std::size_t from = at_;
    if (at_ < text_.size() && text_[at_] == '-') ++at_;
    while (at_ < text_.size() && text_[at_] >= '0' && text_[at_] <= '9') {
      ++at_;
    }
    return text_.substr(from, at_ - from);
  }

  // A string literal's text, its escape sequences read as LRM 5.9 Table 5-1
  // has them; any other escaped character is that character, and an octal
  // code above \377 names no character.
  auto ReadStringLiteral() -> std::optional<std::string> {
    ++at_;
    std::string text;
    while (at_ < text_.size()) {
      const char next = text_[at_++];
      if (next == '"') return text;
      if (next != '\\') {
        text += next;
        continue;
      }
      if (at_ == text_.size()) return std::nullopt;
      const char escaped = text_[at_++];
      switch (escaped) {
        case 'n':
          text += '\n';
          break;
        case 't':
          text += '\t';
          break;
        case 'v':
          text += '\v';
          break;
        case 'f':
          text += '\f';
          break;
        case 'a':
          text += '\a';
          break;
        case '\\':
        case '"':
          text += escaped;
          break;
        case 'x': {
          const std::optional<char> code = ReadDigits(16, 2);
          if (!code.has_value()) return std::nullopt;
          text += *code;
          break;
        }
        default: {
          if (DigitValue(escaped) < 0 || DigitValue(escaped) >= 8) {
            text += escaped;
            break;
          }
          --at_;
          const std::optional<char> code = ReadDigits(8, 3);
          if (!code.has_value()) return std::nullopt;
          text += *code;
          break;
        }
      }
    }
    return std::nullopt;
  }

  // The character coded by one to `most` digits of `base`, none where no
  // digit follows or the code is more than a byte holds.
  auto ReadDigits(int base, std::size_t most) -> std::optional<char> {
    unsigned code = 0;
    std::size_t read = 0;
    while (read < most && at_ < text_.size()) {
      const int digit = DigitValue(text_[at_]);
      if (digit < 0 || digit >= base) break;
      code =
          (code * static_cast<unsigned>(base)) + static_cast<unsigned>(digit);
      ++at_;
      ++read;
    }
    if (read == 0 || code > 0xFFU) return std::nullopt;
    return static_cast<char>(code);
  }

  static auto DigitValue(char digit) -> int {
    if (digit >= '0' && digit <= '9') return digit - '0';
    if (digit >= 'a' && digit <= 'f') return digit - 'a' + 10;
    if (digit >= 'A' && digit <= 'F') return digit - 'A' + 10;
    return -1;
  }

  void SkipWhiteSpace() {
    while (at_ < text_.size() && IsWhiteSpace(text_[at_])) ++at_;
  }

  std::string_view text_;
  std::size_t at_ = 0;
};

// Whether `part` names the scope standing at `segment`.
auto IsNamedBy(const HierarchySegment& segment, const NamePart& part) -> bool {
  return IdentifierOf(segment.Declaration().name) == part.identifier &&
         std::ranges::equal(
             segment.Indices(), part.indices,
             [](std::int64_t index, const NameIndex& named) {
               const auto* number = std::get_if<std::int64_t>(&named);
               return number != nullptr && *number == index;
             });
}

}  // namespace

auto StringLiteralOf(std::string_view text) -> std::string {
  std::string literal = "\"";
  for (const char character : text) {
    switch (character) {
      case '\\':
        literal += "\\\\";
        break;
      case '"':
        literal += "\\\"";
        break;
      case '\n':
        literal += "\\n";
        break;
      case '\t':
        literal += "\\t";
        break;
      default: {
        const auto code = static_cast<unsigned char>(character);
        if (code < 0x20U || code >= 0x7fU) {
          // Three digits always, so a digit after it is no part of it.
          literal += '\\';
          literal += static_cast<char>('0' + ((code >> 6U) & 7U));
          literal += static_cast<char>('0' + ((code >> 3U) & 7U));
          literal += static_cast<char>('0' + (code & 7U));
        } else {
          literal += character;
        }
        break;
      }
    }
  }
  literal += '"';
  return literal;
}

auto ReadHierarchicalName(std::string_view text)
    -> std::optional<HierarchicalName> {
  return NameReader(text).Read();
}

auto IdentifierOf(std::string_view written) -> std::string_view {
  if (!written.starts_with('\\')) return written;
  written.remove_prefix(1);
  while (!written.empty() && IsWhiteSpace(written.back())) {
    written.remove_suffix(1);
  }
  return written;
}

auto LongestNamedPrefix(Scope& from, std::span<const NamePart> parts)
    -> ScopeReached {
  ScopeReached reached{.scope = &from, .parts = 0};
  for (const NamePart& part : parts) {
    Scope* child = ChildNamed(*reached.scope, part);
    if (child == nullptr) break;
    reached = {.scope = child, .parts = reached.parts + 1};
  }
  return reached;
}

auto NamedEnclosingScopes(const abi::LexicalScopeRecord* innermost)
    -> std::vector<const abi::LexicalScopeRecord*> {
  std::vector<const abi::LexicalScopeRecord*> named;
  for (const abi::LexicalScopeRecord* at = innermost; at != nullptr;
       at = at->enclosing) {
    if (at->Named()) named.push_back(at);
  }
  std::ranges::reverse(named);
  return named;
}

auto LexicalDeclarationNamed(
    std::span<const abi::LexicalScopeRecord* const> declarations,
    const abi::LexicalScopeRecord* enclosing, std::string_view identifier)
    -> const abi::LexicalScopeRecord* {
  for (const abi::LexicalScopeRecord* declaration : declarations) {
    if (declaration->enclosing == enclosing &&
        IdentifierOf(declaration->name) == identifier) {
      return declaration;
    }
  }
  return nullptr;
}

auto PackageNamed(
    std::span<const abi::ScopeDeclarations* const> namespaces,
    std::string_view identifier) -> const abi::ScopeDeclarations* {
  for (const abi::ScopeDeclarations* declared : namespaces) {
    if (!support::IsCompilationUnit(declared->definition->Kind()) &&
        IdentifierOf(declared->definition->name) == identifier) {
      return declared;
    }
  }
  return nullptr;
}

auto ChildNamed(Scope& scope, const NamePart& part) -> Scope* {
  Scope* found = nullptr;
  scope.ForEachChild([&](Scope& child) {
    if (found == nullptr && IsNamedBy(child.Segment(), part)) {
      found = &child;
    }
  });
  return found;
}

}  // namespace lyra::runtime
