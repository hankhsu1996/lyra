#include "lyra/runtime/hierarchical_name.hpp"

#include <algorithm>
#include <cctype>
#include <charconv>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <system_error>
#include <vector>

#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/scope.hpp"
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
      std::optional<std::int64_t> index = ReadIndex();
      if (!index.has_value()) return std::nullopt;
      part.indices.push_back(*index);
    }
    return part;
  }

  // `[`, a decimal integer, `]`, white space allowed around the integer.
  auto ReadIndex() -> std::optional<std::int64_t> {
    ++at_;
    SkipWhiteSpace();
    std::int64_t index = 0;
    const char* first = text_.data() + at_;
    const char* last = text_.data() + text_.size();
    const auto [stopped, error] = std::from_chars(first, last, index);
    if (error != std::errc{}) return std::nullopt;
    at_ += static_cast<std::size_t>(stopped - first);
    SkipWhiteSpace();
    if (at_ == text_.size() || text_[at_] != ']') return std::nullopt;
    ++at_;
    return index;
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
         std::ranges::equal(segment.Indices(), part.indices);
}

}  // namespace

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
