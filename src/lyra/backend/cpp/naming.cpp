#include "lyra/backend/cpp/naming.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::backend::cpp {

namespace {

constexpr std::string_view kHexDigits = "0123456789abcdef";

auto IsPlainCppName(std::string_view name) -> bool {
  const auto is_word = [](char c) {
    return std::isalnum(static_cast<unsigned char>(c)) != 0 || c == '_';
  };
  return !name.empty() &&
         std::isdigit(static_cast<unsigned char>(name[0])) == 0 &&
         !name.starts_with(kMintedPrefix) &&
         std::ranges::all_of(name, is_word) &&
         !std::ranges::contains(kCppReservedWords, name);
}

}  // namespace

void WriteOne(TargetText& out, SourceName name) {
  if (IsPlainCppName(name.name)) {
    out += name.name;
    return;
  }
  Write(out, kMintedPrefix, "esc_");
  // Two hex digits a byte, so every byte keeps its own width and no two names
  // escape to one.
  for (const char c : name.name) {
    const auto byte = static_cast<unsigned char>(c);
    const std::array<char, 2> digits{
        kHexDigits[byte >> 4U], kHexDigits[byte & 0xFU]};
    out += std::string_view{digits.data(), digits.size()};
  }
}

namespace {

// A digest as sixteen hex digits, so it has one width wherever it is written.
struct DigestText {
  std::uint64_t digest;
};

void WriteOne(TargetText& out, DigestText text) {
  for (int shift = 60; shift >= 0; shift -= 4) {
    const char digit = kHexDigits[(text.digest >> shift) & 0xFU];
    out += std::string_view{&digit, 1};
  }
}

}  // namespace

void WriteOne(TargetText& out, const MintedName& name) {
  Write(out, kMintedPrefix, name.what, "_", name.ordinal);
  if (name.source.has_value()) {
    Write(out, "_", SourceName{.name = *name.source});
  }
}

void WriteOne(TargetText& out, MintedWord name) {
  Write(out, kMintedPrefix, name.word);
}

void WriteOne(TargetText& out, VerbatimName name) {
  out += name.text;
}

void WriteOne(TargetText& out, MintedPathName name) {
  Write(out, kMintedPrefix, name.what);
  const auto named = [&](std::string_view kind, std::string_view source) {
    const std::string identifier = TextOf(SourceName{.name = source});
    Write(out, "_", kind, identifier.size(), "_", identifier);
  };
  const auto placed = [&](std::string_view kind, std::uint32_t position) {
    Write(out, "_", kind, position);
  };
  const auto bound = [&](const std::optional<std::uint64_t>& arguments) {
    if (arguments.has_value()) Write(out, "_h", DigestText{*arguments});
  };
  for (const support::DefPathData& step : name.steps) {
    std::visit(
        Overloaded{
            [&](const support::GenerateBlockStep& block) {
              named("b", block.label);
              if (block.disambiguator != 0) {
                Write(out, "_x", block.disambiguator);
              }
              bound(block.arguments);
            },
            [&](const support::ClassStep& cls) {
              named("c", cls.name);
              bound(cls.arguments);
            },
            [&](const support::SubroutineStep& subroutine) {
              named("s", subroutine.name);
            },
            [&](const support::NamedBlockStep& block) {
              named("n", block.name);
            },
            [&](const support::UnnamedBlockStep& block) {
              placed("u", block.position);
            },
            [&](const support::TypeStep& type) { named("t", type.name); },
            [&](const support::UnnamedTypeStep& type) {
              placed("a", type.position);
            }},
        step);
  }
}

// The digest is sixteen hex digits and the number at most ten decimal ones, so
// which of them follows the word is told by its length; and a source name as a
// C++ identifier never starts with a digit, so a number after the digest is
// never read as the start of one.
void WriteOne(TargetText& out, const MintedAppliedName& name) {
  Write(out, kMintedPrefix, name.what, "_");
  if (name.arguments.has_value()) {
    Write(out, DigestText{*name.arguments}, "_");
  }
  if (name.disambiguator != 0) {
    Write(out, name.disambiguator, "_");
  }
  Write(out, SourceName{.name = name.source});
}

void WriteOne(TargetText& out, NameLiteral literal) {
  WriteCStringLiteral(literal.name, out);
}

void WriteOne(TargetText& out, const CppName& name) {
  std::visit(
      Overloaded{
          [&out](SourceName n) { WriteOne(out, n); },
          [&out](const MintedName& n) { WriteOne(out, n); },
          [&out](MintedWord n) { WriteOne(out, n); },
          [&out](VerbatimName n) { WriteOne(out, n); },
          [&out](MintedPathName n) { WriteOne(out, n); },
          [&out](const MintedAppliedName& n) { WriteOne(out, n); }},
      name);
}

void WriteOne(TargetText& out, UnitScope scope) {
  Write(out, "::", UnitNamespaceOf(scope.unit_name));
}

}  // namespace lyra::backend::cpp
