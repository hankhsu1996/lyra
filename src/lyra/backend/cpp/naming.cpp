#include "lyra/backend/cpp/naming.hpp"

#include <algorithm>
#include <array>
#include <cctype>
#include <string_view>
#include <variant>

#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/overloaded.hpp"

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

void WriteOne(TargetText& out, const CppName& name) {
  std::visit(
      Overloaded{
          [&out](SourceName n) { WriteOne(out, n); },
          [&out](const MintedName& n) { WriteOne(out, n); },
          [&out](MintedWord n) { WriteOne(out, n); },
          [&out](VerbatimName n) { WriteOne(out, n); }},
      name);
}

void WriteOne(TargetText& out, UnitScope scope) {
  Write(out, "::", UnitNamespaceOf(scope.unit_name));
}

}  // namespace lyra::backend::cpp
