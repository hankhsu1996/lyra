#pragma once

#include <algorithm>
#include <cctype>
#include <cstdint>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/abi_adapter_id.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/namespace_storage_phase.hpp"
#include "lyra/mir/static_constant_id.hpp"

namespace lyra::backend::cpp {

// The prefix every name the compiler mints for the emitted C++ carries, and
// which no rendered source name can begin with. It is what lets a synthesized
// declaration be named at all: SystemVerilog reserves no spelling to the
// compiler, so a word minted without one is a word a program may also declare.
inline constexpr std::string_view kMintedPrefix = "sv_";

// A source name spelled as a C++ identifier. SystemVerilog admits every
// printable character but white space in an identifier (LRM 5.6.1) and C++
// admits letters, digits and underscores, so a name C++ cannot spell is escaped
// rather than repaired: mapping what C++ forbids onto what it allows maps two
// declarations that differ onto one token, and the emitted unit then declares
// one of them twice.
//
// A name renders as itself exactly when it is already a C++ identifier and does
// not begin with the minted prefix; everything else renders as that prefix, an
// escape marker, and its bytes in hex. The two images are disjoint, so no two
// source names meet and nothing the compiler mints is reachable from one.
[[nodiscard]] inline auto ToCppName(std::string_view name) -> std::string {
  const auto is_word = [](char c) {
    return std::isalnum(static_cast<unsigned char>(c)) != 0 || c == '_';
  };
  const bool plain =
      !name.empty() && std::isdigit(static_cast<unsigned char>(name[0])) == 0 &&
      !name.starts_with(kMintedPrefix) && std::ranges::all_of(name, is_word);
  if (plain) {
    return std::string{name};
  }
  std::string out{kMintedPrefix};
  out += "esc_";
  for (const char c : name) {
    out += std::format("{:02x}", static_cast<unsigned char>(c));
  }
  return out;
}

// The namespace a unit's declarations live in. That a unit's emitted peer is a
// namespace is one decision, and two kinds of site spell it: the header that
// opens the unit's own, and every reference that qualifies into another's. They
// have to agree or the reference resolves to nothing, so the mapping is
// answered here rather than at each of them -- which also tells a reader which
// of the identifiers around it is a namespace and which is a class.
[[nodiscard]] inline auto UnitNamespaceOf(std::string_view unit_name)
    -> std::string {
  return ToCppName(unit_name);
}

// The C++ identifier a declaration the compiler synthesized is emitted under.
// `what` says which kind it is and `ordinal` which one, because such a
// declaration has no source name to take one from.
[[nodiscard]] inline auto MintedCppName(
    std::string_view what, std::uint32_t ordinal) -> std::string {
  return std::format("{}{}_{}", kMintedPrefix, what, ordinal);
}

// The C++ identifier one body is emitted under, given the names its owner
// answers. A body the source named takes that name; one nothing names takes a
// minted name over the position it sits at, which no source name reaches. One
// answer, so the declaration, the definition, and every call spell it alike.
[[nodiscard]] inline auto CppCallableName(
    std::span<const mir::NamedCallable> named, mir::CallableId body)
    -> std::string {
  const std::optional<std::string_view> name = mir::NameOf(named, body);
  return name.has_value() ? ToCppName(*name)
                          : MintedCppName("body", body.value);
}

// The C++ identifiers a class's runtime-callback adapters and its compile-time
// constants are emitted under. The source declares neither, so each is named
// over the position it sits at in the arena its class owns -- the same identity
// every reference to one already carries.
[[nodiscard]] inline auto CppAbiAdapterName(mir::AbiAdapterId id)
    -> std::string {
  return MintedCppName("adapter", id.value);
}

[[nodiscard]] inline auto CppStaticConstantName(mir::StaticConstantId id)
    -> std::string {
  return MintedCppName("constant", id.value);
}

// The C++ identifier one of the two bodies bringing up a unit's namespace is
// emitted under. Another unit's emitted text names it, and the source declares
// no name for it, so both ends compose it from which of the two it is.
[[nodiscard]] inline auto CppStorageEntryName(mir::NamespaceStoragePhase phase)
    -> std::string {
  switch (phase) {
    case mir::NamespaceStoragePhase::kInstall:
      return std::format("{}install_namespace_storage", kMintedPrefix);
    case mir::NamespaceStoragePhase::kInitialize:
      return std::format("{}initialize_namespace_storage", kMintedPrefix);
  }
  throw InternalError("backend::cpp: unknown namespace storage phase");
}

// The C++ identifier one body of a unit's namespace is emitted under. Another
// unit's emitted text may name it, so what reaches it decides the spelling
// rather than the position it happens to sit at.
[[nodiscard]] inline auto CppUnitCallableName(
    const mir::CompilationUnit& unit, mir::CallableId body) -> std::string {
  return std::visit(
      Overloaded{
          [](const mir::ReachedByLinkageName& r) {
            return std::string{r.name};
          },
          [](const mir::ReachedByName& r) { return ToCppName(r.name); },
          [](const mir::ReachedByStoragePhase& r) {
            return CppStorageEntryName(r.phase);
          },
          [&](const mir::ReachedByNothing&) {
            return MintedCppName("body", body.value);
          }},
      mir::NamespaceReachOf(unit, body));
}

}  // namespace lyra::backend::cpp
