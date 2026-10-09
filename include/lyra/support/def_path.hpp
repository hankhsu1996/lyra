#pragma once

#include <compare>
#include <cstddef>
#include <cstdint>
#include <format>
#include <functional>
#include <optional>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/hash.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"

namespace lyra::support {

// One application of a generate block (LRM 27.3). Three things tell it from
// every other block of the scope holding it, and each is a field of its own,
// since an escaped label may hold any character one of the others could be
// written with (LRM 5.6.1).
//
// `label` is the label the source wrote. Alternatives may share one, those of
// one conditional and those of two of which a scope builds only one (LRM
// 27.5), so `disambiguator` says which this is among the generate blocks of
// its scope carrying that label, in the order the source wrote them: zero for
// the first, and so for every label nothing shares. `arguments` is the digest
// of what the design fixed for this application, which is what tells the block
// instances of one construct that compile differently apart, and is absent
// where nothing was fixed.
struct GenerateBlockStep {
  std::string label;
  std::uint32_t disambiguator = 0;
  std::optional<std::uint64_t> arguments;

  auto operator==(const GenerateBlockStep&) const -> bool = default;
  auto operator<=>(const GenerateBlockStep&) const
      -> std::strong_ordering = default;
};

// A class the source declared (LRM 8.3), under the name it was declared by.
// `arguments` is the digest of what a specialization of a generic class bound
// its parameters to (LRM 8.25), absent for a class that is no specialization or
// binds nothing.
struct ClassStep {
  std::string name;
  std::optional<std::uint64_t> arguments;

  auto operator==(const ClassStep&) const -> bool = default;
  auto operator<=>(const ClassStep&) const -> std::strong_ordering = default;
};

// A task or a function (LRM 13), under the name it was declared by.
struct SubroutineStep {
  std::string name;

  auto operator==(const SubroutineStep&) const -> bool = default;
  auto operator<=>(const SubroutineStep&) const
      -> std::strong_ordering = default;
};

// A `begin ... end` or `fork ... join` the source labelled (LRM 9.3.4).
struct NamedBlockStep {
  std::string name;

  auto operator==(const NamedBlockStep&) const -> bool = default;
  auto operator<=>(const NamedBlockStep&) const
      -> std::strong_ordering = default;
};

// A block the source gave no label, which nothing tells from the others of its
// scope but where it stands among that scope's declarations.
struct UnnamedBlockStep {
  std::uint32_t position = 0;

  auto operator==(const UnnamedBlockStep&) const -> bool = default;
  auto operator<=>(const UnnamedBlockStep&) const
      -> std::strong_ordering = default;
};

// An unpacked structure or union (LRM 7.2, 7.3), under the name it answers to
// in the scope declaring it: the typedef naming it, or the first data object
// its declaration statement declares (LRM 6.22.1 c).
struct TypeStep {
  std::string name;

  auto operator==(const TypeStep&) const -> bool = default;
  auto operator<=>(const TypeStep&) const -> std::strong_ordering = default;
};

// An unpacked structure or union nothing in its scope declares by name, told
// from the others of that scope by where it stands among its declarations.
struct UnnamedTypeStep {
  std::uint32_t position = 0;

  auto operator==(const UnnamedTypeStep&) const -> bool = default;
  auto operator<=>(const UnnamedTypeStep&) const
      -> std::strong_ordering = default;
};

// One scope on the way from a compilation unit down to a declaration: what
// kind of scope it is, and what tells it from the others of that kind in the
// scope holding it. The kind is part of the step because one scope may give a
// type and a value the same name (LRM 6.22.1 c).
using DefPathData = std::variant<
    GenerateBlockStep, ClassStep, SubroutineStep, NamedBlockStep,
    UnnamedBlockStep, TypeStep, UnnamedTypeStep>;

// Which declaration of a compilation unit one is, as the scopes the source
// nests it in, outermost first, the declaration itself last. Every identifier
// has a unique hierarchical path name (LRM 23.6), and this is that path rooted
// at the unit rather than at the top of the design, so it is the same for
// every instance of the unit. SystemVerilog leaves no character free to join
// the steps with (LRM 5.6.1), so nothing compares or finds a declaration by a
// spelling of them. The path of no steps is the unit's own scope, and the
// class an instance of the unit is.
struct DefPath {
  std::vector<DefPathData> data;

  auto operator==(const DefPath&) const -> bool = default;
  auto operator<=>(const DefPath&) const -> std::strong_ordering = default;
};

// The path one step further in than `path`.
[[nodiscard]] inline auto Extended(DefPath path, DefPathData step) -> DefPath {
  path.data.push_back(std::move(step));
  return path;
}

// The path `further` steps in from `path`.
[[nodiscard]] inline auto Extended(
    DefPath path, std::span<const DefPathData> further) -> DefPath {
  path.data.insert(path.data.end(), further.begin(), further.end());
  return path;
}

// The path of the scope `path`'s last step is written in. A unit's own scope
// stands in none, so asking it is a caller's defect.
[[nodiscard]] inline auto EnclosingPath(const DefPath& path) -> DefPath {
  if (path.data.empty()) {
    throw InternalError(
        "support::EnclosingPath: a unit's own scope stands in no other "
        "declaration");
  }
  return DefPath{.data = {path.data.begin(), path.data.end() - 1}};
}

// Whether the path names a scope of the design hierarchy -- an instance of the
// unit, or a generate block inside one (LRM 23.6) -- as against anything the
// source declared inside such a scope.
[[nodiscard]] inline auto NamesScopeClass(const DefPath& path) -> bool {
  if (path.data.empty()) return true;
  return std::visit(
      Overloaded{
          [](const GenerateBlockStep&) { return true; },
          [](const ClassStep&) { return false; },
          [](const SubroutineStep&) { return false; },
          [](const NamedBlockStep&) { return false; },
          [](const UnnamedBlockStep&) { return false; },
          [](const TypeStep&) { return false; },
          [](const UnnamedTypeStep&) { return false; }},
      path.data.back());
}

// The path as a person reads it, in a dump or a diagnostic. Nothing compares
// or looks a declaration up through it: two paths may read alike here.
[[nodiscard]] inline auto DisplayOf(const DefPath& path) -> std::string {
  if (path.data.empty()) return "<instance>";
  const auto with_arguments = [](std::string text,
                                 const std::optional<std::uint64_t>& digest) {
    if (digest.has_value()) text += std::format("__{:016x}", *digest);
    return text;
  };
  std::string text;
  for (const DefPathData& step : path.data) {
    if (!text.empty()) text += "::";
    text += std::visit(
        Overloaded{
            [&](const GenerateBlockStep& block) {
              std::string shown = block.label;
              if (block.disambiguator != 0) {
                shown += std::format("#{}", block.disambiguator);
              }
              return with_arguments(std::move(shown), block.arguments);
            },
            [&](const ClassStep& cls) {
              return with_arguments(cls.name, cls.arguments);
            },
            [](const SubroutineStep& subroutine) { return subroutine.name; },
            [](const NamedBlockStep& block) { return block.name; },
            [](const UnnamedBlockStep& block) {
              return std::format("<block {}>", block.position);
            },
            [](const TypeStep& type) { return type.name; },
            [](const UnnamedTypeStep& type) {
              return std::format("<type {}>", type.position);
            }},
        step);
  }
  return text;
}

}  // namespace lyra::support

template <>
struct std::hash<lyra::support::DefPath> {
  auto operator()(const lyra::support::DefPath& path) const noexcept
      -> std::size_t {
    using lyra::base::HashField;
    std::size_t seed = path.data.size();
    for (const lyra::support::DefPathData& step : path.data) {
      HashField(seed, step.index());
      std::visit(
          lyra::Overloaded{
              [&](const lyra::support::GenerateBlockStep& block) {
                HashField(seed, block.label);
                HashField(seed, block.disambiguator);
                HashField(seed, block.arguments);
              },
              [&](const lyra::support::ClassStep& cls) {
                HashField(seed, cls.name);
                HashField(seed, cls.arguments);
              },
              [&](const lyra::support::SubroutineStep& subroutine) {
                HashField(seed, subroutine.name);
              },
              [&](const lyra::support::NamedBlockStep& block) {
                HashField(seed, block.name);
              },
              [&](const lyra::support::UnnamedBlockStep& block) {
                HashField(seed, block.position);
              },
              [&](const lyra::support::TypeStep& type) {
                HashField(seed, type.name);
              },
              [&](const lyra::support::UnnamedTypeStep& type) {
                HashField(seed, type.position);
              }},
          step);
    }
    return seed;
  }
};
