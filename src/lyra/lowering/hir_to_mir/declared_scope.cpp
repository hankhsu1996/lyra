#include "lyra/lowering/hir_to_mir/declared_scope.hpp"

#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/base/registry.hpp"
#include "lyra/base/translation.hpp"
#include "lyra/hir/fixed_name.hpp"
#include "lyra/hir/procedural_scope.hpp"

namespace lyra::lowering::hir_to_mir {

namespace {

// `text` with `part` joined to it by `separator`, where both name something.
auto JoinedText(
    const std::string& text, std::string_view separator, std::string_view part)
    -> std::string {
  if (text.empty()) return std::string{part};
  return std::format("{}{}{}", text, separator, part);
}

// `name` followed by `part`, where the source wrote one, and `name` as it is
// where it did not.
auto Joined(
    const hir::FixedName& name, std::string_view separator,
    const std::optional<std::string>& part) -> hir::FixedName {
  if (!part.has_value()) return name;
  return std::visit(
      Overloaded{
          [&](const hir::NameBelowInstance& below) -> hir::FixedName {
            return hir::NameBelowInstance{
                .text = JoinedText(below.text, separator, *part),
                .scopes_out = below.scopes_out};
          },
          [&](const hir::WholeName& whole) -> hir::FixedName {
            return hir::WholeName{
                .text = JoinedText(whole.text, separator, *part)};
          }},
      name);
}

}  // namespace

auto ReportedNames(
    const base::Registry<hir::ProceduralScopeDecl, hir::ProceduralScopeId>&
        scopes,
    const hir::FixedName& declaring)
    -> base::Translation<hir::ProceduralScopeId, hir::FixedName> {
  // A scope's name is its parent's and its own, and the tree states only the
  // children of each scope, so the names are given on a descent from each
  // body's root -- the scopes no other scope holds.
  std::vector<bool> held(scopes.size(), false);
  for (const hir::ProceduralScopeDecl& scope : scopes) {
    for (const hir::ProceduralScopeId child : scope.child_scopes) {
      held[child.value] = true;
    }
  }
  std::vector<std::optional<hir::FixedName>> names(scopes.size());
  const auto name = [&](const auto& self, hir::ProceduralScopeId id,
                        hir::FixedName given) -> void {
    for (const hir::ProceduralScopeId child : scopes.Get(id).child_scopes) {
      self(self, child, Joined(given, ".", scopes.Get(child).source_name));
    }
    names[id.value] = std::move(given);
  };
  for (const hir::ProceduralScopeId id : scopes.Ids()) {
    if (held[id.value]) continue;
    name(name, id, Joined(declaring, "::", scopes.Get(id).source_name));
  }
  std::vector<hir::FixedName> answers;
  answers.reserve(names.size());
  for (std::optional<hir::FixedName>& given : names) {
    if (!given.has_value()) {
      throw InternalError(
          "ReportedNames: a procedural scope is neither a body's root nor held "
          "by another scope");
    }
    answers.push_back(*std::move(given));
  }
  return {scopes.size(), std::move(answers)};
}

}  // namespace lyra::lowering::hir_to_mir
