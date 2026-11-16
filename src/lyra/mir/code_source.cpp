#include "lyra/mir/code_source.hpp"

#include <format>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/callable_ordinal.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/dump.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::mir {

namespace {

// A body the source named goes by that name, and one it did not by where it
// stands among its owner's callables.
auto BodyLabel(
    std::span<const NamedCallable> named, CallableId callable,
    CallableOrdinal ordinal) -> std::string {
  const std::optional<std::string_view> name = NameOf(named, callable);
  return name.has_value() ? std::format("'{}'", *name)
                          : std::format("body {}", ordinal.value);
}

auto DescribeCallable(const CompilationUnit& unit, CallableId callable)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const DeclaredByClass& owner) {
            return std::format(
                "{} of {}",
                BodyLabel(
                    unit.GetClass(owner.cls).named_callables, callable,
                    owner.ordinal),
                DescribeClass(unit, owner.cls));
          },
          [&](const ConstructsClass& owner) {
            return std::format(
                "the constructor of {}", DescribeClass(unit, owner.cls));
          },
          [&](const DeclaredByNamespace& owner) {
            return std::format(
                "{} of unit '{}'",
                BodyLabel(unit.named_callables, callable, owner.ordinal),
                unit.name);
          },
          [&](const DeclaredByRecord& owner) {
            return std::format(
                "method '{}' of record '{}' in unit '{}'",
                support::ValueOperationName(owner.answers),
                support::DisplayOf(unit.GetRecord(owner.record).path),
                unit.name);
          },
          [&](const DeclaredByClosure& owner) {
            return std::format(
                "closure {} of unit '{}'", owner.closure.value, unit.name);
          }},
      unit.OwnerOf(callable));
}

}  // namespace

auto DescribeClass(const CompilationUnit& unit, ClassId owner) -> std::string {
  const Class& cls = unit.GetClass(owner);
  const support::DefPath* path = support::PathOf(cls.identity);
  return path != nullptr ? std::format(
                               "class '{}' in unit '{}'",
                               support::DisplayOf(*path), unit.name)
                         : std::format("a scope of unit '{}'", unit.name);
}

auto Describe(const CompilationUnit& unit, const CodeSource& at)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const CallableTarget& s) {
            return DescribeCallable(unit, s.callable);
          },
          [&](const TypeOperationSource& s) {
            return std::format(
                "operation \"{}\" of type {} in unit '{}'",
                support::ValueOperationName(s.answers), DumpType(unit, s.type),
                unit.name);
          },
          [&](const ForeignScopeEntrySource& s) {
            return std::format("the foreign entry '{}'", s.foreign_name);
          }},
      at);
}

auto IsEnteredForeign(const CompilationUnit& unit, const CodeSource& at)
    -> bool {
  const auto of_callable = [&](CallableId callable) {
    return std::visit(
        Overloaded{
            [](const DeclaredByNamespace& owner) {
              return owner.foreign.has_value();
            },
            [](const DeclaredByClass&) { return false; },
            [](const ConstructsClass&) { return false; },
            [](const DeclaredByClosure&) { return false; },
            [](const DeclaredByRecord&) { return false; }},
        unit.OwnerOf(callable));
  };
  return std::visit(
      Overloaded{
          [&](const CallableTarget& s) { return of_callable(s.callable); },
          [](const TypeOperationSource&) { return false; },
          [](const ForeignScopeEntrySource&) { return false; }},
      at);
}

void ForEachCode(const CompilationUnit& unit, const CodeVisit& visit) {
  for (const CallableId callable : unit.callables.Ids()) {
    if (!IsDefinedElsewhere(unit, callable)) {
      visit(
          CallableTarget{.callable = callable},
          unit.callables.Get(callable).code);
    }
  }
  for (const ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    visit(
        ForeignScopeEntrySource{.foreign_name = entry.linkage.foreign_name},
        entry.definition);
  }
}

auto RewriteEachCode(CompilationUnit unit, const CodeRewrite& rewrite)
    -> CompilationUnit {
  std::map<CodeSource, CallableCode> rewritten;
  ForEachCode(unit, [&](const CodeSource& at, const CallableCode& code) {
    if (std::optional<CallableCode> answer = rewrite(unit, at, code)) {
      rewritten.emplace(at, *std::move(answer));
    }
  });

  const auto answer_for =
      [&](const CodeSource& at) -> std::optional<CallableCode> {
    auto found = rewritten.extract(at);
    if (found.empty()) {
      return std::nullopt;
    }
    return std::move(found.mapped());
  };
  const auto replace = [&](const CodeSource& at, CallableCode& code) {
    if (std::optional<CallableCode> answer = answer_for(at)) {
      code = *std::move(answer);
    }
  };
  unit.callables.RewriteEachCode([&](CallableId callable, CallableCode& code) {
    replace(CallableTarget{.callable = callable}, code);
  });
  for (ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    replace(
        ForeignScopeEntrySource{.foreign_name = entry.linkage.foreign_name},
        entry.definition);
  }
  return unit;
}

}  // namespace lyra::mir
