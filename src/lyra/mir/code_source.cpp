#include "lyra/mir/code_source.hpp"

#include <format>
#include <map>
#include <optional>
#include <span>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/dump.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::mir {

namespace {

// A body the source named goes by that name, and one it did not by where it
// sits.
auto BodyLabel(std::optional<std::string_view> name, CallableId slot)
    -> std::string {
  return name.has_value() ? std::format("'{}'", *name)
                          : std::format("body {}", slot.value);
}

auto DescribeClass(const CompilationUnit& unit, ClassId owner) -> std::string {
  const Class& cls = unit.GetClass(owner);
  return cls.path.has_value() ? std::format(
                                    "class '{}' in unit '{}'",
                                    support::DisplayOf(*cls.path), unit.name)
                              : std::format("a scope of unit '{}'", unit.name);
}

// Only a constructor has nodes outside its statements, so any other source
// answering with some has built nodes nothing will ever reach.
auto WithoutOutsideRoots(SourcedCode answer) -> CallableCode {
  if (!answer.outside_roots.empty()) {
    throw InternalError(
        "mir: a rewrite answered code with outside roots for a source that "
        "names none -- please report this as a bug");
  }
  return std::move(answer.code);
}

}  // namespace

auto Describe(const CompilationUnit& unit, const CodeSource& at)
    -> std::string {
  return std::visit(
      Overloaded{
          [&](const ConstructorSource& s) {
            return std::format(
                "the constructor of {}", DescribeClass(unit, s.owner));
          },
          [&](const CallableTarget& s) {
            return std::format(
                "{} of {}",
                BodyLabel(
                    NameOf(unit.GetClass(s.owner).named_callables, s.slot),
                    s.slot),
                DescribeClass(unit, s.owner));
          },
          [&](const UnitCallableTarget& s) {
            return std::format(
                "{} of unit '{}'",
                BodyLabel(NameOf(unit.named_callables, s.slot), s.slot),
                unit.name);
          },
          [&](const RecordMethodSource& s) {
            return std::format(
                "method '{}' of record '{}' in unit '{}'",
                support::ValueOperationName(s.answers),
                support::DisplayOf(unit.GetRecord(s.record).path), unit.name);
          },
          [&](const TypeOperationSource& s) {
            return std::format(
                "operation \"{}\" of type {} in unit '{}'",
                support::ValueOperationName(s.answers), DumpType(unit, s.type),
                unit.name);
          },
          [&](const ClosureSource& s) {
            return std::format(
                "closure {} of unit '{}'", s.closure.value, unit.name);
          },
          [&](const ForeignScopeEntrySource& s) {
            return std::format("the foreign entry '{}'", s.foreign_name);
          }},
      at);
}

auto IsEnteredForeign(const CompilationUnit& unit, const CodeSource& at)
    -> bool {
  return std::visit(
      Overloaded{
          [](const ConstructorSource&) { return false; },
          [](const CallableTarget&) { return false; },
          [&](const UnitCallableTarget& s) {
            return unit.callables.Get(s.slot).foreign.has_value();
          },
          [](const RecordMethodSource&) { return false; },
          [](const TypeOperationSource&) { return false; },
          [](const ClosureSource&) { return false; },
          [](const ForeignScopeEntrySource&) { return false; }},
      at);
}

void ForEachCode(const CompilationUnit& unit, const CodeVisit& visit) {
  for (const ClassId owner : unit.classes.Ids()) {
    const Class& cls = unit.GetClass(owner);
    if (cls.constructor.has_value()) {
      visit(
          ConstructorSource{.owner = owner}, cls.constructor->code,
          cls.constructor->base_args);
    }
    for (const CallableId slot : cls.callables.Ids()) {
      visit(
          CallableTarget{.owner = owner, .slot = slot},
          cls.callables.Get(slot).code, {});
    }
  }
  for (const CallableId slot : unit.callables.Ids()) {
    visit(UnitCallableTarget{.slot = slot}, unit.callables.Get(slot).code, {});
  }
  for (const RecordId record : unit.record_methods.Ids()) {
    for (const RecordMethod& method : unit.GetRecordMethods(record)) {
      visit(
          RecordMethodSource{.record = record, .answers = method.answers},
          method.code, {});
    }
  }
  for (const TypeOperation& stated : unit.type_operations) {
    visit(
        TypeOperationSource{.type = stated.type, .answers = stated.answers},
        stated.code, {});
  }
  for (const ClosureId closure : unit.closures.Ids()) {
    visit(
        ClosureSource{.closure = closure}, unit.GetClosure(closure).invoke, {});
  }
  for (const ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    visit(
        ForeignScopeEntrySource{.foreign_name = entry.linkage.foreign_name},
        entry.definition, {});
  }
}

auto RewriteEachCode(CompilationUnit unit, const CodeRewrite& rewrite)
    -> CompilationUnit {
  std::map<CodeSource, SourcedCode> rewritten;
  ForEachCode(
      unit, [&](const CodeSource& at, const CallableCode& code,
                std::span<const ExprId> outside_roots) {
        if (std::optional<SourcedCode> answer =
                rewrite(unit, at, code, outside_roots)) {
          rewritten.emplace(at, *std::move(answer));
        }
      });

  const auto answer_for =
      [&](const CodeSource& at) -> std::optional<SourcedCode> {
    auto found = rewritten.extract(at);
    if (found.empty()) {
      return std::nullopt;
    }
    return std::move(found.mapped());
  };
  const auto replace = [&](const CodeSource& at, CallableCode& code) {
    if (std::optional<SourcedCode> answer = answer_for(at)) {
      code = WithoutOutsideRoots(*std::move(answer));
    }
  };
  unit.classes =
      std::move(unit.classes).TransformValues([&](ClassId owner, Class cls) {
        if (std::optional<SourcedCode> answer =
                answer_for(ConstructorSource{.owner = owner})) {
          cls.constructor->code = std::move(answer->code);
          cls.constructor->base_args = std::move(answer->outside_roots);
        }
        cls.callables =
            std::move(cls.callables)
                .TransformValues([&](CallableId slot, CallableDecl decl) {
                  replace(
                      CallableTarget{.owner = owner, .slot = slot}, decl.code);
                  return decl;
                });
        return cls;
      });
  unit.callables =
      std::move(unit.callables)
          .TransformValues([&](CallableId slot, CallableDecl decl) {
            replace(UnitCallableTarget{.slot = slot}, decl.code);
            return decl;
          });
  unit.record_methods =
      std::move(unit.record_methods)
          .TransformValues([&](RecordId record, RecordMethods methods) {
            for (RecordMethod& method : methods) {
              replace(
                  RecordMethodSource{
                      .record = record, .answers = method.answers},
                  method.code);
            }
            return methods;
          });
  for (TypeOperation& stated : unit.type_operations) {
    replace(
        TypeOperationSource{.type = stated.type, .answers = stated.answers},
        stated.code);
  }
  unit.closures = std::move(unit.closures)
                      .TransformValues([&](ClosureId id, ClosureDecl closure) {
                        replace(ClosureSource{.closure = id}, closure.invoke);
                        return closure;
                      });
  for (ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    replace(
        ForeignScopeEntrySource{.foreign_name = entry.linkage.foreign_name},
        entry.definition);
  }
  return unit;
}

}  // namespace lyra::mir
