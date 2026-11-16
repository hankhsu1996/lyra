#include "lyra/mir/caller_location.hpp"

#include <optional>
#include <variant>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/callable_id.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::mir {

auto TakesCallerLocation(
    const CompilationUnit& unit, const CallExpr& call, TypeId result) -> bool {
  const auto reports_at_caller = [](const CallableCode& code) {
    return code.caller_location.has_value();
  };
  const auto construction_reports_at_caller = [&](ClassId cls) {
    const std::optional<CallableId>& constructor =
        unit.CallablesOf(cls).constructor;
    return constructor.has_value() &&
           reports_at_caller(unit.callables.Get(*constructor).code);
  };
  // A record's own operation reports where the entry it answers does. A
  // closure keeps the location of the body that built it, and a function the
  // foreign program defines is no text of the tool's.
  const auto callable_reports_at_caller = [&](CallableId callable) {
    const CallableDecl& decl = unit.callables.Get(callable);
    return std::visit(
        Overloaded{
            [&](const DeclaredByClass&) {
              return reports_at_caller(decl.code);
            },
            [&](const ConstructsClass&) {
              return reports_at_caller(decl.code);
            },
            [&](const DeclaredByNamespace&) {
              return !std::holds_alternative<DefinedByForeignCode>(
                         FormOf(decl)) &&
                     reports_at_caller(decl.code);
            },
            [](const DeclaredByRecord& owner) {
              return support::TakesCallerLocation(owner.answers);
            },
            [](const DeclaredByClosure&) { return false; }},
        unit.OwnerOf(callable));
  };
  return std::visit(
      Overloaded{
          [&](const Direct& direct) {
            return std::visit(
                Overloaded{
                    [](support::BuiltinFn fn) {
                      return support::RuntimeEntryOf(fn).takes_the_location;
                    },
                    [&](const CallableTarget& target) {
                      return callable_reports_at_caller(target.callable);
                    },
                    // A type's own operation reports where the entry it
                    // answers does.
                    [](const TypeOperationTarget& target) {
                      return support::TakesCallerLocation(target.answers);
                    },
                    // A unit's bring-up is no text of the tool's.
                    [](const ExternalBringUpTarget&) { return false; }},
                direct.target);
          },
          // A behavior dispatched on is what its introducing declaration
          // states, which every body answering it takes alike.
          [&](const Virtual& dispatched) {
            return reports_at_caller(
                unit.callables.Get(dispatched.slot.introduced_by).code);
          },
          [](const Indirect&) { return false; },
          // Constructing an object enters its class's constructor, which
          // reports where its declaration says, and building a library value
          // enters the constructor its declaration names.
          [&](const Construct&) {
            const Type& built = unit.types.Get(result);
            if (const auto* library = built.As<LibraryType>()) {
              const std::optional<support::LibraryConstructor> entered =
                  support::DeclarationOf(library->decl).constructor;
              return entered.has_value() &&
                     support::RuntimeEntryOf(entered->entry).takes_the_location;
            }
            const auto* object = built.As<ObjectType>();
            return object != nullptr &&
                   construction_reports_at_caller(object->of);
          }},
      call.callee);
}

}  // namespace lyra::mir
