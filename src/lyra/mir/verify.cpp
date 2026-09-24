#include "lyra/mir/verify.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::mir {

namespace {

// A body the source named goes by that name, and one it did not by where it
// sits, so a violation names what was being lowered wherever it can.
auto BodyLabel(std::optional<std::string_view> name, std::uint32_t position)
    -> std::string {
  return name.has_value() ? std::format("'{}'", *name)
                          : std::format("body {}", position);
}

auto HoldsSuspension(const Block& block) -> bool {
  return std::ranges::any_of(
             block.exprs,
             [](const Expr& expr) {
               return std::holds_alternative<AwaitExpr>(expr.data);
             }) ||
         std::ranges::any_of(block.child_scopes, HoldsSuspension);
}

// `describe` names the body, and is asked only once there is something to
// report: the check runs over every body of every unit, and composing a name
// for each one costs more than the check itself.
void VerifyCode(
    const CompilationUnit& unit, const CallableCode& code,
    const auto& describe) {
  if (!code.body.has_value() ||
      unit.types.Get(code.result_type).Is<CoroutineType>() ||
      !HoldsSuspension(*code.body)) {
    return;
  }
  throw InternalError(
      std::format(
          "mir verify: {} suspends, but its result type is not a coroutine, "
          "so nothing could resume it",
          describe()));
}

void VerifyClass(const CompilationUnit& unit, const Class& cls) {
  const auto owner = [&] {
    return cls.name.has_value()
               ? std::format("class '{}' in unit '{}'", *cls.name, unit.name)
               : std::format("a scope of unit '{}'", unit.name);
  };
  VerifyCode(unit, cls.constructor.code, [&] {
    return std::format("the constructor of {}", owner());
  });
  for (const CallableId id : cls.callables.Ids()) {
    VerifyCode(unit, cls.callables.Get(id).code, [&] {
      return std::format(
          "{} of {}", BodyLabel(NameOf(cls.named_callables, id), id.value),
          owner());
    });
  }
  for (const AbiAdapterId id : cls.abi_adapters.Ids()) {
    VerifyCode(unit, cls.abi_adapters.Get(id).code, [&] {
      return std::format("runtime entry {} of {}", id.value, owner());
    });
  }
}

}  // namespace

void Verify(const CompilationUnit& unit) {
  for (const CallableId id : unit.callables.Ids()) {
    VerifyCode(unit, unit.callables.Get(id).code, [&] {
      return std::format(
          "{} of unit '{}'",
          BodyLabel(NameOf(unit.named_callables, id), id.value), unit.name);
    });
  }
  for (const ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    VerifyCode(unit, entry.definition, [&] {
      return std::format("the foreign entry '{}'", entry.linkage.foreign_name);
    });
  }
  for (const ClosureId id : unit.closures.Ids()) {
    VerifyCode(unit, unit.GetClosure(id).invoke, [&] {
      return std::format("closure {} of unit '{}'", id.value, unit.name);
    });
  }
  for (const ClassId id : unit.classes.Ids()) {
    VerifyClass(unit, unit.GetClass(id));
  }
}

}  // namespace lyra::mir
