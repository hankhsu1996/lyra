#include "lyra/mir/dispatch_table.hpp"

#include <algorithm>
#include <optional>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"

namespace lyra::mir {

namespace {

// What `callable` gives `behavior`: its body, nothing where it restates the
// behavior with no body, and no answer at all where it is not one of the
// behavior's bodies.
auto GivenBody(
    CallableId callable, const CallableDecl& decl, const VirtualSlot& behavior)
    -> std::optional<std::optional<CallableId>> {
  const std::optional<VirtualDispatchRole>& role = VirtualDispatchOf(decl);
  if (!role.has_value()) {
    return std::nullopt;
  }
  const auto body = [&](bool is_pure) -> std::optional<CallableId> {
    if (is_pure) {
      return std::nullopt;
    }
    return callable;
  };
  return std::visit(
      Overloaded{
          [&](const IntroducesVirtualSlot& introduced)
              -> std::optional<std::optional<CallableId>> {
            if (VirtualSlot{.introduced_by = callable} != behavior) {
              return std::nullopt;
            }
            return body(introduced.is_pure);
          },
          [&](const OverridesVirtualSlot& overriding)
              -> std::optional<std::optional<CallableId>> {
            if (overriding.overridden != behavior) {
              return std::nullopt;
            }
            return body(overriding.is_pure);
          }},
      *role);
}

void AppendInterface(
    const CompilationUnit& unit, ClassId iface, std::vector<ClassId>& out) {
  if (std::ranges::find(out, iface) != out.end()) {
    return;
  }
  out.push_back(iface);
  for (const ClassId extended : unit.GetClass(iface).implements) {
    AppendInterface(unit, extended, out);
  }
}

}  // namespace

auto DispatchTableOf(const CompilationUnit& unit, ClassId cls)
    -> std::vector<VirtualSlot> {
  const Class& declared = unit.GetClass(cls);
  std::vector<VirtualSlot> table;
  if (const std::optional<ClassId> base = DeclaredBase(declared)) {
    table = DispatchTableOf(unit, *base);
  }
  for (const CallableId callable : IntroducedBehaviors(unit, cls)) {
    table.push_back(VirtualSlot{.introduced_by = callable});
  }
  return table;
}

auto DispatchedBody(
    const CompilationUnit& unit, ClassId cls, const VirtualSlot& behavior)
    -> std::optional<CallableId> {
  for (std::optional<ClassId> at = cls; at.has_value();
       at = DeclaredBase(unit.GetClass(*at))) {
    for (const CallableId callable : unit.CallablesOf(*at).callables) {
      if (auto given =
              GivenBody(callable, unit.callables.Get(callable), behavior)) {
        return *given;
      }
    }
  }
  throw InternalError(
      "mir::DispatchedBody: a class is asked for a behavior its lineage does "
      "not carry -- please report this as a bug");
}

auto CallbackBodyOf(
    const CompilationUnit& unit, ClassId cls, support::LibraryCallback callback)
    -> std::optional<CallableId> {
  for (std::optional<ClassId> at = cls; at.has_value();
       at = DeclaredBase(unit.GetClass(*at))) {
    if (const std::optional<CallableId> given =
            unit.GetClass(*at).CallbackBody(callback)) {
      return given;
    }
  }
  return std::nullopt;
}

auto InterfacesOf(const CompilationUnit& unit, ClassId cls)
    -> std::vector<ClassId> {
  const Class& declared = unit.GetClass(cls);
  std::vector<ClassId> out;
  if (const std::optional<ClassId> base = DeclaredBase(declared)) {
    out = InterfacesOf(unit, *base);
  }
  for (const ClassId named : declared.implements) {
    AppendInterface(unit, named, out);
  }
  return out;
}

auto RequirementsOf(const CompilationUnit& unit, ClassId iface)
    -> std::vector<VirtualSlot> {
  std::vector<VirtualSlot> out;
  for (const CallableId callable : IntroducedBehaviors(unit, iface)) {
    out.push_back(VirtualSlot{.introduced_by = callable});
  }
  return out;
}

auto IntroducedBehaviors(const CompilationUnit& unit, ClassId cls)
    -> std::vector<CallableId> {
  std::vector<CallableId> introduced;
  for (const CallableId callable : unit.CallablesOf(cls).callables) {
    if (IntroducesSlot(VirtualDispatchOf(unit.callables.Get(callable)))) {
      introduced.push_back(callable);
    }
  }
  return introduced;
}

auto DispatchNameOf(const CompilationUnit& unit, CallableId method)
    -> DispatchName {
  const Class& cls = unit.GetClass(DeclaringClassOf(unit, method));
  const auto own_name = [&]() -> DispatchName {
    const std::optional<std::string_view> name =
        NameOf(cls.named_callables, method);
    if (!name.has_value()) {
      throw InternalError(
          "mir::DispatchNameOf: a method introducing a behavior has no name -- "
          "please report this as a bug");
    }
    return *name;
  };
  // A body the library calls on an object of the class is named by what the
  // library calls it for.
  if (const std::optional<support::LibraryCallback> callback =
          cls.CallbackAnsweredBy(method)) {
    return *callback;
  }
  if (cls.declaring_instance == method) {
    return DeclaringInstanceName{};
  }
  if (cls.scope_declarations.has_value()) {
    if (const std::optional<support::LibraryCallback> callback =
            ScopeDeclarationsCallbackOf(*cls.scope_declarations, method)) {
      return *callback;
    }
  }
  const std::optional<VirtualDispatchRole>& role =
      VirtualDispatchOf(unit.callables.Get(method));
  if (!role.has_value()) {
    throw InternalError(
        "mir::DispatchNameOf: a method that neither takes part in dispatch nor "
        "answers the library is named by no behavior -- please report this as "
        "a bug");
  }
  return std::visit(
      Overloaded{
          [&](const IntroducesVirtualSlot&) -> DispatchName {
            return own_name();
          },
          [&](const OverridesVirtualSlot& taken) -> DispatchName {
            return DispatchNameOf(unit, taken.overridden.introduced_by);
          }},
      *role);
}

}  // namespace lyra::mir
