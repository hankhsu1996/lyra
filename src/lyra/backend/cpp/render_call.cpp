#include "lyra/backend/cpp/render_call.hpp"

#include <cstdint>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::backend::cpp {

namespace {

// Where in the call text the object a call dispatches on goes. C++ offers two
// positions and no third: a member call reaches the object through the callee
// expression, and a free function takes it as an ordinary leading argument,
// because a free function binds nothing. This says where such an object would
// go, not that there is one -- a callee that dispatches on nothing has nothing
// to place and answers here vacuously.
enum class ReceiverPlacement : std::uint8_t {
  kIntoCalleeName,
  kIntoArgumentList
};

// What C++ names a callee, and where the object it dispatches on goes. Every
// callee form -- an instance method, a factory on the type it builds, a free
// function, an indirect closure, a type constructor -- answers with these two
// and nothing else, so one site composes the call text out of them.
struct CalleeSpelling {
  std::string name;
  ReceiverPlacement placement;
};

// The object a call dispatches on, ready to compose into a callee: the rendered
// expression, and the token C++ reaches a member through it with.
struct RenderedReceiver {
  std::string expr;
  std::string_view member_access;
};

auto RenderReceiver(const ScopeView& view, const mir::Callee& callee)
    -> std::optional<RenderedReceiver> {
  const std::optional<mir::ExprId> receiver = mir::CalleeReceiver(callee);
  if (!receiver.has_value()) {
    return std::nullopt;
  }
  const mir::Expr& expr = view.Expr(*receiver);
  return RenderedReceiver{
      .expr = RenderExpr(view, expr),
      .member_access =
          view.Unit().types.Get(expr.type).Is<mir::PointerType>() ? "->" : "."};
}

// What the name a callee spells is reached through, which is what decides
// whether C++ can read an argument list after it: a name reached through a
// value is dependent until the value's type is resolved, and the `template`
// keyword is what says the angle brackets are an argument list and not a
// comparison. A name reached on a type is resolved where it is written.
enum class NameReachedThrough : std::uint8_t { kAValue, kAType };

// An operation the callee names at a position writes that position where C++
// settles types, because the part it names has a type of its own. An operation
// that names no position is its bare identifier and needs none of this.
auto SpelledAt(
    std::string_view identifier,
    const std::optional<base::ComponentIndex>& position,
    NameReachedThrough reached) -> std::string {
  if (!position.has_value()) {
    return std::string{identifier};
  }
  const std::string_view dependent =
      reached == NameReachedThrough::kAValue ? "template " : "";
  return std::format("{}{}<{}>", dependent, identifier, position->value);
}

// A runtime entry, spelled the way the library declares it. Nothing here reads
// the call to decide that: which form the entry takes, and the identifier it is
// written with, are the entry's own declaration.
auto ResolveEntrySpelling(
    const ScopeView& view, const support::RuntimeEntry& entry,
    const std::optional<RenderedReceiver>& receiver,
    const std::optional<base::ComponentIndex>& position,
    mir::TypeId result_type) -> CalleeSpelling {
  return std::visit(
      Overloaded{
          [](const support::FreeFunction& f) -> CalleeSpelling {
            return {
                .name = std::string{f.qualified_name},
                .placement = ReceiverPlacement::kIntoArgumentList};
          },
          // A method names nothing on its own, so a call reaching this
          // spelling without an object to reach it through has no C++ text at
          // all.
          [&](const support::Method& m) -> CalleeSpelling {
            if (!receiver.has_value()) {
              throw InternalError(
                  "Direct call: the instance form of a runtime entry is "
                  "reached through the object it acts on, and this call names "
                  "none -- please report this as a bug");
            }
            return {
                .name = SpelledAt(
                    m.identifier, position, NameReachedThrough::kAValue),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A factory is reached on the type it builds, which is the type of
          // the value the call answers with.
          [&](const support::StaticFactory& s) -> CalleeSpelling {
            return {
                .name = std::format(
                    "{}::{}", RenderTypeAsCpp(view.Unit(), result_type),
                    SpelledAt(
                        s.identifier, position, NameReachedThrough::kAType)),
                .placement = ReceiverPlacement::kIntoCalleeName};
          }},
      entry.declaration);
}

// What C++ names a `Direct` callee. Each alternative is a lookup in the table
// that resolves its own identity space, and how a receiver rides follows from
// what that lookup found.
auto ResolveDirectSpelling(
    const ScopeView& view, const mir::Direct& direct,
    const std::optional<RenderedReceiver>& receiver, mir::TypeId result_type)
    -> CalleeSpelling {
  return std::visit(
      Overloaded{
          // The owner prefix is a fixed function of the target's owner: it is
          // redundant for a non-virtual method and, for a virtual one a direct
          // call reaches (LRM 8.15 super), is what makes C++ bypass the vtable.
          [&](const mir::CallableTarget& t) -> CalleeSpelling {
            const auto& cls = view.Unit().GetClass(t.owner);
            return {
                .name = std::format(
                    "{}::{}", ToCppName(cls.name),
                    CppCallableName(cls.named_callables, t.slot)),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          [&](const support::BuiltinFn& id) -> CalleeSpelling {
            return ResolveEntrySpelling(
                view, support::RuntimeEntryOf(id), receiver, direct.position,
                result_type);
          },
          // Another compilation unit's C++ peer is a namespace, so a callable
          // of it (LRM 26.3) is named through that namespace.
          [](const mir::ExternalUnitCallableTarget& t) -> CalleeSpelling {
            return {
                .name = std::format(
                    "{}::{}", ToCppName(t.unit_name),
                    ToCppName(t.callable_name)),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A method on one of that namespace's classes is named through the
          // class as well. Target-language name lookup resolves it once the
          // declaring unit's header is included, and the class qualification
          // makes C++ bypass the vtable, exactly as a direct call to a virtual
          // method demands (LRM 8.15 super).
          [](const mir::ExternalUnitClassMethodTarget& t) -> CalleeSpelling {
            return {
                .name = std::format(
                    "{}::{}::{}", ToCppName(t.unit_name),
                    ToCppName(t.class_name), ToCppName(t.method_name)),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A body that brings up another unit's namespace answers to no name,
          // so it is named through that unit's namespace by which of the two it
          // is -- the same identifier that unit emitted it under.
          [](const mir::ExternalUnitStorageTarget& t) -> CalleeSpelling {
            return {
                .name = std::format(
                    "{}::{}", ToCppName(t.unit_name),
                    CppStorageEntryName(t.phase)),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A DPI-C symbol is program-global, so it is spelled unqualified
          // (LRM 35.4); the prototype it resolves against is declared once in
          // this artifact.
          [](const mir::ForeignSymbolTarget& t) -> CalleeSpelling {
            return {
                .name = t.linkage_name,
                .placement = ReceiverPlacement::kIntoCalleeName};
          }},
      direct.target);
}

auto ResolveCalleeSpelling(
    const ScopeView& view, const mir::CallExpr& call,
    const std::optional<RenderedReceiver>& receiver, mir::TypeId result_type)
    -> CalleeSpelling {
  return std::visit(
      Overloaded{
          [&](const mir::Direct& d) -> CalleeSpelling {
            return ResolveDirectSpelling(view, d, receiver, result_type);
          },
          [&](const mir::Indirect& i) -> CalleeSpelling {
            return {
                .name =
                    std::format("({})", RenderExpr(view, view.Expr(i.code))),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          [&](const mir::Virtual& v) -> CalleeSpelling {
            return {
                .name = std::visit(
                    Overloaded{
                        [&](const mir::LocalVirtualSlot& l) -> std::string {
                          return CppCallableName(
                              view.Unit()
                                  .GetClass(l.owner_class)
                                  .named_callables,
                              l.slot);
                        },
                        [&](const mir::ExternalVirtualSlot& e) -> std::string {
                          const mir::ExternalClass* introducer =
                              mir::FindExternalClass(
                                  view.Unit().external_classes, e.unit_name,
                                  e.class_name);
                          if (introducer == nullptr ||
                              e.ordinal.value >= introducer->behaviors.size()) {
                            throw InternalError(
                                "RenderCall: a dispatch names a behavior no "
                                "consumed promise describes");
                          }
                          return ToCppName(
                              introducer->behaviors[e.ordinal.value]);
                        },
                        [](const mir::ResolvedVirtualSlot&) -> std::string {
                          throw InternalError(
                              "RenderCall: a dispatch position that arrived as "
                              "a value reached a backend that states it does "
                              "not render one");
                        }},
                    v.slot),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A type has one way to come into existence, and what names it is the
          // type's own answer -- read through type mapping, the way every other
          // target-language spelling of a type is.
          [&](const mir::Construct&) -> CalleeSpelling {
            return {
                .name = RenderTypeConstructionAsCpp(view.Unit(), result_type),
                .placement = ReceiverPlacement::kIntoCalleeName};
          }},
      call.callee);
}

}  // namespace

auto RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type)
    -> std::string {
  const std::optional<RenderedReceiver> receiver =
      RenderReceiver(view, call.callee);
  const CalleeSpelling callee =
      ResolveCalleeSpelling(view, call, receiver, result_type);

  // The object the call dispatches on goes where the spelling puts it, and a
  // call that dispatches on none puts nothing anywhere. Everything after this
  // is punctuation.
  std::string callee_text = callee.name;
  std::vector<std::string> args;
  args.reserve(call.arguments.size() + 1);
  if (receiver.has_value()) {
    switch (callee.placement) {
      case ReceiverPlacement::kIntoCalleeName:
        callee_text = std::format(
            "({}){}{}", receiver->expr, receiver->member_access, callee.name);
        break;
      case ReceiverPlacement::kIntoArgumentList:
        args.push_back(receiver->expr);
        break;
    }
  }
  for (const mir::ExprId id : call.arguments) {
    args.push_back(RenderExpr(view, view.Expr(id)));
  }
  return CallOf(callee_text, args);
}

}  // namespace lyra::backend::cpp
