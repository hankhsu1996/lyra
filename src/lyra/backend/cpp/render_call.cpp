#include "lyra/backend/cpp/render_call.hpp"

#include <cstdint>
#include <optional>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
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

// A callee C++ spells with a name of its own -- an instance method, a factory
// on the type it builds, a free function, a type constructor -- and where the
// object it dispatches on goes.
struct NamedCallee {
  std::string name;
  ReceiverPlacement placement;
};

// A callee the program computes rather than names: the call reaches it through
// the value an expression produces, which C++ spells by parenthesizing that
// expression so it stands wherever a name would.
struct ComputedCallee {
  mir::ExprId code;
};

using CalleeSpelling = std::variant<NamedCallee, ComputedCallee>;

// The object a call dispatches on: which expression it is, and the token C++
// reaches a member through it with.
struct CallReceiver {
  mir::ExprId expr;
  std::string_view member_access;
};

auto ResolveReceiver(const ScopeView& view, const mir::Callee& callee)
    -> std::optional<CallReceiver> {
  const std::optional<mir::ExprId> receiver = mir::CalleeReceiver(callee);
  if (!receiver.has_value()) {
    return std::nullopt;
  }
  const mir::Expr& expr = view.Expr(*receiver);
  return CallReceiver{
      .expr = *receiver,
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
  return TextOf(dependent, identifier, "<", position->value, ">");
}

// A runtime entry, spelled the way the library declares it. Nothing here reads
// the call to decide that: which form the entry takes, and the identifier it is
// written with, are the entry's own declaration.
auto ResolveEntrySpelling(
    const ScopeView& view, const support::RuntimeEntry& entry,
    bool has_receiver, const std::optional<base::ComponentIndex>& position,
    mir::TypeId result_type) -> CalleeSpelling {
  return std::visit(
      Overloaded{
          [](const support::FreeFunction& f) -> CalleeSpelling {
            return NamedCallee{
                .name = std::string{f.qualified_name},
                .placement = ReceiverPlacement::kIntoArgumentList};
          },
          // A method names nothing on its own, so a call reaching this
          // spelling without an object to reach it through has no C++ text at
          // all.
          [&](const support::Method& m) -> CalleeSpelling {
            if (!has_receiver) {
              throw InternalError(
                  "Direct call: the instance form of a runtime entry is "
                  "reached through the object it acts on, and this call names "
                  "none -- please report this as a bug");
            }
            return NamedCallee{
                .name = SpelledAt(
                    m.identifier, position, NameReachedThrough::kAValue),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A factory is reached on the type it builds, which is the type of
          // the value the call answers with.
          [&](const support::StaticFactory& s) -> CalleeSpelling {
            return NamedCallee{
                .name = TextOf(
                    RenderTypeAsCpp(view.Unit(), result_type), "::",
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
    const ScopeView& view, const mir::Direct& direct, bool has_receiver,
    mir::TypeId result_type) -> CalleeSpelling {
  return std::visit(
      Overloaded{
          // The owner prefix is a fixed function of the target's owner: it is
          // redundant for a non-virtual method and, for a virtual one a direct
          // call reaches (LRM 8.15 super), is what makes C++ bypass the vtable.
          [&](const mir::CallableTarget& t) -> CalleeSpelling {
            const auto& cls = view.Unit().GetClass(t.owner);
            return NamedCallee{
                .name = TextOf(
                    CppClassName(cls, t.owner),
                    "::", CppClassCallableName(view.Unit(), cls, t.slot)),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // This unit's C++ peer is a namespace too, so a body of its own is
          // named through that namespace exactly as another unit's body is.
          [&](const mir::UnitCallableTarget& t) -> CalleeSpelling {
            return NamedCallee{
                .name = TextOf(
                    CppUnitScope(view.Unit().name),
                    "::", CppUnitCallableName(view.Unit(), t.slot)),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          [&](const support::BuiltinFn& id) -> CalleeSpelling {
            return ResolveEntrySpelling(
                view, support::RuntimeEntryOf(id), has_receiver,
                direct.position, result_type);
          },
          // Another compilation unit's C++ peer is a namespace, so a callable
          // of it (LRM 26.3) is named through that namespace.
          [](const mir::ExternalUnitCallableTarget& t) -> CalleeSpelling {
            return NamedCallee{
                .name = TextOf(
                    CppUnitScope(t.unit_name),
                    "::", ToCppName(t.callable_name)),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A method on one of that namespace's classes is named through the
          // class as well. Target-language name lookup resolves it once the
          // declaring unit's header is included, and the class qualification
          // makes C++ bypass the vtable, exactly as a direct call to a virtual
          // method demands (LRM 8.15 super).
          [](const mir::ExternalUnitClassMethodTarget& t) -> CalleeSpelling {
            return NamedCallee{
                .name = TextOf(
                    CppUnitScope(t.unit_name), "::", ToCppName(t.class_name),
                    "::", ToCppName(t.method_name)),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A body of another unit that answers to no name is named through
          // that unit's namespace by which of them it is -- the same identifier
          // that unit emitted it under.
          [](const mir::ExternalUnitMintedEntryTarget& t) -> CalleeSpelling {
            return NamedCallee{
                .name = TextOf(
                    CppUnitScope(t.unit_name),
                    "::", CppMintedEntryName(t.entry)),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A DPI-C symbol is program-global, so it is spelled unqualified
          // (LRM 35.4); the prototype it resolves against is declared once in
          // this artifact.
          [](const mir::ForeignSymbolTarget& t) -> CalleeSpelling {
            return NamedCallee{
                .name = t.linkage_name,
                .placement = ReceiverPlacement::kIntoCalleeName};
          }},
      direct.target);
}

auto ResolveCalleeSpelling(
    const ScopeView& view, const mir::CallExpr& call, bool has_receiver,
    mir::TypeId result_type) -> CalleeSpelling {
  return std::visit(
      Overloaded{
          [&](const mir::Direct& d) -> CalleeSpelling {
            return ResolveDirectSpelling(view, d, has_receiver, result_type);
          },
          [&](const mir::Indirect& i) -> CalleeSpelling {
            return ComputedCallee{.code = i.code};
          },
          [&](const mir::Virtual& v) -> CalleeSpelling {
            return NamedCallee{
                .name = std::visit(
                    Overloaded{
                        [&](const mir::LocalVirtualSlot& l) -> std::string {
                          return CppClassCallableName(
                              view.Unit(), view.Unit().GetClass(l.owner_class),
                              l.slot);
                        },
                        [&](const mir::ExternalVirtualSlot& e) -> std::string {
                          return CppExternalBehaviorName(
                              view.Unit(), e.unit_name, e.class_name,
                              e.ordinal);
                        }},
                    v.slot),
                .placement = ReceiverPlacement::kIntoCalleeName};
          },
          // A type has one way to come into existence, and what names it is the
          // type's own answer -- read through type mapping, the way every other
          // target-language spelling of a type is.
          [&](const mir::Construct&) -> CalleeSpelling {
            return NamedCallee{
                .name = RenderTypeConstructionAsCpp(view.Unit(), result_type),
                .placement = ReceiverPlacement::kIntoCalleeName};
          }},
      call.callee);
}

// Where the object a call dispatches on goes, asked only of a call that has
// one. A callee the program computes is reached through no name, and MIR states
// no object to dispatch on for such a call, so this is refused rather than
// answered: an answer here would be one no call can ask for.
auto PlacementOf(const CalleeSpelling& callee) -> ReceiverPlacement {
  return std::visit(
      Overloaded{
          [](const NamedCallee& named) { return named.placement; },
          [](const ComputedCallee&) -> ReceiverPlacement {
            throw InternalError(
                "RenderCallExpr: a call reaching a computed callee names an "
                "object to dispatch on, which MIR states for no such call -- "
                "please report this as a bug");
          }},
      callee);
}

void RenderCallee(
    const ScopeView& view, const CalleeSpelling& callee, TargetText& out) {
  std::visit(
      Overloaded{
          [&](const NamedCallee& named) { out += named.name; },
          [&](const ComputedCallee& computed) {
            Write(view, out, "(", computed.code, ")");
          }},
      callee);
}

}  // namespace

void RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type,
    TargetText& out) {
  const std::optional<CallReceiver> receiver =
      ResolveReceiver(view, call.callee);
  const CalleeSpelling callee =
      ResolveCalleeSpelling(view, call, receiver.has_value(), result_type);

  // The object the call dispatches on goes where the spelling puts it, and a
  // call that dispatches on none puts nothing anywhere and asks nothing.
  // Everything after this is punctuation.
  const bool receiver_leads_the_name =
      receiver.has_value() &&
      PlacementOf(callee) == ReceiverPlacement::kIntoCalleeName;
  if (receiver_leads_the_name) {
    Write(view, out, "(", receiver->expr, ")", receiver->member_access);
  }
  RenderCallee(view, callee, out);

  out += "(";
  bool first = true;
  if (receiver.has_value() && !receiver_leads_the_name) {
    Write(view, out, receiver->expr);
    first = false;
  }
  for (const mir::ExprId id : call.arguments) {
    if (!first) out += ", ";
    Write(view, out, id);
    first = false;
  }
  out += ")";
}

}  // namespace lyra::backend::cpp
