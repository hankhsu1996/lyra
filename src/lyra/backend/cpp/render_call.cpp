#include "lyra/backend/cpp/render_call.hpp"

#include <cstdint>
#include <optional>
#include <span>
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
struct OperationName {
  std::string_view identifier;
  std::optional<base::ComponentIndex> position;
  NameReachedThrough reached;
};

void WriteOne(TargetText& out, const OperationName& name) {
  if (!name.position.has_value()) {
    out += name.identifier;
    return;
  }
  if (name.reached == NameReachedThrough::kAValue) {
    out += "template ";
  }
  Write(out, name.identifier, "<", name.position->value, ">");
}

// A call, written with the object it dispatches on wherever the callee's
// spelling puts that object. Whoever works out the spelling states where the
// object goes and what the callee is called in one breath, so the two are never
// carried apart, and the argument list is written by the same party that knows
// whether the object is still owed there.
class CallWriter {
 public:
  CallWriter(
      const ScopeView& view, const std::optional<CallReceiver>& receiver,
      TargetText& out)
      : view_(&view), receiver_(&receiver), out_(&out) {
  }

  [[nodiscard]] auto HasReceiver() const -> bool {
    return receiver_->has_value();
  }

  // A callee C++ spells with a name of its own -- an instance method, a factory
  // on the type it builds, a free function, a type constructor.
  template <typename... Pieces>
  void Named(ReceiverPlacement placement, const Pieces&... pieces) {
    if (HasReceiver()) {
      switch (placement) {
        case ReceiverPlacement::kIntoCalleeName:
          Write(
              *view_, *out_, "(", (*receiver_)->expr, ")",
              (*receiver_)->member_access);
          break;
        case ReceiverPlacement::kIntoArgumentList:
          receiver_leads_arguments_ = true;
          break;
      }
    }
    Write(*view_, *out_, pieces...);
  }

  // A callee the program computes rather than names: the call reaches it
  // through the value an expression produces, which C++ spells by
  // parenthesizing that expression so it stands wherever a name would. Such a
  // callee is reached through no name, and MIR states no object to dispatch on
  // for such a call, so one arriving with an object is refused rather than
  // placed: a placement here would be one no call can ask for.
  void Computed(mir::ExprId code) {
    if (HasReceiver()) {
      throw InternalError(
          "RenderCallExpr: a call reaching a computed callee names an object "
          "to dispatch on, which MIR states for no such call -- please report "
          "this as a bug");
    }
    Write(*view_, *out_, "(", code, ")");
  }

  // Everything after the callee is punctuation around the arguments, with the
  // object the call dispatches on leading them where the callee's spelling put
  // it there.
  void Arguments(std::span<const mir::ExprId> arguments) {
    *out_ += "(";
    bool first = true;
    if (receiver_leads_arguments_) {
      Write(*view_, *out_, (*receiver_)->expr);
      first = false;
    }
    for (const mir::ExprId argument : arguments) {
      if (!first) *out_ += ", ";
      Write(*view_, *out_, argument);
      first = false;
    }
    *out_ += ")";
  }

 private:
  const ScopeView* view_;
  const std::optional<CallReceiver>* receiver_;
  TargetText* out_;
  bool receiver_leads_arguments_ = false;
};

// A runtime entry, spelled the way the library declares it. Nothing here reads
// the call to decide that: which form the entry takes, and the identifier it is
// written with, are the entry's own declaration.
void WriteEntryCallee(
    const ScopeView& view, const support::RuntimeEntry& entry,
    const std::optional<base::ComponentIndex>& position,
    mir::TypeId result_type, CallWriter& callee) {
  std::visit(
      Overloaded{
          [&](const support::FreeFunction& f) {
            callee.Named(
                ReceiverPlacement::kIntoArgumentList, f.qualified_name);
          },
          // A method names nothing on its own, so a call reaching this
          // spelling without an object to reach it through has no C++ text at
          // all.
          [&](const support::Method& m) {
            if (!callee.HasReceiver()) {
              throw InternalError(
                  "Direct call: the instance form of a runtime entry is "
                  "reached through the object it acts on, and this call names "
                  "none -- please report this as a bug");
            }
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                OperationName{
                    .identifier = m.identifier,
                    .position = position,
                    .reached = NameReachedThrough::kAValue});
          },
          // A factory is reached on the type it builds, which is the type of
          // the value the call answers with.
          [&](const support::StaticFactory& s) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppType(view.Unit(), result_type), "::",
                OperationName{
                    .identifier = s.identifier,
                    .position = position,
                    .reached = NameReachedThrough::kAType});
          }},
      entry.declaration);
}

// What C++ names a `Direct` callee. Each alternative is a lookup in the table
// that resolves its own identity space, and how a receiver rides follows from
// what that lookup found.
void WriteDirectCallee(
    const ScopeView& view, const mir::Direct& direct, mir::TypeId result_type,
    CallWriter& callee) {
  std::visit(
      Overloaded{
          // The owner prefix is a fixed function of the target's owner: it is
          // redundant for a non-virtual method and, for a virtual one a direct
          // call reaches (LRM 8.15 super), is what makes C++ bypass the vtable.
          [&](const mir::CallableTarget& t) {
            const auto& cls = view.Unit().GetClass(t.owner);
            callee.Named(
                ReceiverPlacement::kIntoCalleeName, CppClassName(cls, t.owner),
                "::", CppClassCallableName(view.Unit(), cls, t.slot));
          },
          // This unit's C++ peer is a namespace too, so a body of its own is
          // named through that namespace exactly as another unit's body is.
          [&](const mir::UnitCallableTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppUnitScope(view.Unit().name),
                "::", CppUnitCallableName(view.Unit(), t.slot));
          },
          [&](const support::BuiltinFn& id) {
            WriteEntryCallee(
                view, support::RuntimeEntryOf(id), direct.position, result_type,
                callee);
          },
          // Another compilation unit's C++ peer is a namespace, so a callable
          // of it (LRM 26.3) is named through that namespace.
          [&](const mir::ExternalUnitCallableTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName, CppUnitScope(t.unit_name),
                "::", ToCppName(t.callable_name));
          },
          // A method on one of that namespace's classes is named through the
          // class as well. Target-language name lookup resolves it once the
          // declaring unit's header is included, and the class qualification
          // makes C++ bypass the vtable, exactly as a direct call to a virtual
          // method demands (LRM 8.15 super).
          [&](const mir::ExternalUnitClassMethodTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName, CppUnitScope(t.unit_name),
                "::", ToCppName(t.class_name), "::", ToCppName(t.method_name));
          },
          // A body of another unit that answers to no name is named through
          // that unit's namespace by which of them it is -- the same identifier
          // that unit emitted it under.
          [&](const mir::ExternalUnitMintedEntryTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName, CppUnitScope(t.unit_name),
                "::", CppMintedEntryName(t.entry));
          },
          // A DPI-C symbol is program-global, so it is spelled unqualified
          // (LRM 35.4); the prototype it resolves against is declared once in
          // this artifact.
          [&](const mir::ForeignSymbolTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppForeignSymbolName(t.linkage_name));
          }},
      direct.target);
}

void WriteCallee(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type,
    CallWriter& callee) {
  std::visit(
      Overloaded{
          [&](const mir::Direct& d) {
            WriteDirectCallee(view, d, result_type, callee);
          },
          [&](const mir::Indirect& i) { callee.Computed(i.code); },
          [&](const mir::Virtual& v) {
            std::visit(
                Overloaded{
                    [&](const mir::LocalVirtualSlot& l) {
                      callee.Named(
                          ReceiverPlacement::kIntoCalleeName,
                          CppClassCallableName(
                              view.Unit(), view.Unit().GetClass(l.owner_class),
                              l.slot));
                    },
                    [&](const mir::ExternalVirtualSlot& e) {
                      callee.Named(
                          ReceiverPlacement::kIntoCalleeName,
                          CppExternalBehaviorName(
                              view.Unit(), e.unit_name, e.class_name,
                              e.ordinal));
                    }},
                v.slot);
          },
          // A type has one way to come into existence, and what names it is the
          // type's own answer -- read through type mapping, the way every other
          // target-language spelling of a type is.
          [&](const mir::Construct&) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppConstructorName{.of = CppType(view.Unit(), result_type)});
          }},
      call.callee);
}

}  // namespace

void RenderCallExpr(
    const ScopeView& view, const mir::CallExpr& call, mir::TypeId result_type,
    TargetText& out) {
  const std::optional<CallReceiver> receiver =
      ResolveReceiver(view, call.callee);
  CallWriter text(view, receiver, out);
  WriteCallee(view, call, result_type, text);
  text.Arguments(call.arguments);
}

}  // namespace lyra::backend::cpp
