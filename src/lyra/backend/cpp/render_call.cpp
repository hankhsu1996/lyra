#include "lyra/backend/cpp/render_call.hpp"

#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::backend::cpp {

namespace {

// Where a call's receiver is written: before a method, `obj.f(args)`, or first
// in the argument list of a free function, `f(obj, args)`. A call with no
// receiver has nothing to place and ignores this.
enum class ReceiverPlacement : std::uint8_t {
  kIntoCalleeName,
  kIntoArgumentList
};

// Whether a templated method name follows a value or a type. After a value,
// `v.template Component<2>()` needs the `template` keyword, because C++ cannot
// yet tell whether `<` starts an argument list or a comparison; after a type,
// `T::Make<2>()` does not.
enum class NameReachedThrough : std::uint8_t { kAValue, kAType };

// What a callee states beside its target, as one template argument where C++
// resolves types: the type the callee is called at, or the part it names -- a
// component by its position, `Component<2>`, a property by a pointer to it,
// `ReferProperty<&C::x>`.
using TemplateArgument = std::variant<CppType, mir::CallPart>;

// The template arguments a callee states, in the order the library declares
// its template parameters: the type, then the part.
auto TemplateArgumentsOf(
    const mir::CompilationUnit& unit, std::optional<mir::TypeId> type,
    const std::optional<mir::CallPart>& part) -> std::vector<TemplateArgument> {
  std::vector<TemplateArgument> arguments;
  if (type.has_value()) {
    arguments.emplace_back(CppType(unit, *type));
  }
  if (part.has_value()) {
    arguments.emplace_back(*part);
  }
  return arguments;
}

// A runtime function name, followed by the template arguments its callee
// states.
struct OperationName {
  const mir::CompilationUnit* unit;
  std::string_view identifier;
  std::span<const TemplateArgument> arguments;
  NameReachedThrough reached;
};

void WriteOne(TargetText& out, const OperationName& name) {
  if (name.arguments.empty()) {
    out += name.identifier;
    return;
  }
  if (name.reached == NameReachedThrough::kAValue) {
    out += "template ";
  }
  Write(out, name.identifier, "<");
  std::string_view separator;
  for (const TemplateArgument& argument : name.arguments) {
    out += separator;
    separator = ", ";
    std::visit(
        Overloaded{
            [&](const CppType& type) { Write(out, type); },
            [&](const mir::CallPart& part) {
              std::visit(
                  Overloaded{
                      [&](base::ComponentIndex position) {
                        Write(out, position.value);
                      },
                      [&](const mir::ClassFieldTarget& property) {
                        WriteMemberPointer(out, *name.unit, property);
                      }},
                  part);
            }},
        argument);
  }
  out += ">";
}

// Writes one call. Each callee form states, in the same `Named` call, the
// callee's name and where the receiver goes, so the two cannot disagree; the
// writer remembers the second so the argument list can start with the receiver
// when that is where it goes.
class CallWriter {
 public:
  CallWriter(
      const ScopeView& view, std::optional<mir::ExprId> receiver,
      TargetText& out)
      : view_(&view), receiver_(receiver), out_(&out) {
  }

  [[nodiscard]] auto HasReceiver() const -> bool {
    return receiver_.has_value();
  }

  // A callee with a name: a method, a factory on a type, a free function, or a
  // constructor.
  template <typename... Pieces>
  void Named(ReceiverPlacement placement, const Pieces&... pieces) {
    if (HasReceiver()) {
      switch (placement) {
        case ReceiverPlacement::kIntoCalleeName:
          WriteMemberReceiver(*view_, *out_, *receiver_);
          break;
        case ReceiverPlacement::kIntoArgumentList:
          receiver_leads_arguments_ = true;
          break;
      }
    }
    Write(*view_, *out_, pieces...);
  }

  // A callee that is a value, such as a function pointer: `f(args)`, or
  // `(&C::f)(args)` when the value needs parentheses. MIR gives such a call no
  // receiver, so one arriving with a receiver is a compiler bug.
  void Computed(mir::ExprId code) {
    if (HasReceiver()) {
      throw InternalError(
          "RenderCallExpr: a call reaching a computed callee names an object "
          "to dispatch on, which MIR states for no such call -- please report "
          "this as a bug");
    }
    Write(
        *view_, *out_, Operand{.expr = code, .at_least = Precedence::kPostfix});
  }

  // The argument list, led by the receiver when the callee form puts it there.
  void Arguments(std::span<const mir::ExprId> arguments) {
    std::vector<mir::ExprId> operands;
    if (receiver_leads_arguments_) {
      operands.push_back(*receiver_);
    }
    operands.insert(operands.end(), arguments.begin(), arguments.end());
    *out_ += "(";
    WriteCommaSeparated(*view_, *out_, operands);
    *out_ += ")";
  }

 private:
  const ScopeView* view_;
  std::optional<mir::ExprId> receiver_;
  TargetText* out_;
  bool receiver_leads_arguments_ = false;
};

// The type a factory is reached on: the one its callee states where the entry
// is generic over the type it builds, and otherwise the single type it builds,
// which the call answers with.
auto FactoryType(
    const support::RuntimeEntry& entry, const mir::Direct& direct,
    mir::TypeId result_type) -> mir::TypeId {
  if (!entry.takes_a_type_argument) {
    return result_type;
  }
  if (!direct.type_argument.has_value()) {
    throw InternalError(
        "Direct call: a factory generic over the type it builds is called at "
        "none -- please report this as a bug");
  }
  return *direct.type_argument;
}

// A runtime library function, spelled the way its shared declaration says: a
// free function, a method on the receiver, or a factory on the type it builds.
// Nothing about the call itself is read to decide which.
void WriteEntryCallee(
    const ScopeView& view, const support::RuntimeEntry& entry,
    const mir::Direct& direct, mir::TypeId result_type, CallWriter& callee) {
  const mir::CompilationUnit& unit = view.Unit();
  std::visit(
      Overloaded{
          // `lyra::value::Slice<lyra::value::BitVector<4>>(v, at)`.
          [&](const support::FreeFunction& f) {
            const auto stated =
                TemplateArgumentsOf(unit, direct.type_argument, direct.part);
            callee.Named(
                ReceiverPlacement::kIntoArgumentList, f.scope, "::",
                OperationName{
                    .unit = &unit,
                    .identifier = f.identifier,
                    .arguments = stated,
                    .reached = NameReachedThrough::kAType});
          },
          // A method is called on something, so a call with no receiver
          // cannot be written.
          [&](const support::Method& m) {
            if (!callee.HasReceiver()) {
              throw InternalError(
                  "Direct call: the instance form of a runtime entry is "
                  "reached through the object it acts on, and this call names "
                  "none -- please report this as a bug");
            }
            const auto stated =
                TemplateArgumentsOf(unit, direct.type_argument, direct.part);
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                OperationName{
                    .unit = &unit,
                    .identifier = m.identifier,
                    .arguments = stated,
                    .reached = NameReachedThrough::kAValue});
          },
          // A factory is called on the type it builds, so that type is no
          // template argument of its name: `T::Make<2>(args)`. One generic
          // over the type it builds is called at the type its callee states,
          // and one building a single type answers with it.
          [&](const support::StaticFactory& s) {
            const auto stated =
                TemplateArgumentsOf(unit, std::nullopt, direct.part);
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppType(unit, FactoryType(entry, direct, result_type)), "::",
                OperationName{
                    .unit = &unit,
                    .identifier = s.identifier,
                    .arguments = stated,
                    .reached = NameReachedThrough::kAType});
          }},
      entry.declaration);
}

// The name of a callee fixed at compile time. Each kind of function is looked
// up in its own table, and that kind also decides where a receiver goes.
void WriteDirectCallee(
    const ScopeView& view, const mir::Direct& direct, mir::TypeId result_type,
    CallWriter& callee) {
  std::visit(
      Overloaded{
          // Qualified by the owning class, `obj.Base::f()`. For a virtual
          // method called directly (LRM 8.15 `super`) that is what makes C++
          // skip virtual dispatch; for any other method it changes nothing.
          [&](const mir::CallableTarget& t) {
            const auto& cls = view.Unit().GetClass(t.owner);
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppClassPath(view.Unit(), t.owner),
                "::", CppClassCallableName(view.Unit(), cls, t.slot));
          },
          // A function of this unit's namespace is qualified the same way as
          // another unit's: `::Top::f`.
          [&](const mir::UnitCallableTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppUnitScope(view.Unit().name),
                "::", CppUnitCallableName(view.Unit(), t.slot));
          },
          [&](const support::BuiltinFn& id) {
            WriteEntryCallee(
                view, support::RuntimeEntryOf(id), direct, result_type, callee);
          },
          // A function of another unit (LRM 26.3): `::Pkg::f`.
          [&](const mir::ExternalUnitCallableTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName, CppUnitScope(t.unit_name),
                "::", ToCppName(t.callable_name));
          },
          // A method of another unit's class, `obj.::Pkg::C::f()`, found once
          // that unit's header is included. The class qualifier skips virtual
          // dispatch, as a direct call to a virtual method requires (LRM 8.15
          // `super`).
          [&](const mir::ExternalUnitClassMethodTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppExternalClassPath(t.unit_name, t.class_path),
                "::", ToCppName(t.method_name));
          },
          // A struct's method, qualified by the struct the way a class method
          // is: `v.::Pkg::sv_types::s::IsBitIdentical(w)` on the value it is
          // asked of, `::Pkg::sv_types::s::FromBitstream(b, p)` for a question
          // asked of the type.
          [&](const mir::StructMethodTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppStructRef(t.declaration.unit_name, t.declaration.path),
                "::", CppStructMethodName(t.answers));
          },
          // One of another unit's fixed entries: `::Pkg::sv_create`.
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
                              view.Unit(), e.unit_name, e.class_path,
                              e.ordinal));
                    }},
                v.slot);
          },
          // What a construction names comes from its result type, through the
          // type mapping.
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
  CallWriter text(view, mir::CalleeReceiver(call.callee), out);
  WriteCallee(view, call, result_type, text);
  text.Arguments(call.arguments);
}

void RenderStructuralCall(
    const ScopeView& view, support::BuiltinFn fn, mir::TypeId result_type,
    TargetText& out) {
  CallWriter text(view, std::nullopt, out);
  WriteEntryCallee(
      view, support::RuntimeEntryOf(fn), mir::Direct{.target = fn}, result_type,
      text);
  text.Arguments({});
}

}  // namespace lyra::backend::cpp
