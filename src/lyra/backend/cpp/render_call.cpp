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
#include "lyra/mir/closure_signature.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/runtime_class.hpp"

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
// `T::FromBitstream<R>(bits)` does not.
enum class NameReachedThrough : std::uint8_t { kAValue, kAType };

// What a callee states beside its target, as one template argument where C++
// resolves types: the type the callee is called at, or the signature of a
// closure of no known type it names, or the part it names -- a component by
// its position, `Component<2>`, a property by a pointer to it,
// `ReferProperty<&C::x>`.
using TemplateArgument = std::variant<CppType, CppCallSignature, mir::CallPart>;

// The template arguments a callee states, in the order the library declares
// its template parameters: the type, then the part.
auto TemplateArgumentsOf(
    const mir::CompilationUnit& unit, std::span<const mir::TypeId> types,
    const std::optional<mir::CallPart>& part) -> std::vector<TemplateArgument> {
  std::vector<TemplateArgument> arguments;
  for (const mir::TypeId type : types) {
    arguments.emplace_back(CppType(unit, type));
  }
  if (part.has_value()) {
    arguments.emplace_back(*part);
  }
  return arguments;
}

// The two classes an entry seeing an object as another class is told: the one
// the handle it is handed refers through, then the one the type it is called
// at refers through, as `ViewAs<From, To>(r)` names both.
auto ClassesSeen(
    const mir::CompilationUnit& unit, mir::TypeId handed, mir::TypeId called_at)
    -> std::vector<TemplateArgument> {
  const auto class_of = [&](mir::TypeId handle) {
    return CppType(
        unit, unit.types.Get(handle).Get<mir::ManagedRefType>().pointee);
  };
  return {class_of(handed), class_of(called_at)};
}

// The template arguments a callee states, `<A, B>`, written after its name;
// none writes nothing.
struct TemplateArgumentList {
  const mir::CompilationUnit* unit;
  std::span<const TemplateArgument> arguments;
};

// A runtime function name, followed by the template arguments its callee
// states.
struct OperationName {
  const mir::CompilationUnit* unit;
  std::string_view identifier;
  std::span<const TemplateArgument> arguments;
  NameReachedThrough reached;
};

void WriteOne(TargetText& out, const TemplateArgumentList& list);

void WriteOne(TargetText& out, const OperationName& name) {
  if (!name.arguments.empty() && name.reached == NameReachedThrough::kAValue) {
    out += "template ";
  }
  Write(
      out, name.identifier,
      TemplateArgumentList{.unit = name.unit, .arguments = name.arguments});
}

void WriteOne(TargetText& out, const TemplateArgumentList& list) {
  if (list.arguments.empty()) {
    return;
  }
  out += "<";
  std::string_view separator;
  for (const TemplateArgument& argument : list.arguments) {
    out += separator;
    separator = ", ";
    std::visit(
        Overloaded{
            [&](const CppType& type) { Write(out, type); },
            [&](const CppCallSignature& signature) {
              WriteOne(out, signature);
            },
            [&](const mir::CallPart& part) {
              std::visit(
                  Overloaded{
                      [&](base::ComponentIndex position) {
                        Write(out, position.value);
                      },
                      [&](const mir::ClassFieldTarget& property) {
                        WriteMemberPointer(out, *list.unit, property);
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
  if (direct.type_arguments.size() != 1) {
    throw InternalError(
        "Direct call: a factory generic over the type it builds is called at "
        "other than one -- please report this as a bug");
  }
  return direct.type_arguments.front();
}

// A runtime library function, spelled the way its shared declaration says: a
// free function, a method on the receiver, or a factory on the type it builds.
// Nothing about the call itself is read to decide which.
void WriteEntryCallee(
    const ScopeView& view, const support::RuntimeEntry& entry,
    const mir::Direct& direct, std::span<const mir::ExprId> arguments,
    mir::TypeId result_type, CallWriter& callee) {
  const mir::CompilationUnit& unit = view.Unit();
  std::visit(
      Overloaded{
          // `lyra::value::Slice<lyra::value::BitVector<4>>(v, at)`.
          [&](const support::FreeFunction& f) {
            const std::vector<TemplateArgument> stated = [&] {
              switch (entry.type_argument_named) {
                case support::TypeArgumentNamed::kTheType:
                  return TemplateArgumentsOf(
                      unit, direct.type_arguments, direct.part);
                case support::TypeArgumentNamed::kTheClassesSeen:
                  return ClassesSeen(
                      unit, view.Expr(arguments.front()).type,
                      FactoryType(entry, direct, result_type));
                case support::TypeArgumentNamed::kTheCallSignature:
                  return std::vector<TemplateArgument>{CppCallSignature(
                      unit, FactoryType(entry, direct, result_type))};
              }
              throw InternalError("Direct call: unknown TypeArgumentNamed");
            }();
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
                TemplateArgumentsOf(unit, direct.type_arguments, direct.part);
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                OperationName{
                    .unit = &unit,
                    .identifier = m.identifier,
                    .arguments = stated,
                    .reached = NameReachedThrough::kAValue});
          },
          // A factory is called on the type it builds, so that type is no
          // template argument of its name: `T::FromBitstream(bits)`. One
          // generic
          // over the type it builds is called at the type its callee states,
          // and one building a single type answers with it.
          [&](const support::StaticFactory& s) {
            const auto stated = TemplateArgumentsOf(unit, {}, direct.part);
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

// Whether the library's member answering `operation` on a value is generic
// over the type it answers, as its runtime entry declares: the answer's type is
// then the member's template argument, which only this backend instantiates.
auto OperationTakesATypeArgument(support::ValueOperation operation) -> bool {
  return std::visit(
      Overloaded{
          [](support::ValueOperator) { return false; },
          [](support::BuiltinFn fn) {
            return support::RuntimeEntryOf(fn).takes_a_type_argument;
          }},
      operation);
}

// The name of a callee fixed at compile time. Each kind of function is looked
// up in its own table, and that kind also decides where a receiver goes.
void WriteDirectCallee(
    const ScopeView& view, const mir::Direct& direct,
    std::span<const mir::ExprId> arguments, mir::TypeId result_type,
    CallWriter& callee) {
  std::visit(
      Overloaded{
          // Qualified by the owning class, `obj.Base::f()`, or
          // `obj.::Pkg::C::f()` for a class another unit defines, found once
          // that unit's header is included. For a virtual method called
          // directly (LRM 8.15 `super`) that is what makes C++ skip virtual
          // dispatch; for any other method it changes nothing.
          [&](const mir::CallableTarget& t) {
            const auto& cls = view.Unit().GetClass(t.owner);
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppClassPath(view.Unit(), t.owner),
                "::", CppClassCallableName(view.Unit(), cls, t.slot));
          },
          // A closure's body, as its type's call operator on the closure it
          // borrows, `c.sv_closure_3::operator()(x)`, or as the type's
          // function taking the box the closure was made in over,
          // `sv_closure_3::sv_call_once(box)`.
          [&](const mir::ClosureTarget& t) {
            const MintedName name = CppClosureName(t.closure);
            switch (mir::ClosureKindOf(
                view.Unit().types, view.Unit().GetClosure(t.closure))) {
              case support::ClosureKind::kFn:
              case support::ClosureKind::kFnMut:
                callee.Named(
                    ReceiverPlacement::kIntoCalleeName, name, "::operator()");
                return;
              case support::ClosureKind::kFnOnce:
                callee.Named(
                    ReceiverPlacement::kIntoArgumentList, name,
                    "::", CppClosureCallOnceName());
                return;
            }
            throw InternalError("backend::cpp: unknown ClosureKind");
          },
          // A function of a unit's namespace is qualified by that unit's,
          // `::Top::f`, this unit's own included, and a generic one names the
          // types it is called at as its template's arguments, `::Top::f<A,
          // B>`.
          [&](const mir::UnitCallableTarget& t) {
            const std::vector<TemplateArgument> stated =
                TemplateArgumentsOf(view.Unit(), direct.type_arguments, {});
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppUnitScope(CppUnitCallableOwner(view.Unit(), t.slot)),
                "::", CppUnitCallableName(view.Unit(), t.slot),
                TemplateArgumentList{
                    .unit = &view.Unit(), .arguments = stated});
          },
          // The closure a type parameter is made concrete at, called in place
          // as the parameter's bound asks: `f.operator()(x)`, which every
          // closure called in place declares, so a template body names it
          // without knowing which closure it is.
          [&](const mir::BoundCallTarget&) {
            callee.Named(ReceiverPlacement::kIntoCalleeName, "operator()");
          },
          // The keeper a type parameter is made concrete at, committing the
          // effect made at an address it handed out, `k.Commit(made)`, which
          // every keeper the bound admits declares.
          [&](const mir::BoundCommitTarget&) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                VerbatimName{.text = support::kKeeperCommitMethod});
          },
          [&](const support::BuiltinFn& id) {
            WriteEntryCallee(
                view, support::RuntimeEntryOf(id), direct, arguments,
                result_type, callee);
          },
          // A base's constructor on the object being constructed,
          // `(*self).Base::sv_construct(args)`, qualified as a direct call to a
          // member is.
          [&](const mir::ConstructorTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppClassRef(view.Unit(), t.cls),
                "::", CppConstructorMemberName());
          },
          // A method of a struct or a union, qualified by its type the way a
          // class method is: `v.::Pkg::sv_types::s::IsBitIdentical(w)` on the
          // value it is asked of, `::Pkg::sv_types::s::FromBitstream(b)` for a
          // question asked of the type.
          [&](const mir::RecordMethodTarget& t) {
            const mir::TypeDeclarationRef declaration =
                mir::RecordDeclarationOf(view.Unit(), t.record);
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppRecordQualifiedName(declaration.unit_name, declaration.path),
                "::", CppValueOperationMemberName(t.answers));
          },
          // An operation a fixed-size array answers is the library
          // template's own member, which the host compiler instantiates for
          // the type where the call is written:
          // `a.lyra::value::UnpackedArray<T, 4>::operator==(b)`. A member that
          // is itself generic over what it answers is called at the answer
          // the call states:
          // `a.UnpackedArray<T, 4>::template ToBitstream<R>()`. The body the
          // unit states for the operation is for a backend that resolves no
          // type where the call is written.
          [&](const mir::TypeOperationTarget& t) {
            const std::vector<TemplateArgument> answered =
                OperationTakesATypeArgument(t.answers)
                    ? std::vector<TemplateArgument>{CppType(
                          view.Unit(), result_type)}
                    : std::vector<TemplateArgument>{};
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppType(view.Unit(), t.type), "::",
                OperationName{
                    .unit = &view.Unit(),
                    .identifier = CppValueOperationMemberName(t.answers).text,
                    .arguments = answered,
                    .reached = NameReachedThrough::kAValue});
          },
          // One of another unit's bring-up entries:
          // `::Pkg::sv_initialize_namespace_storage`.
          [&](const mir::ExternalBringUpTarget& t) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName, CppUnitScope(t.unit_name),
                "::", CppBringUpEntryName(t.entry));
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
            WriteDirectCallee(view, d, call.arguments, result_type, callee);
          },
          [&](const mir::Indirect& i) { callee.Computed(i.code); },
          [&](const mir::Virtual& v) {
            callee.Named(
                ReceiverPlacement::kIntoCalleeName,
                CppVirtualSlotName(view.Unit(), v.slot));
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
      view, support::RuntimeEntryOf(fn), mir::Direct{.target = fn}, {},
      result_type, text);
  text.Arguments({});
}

}  // namespace lyra::backend::cpp
