#include "lyra/backend/cpp/render_decl.hpp"

#include <cstddef>
#include <format>
#include <optional>
#include <string>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/field.hpp"

namespace lyra::backend::cpp {

namespace {

// A field declaration is (name, type): the type carries the target storage
// form, the name the source identifier. Every per-field state -- a cell's
// declared representation, a net's fold, an initial value -- arrives as
// ordinary MIR statements in the constructor body, so the declaration
// value-initializes and carries nothing else.
auto RenderField(
    const mir::CompilationUnit& unit,
    std::span<const mir::NamedField> named_fields,
    const base::Arena<mir::FieldDecl, mir::FieldId>& fields, mir::FieldId slot,
    std::size_t indent) -> std::string {
  return std::format(
      "{}{} {}{{}};\n", Indent(indent),
      RenderTypeAsCpp(unit, fields.Get(slot).type),
      CppFieldName(named_fields, slot));
}

// The value-init field declarations of any field-bearing storage -- a class's
// fields, a promoted scope's fields. Shared so a generated struct carries no
// field-emission of its own; it feeds the same declarations a class does.
auto RenderFieldList(
    const mir::CompilationUnit& unit,
    std::span<const mir::NamedField> named_fields,
    const base::Arena<mir::FieldDecl, mir::FieldId>& fields, std::size_t indent)
    -> std::string {
  std::string out;
  for (const mir::FieldId slot : fields.Ids()) {
    out += RenderField(unit, named_fields, fields, slot, indent);
  }
  return out;
}

// A class static property (LRM 8.9) renders as an `inline static` member of
// the C++ class: one cell owned by the type, default-constructed here and
// given its declared representation and value where whatever brings the class's
// owner up runs, never baked into the declaration.
auto RenderClassStaticProperty(
    const mir::CompilationUnit& unit, const mir::Class& s,
    mir::StaticPropertyId slot, std::size_t indent) -> std::string {
  const std::string type =
      RenderTypeAsCpp(unit, s.static_properties.Get(slot).type);
  return std::format(
      "{}inline static {} {}{{}};\n", Indent(indent), type,
      CppStaticPropertyName(s.named_static_properties, slot));
}

auto RenderClassStaticProperties(
    const mir::CompilationUnit& unit, const mir::Class& s) -> std::string {
  std::string out;
  for (const mir::StaticPropertyId slot : s.static_properties.Ids()) {
    out += RenderClassStaticProperty(unit, s, slot, 1);
  }
  return out;
}

auto RenderCallableParam(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    mir::LocalId param) -> std::string {
  // Every formal is a value parameter: an `input` by value (LRM 13.5.1), a
  // `ref` / `const ref` whose `RefType` already renders as `(const) Ref<T>` so
  // the reference value carries the aliasing (LRM 13.5.2). `output` / `inout`
  // are not parameters -- they ride the completion payload.
  return std::format(
      "{} {}", RenderTypeAsCpp(unit, code.locals.Get(param).type),
      CppLocalName(code.named_locals, param));
}

// The C++ specifier this callable's dispatch role prefixes its declaration
// with: `virtual` when the callable introduces a new dispatch slot on this
// class, empty otherwise; the source of virtualness for an override is the
// slot the base already declares, which the `override` suffix records
// separately.
auto VirtualPrefix(const mir::CallableDecl& m) -> std::string_view {
  return mir::IntroducesSlot(m.virtual_dispatch) ? "virtual " : "";
}

// The trailing specifier attached after the return type when this callable
// fills an inherited dispatch slot: `override` records that the base's slot
// resolves through this implementation, so a name-only compilation cannot
// silently disagree with the intended override target.
auto OverrideSuffix(const mir::CallableDecl& m) -> std::string_view {
  if (!m.virtual_dispatch.has_value()) return "";
  return mir::IntroducesSlot(m.virtual_dispatch) ? "" : " override";
}

// The parameter list a class callable declares, and the position its user
// formals start at. Instance vs static (LRM 8.10) is a signature-level fact
// carried by the presence of a self-typed `params[0]`: the C++ `static` prefix,
// the omission of that parameter from the C++ list, and the body's
// receiver-alias all read off this one check, so no side flag restates what the
// signature already fixes.
auto RenderUserParams(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    std::size_t start) -> std::string {
  std::string out;
  for (std::size_t i = start; i < code.params.size(); ++i) {
    if (i != start) out += ", ";
    out += RenderCallableParam(unit, code, code.params[i]);
  }
  return out;
}

auto RenderClassCallableDecl(
    const mir::CompilationUnit& unit, const mir::Class& s, mir::CallableId id,
    const mir::CallableDecl& m) -> std::string {
  const mir::CallableCode& code = m.code;
  const bool has_receiver = code.HasReceiver(s.self_pointer_type);
  const std::string sig = std::format(
      "{}{}auto {}({}) -> {}{}", has_receiver ? "" : "static ",
      VirtualPrefix(m), CppClassCallableName(s, id),
      RenderUserParams(unit, code, has_receiver ? 1 : 0),
      RenderTypeAsCpp(unit, code.result_type), OverrideSuffix(m));
  // A class method this declaration does not define is a pure virtual (LRM
  // 8.21) -- the only bodyless form a class member takes, since a foreign
  // callable is never one. `= 0` states that, and C++ then treats the enclosing
  // class as abstract with no class-level marker of its own.
  return std::format(
      "{}{}{};\n", Indent(1), sig, code.body.has_value() ? "" : " = 0");
}

// The definition of a class-owned callable -- an instance method (LRM 8.6), a
// static method (LRM 8.10), a process, or a lifecycle body -- out of line, so
// its body may reach any class of the unit as a complete type. An instance
// callable's body opens with a one-line `self = this` adapter, so the body's
// expressions resolve receiver-relative references uniformly. A pure virtual
// prototype has no definition. A namespace's receiver-less callable renders
// through the free-function path instead.
auto RenderClassCallableDef(
    const mir::CompilationUnit& unit, mir::ClassId cls_id, const mir::Class& s,
    mir::CallableId id, const mir::CallableDecl& m) -> std::string {
  const mir::CallableCode& code = m.code;
  if (!code.body.has_value()) return "";
  const bool has_receiver = code.HasReceiver(s.self_pointer_type);
  std::string out = std::format(
      "inline auto {}::{}({}) -> {} {{\n", CppClassName(s, cls_id),
      CppClassCallableName(s, id),
      RenderUserParams(unit, code, has_receiver ? 1 : 0),
      RenderTypeAsCpp(unit, code.result_type));
  if (has_receiver) {
    const mir::LocalId self = code.params[0];
    out += std::format(
        "{}{} {} = this;\n", Indent(1),
        RenderTypeAsCpp(unit, code.locals.Get(self).type),
        CppLocalName(code.named_locals, self));
  }
  out += RenderBlockStatements(ScopeView::ForRoot(unit, cls_id, s, code), 1);
  out += "}\n";
  return out;
}

// A runtime-callback adapter: a static class member so its address decays to a
// plain function pointer of the shape the runtime callback table requires. The
// receiver is the callable's first explicit parameter, rendered like any other
// formal.
auto RenderAbiAdapterDecl(
    const mir::CompilationUnit& unit, mir::AbiAdapterId id,
    const mir::AbiAdapter& a) -> std::string {
  return std::format(
      "{}static auto {}({}) -> {};\n", Indent(1), CppAbiAdapterName(id),
      RenderUserParams(unit, a.code, 0),
      RenderTypeAsCpp(unit, a.code.result_type));
}

auto RenderAbiAdapterDef(
    const mir::CompilationUnit& unit, mir::ClassId cls_id, const mir::Class& s,
    mir::AbiAdapterId id, const mir::AbiAdapter& a) -> std::string {
  return std::format(
      "inline auto {}::{}({}) -> {} {{\n{}}}\n", CppClassName(s, cls_id),
      CppAbiAdapterName(id), RenderUserParams(unit, a.code, 0),
      RenderTypeAsCpp(unit, a.code.result_type),
      RenderBlockStatements(ScopeView::ForRoot(unit, cls_id, s, a.code), 1));
}

// The C++ construction shell, split the way every other member is: the class
// body declares the constructor and its `init` helper, and the definitions
// carry the base's initializer clause and the body -- which is what lets a
// constructor build a child whose own body reaches back into this class.
//
// The C++ ctor is composed from the class's construction protocol: the ctor's
// own callable carries the signature (with `self` at position 0 per MIR
// contract -- omitted from the C++ list because C++ makes `this` implicit), and
// the protocol carries what the base is entered with. Property initialization
// needs nothing here: it arrives as statements at the head of the ctor's own
// body, already in the order LRM 8.7 requires. The body is threaded through a
// static `init(self, ...)` helper so a body-local `self` reference resolves the
// same way it does in every other method render.
struct ConstructorText {
  std::string declaration;
  std::string definitions;
};

auto RenderConstructor(
    const mir::CompilationUnit& unit, mir::ClassId cls_id, const mir::Class& s)
    -> ConstructorText {
  const ScopeView scope_view =
      ScopeView::ForRoot(unit, cls_id, s, s.constructor.code);
  const auto& ctor_code = s.constructor.code;
  const auto render_typed_name = [&](mir::TypeId type, std::string_view name) {
    return std::format("{} {}", RenderTypeAsCpp(unit, type), name);
  };

  std::vector<std::string> sig_args;
  std::vector<std::string> forward_names;
  sig_args.reserve(ctor_code.params.size());
  forward_names.reserve(ctor_code.params.size());
  // Skip params[0] (self, MIR contract); the C++ ctor's receiver is `this`.
  for (std::size_t i = 1; i < ctor_code.params.size(); ++i) {
    const mir::LocalId param = ctor_code.params[i];
    const std::string name = CppLocalName(ctor_code.named_locals, param);
    sig_args.push_back(
        render_typed_name(ctor_code.locals.Get(param).type, name));
    forward_names.push_back(name);
  }

  std::optional<std::string> base_clause;
  if (s.base.has_value()) {
    std::vector<std::string> base_args_rendered;
    base_args_rendered.reserve(s.constructor.base_args.size());
    for (const mir::ExprId arg : s.constructor.base_args) {
      base_args_rendered.push_back(
          RenderExpr(scope_view, scope_view.Expr(arg)));
    }
    base_clause = std::format(
        " : {}({})", RenderClassRefAsCpp(unit, *s.base),
        JoinCommaSeparated(base_args_rendered));
  }

  const std::string cpp_name = CppClassName(s, cls_id);
  const std::string params = JoinCommaSeparated(sig_args);

  // The base subobject is constructed before any statement of the body can
  // run, so that one step stays in the C++ constructor's own initializer
  // clause and the constructor is otherwise an allocation shell handing off to
  // a static `init(receiver, ...)` -- the same static-over-self shape every
  // method render uses. The receiver is spelled from the same binding the body
  // reads it through, so the parameter and every reference to it agree by
  // construction rather than by both reaching for one word. The constructor
  // formals ride alongside it so the body reaches them the same way it reaches
  // any parameter.
  std::vector<std::string> init_params;
  init_params.reserve(sig_args.size() + 1);
  init_params.push_back(
      std::format(
          "{}* {}", cpp_name,
          CppLocalName(ctor_code.named_locals, ctor_code.params[0])));
  for (const std::string& arg : sig_args) {
    init_params.push_back(arg);
  }
  std::vector<std::string> init_call_args;
  init_call_args.reserve(forward_names.size() + 1);
  init_call_args.emplace_back("this");
  for (const std::string& name : forward_names) {
    init_call_args.push_back(name);
  }
  const std::string init_signature = JoinCommaSeparated(init_params);
  return ConstructorText{
      .declaration = std::format(
          "{0}{1}({2});\n"
          "{0}static auto init({3}) -> void;\n",
          Indent(1), cpp_name, params, init_signature),
      .definitions = std::format(
          "inline {0}::{0}({1}){2} {{ init({3}); }}\n"
          "inline auto {0}::init({4}) -> void {{\n"
          "{5}"
          "}}\n",
          cpp_name, params, base_clause.value_or(std::string{}),
          JoinCommaSeparated(init_call_args), init_signature,
          RenderBlockStatements(scope_view, 1))};
}

// A compiler-generated struct emits as a plain struct of value-init fields --
// a promoted automatic scope synthesized while lowering some body. Storage
// only: no base, no constructor, no methods.
auto RenderStruct(
    const mir::CompilationUnit& unit, mir::StructId id,
    const mir::StructDecl& decl) -> std::string {
  std::string out = "struct " + CppStructName(id) + " {\n";
  out += RenderFieldList(unit, {}, decl.fields, 1);
  out += "};\n";
  return out;
}

// A class-level static constant, declared as a static member whose initializer
// is the translated value expression. A runtime scope's generated-behavior
// record is one such constant; the constructor forwards its address to the
// base. It stays in the class body because its initializer names the class's
// own members, which are in scope there. A constant that points into another
// only ever takes its address, which does not depend on that one's initializer
// having run.
auto RenderStaticConstant(
    const mir::CompilationUnit& unit, mir::ClassId cls_id, const mir::Class& s,
    mir::StaticConstantId id, const mir::StaticConstantDecl& c) -> std::string {
  const ScopeView view = ScopeView::ForClassConstant(unit, cls_id, s, c.body);
  return Indent(1) + ClassConstantOf(
                         RenderTypeAsCpp(unit, c.type),
                         CppStaticConstantName(id),
                         RenderExpr(view, view.Expr(c.value)));
}

auto RenderClass(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& s)
    -> ClassText;

// Appends a class and every intra-unit base it depends on, in an order that
// guarantees each base is a complete C++ type before its derived (C++ requires
// base completeness at derivation). The interning walk sets the registry order,
// which may reach a derived class first, so this walker climbs the base chain
// first and marks visited classes in `emitted`.
void AppendClassInDependencyOrder(
    const mir::CompilationUnit& unit, mir::ClassId id,
    std::vector<bool>& emitted, ClassText& text) {
  if (emitted[id.value]) return;
  const mir::Class& cls = unit.GetClass(id);
  if (cls.base.has_value()) {
    if (const auto* intra = std::get_if<mir::IntraUnitClassRef>(&*cls.base)) {
      AppendClassInDependencyOrder(unit, intra->class_id, emitted, text);
    }
  }
  if (emitted[id.value]) return;
  emitted[id.value] = true;
  const ClassText rendered = RenderClass(unit, id, cls);
  AppendSection(text.declaration, rendered.declaration);
  AppendSection(text.definitions, rendered.definitions);
}

auto RenderClass(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& s)
    -> ClassText {
  ClassText text;
  std::string& out = text.declaration;
  out += "class " + CppClassName(s, id);
  if (s.is_final) {
    out += " final";
  }
  // Concrete base class first (LRM 8.13), then each interface contract
  // (LRM 8.26). C++ handles the multi-base combination natively: an
  // interface class carries no instance storage, so the multiple-inheritance
  // does not introduce diamond storage; the target-language virtual-call
  // machinery routes each vtable slot to the one implementation the class
  // provides.
  bool base_emitted = false;
  const auto append_base = [&](std::string_view rendered) {
    out += base_emitted ? ", public " : " : public ";
    out += rendered;
    base_emitted = true;
  };
  if (s.base.has_value()) {
    append_base(RenderClassRefAsCpp(unit, *s.base));
  } else if (!s.is_interface_class) {
    // A class extending nothing roots an SV class hierarchy -- a scope names a
    // runtime base and so took the branch above -- and what this target roots
    // one over is the object model's own answer. An interface class declares no
    // storage and is never constructed (LRM 8.26), so nothing roots it.
    append_base(ManagedObjectRootCppType());
  }
  for (const mir::ClassRef& iface : s.implements) {
    append_base(RenderClassRefAsCpp(unit, iface));
  }
  out += " {\n";
  out += " public:\n";

  // An interface class carries only pure virtual method contracts and no
  // instance storage (LRM 8.26), so it has no constructor to emit; C++
  // makes the class implicitly abstract by virtue of the pure virtual
  // methods and forbids `new` on it.
  if (!s.is_interface_class) {
    const ConstructorText ctor = RenderConstructor(unit, id, s);
    AppendSection(out, ctor.declaration);
    AppendSection(text.definitions, ctor.definitions);
  }

  // Members are public so cross-unit references can reach them directly.
  AppendSection(out, RenderFieldList(unit, s.named_fields, s.fields, 1));

  // Type-associated storage (LRM 8.9): one cell per class, declared here and
  // given its value where whatever brings the class's owner up runs, the same
  // way an instance member is given one by the constructor.
  AppendSection(out, RenderClassStaticProperties(unit, s));

  // Every callable the class owns. The constructor is not in this arena; it was
  // emitted above with C++ mem-init-list syntax. A pure virtual prototype (LRM
  // 8.21) declares its `= 0` marker and defines nothing, so its definition is
  // an empty section.
  std::string callable_decls;
  for (const mir::CallableId callable_id : s.callables.Ids()) {
    const mir::CallableDecl& callable = s.callables.Get(callable_id);
    callable_decls += RenderClassCallableDecl(unit, s, callable_id, callable);
    AppendSection(
        text.definitions,
        RenderClassCallableDef(unit, id, s, callable_id, callable));
  }
  AppendSection(out, callable_decls);

  // The class's runtime-callback adapters. Each renders as a static member
  // whose address decays to a plain function pointer for the runtime
  // callback table.
  std::string adapter_decls;
  for (const mir::AbiAdapterId adapter_id : s.abi_adapters.Ids()) {
    const mir::AbiAdapter& a = s.abi_adapters.Get(adapter_id);
    adapter_decls += RenderAbiAdapterDecl(unit, adapter_id, a);
    AppendSection(
        text.definitions, RenderAbiAdapterDef(unit, id, s, adapter_id, a));
  }
  AppendSection(out, adapter_decls);

  // The class's static constants (a tree node's generated-behavior record among
  // them), each emitted as a static member. Its initializer names the class's
  // own adapters, declared just above, so it stays in the class body.
  for (const mir::StaticConstantId constant_id : s.static_constants.Ids()) {
    AppendSection(
        out,
        RenderStaticConstant(
            unit, id, s, constant_id, s.static_constants.Get(constant_id)));
  }

  out += "};\n";
  return text;
}

// The free-function signature of a callable the unit's namespace or the DPI-C
// name space owns: its storage class, the symbol it is reached by, its named
// parameters, and its result type. A plain callable is `inline`, because its
// definition sits in the header every caller includes; a foreign one takes C
// linkage, since its symbol is program-global (LRM 35.4). Every use of this --
// an import's declaration, an export entry point's definition, a package
// function's definition -- reads the one signature the callable carries, so no
// two of them can disagree.
auto RenderFreeCallableSignature(
    const mir::CompilationUnit& unit, mir::CallableId id,
    const mir::CallableDecl& callable) -> std::string {
  const mir::CallableCode& code = callable.code;
  std::vector<std::string> params;
  params.reserve(code.params.size());
  for (const mir::LocalId param : code.params) {
    params.push_back(RenderCallableParam(unit, code, param));
  }
  return std::format(
      "{} auto {}({}) -> {}",
      callable.foreign.has_value() ? R"(extern "C")" : "inline",
      CppUnitCallableName(unit, id), JoinCommaSeparated(params),
      RenderTypeAsCpp(unit, code.result_type));
}

// A callable the unit owns directly, rendered as a free function definition:
// there is no receiver and it belongs to no class, so the body renders against
// a classless scope view and every name it uses resolves in the unit's
// namespace, which is where this lands. For a DPI-C export entry point that
// means its context recovery, marshaling, exported-subroutine call, and
// writeback all render mechanically, the inner call reaching its class by the
// one name that class carries.
auto RenderFreeCallable(
    const mir::CompilationUnit& unit, mir::CallableId id,
    const mir::CallableDecl& callable) -> std::string {
  std::string out;
  out +=
      std::format("{} {{\n", RenderFreeCallableSignature(unit, id, callable));
  out += RenderBlockStatements(ScopeView::ForNamespace(unit, callable.code), 1);
  out += "}\n";
  return out;
}

}  // namespace

// Every class of the unit, declarations first and definitions after, together
// with the forward declarations that let a field or a signature name a class
// whose own declaration has not been reached yet.
auto RenderUnitClasses(const mir::CompilationUnit& unit) -> ClassText {
  ClassText text;
  std::string forward_declarations;
  std::string struct_definitions;
  for (const mir::ClassId id : unit.classes.Ids()) {
    forward_declarations +=
        std::format("class {};\n", CppClassName(unit.GetClass(id), id));
  }
  // A struct's forward declaration leads every struct body, because one
  // generated scope's field may name another's.
  for (const mir::StructId id : unit.structs.Ids()) {
    forward_declarations += std::format("struct {};\n", CppStructName(id));
    struct_definitions += RenderStruct(unit, id, unit.GetStruct(id));
  }
  AppendSection(text.declaration, forward_declarations);
  AppendSection(text.declaration, struct_definitions);
  std::vector<bool> emitted(unit.classes.size(), false);
  for (const mir::ClassId id : unit.classes.Ids()) {
    AppendClassInDependencyOrder(unit, id, emitted, text);
  }
  return text;
}

// A foreign callable lands with the unit's own, not apart from it. Its C symbol
// is program-global and belongs to no scope (LRM 35.4, 35.7), but that is what
// C language linkage delivers wherever the declaration is written -- so writing
// it among the unit's declarations costs the symbol nothing and lets an
// export's entry point name the unit's classes the way every other body does.
auto RenderUnitCallables(const mir::CompilationUnit& unit) -> UnitCallableText {
  UnitCallableText text;
  for (const mir::CallableId id : unit.callables.Ids()) {
    const mir::CallableDecl& callable = unit.callables.Get(id);
    if (!callable.code.body.has_value()) {
      text.declarations +=
          RenderFreeCallableSignature(unit, id, callable) + ";\n";
      continue;
    }
    AppendSection(text.definitions, RenderFreeCallable(unit, id, callable));
  }
  return text;
}

}  // namespace lyra::backend::cpp
