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
auto RenderFieldList(
    const mir::CompilationUnit& unit,
    std::span<const mir::NamedField> named_fields,
    const base::Arena<mir::FieldDecl, mir::FieldId>& fields, std::size_t indent)
    -> std::string {
  std::string out;
  for (const mir::FieldId slot : fields.Ids()) {
    const std::string type = RenderTypeAsCpp(unit, fields.Get(slot).type);
    const std::string name = CppFieldName(named_fields, slot);
    out += RenderDeclaration(
        DeclaredCell{
            .owner = CellOwner::kObject,
            .text = CellText::kDefined,
            .immutable = false,
            .type = type,
            .name = name,
            .qualifier = {},
            .value = {}},
        indent);
  }
  return out;
}

// A class static property (LRM 8.9): one cell the type owns, established here
// and given its declared representation and value where whatever brings the
// class's owner up runs, never baked into the declaration.
auto RenderClassStaticProperty(
    const mir::CompilationUnit& unit, const mir::Class& s,
    mir::StaticPropertyId slot, std::size_t indent) -> std::string {
  const std::string type =
      RenderTypeAsCpp(unit, s.static_properties.Get(slot).type);
  const std::string name =
      CppStaticPropertyName(s.named_static_properties, slot);
  return RenderDeclaration(
      DeclaredCell{
          .owner = CellOwner::kType,
          .text = CellText::kDefined,
          .immutable = false,
          .type = type,
          .name = name,
          .qualifier = {},
          .value = {}},
      indent);
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
      VirtualPrefix(m), CppClassCallableName(unit, s, id),
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
      "auto {}::{}({}) -> {} {{\n", CppClassName(s, cls_id),
      CppClassCallableName(unit, s, id),
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
      "auto {}::{}({}) -> {} {{\n{}}}\n", CppClassName(s, cls_id),
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
auto RenderConstructor(
    const mir::CompilationUnit& unit, mir::ClassId cls_id, const mir::Class& s)
    -> UnitText {
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
  return UnitText{
      .signature = std::format(
          "{0}{1}({2});\n"
          "{0}static auto init({3}) -> void;\n",
          Indent(1), cpp_name, params, init_signature),
      .code = std::format(
          "{0}::{0}({1}){2} {{ init({3}); }}\n"
          "auto {0}::init({4}) -> void {{\n"
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

// A class-level static constant: the class body declares it, and the value is
// given apart from the class, beside the unit's own bodies. A runtime scope's
// generated-behavior record is one such constant; the constructor forwards its
// address to the base. What a referrer needs of one is its address, which the
// declaration alone settles, so the expression building the value is the
// declaring unit's own and never travels with the declaration. A constant that
// points into another only ever takes its address, which does not depend on
// that one's value having been given.
//
// What one is called is the caller's to say: the record every object carries
// takes a name off the class, because it is the one constant a class outside
// this unit spells, and the rest take one off the position they sit at, which
// nothing outside can count.
auto RenderStaticConstant(
    const mir::CompilationUnit& unit, mir::ClassId cls_id, const mir::Class& s,
    std::string_view name, const mir::StaticConstantDecl& c) -> UnitText {
  const ScopeView view = ScopeView::ForClassConstant(unit, cls_id, s, c.body);
  const std::string type = RenderTypeAsCpp(unit, c.type);
  const std::string owner = CppClassName(s, cls_id);
  const std::string value = RenderExpr(view, view.Expr(c.value));
  return UnitText{
      .signature = RenderDeclaration(
          DeclaredCell{
              .owner = CellOwner::kType,
              .text = CellText::kAnnounced,
              .immutable = true,
              .type = type,
              .name = name,
              .qualifier = {},
              .value = {}},
          1),
      .code = RenderDeclaration(
          DeclaredCell{
              .owner = CellOwner::kType,
              .text = CellText::kDefined,
              .immutable = true,
              .type = type,
              .name = name,
              .qualifier = owner,
              .value = value},
          0)};
}

auto RenderClass(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& s)
    -> UnitText;

// Appends a class and every intra-unit class it rests on, each before whatever
// rests on it. The interning walk sets the registry order, which may reach a
// resting class first, so this walker climbs what a class rests on before
// writing it and marks written classes in `emitted`.
//
// What the order is for is the code artifact, where a definition may name any
// class of the unit. A class a referrer may name takes a file of its own and is
// reached through the include its own file carries, so where it sits in this
// list decides nothing.
void AppendClassInDependencyOrder(
    const mir::CompilationUnit& unit, mir::ClassId id,
    std::vector<bool>& emitted, UnitClasses& text) {
  if (emitted[id.value]) return;
  emitted[id.value] = true;
  const mir::Class& cls = unit.GetClass(id);
  for (const mir::ClassRef& rests_on : mir::RestsOnDeclaredClasses(cls)) {
    if (const auto* intra = std::get_if<mir::IntraUnitClassRef>(&rests_on)) {
      AppendClassInDependencyOrder(unit, intra->class_id, emitted, text);
    }
  }
  const UnitText rendered = RenderClass(unit, id, cls);
  if (mir::IsPromised(unit, id)) {
    text.promised.push_back(
        PromisedClass{.id = id, .text = rendered.signature});
  } else {
    AppendSection(text.internal, rendered.signature);
  }
  AppendSection(text.definitions, rendered.code);
}

auto RenderClass(
    const mir::CompilationUnit& unit, mir::ClassId id, const mir::Class& s)
    -> UnitText {
  UnitText text;
  std::string& out = text.signature;
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
    const UnitText ctor = RenderConstructor(unit, id, s);
    AppendSection(out, ctor.signature);
    AppendSection(text.code, ctor.code);
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
        text.code, RenderClassCallableDef(unit, id, s, callable_id, callable));
  }
  AppendSection(out, callable_decls);

  // The class's runtime-callback adapters. Each renders as a static member
  // whose address decays to a plain function pointer for the runtime
  // callback table.
  std::string adapter_decls;
  for (const mir::AbiAdapterId adapter_id : s.abi_adapters.Ids()) {
    const mir::AbiAdapter& a = s.abi_adapters.Get(adapter_id);
    adapter_decls += RenderAbiAdapterDecl(unit, adapter_id, a);
    AppendSection(text.code, RenderAbiAdapterDef(unit, id, s, adapter_id, a));
  }
  AppendSection(out, adapter_decls);

  // The class's static constants (a tree node's generated-behavior record among
  // them), each declared here and given its value apart from the class, in the
  // order the class states them -- which is the order a constant built from
  // another one needs, since a file initializes its own constants in the order
  // it writes them.
  for (const mir::StaticConstantId constant_id : s.static_constants.Ids()) {
    const UnitText constant = RenderStaticConstant(
        unit, id, s, CppStaticConstantName(constant_id),
        s.static_constants.Get(constant_id));
    AppendSection(out, constant.signature);
    AppendSection(text.code, constant.code);
  }

  // The record every object of the class carries, and beside it the name the
  // allocation reads to hand an object that record. It takes a name off the
  // class rather than off a position, because it is the one constant a class
  // outside this unit spells.
  if (s.object_record.has_value()) {
    const UnitText record = RenderStaticConstant(
        unit, id, s, CppObjectRecordName(), *s.object_record);
    AppendSection(
        out, record.signature + Indent(1) +
                 std::format(
                     "static constexpr const {}* {} = &{};\n",
                     RenderTypeAsCpp(unit, s.object_record->type),
                     CppClassRecordHookName(), CppObjectRecordName()));
    AppendSection(text.code, record.code);
  }

  out += "};\n";
  return text;
}

// The language linkage a free callable is reached by. A foreign one takes C
// linkage, since its symbol is program-global (LRM 35.4) and is reached from
// outside this language; a callable of the unit's own namespace is reached by
// every referrer through the declaration it publishes, which the default
// linkage already serves.
auto RenderFreeCallableLinkage(const mir::CallableDecl& callable)
    -> std::string_view {
  return callable.foreign.has_value() ? R"(extern "C" )" : "";
}

// The signature of a function emitted at the unit's own scope: its language
// linkage, the symbol it is reached by, its named parameters, and its result
// type. Every use of this -- an import's declaration, an export entry point's
// definition, a package function's definition -- reads the one signature its
// code carries, so no two of them can disagree.
auto RenderFreeSignature(
    const mir::CompilationUnit& unit, std::string_view linkage,
    std::string_view symbol, const mir::CallableCode& code) -> std::string {
  std::vector<std::string> params;
  params.reserve(code.params.size());
  for (const mir::LocalId param : code.params) {
    params.push_back(RenderCallableParam(unit, code, param));
  }
  return std::format(
      "{}auto {}({}) -> {}", linkage, symbol, JoinCommaSeparated(params),
      RenderTypeAsCpp(unit, code.result_type));
}

auto RenderFreeCallableSignature(
    const mir::CompilationUnit& unit, mir::CallableId id,
    const mir::CallableDecl& callable) -> std::string {
  return RenderFreeSignature(
      unit, RenderFreeCallableLinkage(callable), CppUnitCallableName(unit, id),
      callable.code);
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

auto RenderUnitForwardDeclarations(const mir::CompilationUnit& unit)
    -> UnitText {
  UnitText text;
  for (const mir::ClassId id : unit.classes.Ids()) {
    (mir::IsPromised(unit, id) ? text.signature : text.code) +=
        std::format("class {};\n", CppClassName(unit.GetClass(id), id));
  }
  // A struct is a scope a lowering promoted out of some body, so nothing
  // outside this unit reaches one.
  for (const mir::StructId id : unit.structs.Ids()) {
    text.code += std::format("struct {};\n", CppStructName(id));
  }
  return text;
}

auto RenderUnitClasses(const mir::CompilationUnit& unit) -> UnitClasses {
  UnitClasses text;
  for (const mir::StructId id : unit.structs.Ids()) {
    text.internal += RenderStruct(unit, id, unit.GetStruct(id));
  }
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
auto RenderUnitCallables(const mir::CompilationUnit& unit) -> UnitText {
  UnitText text;
  // Every one is declared before any class of the unit, because a class's body
  // may call one -- a type-associated function the compiler synthesized is
  // reached from wherever the source wrote the construct that needs it -- and
  // the definitions land after the classes so an export's entry point can name
  // them. Only a callable this program defines has a definition to land.
  for (const mir::CallableId id : unit.callables.Ids()) {
    const mir::CallableDecl& callable = unit.callables.Get(id);
    text.signature += RenderFreeCallableSignature(unit, id, callable) + ";\n";
    if (callable.code.body.has_value()) {
      AppendSection(text.code, RenderFreeCallable(unit, id, callable));
    }
  }
  return text;
}

// A package variable is one program-global observable cell (LRM 26.2), so a
// referrer's own artifact carries none of the storage of the unit it reached. A
// unit rooted in a design element declares none at all: its storage is
// per-instance.
auto RenderUnitStaticVariables(const mir::CompilationUnit& unit) -> UnitText {
  UnitText text;
  for (const mir::StaticVariableId id : unit.static_variables.Ids()) {
    const std::string type =
        RenderTypeAsCpp(unit, unit.static_variables.Get(id).type);
    const std::string name =
        CppStaticVariableName(unit.named_static_variables, id);
    text.signature += RenderDeclaration(
        DeclaredCell{
            .owner = CellOwner::kNamespace,
            .text = CellText::kAnnounced,
            .immutable = false,
            .type = type,
            .name = name,
            .qualifier = {},
            .value = {}},
        0);
    text.code += RenderDeclaration(
        DeclaredCell{
            .owner = CellOwner::kNamespace,
            .text = CellText::kDefined,
            .immutable = false,
            .type = type,
            .name = name,
            .qualifier = {},
            .value = {}},
        0);
  }
  return text;
}

auto RenderExternalObjectDeclarations(const mir::CompilationUnit& unit)
    -> std::string {
  std::string out;
  for (const mir::ExternalUnitObjectId id : unit.external_unit_objects.Ids()) {
    const mir::ExternalUnitObject& object = unit.external_unit_objects.Get(id);
    out += std::format(
        "namespace {} {{\nclass {};\n}}\n", UnitNamespaceOf(object.unit_name),
        ToCppName(object.class_name));
  }
  return out;
}

// The merge rule has to be one that holds a definition nothing in this language
// references, because such a symbol is reached only from outside it (LRM 35.4,
// 35.7): a rule free to drop an unreferenced definition drops it from every
// artifact at once and the program fails to link.
auto RenderForeignScopeSymbols(const mir::CompilationUnit& unit)
    -> std::string {
  std::string out;
  for (const mir::ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    std::string definition = std::format(
        "{} {{\n", RenderFreeSignature(
                       unit, R"(extern "C" [[gnu::weak]] )",
                       CppForeignSymbolName(entry.linkage.foreign_name),
                       entry.definition));
    definition += RenderBlockStatements(
        ScopeView::ForNamespace(unit, entry.definition), 1);
    definition += "}\n";
    AppendSection(out, definition);
  }
  return out;
}

}  // namespace lyra::backend::cpp
