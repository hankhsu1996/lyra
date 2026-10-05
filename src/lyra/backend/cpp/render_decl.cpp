#include "lyra/backend/cpp/render_decl.hpp"

#include <span>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_constant_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/struct_decl.hpp"
#include "lyra/mir/struct_id.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/value_build.hpp"

namespace lyra::backend::cpp {

void WriteParameters(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    std::span<const mir::LocalId> params, TargetText& out) {
  WriteSeparated(out, params, ", ", [&](mir::LocalId param) {
    Write(
        out, CppType(unit, code.locals.Get(param).type), " ",
        CppLocalName(code.named_locals, param));
  });
}

namespace {

// The body's first line, binding its receiver to the object it runs against,
// `C* self = this;`: MIR reaches the object through that binding in every body,
// and what C++ holds it as is `from`.
template <typename... From>
void WriteReceiverBinding(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    TargetText& out, const From&... from) {
  const mir::LocalId receiver = *code.receiver;
  out.OpenLine();
  Write(
      out, CppType(unit, code.locals.Get(receiver).type), " ",
      CppLocalName(code.named_locals, receiver), " = ", from..., ";\n");
}

// Whether a body receives the object it runs against as a value rather than
// through a pointer, which is how a struct's method receives its struct: it
// runs against a copy, so it cannot change the object it is called on.
auto ReceivesAValue(
    const mir::CompilationUnit& unit, const mir::CallableCode& code) -> bool {
  return code.TakesReceiver() &&
         !unit.types.Get(code.locals.Get(*code.receiver).type)
              .Is<mir::PointerType>();
}

// `static ` for a member function entered on no object (LRM 8.10), or handed
// one it narrows itself.
auto StaticPrefix(const mir::CallableCode& code) -> std::string_view {
  return code.TakesReceiver() ? "" : "static ";
}

// What a member function's declaration and its definition share after its
// name, `(params) -> R`, with `const` for a body receiving its object as a
// value.
void WriteMemberSignatureTail(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    TargetText& out) {
  out += "(";
  WriteParameters(unit, code, code.ParamsAfterReceiver(), out);
  Write(
      out, ")", ReceivesAValue(unit, code) ? " const" : "", " -> ",
      CppType(unit, code.result_type));
}

// The definition of a member function -- a class's method (LRM 8.6), static
// method (LRM 8.10), process or lifecycle body, the entry a scope answers a
// foreign caller with (LRM 35.5.3), or a struct's method -- written
// outside its type so the body can use any type of the unit as a complete one.
// The body starts by binding its receiver, because MIR reaches the object
// through a parameter like any other: `C* self = this;` for an object reached
// through a pointer, `S self = *this;` for a struct received as a value.
template <typename Owner, typename Member>
void RenderMemberFunctionDef(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    const Owner& owner, const Member& member, const mir::CallableCode& code,
    TargetText& out) {
  Write(out, "auto ", owner, "::", member);
  WriteMemberSignatureTail(unit, code, out);
  out += " ";
  WriteBody(out, [&] {
    if (code.TakesReceiver()) {
      WriteReceiverBinding(
          unit, code, out, ReceivesAValue(unit, code) ? "*this" : "this");
    }
    RenderBlockStatements(ScopeView::ForCode(unit, code, refusals), out);
  });
  out += "\n";
}

// Each field is declared as `Type name{};` and nothing more. Its initial value,
// and any other setup such as how a net resolves its drivers, is done by
// statements in the constructor body.
void RenderFieldList(
    const mir::CompilationUnit& unit,
    std::span<const mir::NamedField> named_fields,
    const base::Arena<mir::FieldDecl, mir::FieldId>& fields, TargetText& out) {
  for (const mir::FieldId slot : fields.Ids()) {
    WriteDeclaration(
        out, VariableDeclaration{
                 .form = VariableForm::kNonStaticDataMember,
                 .is_const = false,
                 .type = CppType(unit, fields.Get(slot).type),
                 .name = CppFieldName(named_fields, slot)});
  }
}

// A class's static properties (LRM 8.9), one cell per class. Like a field, the
// declaration holds no value; code run when the class's owner is set up gives
// it one.
void RenderClassStaticProperties(
    const mir::CompilationUnit& unit, const mir::Class& s, TargetText& out) {
  for (const mir::StaticPropertyId slot : s.static_properties.Ids()) {
    WriteDeclaration(
        out,
        VariableDeclaration{
            .form = VariableForm::kInlineStaticDataMember,
            .is_const = false,
            .type = CppType(unit, s.static_properties.Get(slot).type),
            .name = CppStaticPropertyName(s.named_static_properties, slot)});
  }
}

// `virtual ` for a method that declares a new virtual. An override is marked
// with `override` after the signature instead.
auto VirtualPrefix(const mir::CallableDecl& m) -> std::string_view {
  return mir::IntroducesSlot(m.virtual_dispatch) ? "virtual " : "";
}

// ` override` for a method that overrides a virtual, so the C++ compiler
// rejects it if no base virtual has that name and signature.
auto OverrideSuffix(const mir::CallableDecl& m) -> std::string_view {
  if (!m.virtual_dispatch.has_value()) return "";
  return mir::IntroducesSlot(m.virtual_dispatch) ? "" : " override";
}

void RenderClassCallableDecl(
    const mir::CompilationUnit& unit, const mir::Class& s, mir::CallableId id,
    const mir::CallableDecl& m, TargetText& out) {
  const mir::CallableCode& code = m.code;
  out.OpenLine();
  out += StaticPrefix(code);
  out += VirtualPrefix(m);
  Write(out, "auto ", CppClassCallableName(unit, s, id));
  WriteMemberSignatureTail(unit, code, out);
  out += OverrideSuffix(m);
  // `= 0` also makes C++ treat the class as abstract.
  std::visit(
      Overloaded{
          [](mir::DefinedHere) {}, [&](mir::LeftAbstract) { out += " = 0"; },
          [](mir::DefinedByForeignCode) {
            throw InternalError(
                "RenderClassCallableDecl: a foreign function belongs to no "
                "class (LRM 35.4) -- please report this as a bug");
          }},
      mir::FormOf(m));
  out += ";\n";
}

// A class function's definition. A pure virtual has none.
void RenderClassCallableDef(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::ClassId cls_id, const mir::Class& s, mir::CallableId id,
    const mir::CallableDecl& m, TargetText& out) {
  if (!std::holds_alternative<mir::DefinedHere>(mir::FormOf(m))) return;
  RenderMemberFunctionDef(
      unit, refusals, CppClassName(s, cls_id),
      CppClassCallableName(unit, s, id), m.code, out);
}

// The constructor, declared in the class and defined in the code file, outside
// the class so the body can build a child whose own body uses this class:
//
//   C::C(args) : Base(base_args) { C* self = this; body }
//
// The base goes in the initializer list because it has to exist before any
// statement runs. Everything else, property initialization in LRM 8.7 order
// included, is a statement of the body, which reaches the object through its
// receiver as every other body does.
void RenderConstructor(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::ClassId cls_id, const mir::Class& s, const mir::ConstructorDecl& ctor,
    TargetText& signature, TargetText& code) {
  const mir::CallableCode& ctor_code = ctor.code;
  if (!ctor_code.receiver.has_value()) {
    throw InternalError(
        "RenderConstructor: a constructor is entered on the object it builds, "
        "yet states no receiver -- please report this as a bug");
  }
  const ScopeView scope_view = ScopeView::ForCode(unit, ctor_code, refusals);
  const CppName cpp_name = CppClassName(s, cls_id);
  const std::span<const mir::LocalId> formals = ctor_code.ParamsAfterReceiver();

  signature.OpenLine();
  Write(signature, cpp_name, "(");
  WriteParameters(unit, ctor_code, formals, signature);
  signature += ");\n";

  Write(code, cpp_name, "::", cpp_name, "(");
  WriteParameters(unit, ctor_code, formals, code);
  code += ")";
  if (s.base.has_value()) {
    Write(code, " : ", CppClassRef(unit, *s.base), "(");
    WriteCommaSeparated(scope_view, code, ctor.base_args);
    code += ")";
  }
  code += " ";
  WriteBody(code, [&] {
    WriteReceiverBinding(unit, ctor_code, code, "this");
    RenderBlockStatements(scope_view, code);
  });
  code += "\n";
}

// A constant class `s` holds, announced in the class as `static const T name;`
// and defined in the code file as `const T Class::name = value;`. Another unit
// only ever needs its address, which the announcement is enough for, so the
// value stays in this unit. The caller passes the name, since the definition's
// is fixed and the others are positions.
void RenderClassConstant(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::ClassId id, const mir::Class& s, mir::TypeId type, const CppName& name,
    const mir::ValueBuild& build, TargetText& announced, TargetText& defined) {
  WriteDeclaration(
      announced, VariableDeclaration{
                     .form = VariableForm::kStaticDataMemberDeclaration,
                     .is_const = true,
                     .type = CppType(unit, type),
                     .name = name});
  const ScopeView view = ScopeView::ForConstant(unit, build.body, refusals);
  WriteDeclaration(
      defined,
      VariableDeclaration{
          .form = VariableForm::kStaticDataMemberDefinition,
          .is_const = true,
          .type = CppType(unit, type),
          .name = name,
          .qualifier = CppClassName(s, id)},
      [&](TargetText& value) { Write(view, value, build.value); });
}

void RenderClass(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::ClassId id, const mir::Class& s, TargetText& signature,
    TargetText& code);

// Writes a class after every class of this unit it derives from, which the
// unit's class list does not guarantee, marking written classes in `emitted`.
// The order matters for the classes written into the code file; a class
// another unit may name has its own file, which includes its bases' files, so
// its position here does not matter.
void AppendClassInDependencyOrder(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::ClassId id, std::vector<bool>& emitted, UnitClasses& text) {
  if (emitted[id.value]) return;
  emitted[id.value] = true;
  const mir::Class& cls = unit.GetClass(id);
  for (const mir::DeclaredClassRef& rests_on :
       mir::RestsOnDeclaredClasses(cls)) {
    std::visit(
        Overloaded{
            [&](const mir::IntraUnitClassRef& intra) {
              AppendClassInDependencyOrder(
                  unit, refusals, intra.class_id, emitted, text);
            },
            // Another unit's class is declared in that unit's own header,
            // which the file declaring this one includes.
            [](const mir::CrossUnitClassRef&) {}},
        rests_on);
  }
  const TargetText::Section defined(text.definitions);
  if (mir::IsPublished(unit, id)) {
    PublishedClass published{.id = id, .text = TargetText{}};
    RenderClass(unit, refusals, id, cls, published.text, text.definitions);
    text.published.push_back(std::move(published));
    return;
  }
  const TargetText::Section declared(text.internal);
  RenderClass(unit, refusals, id, cls, text.internal, text.definitions);
}

void RenderClass(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::ClassId id, const mir::Class& s, TargetText& signature,
    TargetText& code) {
  TargetText& out = signature;
  Write(out, "class ", CppClassName(s, id));
  if (s.is_final) {
    out += " final";
  }
  // The base class first (LRM 8.13), then each interface class (LRM 8.26), all
  // as C++ bases. An interface class is a virtual base, because one reached
  // along several paths is one type the object is, not several (LRM 8.26.6.3):
  // there is one copy of it to view the object as.
  struct Base {
    mir::ClassRef of;
    bool is_interface_class;
  };
  std::vector<Base> bases;
  if (s.base.has_value()) {
    bases.push_back(Base{.of = *s.base, .is_interface_class = false});
  }
  for (const mir::DeclaredClassRef& implemented : s.implements) {
    bases.push_back(
        Base{.of = mir::AsClassRef(implemented), .is_interface_class = true});
  }
  if (!bases.empty()) {
    out += " : ";
    WriteSeparated(out, bases, ", ", [&](const Base& base) {
      Write(
          out, base.is_interface_class ? "public virtual " : "public ",
          CppClassRef(unit, base.of));
    });
  }
  out += " {\n";
  out += " public:\n";
  out.Indent();

  if (s.constructor.has_value()) {
    const TargetText::Section declared(out);
    const TargetText::Section defined(code);
    RenderConstructor(unit, refusals, id, s, *s.constructor, out, code);
  }

  // The destructor is virtual, because whoever ends a value holds it as the
  // class every value extends, or as an interface class. It is defined in the
  // code file so it is the class's key function, and the class's table and
  // type description are emitted there once rather than in every file that
  // casts to it.
  {
    const TargetText::Section ended(out);
    out.OpenLine();
    if (s.base.has_value()) {
      Write(out, "~", CppClassName(s, id), "() override;\n");
    } else {
      Write(out, "virtual ~", CppClassName(s, id), "();\n");
    }
    const TargetText::Section defined(code);
    code.OpenLine();
    Write(
        code, CppClassName(s, id), "::~", CppClassName(s, id),
        "() = default;\n");
  }

  // Members are public so cross-unit references can reach them directly.
  {
    const TargetText::Section fields(out);
    RenderFieldList(unit, s.named_fields, s.fields, out);
  }

  {
    const TargetText::Section properties(out);
    RenderClassStaticProperties(unit, s, out);
  }

  // Every function the class owns except the constructor, written above. A
  // pure virtual writes no definition, which leaves its section empty.
  {
    const TargetText::Section declared(out);
    for (const mir::CallableId callable_id : s.callables.Ids()) {
      const mir::CallableDecl& callable = s.callables.Get(callable_id);
      RenderClassCallableDecl(unit, s, callable_id, callable, out);
      const TargetText::Section defined(code);
      RenderClassCallableDef(
          unit, refusals, id, s, callable_id, callable, code);
    }
  }

  // The constants the class holds and then its definition, which names them:
  // each defined in the code file beside the bodies it names.
  {
    const TargetText::Section constants(out);
    const TargetText::Section defined(code);
    for (const mir::ClassConstantId constant : s.constants.Ids()) {
      const mir::ClassConstantDecl& decl = s.constants.Get(constant);
      RenderClassConstant(
          unit, refusals, id, s, decl.type, CppClassConstantName(constant),
          decl.initializer, out, code);
    }
    RenderClassConstant(
        unit, refusals, id, s, mir::ClassDefinitionType(unit.types),
        CppDefinitionName(), s.object_definition_initializer, out, code);
  }

  out.Outdent();
  out += "};\n";
}

// `extern "C" ` for a body reached by its DPI-C linkage name, whose symbol is
// global and called from C (LRM 35.4); nothing for one reached any other way.
auto RenderFreeCallableLinkage(const mir::NamespaceReach& reach)
    -> std::string_view {
  return std::visit(
      Overloaded{
          [](const mir::ReachedByLinkageName&) -> std::string_view {
            return R"(extern "C" )";
          },
          [](const mir::ReachedByName&) -> std::string_view { return ""; },
          [](const mir::ReachedByMintedEntry&) -> std::string_view {
            return "";
          },
          [](const mir::ReachedByPosition&) -> std::string_view { return ""; }},
      reach);
}

// The signature of a namespace function: `linkage auto name(params) -> R`. A
// DPI-C import's declaration, an export's entry point, and a package function
// all write it from here, so they cannot disagree.
void RenderFreeSignature(
    const mir::CompilationUnit& unit, std::string_view linkage,
    const CppName& symbol, const mir::CallableCode& code, TargetText& out) {
  Write(out, linkage, "auto ", symbol, "(");
  WriteParameters(unit, code, code.params, out);
  Write(out, ") -> ", CppType(unit, code.result_type));
}

void RenderFreeCallableSignature(
    const mir::CompilationUnit& unit, mir::CallableId id,
    const mir::CallableDecl& callable, TargetText& out) {
  RenderFreeSignature(
      unit, RenderFreeCallableLinkage(mir::NamespaceReachOf(unit, id)),
      CppUnitCallableName(unit, id), callable.code, out);
}

// A function of the unit's namespace, written as a free function. It has no
// receiver and no class, and every name in its body resolves in the unit's
// namespace, where it is written.
void RenderFreeCallable(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::CallableId id, const mir::CallableDecl& callable, TargetText& out) {
  RenderFreeCallableSignature(unit, id, callable, out);
  out += " ";
  WriteBody(out, [&] {
    RenderBlockStatements(
        ScopeView::ForCode(unit, callable.code, refusals), out);
  });
  out += "\n";
}

// A struct the unit declares: the library's product of its components, as a
// struct of its own so that two structs with the same components stay two
// types (LRM 6.22.1), whose member functions are the struct's methods --
// declared in it, and defined apart from it once every struct of the unit is
// declared.
void RenderStruct(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::StructId id, TargetText& declared, TargetText& defined) {
  const mir::StructDecl& decl = unit.GetStruct(id);
  const CppName name = CppStructName(decl);
  const CppTupleComponents components{.unit = &unit, .of = decl.elements};
  const MintedWord base = CppTupleBaseName();
  Write(declared, "struct ", name, " : ", components, " {\n");
  declared.Indent();
  declared.OpenLine();
  Write(declared, "using ", base, " = ", components, ";\n");
  declared.OpenLine();
  Write(declared, "using ", base, "::", base, ";\n");
  for (const mir::StructMethod& method : decl.methods) {
    const VerbatimName member = CppStructMethodName(method.answers);
    declared.OpenLine();
    Write(declared, StaticPrefix(method.code), "auto ", member);
    WriteMemberSignatureTail(unit, method.code, declared);
    declared += ";\n";
    RenderMemberFunctionDef(unit, refusals, name, member, method.code, defined);
  }
  declared.Outdent();
  declared += "};\n";
}

}  // namespace

auto RenderUnitForwardDeclarations(const mir::CompilationUnit& unit)
    -> UnitText {
  UnitText text;
  for (const mir::ClassId id : unit.classes.Ids()) {
    const mir::Class& cls = unit.GetClass(id);
    const CppName name = CppClassName(cls, id);
    TargetText& out = mir::IsPublished(unit, id) ? text.signature : text.code;
    Write(out, "class ", name, ";\n");
    for (const std::string& alias : cls.aliases) {
      Write(out, "using ", ToCppName(alias), " = ", name, ";\n");
    }
  }
  return text;
}

auto RenderUnitClasses(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> UnitClasses {
  UnitClasses text;
  std::vector<bool> emitted(unit.classes.size(), false);
  for (const mir::ClassId id : unit.classes.Ids()) {
    AppendClassInDependencyOrder(unit, refusals, id, emitted, text);
  }
  return text;
}

auto RenderUnitClosures(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> UnitClosures {
  UnitClosures text;
  for (const mir::ClosureId id : unit.closures.Ids()) {
    const mir::ClosureDecl& decl = unit.GetClosure(id);
    const mir::CallableCode& code = decl.invoke;
    const MintedName name = CppClosureName(id);
    const bool started =
        unit.types.Get(code.result_type).Is<mir::CoroutineType>();
    if (started && !code.params.empty()) {
      throw InternalError(
          "RenderUnitClosures: a closure started as it is built is called "
          "with nothing, yet its body takes per-invocation parameters -- "
          "please report this as a bug");
    }
    // What follows the function's name or qualifier, the same in the
    // declaration and the definition.
    const auto write_function = [&](TargetText& out) {
      if (started) {
        Write(
            out, CppClosureStartName(), "(", name, " ", CppStartedClosureName(),
            ") -> ", CppType(unit, code.result_type));
        return;
      }
      out += "operator()(";
      WriteParameters(unit, code, code.params, out);
      Write(out, ") const -> ", CppType(unit, code.result_type));
    };

    {
      TargetText& out = text.declarations;
      const TargetText::Section declared(out);
      Write(out, "struct ", name, " {\n");
      out.Indent();
      for (const mir::FieldId field : decl.field_order) {
        WriteDeclaration(
            out, VariableDeclaration{
                     .form = VariableForm::kNonStaticDataMember,
                     .is_const = false,
                     .type = CppType(unit, decl.fields.Get(field).type),
                     .name = CppClosureCaptureName(field)});
      }
      out.OpenLine();
      out += started ? "static auto " : "auto ";
      write_function(out);
      out += ";\n";
      out.Outdent();
      out += "};\n";
    }

    TargetText& out = text.definitions;
    const TargetText::Section defined(out);
    Write(out, "auto ", name, "::");
    write_function(out);
    out += " ";
    if (!code.receiver.has_value()) {
      throw InternalError(
          "RenderUnitClosures: a closure's body reads its captures through the "
          "closure, yet states no receiver -- please report this as a bug");
    }
    WriteBody(out, [&] {
      if (started) {
        WriteReceiverBinding(unit, code, out, "&", CppStartedClosureName());
      } else {
        WriteReceiverBinding(unit, code, out, "this");
      }
      RenderBlockStatements(ScopeView::ForCode(unit, code, refusals), out);
    });
    out += "\n";
  }
  return text;
}

// A DPI-C function is written inside the unit's namespace like the others. Its
// symbol is still global (LRM 35.4, 35.7), since `extern "C"` ignores the
// namespace, and inside it an export's entry point can name the unit's classes
// the way every other body does.
auto RenderUnitCallables(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> UnitText {
  UnitText text;
  // All are declared before any class, because a class's body may call one,
  // and defined after the classes, because an export's entry point uses them.
  // An import has no body here; the user's C code defines it.
  for (const mir::CallableId id : unit.callables.Ids()) {
    const mir::CallableDecl& callable = unit.callables.Get(id);
    RenderFreeCallableSignature(unit, id, callable, text.signature);
    text.signature += ";\n";
    if (std::holds_alternative<mir::DefinedHere>(mir::FormOf(callable))) {
      const TargetText::Section defined(text.code);
      RenderFreeCallable(unit, refusals, id, callable, text.code);
    }
  }
  return text;
}

auto RenderUnitStructs(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> UnitText {
  // Another unit names a struct by the declaration it answers to, so it is
  // declared where that unit reads, in the unit's types namespace, and its
  // methods are defined in the same namespace in the code file.
  TargetText declared;
  TargetText definitions;
  for (const mir::StructId id : unit.structs.Ids()) {
    const TargetText::Section declared_section(declared);
    const TargetText::Section defined_section(definitions);
    RenderStruct(unit, refusals, id, declared, definitions);
  }
  UnitText text;
  AppendSectionInNamespace(text.signature, CppStructTypesNamespace(), declared);
  AppendSectionInNamespace(text.code, CppStructTypesNamespace(), definitions);
  return text;
}

// Package variables (LRM 26.2), one cell for the whole program: declared
// `extern` in the header and defined once in this unit's code file. A unit of a
// design element has none, since its storage is per instance.
auto RenderUnitStaticVariables(const mir::CompilationUnit& unit) -> UnitText {
  UnitText text;
  for (const mir::StaticVariableId id : unit.static_variables.Ids()) {
    const CppType type(unit, unit.static_variables.Get(id).type);
    const CppName name = CppStaticVariableName(unit.named_static_variables, id);
    WriteDeclaration(
        text.signature, VariableDeclaration{
                            .form = VariableForm::kExternDeclaration,
                            .is_const = false,
                            .type = type,
                            .name = name});
    WriteDeclaration(
        text.code, VariableDeclaration{
                       .form = VariableForm::kNamespaceScopeDefinition,
                       .is_const = false,
                       .type = type,
                       .name = name});
  }
  return text;
}

// Every unit declaring the same foreign scope writes the same definition, so it
// is `[[gnu::weak]]` for the linker to keep one. It is not `inline`: only C
// code calls it (LRM 35.4, 35.7), and the compiler may drop an inline
// definition nothing in C++ uses, which would fail the link.
void RenderForeignScopeSymbols(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    TargetText& out) {
  for (const mir::ForeignScopeEntry& entry : unit.foreign_scope_entries) {
    const TargetText::Section defined(out);
    RenderFreeSignature(
        unit, R"(extern "C" [[gnu::weak]] )",
        CppForeignSymbolName(entry.linkage.foreign_name), entry.definition,
        out);
    out += " ";
    WriteBody(out, [&] {
      RenderBlockStatements(
          ScopeView::ForCode(unit, entry.definition, refusals), out);
    });
    out += "\n";
  }
}

}  // namespace lyra::backend::cpp
