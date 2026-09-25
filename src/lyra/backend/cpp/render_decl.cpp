#include "lyra/backend/cpp/render_decl.hpp"

#include <span>
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
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/callable.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/closure_decl.hpp"
#include "lyra/mir/closure_id.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/type.hpp"

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

// Each field is declared as `Type name{};` and nothing more. Its initial value,
// and any other setup such as how a net resolves its drivers, is done by
// statements in the constructor body.
void RenderFieldList(
    const mir::CompilationUnit& unit,
    std::span<const mir::NamedField> named_fields,
    const base::Arena<mir::FieldDecl, mir::FieldId>& fields, TargetText& out) {
  for (const mir::FieldId slot : fields.Ids()) {
    WriteDeclaration(
        out, DeclaredCell{
                 .owner = CellOwner::kObject,
                 .text = CellText::kDefined,
                 .immutable = false,
                 .type = CppType(unit, fields.Get(slot).type),
                 .name = CppFieldName(named_fields, slot),
                 .qualifier = {}});
  }
}

// A class's static properties (LRM 8.9), one cell per class. Like a field, the
// declaration holds no value; code run when the class's owner is set up gives
// it one.
void RenderClassStaticProperties(
    const mir::CompilationUnit& unit, const mir::Class& s, TargetText& out) {
  for (const mir::StaticPropertyId slot : s.static_properties.Ids()) {
    WriteDeclaration(
        out, DeclaredCell{
                 .owner = CellOwner::kType,
                 .text = CellText::kDefined,
                 .immutable = false,
                 .type = CppType(unit, s.static_properties.Get(slot).type),
                 .name = CppStaticPropertyName(s.named_static_properties, slot),
                 .qualifier = {}});
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
  // Entered on no object (LRM 8.10).
  if (!code.receiver.has_value()) out += "static ";
  out += VirtualPrefix(m);
  Write(out, "auto ", CppClassCallableName(unit, s, id), "(");
  WriteParameters(unit, code, code.ParamsAfterReceiver(), out);
  Write(out, ") -> ", CppType(unit, code.result_type));
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

// The definition of a class function -- a method (LRM 8.6), a static method
// (LRM 8.10), a process, or a lifecycle body -- written outside the class so
// its body can use any class of the unit as a complete type. A method's body
// starts with `Cls* self = this;`, because MIR reaches the receiver through a
// parameter like any other. A pure virtual has no definition.
void RenderClassCallableDef(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::ClassId cls_id, const mir::Class& s, mir::CallableId id,
    const mir::CallableDecl& m, TargetText& out) {
  const mir::CallableCode& code = m.code;
  if (!std::holds_alternative<mir::DefinedHere>(mir::FormOf(m))) return;
  Write(
      out, "auto ", CppClassName(s, cls_id),
      "::", CppClassCallableName(unit, s, id), "(");
  WriteParameters(unit, code, code.ParamsAfterReceiver(), out);
  Write(out, ") -> ", CppType(unit, code.result_type), " ");
  WriteBody(out, [&] {
    if (code.receiver.has_value()) {
      WriteReceiverBinding(unit, code, out, "this");
    }
    RenderBlockStatements(ScopeView::ForCode(unit, code, refusals), out);
  });
  out += "\n";
}

// A runtime callback, declared `static` so `&C::sv_adapter_0` is a plain
// function pointer the runtime's tables can hold. Its receiver is an ordinary
// first parameter.
void RenderAbiAdapterDecl(
    const mir::CompilationUnit& unit, mir::AbiAdapterId id,
    const mir::AbiAdapter& a, TargetText& out) {
  out.OpenLine();
  Write(out, "static auto ", CppAbiAdapterName(id), "(");
  WriteParameters(unit, a.code, a.code.params, out);
  Write(out, ") -> ", CppType(unit, a.code.result_type), ";\n");
}

void RenderAbiAdapterDef(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::ClassId cls_id, const mir::Class& s, mir::AbiAdapterId id,
    const mir::AbiAdapter& a, TargetText& out) {
  Write(
      out, "auto ", CppClassName(s, cls_id), "::", CppAbiAdapterName(id), "(");
  WriteParameters(unit, a.code, a.code.params, out);
  Write(out, ") -> ", CppType(unit, a.code.result_type), " ");
  WriteBody(out, [&] {
    RenderBlockStatements(ScopeView::ForCode(unit, a.code, refusals), out);
  });
  out += "\n";
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

// A struct the compiler made to hold a scope's variables: fields only, with no
// base, constructor, or methods.
void RenderStruct(
    const mir::CompilationUnit& unit, mir::StructId id,
    const mir::StructDecl& decl, TargetText& out) {
  Write(out, "struct ", CppStructName(id), " {\n");
  out.Indent();
  RenderFieldList(unit, {}, decl.fields, out);
  out.Outdent();
  out += "};\n";
}

// A class's static constant: declared `static const T name;` in the class and
// defined `const T C::name = value;` in the code file. Another unit only ever
// needs its address, which the declaration is enough for, so the value stays
// in this unit. A constant built from another one only takes that one's
// address, so their order of initialization does not matter. The caller passes
// the name, since the object record's name is fixed and the others are
// positions.
void RenderStaticConstant(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals,
    mir::ClassId cls_id, const mir::Class& s, const CppName& name,
    const mir::StaticConstantDecl& c, TargetText& signature, TargetText& code) {
  const ScopeView view = ScopeView::ForConstant(unit, c.body, refusals);
  const CppType type(unit, c.type);
  const CppName owner = CppClassName(s, cls_id);
  WriteDeclaration(
      signature, DeclaredCell{
                     .owner = CellOwner::kType,
                     .text = CellText::kAnnounced,
                     .immutable = true,
                     .type = type,
                     .name = name,
                     .qualifier = {}});
  WriteDeclaration(
      code,
      DeclaredCell{
          .owner = CellOwner::kType,
          .text = CellText::kDefined,
          .immutable = true,
          .type = type,
          .name = name,
          .qualifier = owner},
      [&](TargetText& value) { Write(view, value, c.value); });
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
  for (const mir::ClassRef& rests_on : mir::RestsOnDeclaredClasses(cls)) {
    if (const auto* intra = std::get_if<mir::IntraUnitClassRef>(&rests_on)) {
      AppendClassInDependencyOrder(
          unit, refusals, intra->class_id, emitted, text);
    }
  }
  const TargetText::Section defined(text.definitions);
  if (mir::IsPromised(unit, id)) {
    PromisedClass promised{.id = id, .text = TargetText{}};
    RenderClass(unit, refusals, id, cls, promised.text, text.definitions);
    text.promised.push_back(std::move(promised));
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
  // as C++ bases. An interface class holds no storage, so this multiple
  // inheritance never duplicates a base's fields.
  std::vector<mir::ClassRef> bases;
  if (s.base.has_value()) {
    bases.push_back(*s.base);
  }
  bases.insert(bases.end(), s.implements.begin(), s.implements.end());
  if (!bases.empty()) {
    out += " : public ";
    WriteSeparated(out, bases, ", public ", [&](const mir::ClassRef& base) {
      Write(out, CppClassRef(unit, base));
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

  {
    const TargetText::Section declared(out);
    for (const mir::AbiAdapterId adapter_id : s.abi_adapters.Ids()) {
      const mir::AbiAdapter& a = s.abi_adapters.Get(adapter_id);
      RenderAbiAdapterDecl(unit, adapter_id, a, out);
      const TargetText::Section defined(code);
      RenderAbiAdapterDef(unit, refusals, id, s, adapter_id, a, code);
    }
  }

  // Static constants, in the class's order: C++ initializes the constants of
  // one file in the order they are written, and a constant built from another
  // comes after it in that order.
  for (const mir::StaticConstantId constant_id : s.static_constants.Ids()) {
    const TargetText::Section declared(out);
    const TargetText::Section defined(code);
    RenderStaticConstant(
        unit, refusals, id, s, CppStaticConstantName(constant_id),
        s.static_constants.Get(constant_id), out, code);
  }

  // The object record, and the static member the runtime's allocation reads
  // to find it.
  if (s.object_record.has_value()) {
    const TargetText::Section declared(out);
    const TargetText::Section defined(code);
    RenderStaticConstant(
        unit, refusals, id, s, CppObjectRecordName(), *s.object_record, out,
        code);
    out.OpenLine();
    Write(
        out, "static constexpr const ", CppType(unit, s.object_record->type),
        "* ", CppClassRecordHookName(), " = &", CppObjectRecordName(), ";\n");
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

}  // namespace

auto RenderUnitForwardDeclarations(const mir::CompilationUnit& unit)
    -> UnitText {
  UnitText text;
  for (const mir::ClassId id : unit.classes.Ids()) {
    TargetText& out = mir::IsPromised(unit, id) ? text.signature : text.code;
    Write(out, "class ", CppClassName(unit.GetClass(id), id), ";\n");
  }
  // A struct holds a scope's variables inside one function, so no other unit
  // ever names it.
  for (const mir::StructId id : unit.structs.Ids()) {
    Write(text.code, "struct ", CppStructName(id), ";\n");
  }
  return text;
}

auto RenderUnitClasses(
    const mir::CompilationUnit& unit, diag::DiagnosticSink& refusals)
    -> UnitClasses {
  UnitClasses text;
  for (const mir::StructId id : unit.structs.Ids()) {
    RenderStruct(unit, id, unit.GetStruct(id), text.internal);
  }
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
            out, DeclaredCell{
                     .owner = CellOwner::kObject,
                     .text = CellText::kDefined,
                     .immutable = false,
                     .type = CppType(unit, decl.fields.Get(field).type),
                     .name = CppClosureCaptureName(field),
                     .qualifier = {}});
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

// Package variables (LRM 26.2), one cell for the whole program: declared
// `extern` in the header and defined once in this unit's code file. A unit of a
// design element has none, since its storage is per instance.
auto RenderUnitStaticVariables(const mir::CompilationUnit& unit) -> UnitText {
  UnitText text;
  for (const mir::StaticVariableId id : unit.static_variables.Ids()) {
    const CppType type(unit, unit.static_variables.Get(id).type);
    const CppName name = CppStaticVariableName(unit.named_static_variables, id);
    WriteDeclaration(
        text.signature, DeclaredCell{
                            .owner = CellOwner::kNamespace,
                            .text = CellText::kAnnounced,
                            .immutable = false,
                            .type = type,
                            .name = name,
                            .qualifier = {}});
    WriteDeclaration(
        text.code, DeclaredCell{
                       .owner = CellOwner::kNamespace,
                       .text = CellText::kDefined,
                       .immutable = false,
                       .type = type,
                       .name = name,
                       .qualifier = {}});
  }
  return text;
}

void RenderExternalObjectDeclarations(
    const mir::CompilationUnit& unit, TargetText& out) {
  for (const mir::ExternalUnitObjectId id : unit.external_unit_objects.Ids()) {
    const mir::ExternalUnitObject& object = unit.external_unit_objects.Get(id);
    OpenNamespace(out, UnitNamespaceOf(object.unit_name));
    Write(out, "class ", ToCppName(object.class_name), ";\n");
    CloseNamespace(out, UnitNamespaceOf(object.unit_name));
  }
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
