#include "lyra/backend/cpp/render_decl.hpp"

#include <map>
#include <optional>
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
#include "lyra/mir/closure_signature.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/local.hpp"
#include "lyra/mir/record_decl.hpp"
#include "lyra/mir/record_id.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/value_build.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/support/def_path.hpp"

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

// How C++ hands a body its receiver, for a body with one.
auto ReceiverOf(const mir::CompilationUnit& unit, const mir::CallableCode& code)
    -> ReceiverSpelling {
  return ReceiverSpellingAsCpp(unit, code.locals.Get(*code.receiver).type);
}

// The same for a member function, which is handed none where its caller
// enters it on no object (LRM 8.10), or hands it a scope it narrows itself.
auto MemberReceiverOf(
    const mir::CompilationUnit& unit, const mir::CallableCode& code)
    -> std::optional<ReceiverSpelling> {
  if (!code.TakesReceiver()) return std::nullopt;
  return ReceiverOf(unit, code);
}

// `static ` for a member function C++ hands no object as `this`.
auto StaticPrefix(const std::optional<ReceiverSpelling>& receiver)
    -> std::string_view {
  if (!receiver.has_value()) return "static ";
  return std::visit(
      Overloaded{
          [](const ReceiverFromThis&) -> std::string_view { return ""; },
          [](ReceiverAsParameter) -> std::string_view { return "static "; }},
      *receiver);
}

// What a member function's declaration and its definition share after its
// name, `(params) const -> R`: a receiver taken as a parameter leads the
// parameters, and `const` follows them where the body is handed its object
// read-only.
void WriteMemberSignatureTail(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    const std::optional<ReceiverSpelling>& receiver,
    std::span<const mir::LocalId> params, TargetText& out) {
  const CppType result(unit, code.result_type);
  out += "(";
  if (!receiver.has_value()) {
    WriteParameters(unit, code, params, out);
    Write(out, ") -> ", result);
    return;
  }
  std::visit(
      Overloaded{
          [&](const ReceiverFromThis& from) {
            WriteParameters(unit, code, params, out);
            Write(out, ")", CppConstQualifier{from.object}, " -> ", result);
          },
          [&](ReceiverAsParameter) {
            std::vector<mir::LocalId> taken{*code.receiver};
            taken.insert(taken.end(), params.begin(), params.end());
            WriteParameters(unit, code, taken, out);
            Write(out, ") -> ", result);
          }},
      *receiver);
}

// The body's first line, binding its receiver to the object C++ hands it as
// `this`: `C* self = this;` for an object pointed at, `const S& self = *this;`
// for a record referred to. MIR reaches the object through that binding in
// every body. A receiver taken as a parameter is bound already.
void WriteReceiverBinding(
    const mir::CompilationUnit& unit, const mir::CallableCode& code,
    const std::optional<ReceiverSpelling>& receiver, TargetText& out) {
  if (!receiver.has_value()) return;
  std::visit(
      Overloaded{
          [&](const ReceiverFromThis& from) {
            out.OpenLine();
            Write(
                out, CppType(unit, code.locals.Get(*code.receiver).type), " ",
                CppLocalName(code.named_locals, *code.receiver), " = ",
                from.bound_from, ";\n");
          },
          [](ReceiverAsParameter) {}},
      *receiver);
}

// The definition of a member function -- a class's method (LRM 8.6), static
// method (LRM 8.10), process or lifecycle body, the entry a scope answers a
// foreign caller with (LRM 35.5.3), or a record's method -- written
// outside its type so the body can use any type of the unit as a complete one.
template <typename Owner, typename Member>
void RenderMemberFunctionDef(
    const mir::CompilationUnit& unit, UnitRenderReport& report,
    std::string_view specifiers, const Owner& owner, const Member& member,
    const mir::CallableCode& code, TargetText& out) {
  const std::optional<ReceiverSpelling> receiver = MemberReceiverOf(unit, code);
  Write(out, specifiers, "auto ", owner, "::", member);
  WriteMemberSignatureTail(
      unit, code, receiver, code.ParamsAfterReceiver(), out);
  out += " ";
  WriteBody(out, [&] {
    WriteReceiverBinding(unit, code, receiver, out);
    RenderBlockStatements(ScopeView::ForCode(unit, code, report), out);
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
                 .constness = Constness::kMutable,
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
            .constness = Constness::kMutable,
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
  const std::optional<ReceiverSpelling> receiver = MemberReceiverOf(unit, code);
  out.OpenLine();
  out += StaticPrefix(receiver);
  out += VirtualPrefix(m);
  Write(out, "auto ", CppClassCallableName(unit, s, id));
  WriteMemberSignatureTail(
      unit, code, receiver, code.ParamsAfterReceiver(), out);
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
    const mir::CompilationUnit& unit, UnitRenderReport& report,
    std::string_view specifiers, mir::ClassId cls_id, const mir::Class& s,
    mir::CallableId id, const mir::CallableDecl& m, TargetText& out) {
  if (!std::holds_alternative<mir::DefinedHere>(mir::FormOf(m))) return;
  RenderMemberFunctionDef(
      unit, report, specifiers, CppClassPath(unit, cls_id),
      CppClassCallableName(unit, s, id), m.code, out);
}

// The constructor, a member function declared in the class and defined in the
// code file, outside the class so the body can build a child whose own body
// uses this class:
//
//   auto C::sv_construct(args) -> void { C* self = this; body }
//
// It is entered on an object already created whole -- by the C++ constructor
// the class leaves implicit, which builds every member and gives the object
// its class -- so every step of the construction, the base's included, is a
// statement of the body, which reaches the object through its receiver as
// every other body does.
void RenderConstructor(
    const mir::CompilationUnit& unit, UnitRenderReport& report,
    mir::ClassId cls_id, const mir::ConstructorDecl& ctor,
    std::string_view specifiers, TargetText& signature, TargetText& code) {
  const mir::CallableCode& ctor_code = ctor.code;
  const std::optional<ReceiverSpelling> receiver =
      MemberReceiverOf(unit, ctor_code);
  signature.OpenLine();
  Write(signature, "auto ", CppConstructorMemberName());
  WriteMemberSignatureTail(
      unit, ctor_code, receiver, ctor_code.ParamsAfterReceiver(), signature);
  signature += ";\n";
  RenderMemberFunctionDef(
      unit, report, specifiers, CppClassPath(unit, cls_id),
      CppConstructorMemberName(), ctor_code, code);
}

// A constant class `s` holds, announced in the class as `static const T name;`
// and defined in the code file as `const T Class::name = value;`. Another unit
// only ever needs its address, which the announcement is enough for, so the
// value stays in this unit. The caller passes the name, since the definition's
// is fixed and the others are positions.
void RenderClassConstant(
    const mir::CompilationUnit& unit, UnitRenderReport& report, mir::ClassId id,
    mir::TypeId type, const CppName& name, const mir::ValueBuild& build,
    VariableForm defined_as, TargetText& announced, TargetText& defined) {
  WriteDeclaration(
      announced, VariableDeclaration{
                     .form = VariableForm::kStaticDataMemberDeclaration,
                     .constness = Constness::kConst,
                     .type = CppType(unit, type),
                     .name = name});
  const ScopeView view = ScopeView::ForConstant(unit, build.body, report);
  WriteDeclaration(
      defined,
      VariableDeclaration{
          .form = defined_as,
          .constness = Constness::kConst,
          .type = CppType(unit, type),
          .name = name,
          .qualifier = CppClassPath(unit, id)},
      [&](TargetText& value) { Write(view, value, build.value); });
}

// Which classes of a unit C++ declares inside another, by each class's
// position in the unit: the class each is declared inside, and the classes
// each declares inside itself, in the unit's order. A generate block is a
// declaration nested in the scope holding it (LRM 27.3), so the class of one
// is declared inside the class the unit published under its path short of the
// last step.
struct ClassNesting {
  std::vector<std::optional<mir::ClassId>> declared_inside;
  std::vector<std::vector<mir::ClassId>> declares_inside;
};

auto NestingOf(const mir::CompilationUnit& unit) -> ClassNesting {
  std::map<support::DefPath, mir::ClassId> published_at;
  for (const mir::ClassId id : unit.classes.Ids()) {
    if (const auto& path = unit.GetClass(id).path) {
      published_at.emplace(*path, id);
    }
  }
  ClassNesting nesting{
      .declared_inside =
          std::vector<std::optional<mir::ClassId>>(unit.classes.size()),
      .declares_inside =
          std::vector<std::vector<mir::ClassId>>(unit.classes.size())};
  for (const mir::ClassId id : unit.classes.Ids()) {
    const std::optional<support::DefPath>& path = unit.GetClass(id).path;
    if (!path.has_value() || !IsNestedClass(*path)) continue;
    const auto enclosing = published_at.find(support::EnclosingPath(*path));
    if (enclosing == published_at.end()) {
      throw InternalError(
          "backend::cpp: the unit published the class of a generate block and "
          "not the class of the scope holding it -- please report this as a "
          "bug");
    }
    nesting.declared_inside[id.value] = enclosing->second;
    nesting.declares_inside[enclosing->second.value].push_back(id);
  }
  return nesting;
}

void RenderClass(
    const mir::CompilationUnit& unit, UnitRenderReport& report, mir::ClassId id,
    const mir::Class& s, std::span<const mir::ClassId> declares_inside,
    TargetText& signature, TargetText& code);

// Writes a class after every class of this unit it derives from or is
// declared inside, which the unit's class list does not guarantee, marking
// written classes in `emitted`. The order matters wherever several classes
// share a file: the code file, and the header a unit's scope classes share.
void AppendClassInDependencyOrder(
    const mir::CompilationUnit& unit, UnitRenderReport& report,
    const ClassNesting& nesting, mir::ClassId id, std::vector<bool>& emitted,
    UnitClasses& text) {
  if (emitted[id.value]) return;
  emitted[id.value] = true;
  const mir::Class& cls = unit.GetClass(id);
  if (const auto& enclosing = nesting.declared_inside[id.value]) {
    AppendClassInDependencyOrder(
        unit, report, nesting, *enclosing, emitted, text);
  }
  for (const mir::DeclaredClassRef& rests_on :
       mir::RestsOnDeclaredClasses(cls)) {
    std::visit(
        Overloaded{
            [&](const mir::IntraUnitClassRef& intra) {
              AppendClassInDependencyOrder(
                  unit, report, nesting, intra.class_id, emitted, text);
            },
            // Another unit's class is declared in that unit's own header,
            // which the file declaring this one includes.
            [](const mir::CrossUnitClassRef&) {}},
        rests_on);
  }
  const std::span<const mir::ClassId> declares_inside =
      nesting.declares_inside[id.value];
  const TargetText::Section defined(text.definitions);
  // A class every body states alike is what a scope published, defined beside
  // its declaration, since a referrer reads its definition too.
  if (mir::IsStatedByEveryBody(unit, id)) {
    PublishedClass published{.id = id, .text = TargetText{}};
    TargetText definitions;
    RenderClass(
        unit, report, id, cls, declares_inside, published.text, definitions);
    AppendSection(published.text, definitions);
    text.published.push_back(std::move(published));
    return;
  }
  if (mir::IsPublished(unit, id)) {
    PublishedClass published{.id = id, .text = TargetText{}};
    RenderClass(
        unit, report, id, cls, declares_inside, published.text,
        text.definitions);
    text.published.push_back(std::move(published));
    return;
  }
  const TargetText::Section declared(text.internal);
  RenderClass(
      unit, report, id, cls, declares_inside, text.internal, text.definitions);
}

void RenderClass(
    const mir::CompilationUnit& unit, UnitRenderReport& report, mir::ClassId id,
    const mir::Class& s, std::span<const mir::ClassId> declares_inside,
    TargetText& signature, TargetText& code) {
  // A class every body states is defined in its header, so each definition of
  // it is inline, where every one of them is kept as one.
  const bool stated_by_every_body = mir::IsStatedByEveryBody(unit, id);
  const std::string_view specifiers = stated_by_every_body ? "inline " : "";
  TargetText& out = signature;
  Write(out, "class ", CppClassPath(unit, id));
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
          CppBaseClass{.of = CppClassRef(unit, base.of)});
    });
  }
  out += " {\n";
  out += " public:\n";
  out.Indent();

  // A class declared inside this one is declared here and defined after it,
  // outside, so each reads as a class of its own.
  {
    const TargetText::Section nested(out);
    for (const mir::ClassId inside : declares_inside) {
      out.OpenLine();
      Write(out, "class ", CppClassName(unit, inside), ";\n");
    }
  }

  if (s.constructor.has_value()) {
    const TargetText::Section declared(out);
    const TargetText::Section defined(code);
    RenderConstructor(unit, report, id, *s.constructor, specifiers, out, code);
  }

  // The destructor is virtual, because whoever ends a value holds it as the
  // class every value extends, or as an interface class. Defined out of line
  // and not inline, it is the class's key function, and the class's table and
  // type description are emitted with it once rather than in every file that
  // casts to it; defined inline, they are emitted wherever it is, and kept as
  // one.
  {
    const TargetText::Section ended(out);
    out.OpenLine();
    if (s.base.has_value()) {
      Write(out, "~", CppClassName(unit, id), "() override;\n");
    } else {
      Write(out, "virtual ~", CppClassName(unit, id), "();\n");
    }
    const TargetText::Section defined(code);
    code.OpenLine();
    Write(
        code, specifiers, CppClassPath(unit, id), "::~", CppClassName(unit, id),
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
          unit, report, specifiers, id, s, callable_id, callable, code);
    }
  }

  // The constants the class holds and then its definition, which names them:
  // each defined in the code file beside the bodies it names.
  {
    const TargetText::Section constants(out);
    const TargetText::Section defined(code);
    const VariableForm defined_as =
        stated_by_every_body ? VariableForm::kInlineStaticDataMemberDefinition
                             : VariableForm::kStaticDataMemberDefinition;
    for (const mir::ClassConstantId constant : s.constants.Ids()) {
      const mir::ClassConstantDecl& decl = s.constants.Get(constant);
      RenderClassConstant(
          unit, report, id, decl.type, CppClassConstantName(constant),
          decl.initializer, defined_as, out, code);
    }
    RenderClassConstant(
        unit, report, id, mir::ClassDefinitionType(unit.types),
        CppDefinitionName(), s.object_definition_initializer, defined_as, out,
        code);
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
          [](const mir::ReachedByBringUpEntry&) -> std::string_view {
            return "";
          },
          [](const mir::ReachedAsObjectEntry&) -> std::string_view {
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
    const mir::CompilationUnit& unit, UnitRenderReport& report,
    mir::CallableId id, const mir::CallableDecl& callable, TargetText& out) {
  RenderFreeCallableSignature(unit, id, callable, out);
  out += " ";
  WriteBody(out, [&] {
    RenderBlockStatements(ScopeView::ForCode(unit, callable.code, report), out);
  });
  out += "\n";
}

// A struct or a union the unit declares: the library type holding its members,
// as a type of its own so that two declarations with the same members stay two
// types (LRM 6.22.1), whose member functions are its methods -- declared in it,
// and defined apart from it once every one of the unit is declared.
void RenderRecord(
    const mir::CompilationUnit& unit, UnitRenderReport& report,
    mir::RecordId id, TargetText& declared, TargetText& defined) {
  const mir::RecordDecl& decl = unit.GetRecord(id);
  const CppName name = CppRecordName(decl);
  // Every body's code file defines the methods, and another unit calls them
  // with only the declaration in hand. So each definition is weak, and the
  // link keeps one: an `inline` one would be emitted only by a file that calls
  // it, which none of these need to.
  const std::string_view specifiers = "[[gnu::weak]] ";
  const CppRecordBase library_type{.unit = &unit, .of = &decl};
  const MintedWord base = CppRecordBaseName();
  Write(declared, "struct ", name, " : ", library_type, " {\n");
  declared.Indent();
  declared.OpenLine();
  Write(declared, "using ", base, " = ", library_type, ";\n");
  declared.OpenLine();
  Write(declared, "using ", base, "::", base, ";\n");
  for (const mir::RecordMethod& method : unit.GetRecordMethods(id)) {
    const VerbatimName member = CppValueOperationMemberName(method.answers);
    const std::optional<ReceiverSpelling> receiver =
        MemberReceiverOf(unit, method.code);
    declared.OpenLine();
    Write(declared, StaticPrefix(receiver), "auto ", member);
    WriteMemberSignatureTail(
        unit, method.code, receiver, method.code.ParamsAfterReceiver(),
        declared);
    declared += ";\n";
    RenderMemberFunctionDef(
        unit, report, specifiers, name, member, method.code, defined);
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
    if (cls.path.has_value() && IsNestedClass(*cls.path)) continue;
    TargetText& out = mir::IsPublished(unit, id) ? text.signature : text.code;
    Write(out, "class ", CppClassName(unit, id), ";\n");
  }
  return text;
}

auto RenderUnitClasses(
    const mir::CompilationUnit& unit, UnitRenderReport& report) -> UnitClasses {
  UnitClasses text;
  const ClassNesting nesting = NestingOf(unit);
  std::vector<bool> emitted(unit.classes.size(), false);
  for (const mir::ClassId id : unit.classes.Ids()) {
    AppendClassInDependencyOrder(unit, report, nesting, id, emitted, text);
  }
  return text;
}

auto RenderUnitClosures(
    const mir::CompilationUnit& unit, UnitRenderReport& report)
    -> UnitClosures {
  UnitClosures text;
  for (const mir::ClosureId id : unit.closures.Ids()) {
    const mir::ClosureDecl& decl = unit.GetClosure(id);
    const mir::CallableCode& code = decl.invoke;
    const MintedName name = CppClosureName(id);
    if (!code.receiver.has_value()) {
      throw InternalError(
          "RenderUnitClosures: a closure's body reads its captures through the "
          "closure, yet states no receiver -- please report this as a bug");
    }
    const ReceiverSpelling receiver = ReceiverOf(unit, code);
    // The function's name and what follows it, the same in the declaration
    // and the definition. The library calls a body borrowing its closure, as
    // Rust's `Fn` and `FnMut` take `&self` and `&mut self`, as the closure's
    // call operator, and one taking over the box it was made in, as `FnOnce`
    // takes `self`, by a name of its own.
    const auto write_function = [&](TargetText& out) {
      switch (mir::ClosureKindOf(unit.types, decl)) {
        case support::ClosureKind::kFn:
        case support::ClosureKind::kFnMut:
          out += "operator()";
          break;
        case support::ClosureKind::kFnOnce:
          Write(out, CppClosureCallOnceName());
          break;
      }
      WriteMemberSignatureTail(unit, code, receiver, code.params, out);
    };
    // The constructor takes each capture in the closure's member order.
    const auto write_constructor_params = [&](TargetText& out) {
      WriteSeparated(out, decl.field_order, ", ", [&](mir::FieldId field) {
        Write(
            out, CppType(unit, decl.fields.Get(field).type), " ",
            CppClosureCaptureName(field));
      });
    };

    {
      TargetText& out = text.declarations;
      const TargetText::Section declared(out);
      // The library holds a closure of no known type by the part it keeps,
      // which leads the value: the definition it calls and ends the closure
      // through, then the captures.
      Write(out, "struct ", name, " final : ", CppClosureBase(), " {\n");
      out.Indent();
      out.OpenLine();
      Write(out, "explicit ", name, "(");
      write_constructor_params(out);
      out += ");\n";
      for (const mir::FieldId field : decl.field_order) {
        WriteDeclaration(
            out, VariableDeclaration{
                     .form = VariableForm::kNonStaticDataMember,
                     .constness = Constness::kMutable,
                     .type = CppType(unit, decl.fields.Get(field).type),
                     .name = CppClosureCaptureName(field)});
      }
      out.OpenLine();
      Write(out, StaticPrefix(receiver), "auto ");
      write_function(out);
      out += ";\n";
      out.Outdent();
      out += "};\n";
    }

    TargetText& out = text.definitions;
    const TargetText::Section defined(out);
    // How the runtime library calls the closure and ends it, which the
    // library reads off the closure's type.
    const MintedName definition = CppClosureDefinitionName(id);
    Write(
        out, "inline constexpr auto ", definition, " = ",
        CppClosureDefinitionOf(), "<", name, ">();\n");

    Write(out, name, "::", name, "(");
    write_constructor_params(out);
    Write(out, ") : ", CppClosureBase(), "(&", definition, ")");
    for (const mir::FieldId field : decl.field_order) {
      const MintedName capture = CppClosureCaptureName(field);
      Write(out, ", ", capture, "(std::move(", capture, "))");
    }
    out += " {}\n";

    Write(out, "auto ", name, "::");
    write_function(out);
    out += " ";
    WriteBody(out, [&] {
      WriteReceiverBinding(unit, code, receiver, out);
      RenderBlockStatements(ScopeView::ForCode(unit, code, report), out);
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
    const mir::CompilationUnit& unit, UnitRenderReport& report)
    -> UnitCallables {
  UnitCallables text;
  // All are declared before any class, because a class's body may call one,
  // and defined after the classes, because an export's entry point uses them.
  // An import has no body here; the user's C code defines it.
  for (const mir::CallableId id : unit.callables.Ids()) {
    const mir::CallableDecl& callable = unit.callables.Get(id);
    UnitText& out = std::visit(
        Overloaded{
            [&](const mir::ReachedByLinkageName&) -> UnitText& {
              return text.of_unit;
            },
            [&](const mir::ReachedByName&) -> UnitText& {
              return text.of_unit;
            },
            [&](const mir::ReachedByBringUpEntry&) -> UnitText& {
              return text.of_unit;
            },
            [&](const mir::ReachedAsObjectEntry&) -> UnitText& {
              return text.of_body;
            },
            [&](const mir::ReachedByPosition&) -> UnitText& {
              return text.of_artifact;
            }},
        mir::NamespaceReachOf(unit, id));
    RenderFreeCallableSignature(unit, id, callable, out.signature);
    out.signature += ";\n";
    if (std::holds_alternative<mir::DefinedHere>(mir::FormOf(callable))) {
      const TargetText::Section defined(out.code);
      RenderFreeCallable(unit, report, id, callable, out.code);
    }
  }
  return text;
}

auto RenderUnitRecords(
    const mir::CompilationUnit& unit, UnitRenderReport& report) -> UnitRecords {
  // Another unit names a struct or a union by the declaration it answers to,
  // so it is declared where that unit reads, in the unit's types namespace, and
  // its methods are defined in the same namespace in the code file.
  UnitRecords records;
  TargetText definitions;
  for (const mir::RecordId id : unit.records.Ids()) {
    TargetText declared;
    const TargetText::Section defined_section(definitions);
    RenderRecord(unit, report, id, declared, definitions);
    TargetText declaration;
    AppendSectionInNamespace(declaration, CppRecordTypesNamespace(), declared);
    records.declared.push_back(
        UnitRecord{.id = id, .declaration = std::move(declaration)});
  }
  AppendSectionInNamespace(
      records.code, CppRecordTypesNamespace(), definitions);
  return records;
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
                            .constness = Constness::kMutable,
                            .type = type,
                            .name = name});
    WriteDeclaration(
        text.code, VariableDeclaration{
                       .form = VariableForm::kNamespaceScopeDefinition,
                       .constness = Constness::kMutable,
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
    const mir::CompilationUnit& unit, UnitRenderReport& report,
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
          ScopeView::ForCode(unit, entry.definition, report), out);
    });
    out += "\n";
  }
}

}  // namespace lyra::backend::cpp
