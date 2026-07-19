#include <algorithm>
#include <cstddef>
#include <format>
#include <optional>
#include <span>
#include <string>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/support/dpi_abi.hpp"

namespace lyra::backend::cpp {

namespace {

// A field declaration is (name, type): the type carries the target storage
// form, the name the source identifier. Per-field construction state -- a
// cell's declared representation, its initial value -- arrives as ordinary MIR
// statements in the constructor body; render never composes it here from type
// payload. The field value-initializes; an integral cell's declared
// representation is established by its first store.
auto RenderField(
    const ScopeView& ctor_view, const mir::FieldDecl& field, std::size_t indent)
    -> std::string {
  const auto& unit = ctor_view.Unit();
  const std::string type = RenderTypeAsCpp(unit, field.type);
  return std::format("{}{} {}{{}};\n", Indent(indent), type, field.name);
}

// The value-init field declarations of any field-bearing storage -- a class's
// fields, a promoted scope's fields. Shared so a generated struct carries no
// field-emission of its own; it feeds the same declarations a class does.
auto RenderFieldList(
    const ScopeView& view,
    const base::Arena<mir::FieldDecl, mir::FieldId>& fields, std::size_t indent)
    -> std::string {
  std::string out;
  for (const auto& field : fields) {
    out += RenderField(view, field, indent);
  }
  return out;
}

// A class static property (LRM 8.9) renders as an `inline static` member of
// the C++ class: one cell owned by the type, value-initialized at
// program-startup time before any process runs, which matches LRM 10.5's
// "before any initial or always" ordering natively. The declaration is
// bare `<type> <name>{}`; a source-declared initializer, when present,
// arrives as a class-level assignment statement in the design-init body,
// never baked into the declaration.
auto RenderClassStaticProperty(
    const mir::CompilationUnit& unit, const mir::StaticPropertyDecl& sp,
    std::size_t indent) -> std::string {
  const std::string type = RenderTypeAsCpp(unit, sp.type);
  return std::format(
      "{}inline static {} {}{{}};\n", Indent(indent), type, sp.name);
}

auto RenderCallableParam(
    const mir::CompilationUnit& unit, const mir::LocalDecl& param)
    -> std::string {
  // Every formal is a value parameter: an `input` by value (LRM 13.5.1), a
  // `ref` / `const ref` whose `RefType` already renders as `(const) Ref<T>` so
  // the reference value carries the aliasing (LRM 13.5.2). `output` / `inout`
  // are not parameters -- they ride the completion payload.
  return std::format("{} {}", RenderTypeAsCpp(unit, param.type), param.name);
}

// The C++ specifier this callable's dispatch role prefixes its declaration
// with: `virtual` when the callable introduces a new dispatch slot on this
// class, empty otherwise; the source of virtualness for an override is the
// slot the base already declares, which the `override` suffix records
// separately.
auto VirtualPrefix(const mir::CallableDecl& m) -> std::string_view {
  if (!m.virtual_dispatch.has_value()) return "";
  if (std::holds_alternative<mir::IntroducesVirtualSlot>(*m.virtual_dispatch)) {
    return "virtual ";
  }
  return "";
}

// The trailing specifier attached after the return type when this callable
// fills an inherited dispatch slot: `override` records that the base's slot
// resolves through this implementation, so a name-only compilation cannot
// silently disagree with the intended override target.
auto OverrideSuffix(const mir::CallableDecl& m) -> std::string_view {
  if (!m.virtual_dispatch.has_value()) return "";
  if (std::holds_alternative<mir::IntroducesVirtualSlot>(*m.virtual_dispatch)) {
    return "";
  }
  return " override";
}

// The renderer for a class-owned callable -- an instance method (LRM 8.6),
// a static method (LRM 8.10), a process, or a lifecycle body. An instance
// callable renders as a C++ instance member function whose body opens with
// a one-line `self = this` adapter, so the body's expressions resolve
// receiver-relative references uniformly. A static callable renders with
// the `static` keyword and no receiver alias. The callable's dispatch role
// decorates the declaration with `virtual` or `override` where applicable.
// A pure virtual prototype (LRM 8.21) is rendered as a bodyless declaration
// suffixed with `= 0`; C++ then treats the enclosing class as abstract
// without any class-level marker required. A namespace's receiver-less
// callable renders through the free-function path instead.
auto RenderClassCallable(
    const ScopeView* parent_struct_view, const mir::CompilationUnit& unit,
    const mir::Class& s, const mir::CallableDecl& m, std::size_t indent)
    -> std::string {
  // A pure virtual prototype (LRM 8.21) exposes only the signature; body
  // rendering is skipped in favor of a `= 0` marker on the declaration.
  const mir::CallableCode& code =
      std::holds_alternative<mir::PurePrototype>(m.impl)
          ? std::get<mir::PurePrototype>(m.impl).code
          : std::get<mir::InternalCallable>(m.impl).code;
  const std::string ret = RenderTypeAsCpp(unit, code.result_type);
  // Instance vs static (LRM 8.10) is a signature-level fact carried by the
  // presence of a self-typed `params[0]`. The C++ `static` prefix and the
  // body's receiver-alias are gated on the same check so no side flag
  // restates what the signature already fixes.
  const bool has_receiver = code.HasReceiver(s.self_pointer_type);
  const std::size_t user_params_start = has_receiver ? 1 : 0;
  const std::string_view static_prefix = has_receiver ? "" : "static ";

  std::string sig =
      std::format("{}{}auto {}(", static_prefix, VirtualPrefix(m), m.name);
  for (std::size_t i = user_params_start; i < code.params.size(); ++i) {
    if (i != user_params_start) sig += ", ";
    sig += RenderCallableParam(unit, code.locals.Get(code.params[i]));
  }
  sig += std::format(") -> {}{}", ret, OverrideSuffix(m));

  if (std::holds_alternative<mir::PurePrototype>(m.impl)) {
    return std::format("{}{} = 0;\n", Indent(indent), sig);
  }

  const ScopeView body_view = (parent_struct_view == nullptr)
                                  ? ScopeView::ForRoot(unit, s, code)
                                  : parent_struct_view->WithClass(s, code);

  std::string out;
  out += std::format("{}{} {{\n", Indent(indent), sig);
  if (has_receiver) {
    const mir::LocalId self_local = code.params[0];
    const auto& self_decl = code.locals.Get(self_local);
    const std::string self_type = RenderTypeAsCpp(unit, self_decl.type);
    out += std::format(
        "{}{} {} = this;\n", Indent(indent + 1), self_type, self_decl.name);
  }
  out += RenderBlockStatements(body_view, indent + 1);
  out += std::format("{}}}\n", Indent(indent));
  return out;
}

// The renderer for a runtime-callback adapter: a static class member so its
// address decays to a plain function pointer of the shape the runtime
// callback table requires. The receiver is the callable's first explicit
// parameter, rendered like any other formal.
auto RenderAbiAdapter(
    const ScopeView* parent_struct_view, const mir::CompilationUnit& unit,
    const mir::Class& s, const mir::AbiAdapter& a, std::size_t indent)
    -> std::string {
  const ScopeView body_view = (parent_struct_view == nullptr)
                                  ? ScopeView::ForRoot(unit, s, a.code)
                                  : parent_struct_view->WithClass(s, a.code);

  const std::string ret = RenderTypeAsCpp(unit, a.code.result_type);
  std::string sig = std::format("static auto {}(", a.name);
  for (std::size_t i = 0; i < a.code.params.size(); ++i) {
    if (i != 0) sig += ", ";
    sig += RenderCallableParam(unit, a.code.locals.Get(a.code.params[i]));
  }
  sig += std::format(") -> {}", ret);

  std::string out;
  out += std::format("{}{} {{\n", Indent(indent), sig);
  out += RenderBlockStatements(body_view, indent + 1);
  out += std::format("{}}}\n", Indent(indent));
  return out;
}

// Joins string parts with ", " for emission of ctor sig args, base-init
// forwards, and member-init list entries.
auto JoinCommaSeparated(const std::vector<std::string>& parts) -> std::string {
  std::string out;
  for (const auto& part : parts) {
    if (!out.empty()) {
      out.append(", ");
    }
    out.append(part);
  }
  return out;
}

auto RenderConstructor(
    const ScopeView& scope_view, const mir::Class& s, std::size_t indent)
    -> std::string {
  // The C++ ctor is composed from the class's construction protocol: the
  // ctor's own callable carries the signature (with `self` at position 0
  // per MIR contract -- omitted here because C++ makes `this` implicit); the
  // base-init phase carries the args to forward to the base ctor; each
  // member-init maps a field to its initializing expression. The body is the
  // ctor callable's own body, threaded through a static `init(self)` helper
  // so a body-local `self` reference resolves the same way it does in every
  // other method render.
  const auto& unit = scope_view.Unit();
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
    const auto& p = ctor_code.locals.Get(ctor_code.params[i]);
    sig_args.push_back(render_typed_name(p.type, p.name));
    forward_names.emplace_back(p.name);
  }

  std::vector<std::string> init_parts;
  if (s.constructor.base_init.has_value()) {
    const std::string base_class = RenderClassRefAsCpp(unit, *s.base);
    std::vector<std::string> base_args_rendered;
    base_args_rendered.reserve(s.constructor.base_init->args.size());
    for (const mir::ExprId arg : s.constructor.base_init->args) {
      base_args_rendered.push_back(
          RenderExpr(scope_view, scope_view.Expr(arg)));
    }
    init_parts.push_back(
        std::format(
            "{}({})", base_class, JoinCommaSeparated(base_args_rendered)));
  }
  for (const mir::FieldInit& mi : s.constructor.member_inits) {
    const auto& f = s.fields.Get(mi.target);
    init_parts.push_back(
        std::format(
            "{}({})", f.name,
            RenderExpr(scope_view, scope_view.Expr(mi.value))));
  }

  const std::string cpp_name = ToCppName(s.name);
  const std::string sig =
      init_parts.empty()
          ? std::format("{}({})", cpp_name, JoinCommaSeparated(sig_args))
          : std::format(
                "{}({}) : {}", cpp_name, JoinCommaSeparated(sig_args),
                JoinCommaSeparated(init_parts));

  // The C++ ctor is an allocation shell that forwards to the base and the
  // field members, then hands off to a static `init(self, ...)` -- the same
  // static-over-self shape every method render uses, so a body-local self
  // reference resolves as `self` here just like in any other body. The
  // constructor formals ride alongside `self` so the body reaches them the same
  // way it reaches any parameter.
  std::vector<std::string> init_params;
  init_params.reserve(sig_args.size() + 1);
  init_params.push_back(std::format("{}* self", cpp_name));
  for (const std::string& arg : sig_args) {
    init_params.push_back(arg);
  }
  std::vector<std::string> init_call_args;
  init_call_args.reserve(forward_names.size() + 1);
  init_call_args.emplace_back("this");
  for (const std::string& name : forward_names) {
    init_call_args.push_back(name);
  }
  return std::format(
      "{0}{1} {{ init({2}); }}\n"
      "{0}static auto init({3}) -> void {{\n"
      "{4}"
      "{0}}}\n",
      Indent(indent), sig, JoinCommaSeparated(init_call_args),
      JoinCommaSeparated(init_params),
      RenderBlockStatements(scope_view, indent + 1));
}

auto VisibilityKeyword(mir::CallableVisibility visibility) -> std::string_view {
  switch (visibility) {
    case mir::CallableVisibility::kPublic:
      return "public";
    case mir::CallableVisibility::kInternal:
      return "private";
  }
  throw InternalError("VisibilityKeyword: unknown CallableVisibility");
}

// A compiler-generated struct emits as a plain struct of value-init fields. It
// is nested in its host class purely as a C++ emission placement -- so a
// field's enum type resolves against that class's type aliases -- not as a
// claim that the class owns it (the declaring body may be a closure). Storage
// only: no base, no constructor, no methods.
auto RenderStruct(
    const ScopeView& view, const mir::StructDecl& decl, std::size_t indent)
    -> std::string {
  std::string out = Indent(indent) + "struct " + decl.name + " {\n";
  out += RenderFieldList(view, decl.fields, indent + 1);
  out += Indent(indent) + "};\n";
  return out;
}

// A class-level static constant, declared as a static member whose initializer
// is the translated value expression. A runtime scope's generated-behavior
// record is one such constant; the constructor forwards its address to the
// base.
auto RenderStaticConstant(
    const ScopeView& parent_view, const mir::Class& s,
    const mir::StaticConstantDecl& c, std::size_t indent) -> std::string {
  const ScopeView view =
      parent_view.WithClass(s, s.constructor.code).WithBlock(c.body);
  return std::format(
      "\n{0}static constexpr {1} {2} = {3};\n", Indent(indent),
      RenderTypeAsCpp(parent_view.Unit(), c.type), c.name,
      RenderExpr(view, view.Expr(c.value)));
}

// The class-level design-init body (LRM 8.9 / 10.5): renders as a static
// class method whose body assigns each static property its source-declared
// initial value, plus an `inline static const` sentinel whose initializer
// invokes it. C++ evaluates `inline static` variables at program-startup
// time, before `main` and before any process, which realizes the LRM
// "before any initial or always" ordering without a runtime hook. When the
// class declared no source initializers the body is empty and nothing is
// emitted -- the properties' value-init on their inline declarations
// already covers the type-default case.
auto RenderClassStaticInit(
    const mir::CompilationUnit& unit, const mir::Class& s, std::size_t indent)
    -> std::string {
  if (s.static_init.body.root_stmts.empty()) return "";
  const ScopeView view = ScopeView::ForRoot(unit, s, s.static_init);
  std::string out;
  out += std::format(
      "{}static auto __static_init__() -> void {{\n", Indent(indent));
  out += RenderBlockStatements(view, indent + 1);
  out += std::format("{}}}\n", Indent(indent));
  out += std::format(
      "{}inline static const int __static_init_trigger__ = "
      "(__static_init__(), 0);\n",
      Indent(indent));
  return out;
}

auto RenderScopeAsClass(
    const mir::CompilationUnit& unit, const mir::Class& s, std::size_t indent,
    const ScopeView* parent_struct_view) -> std::string;

// Renders a class and every intra-unit base it depends on into `out`, in
// an order that guarantees each base is a complete C++ type before its
// derived (C++ requires base completeness at derivation). The interning
// walk sets the registry order, which may reach a derived class first, so
// this walker climbs the base chain first and marks visited classes in
// `emitted`. Skips runtime-tree-node classes; those emit through the
// scope-tree walk.
void RenderClassInDependencyOrder(
    const mir::CompilationUnit& unit, mir::ClassId id,
    std::vector<bool>& emitted, std::string& out) {
  if (emitted[id.value]) return;
  if (!unit.classes.IsDefined(id)) return;
  const mir::Class& cls = unit.GetClass(id);
  if (cls.is_scope_tree_node) return;
  if (cls.base.has_value()) {
    if (const auto* intra = std::get_if<mir::IntraUnitClassRef>(&*cls.base)) {
      RenderClassInDependencyOrder(unit, intra->class_id, emitted, out);
    }
  }
  if (emitted[id.value]) return;
  emitted[id.value] = true;
  out += RenderScopeAsClass(unit, cls, 0, nullptr);
  out += "\n";
}

auto RenderScopeAsClass(
    const mir::CompilationUnit& unit, const mir::Class& s, std::size_t indent,
    const ScopeView* parent_struct_view) -> std::string {
  // `this_anchor` is bound to `s.constructor` so it doubles as the view for
  // rendering the constructor body. Children's bodies use it as their enclosing
  // class (one hop above the child).
  const ScopeView this_anchor =
      (parent_struct_view == nullptr)
          ? ScopeView::ForRoot(unit, s, s.constructor.code)
          : parent_struct_view->WithClass(s, s.constructor.code);

  std::string out;
  out += Indent(indent) + "class " + ToCppName(s.name);
  if (s.is_final) {
    out += " final";
  }
  if (s.base.has_value()) {
    out += " : public " + RenderClassRefAsCpp(unit, *s.base);
  }
  out += " {\n";
  out += Indent(indent) + " public:\n";

  for (const mir::ClassId child_id : s.contained) {
    out += RenderScopeAsClass(
        unit, unit.GetClass(child_id), indent + 1, &this_anchor);
  }
  if (!s.contained.empty()) {
    out += "\n";
  }

  for (const mir::StructId sid : s.structs) {
    out += RenderStruct(this_anchor, unit.GetStruct(sid), indent + 1);
  }
  if (!s.structs.empty()) {
    out += "\n";
  }

  out += RenderConstructor(this_anchor, s, indent + 1);

  // Members follow the constructor and methods. They are public so cross-unit
  // references can reach them directly.
  if (!s.fields.empty()) {
    out += "\n";
  }
  out += RenderFieldList(this_anchor, s.fields, indent + 1);

  // Type-associated storage (LRM 8.9): one cell per class, value-initialized
  // by C++ at program-startup time so the type-default case needs no
  // explicit statement. A source-declared initializer is separately emitted
  // through the `static_init` body below.
  if (!s.static_properties.empty()) {
    out += "\n";
  }
  for (const mir::StaticPropertyDecl& sp : s.static_properties) {
    out += RenderClassStaticProperty(unit, sp, indent + 1);
  }

  // Each callable declares its access -- a class instance method is the
  // object's public callable surface, a scope's processes and lifecycle hooks
  // are internal -- and the access specifier follows that stated visibility,
  // coalescing a run of callables that share one. The constructor is not in
  // this arena; it was emitted above with C++ mem-init-list syntax. An
  // external (DPI-C import) callable is a global `extern "C"` symbol, not a
  // class member; it is emitted as a prototype outside the class. A pure
  // virtual prototype (LRM 8.21) is a class member and renders inline with
  // its `= 0` marker.
  std::optional<mir::CallableVisibility> open_section;
  for (std::size_t i = 0; i < s.callables.size(); ++i) {
    const mir::CallableId cid{static_cast<std::uint32_t>(i)};
    const auto& callable = s.callables.Get(cid);
    if (std::holds_alternative<mir::ExternalCallable>(callable.impl)) continue;
    if (open_section != callable.visibility) {
      open_section = callable.visibility;
      out += std::format(
          "\n{} {}:\n", Indent(indent), VisibilityKeyword(callable.visibility));
    }
    out += "\n";
    out +=
        RenderClassCallable(parent_struct_view, unit, s, callable, indent + 1);
  }

  // The class's runtime-callback adapters. Each renders as a static member
  // whose address decays to a plain function pointer for the runtime
  // callback table; the class never exposes them on its public surface.
  if (!s.abi_adapters.empty()) {
    if (open_section != mir::CallableVisibility::kInternal) {
      open_section = mir::CallableVisibility::kInternal;
      out += std::format(
          "\n{} {}:\n", Indent(indent),
          VisibilityKeyword(mir::CallableVisibility::kInternal));
    }
    for (std::size_t i = 0; i < s.abi_adapters.size(); ++i) {
      const auto& a =
          s.abi_adapters.Get(mir::AbiAdapterId{static_cast<std::uint32_t>(i)});
      out += "\n";
      out += RenderAbiAdapter(parent_struct_view, unit, s, a, indent + 1);
    }
  }

  // The class's static constants (a tree node's generated-behavior record among
  // them), each emitted as a static member.
  for (const mir::StaticConstantDecl& c : s.static_constants) {
    out += RenderStaticConstant(this_anchor, s, c, indent + 1);
  }

  // The class's design-init body (LRM 8.9 / 10.5). Renders only when the
  // source declared at least one static property initializer; otherwise the
  // value-init on each `inline static` declaration above already realizes
  // the type-default case.
  const std::string static_init_text =
      RenderClassStaticInit(unit, s, indent + 1);
  if (!static_init_text.empty()) {
    out += "\n";
    out += static_init_text;
  }

  out += std::format("{}}};\n", Indent(indent));
  return out;
}

// The `extern "C"` declarations for every DPI-C import the unit calls: each
// import's foreign symbol as a C-linkage prototype over its ABI carrier types
// (LRM 35.4 / 35.5.6). An output / inout argument crosses by pointer, so its
// carrier type gains a trailing `*` (a scalar `int` -> `int*`, a `const char*`
// string -> `const char**`); an input argument crosses by value. A task's
// foreign symbol returns the DPI disable-acknowledgment `int` (LRM 35.8), which
// the call discards; a function returns its result carrier. Emitted before the
// classes that call them so a foreign call resolves against a declared
// signature; the definitions come from the user's linked C. Empty when the unit
// declares no import.
auto RenderForeignImportDeclarations(const mir::CompilationUnit& unit)
    -> std::string {
  std::string out;
  for (std::size_t i = 0; i < unit.classes.size(); ++i) {
    const mir::ClassId id{static_cast<std::uint32_t>(i)};
    if (!unit.classes.IsDefined(id)) continue;
    for (const auto& callable : unit.GetClass(id).callables) {
      const auto* ext = std::get_if<mir::ExternalCallable>(&callable.impl);
      if (ext == nullptr) {
        continue;
      }
      std::string params;
      for (std::size_t p = 0; p < ext->params.size(); ++p) {
        if (p != 0) params += ", ";
        const bool writes_back =
            support::DpiDirectionWritesBack(ext->params[p].direction);
        if (const auto* scalar =
                std::get_if<support::ScalarCarrier>(&ext->params[p].carrier)) {
          // A by-value scalar crosses by value for input, by pointer for a
          // writeback direction.
          params += std::string{DpiScalarCarrierCppType(scalar->abi)};
          if (writes_back) params += "*";
        } else {
          // A canonical vector always crosses by pointer to its chunk buffer;
          // an input is read-only (`const`).
          const auto& vec =
              std::get<support::VectorCarrier>(ext->params[p].carrier);
          if (!writes_back) params += "const ";
          params += vec.four_state ? "svLogicVecVal*" : "svBitVecVal*";
        }
      }
      const std::string_view ret_type =
          ext->is_task ? DpiScalarCarrierCppType(support::DpiScalarAbi::kInt)
                       : DpiScalarCarrierCppType(ext->ret_abi);
      out += std::format(
                 R"(extern "C" {} {}({});)", ret_type,
                 ext->external.foreign_name, params) +
             "\n";
    }
  }
  return out;
}

auto CollectExternalUnitNames(const mir::CompilationUnit& unit)
    -> std::vector<std::string> {
  std::vector<std::string> names;
  const auto add = [&](const std::string& name) {
    if (std::ranges::find(names, name) == names.end()) {
      names.push_back(name);
    }
  };
  // A unit an instance is built from names its unit through the child object's
  // `ExternalUnitObjectType`; a unit whose namespace symbol is reached by name
  // (a receiver-less callable or a package variable) names its unit in the
  // reference-dependency list, since such a reference interns no such type;
  // a unit a class is referenced from -- as a handle, a `new`, a field /
  // method / static access, or a base extension -- names its unit in the
  // class-dependency list. All three are external units this unit's artifact
  // includes.
  for (const auto& t : unit.types) {
    if (const auto* ext = std::get_if<mir::ExternalUnitObjectType>(&t.data)) {
      add(ext->unit_name);
    }
  }
  for (const std::string& name : unit.external_referenced_units) {
    add(name);
  }
  for (const std::string& name : unit.external_class_units) {
    add(name);
  }
  return names;
}

// A DPI-C export's foreign-linkage entry point (LRM 35.5): a free `extern "C"`
// definition foreign code calls. It recovers the exported method's receiver
// from the running design, then runs the marshaling body -- an ordinary
// statement render, so every carrier conversion and the method call come from
// MIR. Each parameter renders from its MIR type through ordinary type mapping;
// a packed-vector carrier's C spelling (`svBitVecVal*` / `svLogicVecVal*`, made
// `const` for an input by the pointer's mutability) is carried by that type, so
// render makes no per-parameter ABI decision. Emitted after the class it
// dispatches into so that class is complete.
auto RenderForeignExportWrapper(
    const mir::CompilationUnit& unit, const mir::Class& s,
    const mir::ForeignExportWrapper& w) -> std::string {
  const ScopeView body_view = ScopeView::ForRoot(unit, s, w.code);
  const mir::LocalDecl& self_decl = w.code.locals.Get(w.self_local);
  const std::string self_type = RenderTypeAsCpp(unit, self_decl.type);

  std::string sig = std::format(
      "extern \"C\" {} {}(", RenderTypeAsCpp(unit, w.code.result_type),
      w.foreign_name);
  for (std::size_t i = 0; i < w.code.params.size(); ++i) {
    if (i != 0) sig += ", ";
    sig += RenderCallableParam(unit, w.code.locals.Get(w.code.params[i]));
  }
  sig += ")";

  std::string out;
  out += std::format("{} {{\n", sig);
  out += std::format(
      "  {0} {1} = static_cast<{0}>("
      "lyra::runtime::ResolveExportInstance(\"{2}\"));\n",
      self_type, self_decl.name, w.instance_name);
  out += RenderBlockStatements(body_view, 1);
  out += "}\n";
  return out;
}

auto RenderForeignExportWrappers(const mir::CompilationUnit& unit)
    -> std::string {
  std::string out;
  for (std::size_t i = 0; i < unit.classes.size(); ++i) {
    const mir::ClassId id{static_cast<std::uint32_t>(i)};
    if (!unit.classes.IsDefined(id)) continue;
    for (const mir::ForeignExportWrapper& w :
         unit.GetClass(id).foreign_export_wrappers) {
      out += "\n";
      out += RenderForeignExportWrapper(unit, unit.GetClass(id), w);
    }
  }
  return out;
}

// The include preamble every emitted unit header shares: the standard library,
// the Lyra runtime and value libraries the rendered bodies call into, and one
// include per external unit this unit references (instantiated or called), so a
// cross-unit name resolves against the other unit's emitted header.
auto RenderUnitIncludes(const mir::CompilationUnit& unit) -> std::string {
  std::string out;
  out += "#include <array>\n";
  out += "#include <cmath>\n";
  out += "#include <cstdint>\n";
  out += "#include <functional>\n";
  out += "#include <memory>\n";
  out += "#include <span>\n";
  out += "#include <stdexcept>\n";
  out += "#include <string>\n";
  out += "#include <vector>\n";
  out += "#include \"lyra/runtime/coroutine.hpp\"\n";
  out += "#include \"lyra/runtime/delay.hpp\"\n";
  out += "#include \"lyra/runtime/file_table.hpp\"\n";
  out += "#include \"lyra/runtime/finish.hpp\"\n";
  out += "#include \"lyra/runtime/fork.hpp\"\n";
  out += "#include \"lyra/runtime/ambient_run_context.hpp\"\n";
  out += "#include \"lyra/runtime/gc_ref.hpp\"\n";
  out += "#include \"lyra/runtime/named_event.hpp\"\n";
  out += "#include \"lyra/runtime/net.hpp\"\n";
  out += "#include \"lyra/runtime/process_control.hpp\"\n";
  out += "#include \"lyra/runtime/runtime_services.hpp\"\n";
  out += "#include \"lyra/runtime/scope.hpp\"\n";
  out += "#include \"lyra/runtime/sim_time.hpp\"\n";
  out += "#include \"lyra/runtime/var.hpp\"\n";
  out += "#include \"lyra/value/enum.hpp\"\n";
  out += "#include \"lyra/value/format.hpp\"\n";
  out += "#include \"lyra/value/packed_type.hpp\"\n";
  out += "#include \"lyra/value/packed.hpp\"\n";
  out += "#include \"lyra/value/packed_array.hpp\"\n";
  out += "#include \"lyra/value/packed_bitwise.hpp\"\n";
  out += "#include \"lyra/value/packed_convert.hpp\"\n";
  out += "#include \"lyra/value/packed_reduction.hpp\"\n";
  out += "#include \"lyra/value/real.hpp\"\n";
  out += "#include \"lyra/value/scan.hpp\"\n";
  out += "#include \"lyra/value/string.hpp\"\n";
  out += "#include \"lyra/value/string_op.hpp\"\n";
  out += "#include \"lyra/value/tuple.hpp\"\n";
  out += "#include \"lyra/value/unpacked_array.hpp\"\n";
  out += "#include \"lyra/value/dynamic_array.hpp\"\n";
  out += "#include \"lyra/value/union.hpp\"\n";
  for (const auto& name : CollectExternalUnitNames(unit)) {
    out += std::format("#include \"{}.hpp\"\n", ToCppName(name));
  }
  return out;
}

// The unit's named type definitions, rendered as C++ definitions: each enum in
// the unit's type universe as an `Enum<>` class under its source name. A pure
// typedef (a scalar alias, a struct alias) needs no definition -- it resolved
// to its underlying type before MIR, so it is not a distinct type here. A
// caller places the result inside the unit's C++ peer: a class's header region
// for a rooted unit, the namespace block for a rootless one.
auto RenderUnitTypeDeclarations(const mir::CompilationUnit& unit)
    -> std::string {
  std::string out;
  bool any_enum = false;
  for (std::size_t i = 0; i < unit.types.size(); ++i) {
    const mir::TypeId type_id{static_cast<std::uint32_t>(i)};
    const auto* enum_type =
        std::get_if<mir::EnumType>(&unit.types.Get(type_id).data);
    if (enum_type == nullptr) continue;
    any_enum = true;
    const auto class_name = RenderEnumClassName(unit, type_id);
    const auto& base = enum_type->base;
    out += std::format(
        "class {} final : public lyra::value::Enum<{}> {{\n", class_name,
        class_name);
    out += " public:\n";
    out += "  using Enum::Enum;\n";
    out += std::format(
        "  static inline const auto kBase = {};\n", RenderPackedType(base));
    out += "  static constexpr lyra::value::EnumMember kMembers[] = {\n";
    for (const auto& member : enum_type->members) {
      out += std::format(
          "      {{{}, {}}},\n", RenderCStringLiteral(member.name),
          member.value);
    }
    out += "  };\n";
    out += "};\n";
  }
  if (any_enum) {
    out += "\n";
  }
  return out;
}

auto RenderScopeHeaderFile(
    const mir::CompilationUnit& unit, const mir::Class& s) -> std::string {
  std::string out;
  out += "#pragma once\n";
  out += RenderUnitIncludes(unit);
  out += "\n";
  if (const std::string foreign_decls = RenderForeignImportDeclarations(unit);
      !foreign_decls.empty()) {
    out += foreign_decls;
    out += "\n";
  }
  out += RenderUnitTypeDeclarations(unit);

  // A SystemVerilog class is a free-standing registry object with no
  // structural parent, so it is not reached by the scope tree's `contained`
  // walk. Emit every non-tree-node class before the scope tree that may
  // reference it through a handle or `new`. (The scope objects -- the module
  // and its generate scopes -- are runtime tree nodes emitted by the tree
  // walk below.) The dependency-order walker inside handles base-before-
  // derived ordering.
  std::vector<bool> emitted(unit.classes.size(), false);
  for (std::size_t i = 0; i < unit.classes.size(); ++i) {
    RenderClassInDependencyOrder(
        unit, mir::ClassId{static_cast<std::uint32_t>(i)}, emitted, out);
  }

  out += RenderScopeAsClass(unit, s, 0, nullptr);
  out += RenderForeignExportWrappers(unit);
  return out;
}

// A callable the unit's namespace owns, rendered as a free function (LRM 26.3
// for a package): no receiver, no enclosing class, so its body renders against
// a classless scope view and its types resolve against the namespace's own
// declarations. `inline` because the definition sits in the header every caller
// includes.
auto RenderNamespaceCallable(
    const mir::CompilationUnit& unit, const std::string& name,
    const mir::CallableCode& code) -> std::string {
  const ScopeView body_view = ScopeView::ForNamespace(unit, code);
  const std::string ret = RenderTypeAsCpp(unit, code.result_type);
  std::string sig = std::format("inline auto {}(", name);
  for (std::size_t i = 0; i < code.params.size(); ++i) {
    if (i != 0) sig += ", ";
    sig += RenderCallableParam(unit, code.locals.Get(code.params[i]));
  }
  sig += std::format(") -> {}", ret);

  std::string out;
  out += std::format("{} {{\n", sig);
  out += RenderBlockStatements(body_view, 1);
  out += "}\n";
  return out;
}

// A unit whose root is a namespace rather than a class (a package): its C++
// peer is a namespace holding the unit's type declarations, class
// declarations (LRM 8 classes declared at package scope), and its
// receiver-less callables. It owns no runtime object, so there is no scope-
// tree construction.
auto RenderNamespaceUnitHeaderFile(const mir::CompilationUnit& unit)
    -> std::string {
  std::string out;
  out += "#pragma once\n";
  out += RenderUnitIncludes(unit);
  out += "\n";
  out += std::format("namespace {} {{\n\n", ToCppName(unit.name));
  out += RenderUnitTypeDeclarations(unit);
  // A package variable is one program-global observable cell (LRM 26.2). C++17
  // `inline` gives it a single definition across every translation unit that
  // includes the header, matching the header-only, link-by-name model the
  // namespace callables use. It is declared before the callables because the
  // synthesized `Initialize` callable, itself one of them, references it.
  for (std::size_t i = 0; i < unit.static_variables.size(); ++i) {
    const auto& var = unit.static_variables.Get(
        mir::StaticVariableId{static_cast<std::uint32_t>(i)});
    out += std::format(
        "inline {} {}{{}};\n", RenderTypeAsCpp(unit, var.type), var.name);
  }
  if (unit.static_variables.size() != 0) {
    out += "\n";
  }

  // A package class is a free-standing registry object with no structural
  // parent; a referring unit reaches it by name through the include, so it
  // must be emitted here. The dependency-order walker handles base-before-
  // derived ordering.
  std::vector<bool> emitted(unit.classes.size(), false);
  for (std::size_t i = 0; i < unit.classes.size(); ++i) {
    RenderClassInDependencyOrder(
        unit, mir::ClassId{static_cast<std::uint32_t>(i)}, emitted, out);
  }

  for (std::size_t i = 0; i < unit.callables.size(); ++i) {
    const auto& callable =
        unit.callables.Get(mir::CallableId{static_cast<std::uint32_t>(i)});
    const auto& code = std::get<mir::InternalCallable>(callable.impl).code;
    out += RenderNamespaceCallable(unit, callable.name, code);
    out += "\n";
  }
  out += std::format("}}  // namespace {}\n", ToCppName(unit.name));
  return out;
}

auto RenderHostMain(const mir::CompilationUnit& root) -> std::string {
  const auto& root_class = root.GetClass(*root.root);
  const std::string root_cpp_name = ToCppName(root_class.name);
  const std::string segment_cpp =
      RenderTypeAsCpp(root, root.builtins.hierarchy_segment);

  // Every invariant host-boundary concern -- argv parsing, engine
  // construction, bind, scheduler drive, exception mapping -- lives in
  // `RunDesignHost`. The emitter contributes only the C++ call that
  // allocates `$root`: the concrete root class, its constructor arguments
  // (parent, hierarchy segment, services). That call is composed here from
  // MIR-known parts (the root class name, the hierarchy-segment type)
  // rather than rendered from a single MIR/LIR root-construct artifact --
  // allocation is realized per backend (a JIT backend allocates a generic
  // runtime object; the C++ backend allocates its concrete class), so this
  // shim carries the C++ form. `main` is a one-line hand-off; a new
  // host-boundary concept is added to `RunDesignHost` in runtime C++,
  // never to this shim.
  std::string out;
  out += "#include <memory>\n";
  out += "\n";
  out += "#include \"lyra/runtime/scope.hpp\"\n";
  out += "#include \"lyra/runtime/simulation_entry.hpp\"\n";
  out += std::format("#include \"{}.hpp\"\n", root_cpp_name);
  out += "\n";
  out += "namespace {\n";
  out += "\n";
  out += "auto BuildRoot(lyra::runtime::RuntimeServices& services)\n";
  out += "    -> std::unique_ptr<lyra::runtime::Scope> {\n";
  out += std::format(
      "  return std::make_unique<{0}>(nullptr, {1}{{\"{2}\", {{}}}}, "
      "services);\n",
      root_cpp_name, segment_cpp, root_class.name);
  out += "}\n";
  out += "\n";
  out += "}  // namespace\n";
  out += "\n";
  out += "auto main(int argc, char** argv) -> int {\n";
  out += "  return lyra::runtime::RunDesignHost(argc, argv, &BuildRoot);\n";
  out += "}\n";
  return out;
}

}  // namespace

auto EmitCppDeclarations(const mir::CompilationUnit& unit) -> CppArtifact {
  // A rooted unit names its top class as the root and emits as that class; a
  // rootless unit has no root class and emits as a namespace of its type
  // declarations and free functions.
  return {
      .relpath = std::format("{}.hpp", ToCppName(unit.name)),
      .content = unit.root.has_value()
                     ? RenderScopeHeaderFile(unit, unit.GetClass(*unit.root))
                     : RenderNamespaceUnitHeaderFile(unit)};
}

auto EmitCppHostMain(const mir::CompilationUnit& root) -> CppArtifact {
  return {.relpath = "main.cpp", .content = RenderHostMain(root)};
}

auto EmitCpp(
    std::span<const mir::CompilationUnit> units,
    const mir::CompilationUnit& root) -> CppArtifactSet {
  CppArtifactSet set;
  for (const auto& unit : units) {
    set.files.push_back(EmitCppDeclarations(unit));
  }
  set.files.push_back(EmitCppDeclarations(root));
  set.files.push_back(EmitCppHostMain(root));
  return set;
}

}  // namespace lyra::backend::cpp
