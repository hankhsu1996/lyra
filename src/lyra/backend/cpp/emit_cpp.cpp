#include <algorithm>
#include <cstddef>
#include <format>
#include <functional>
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
  const std::string type = RenderTypeAsCpp(unit, ctor_view.Class(), field.type);
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

auto RenderMethodParam(
    const mir::CompilationUnit& unit, const mir::Class& s,
    const mir::LocalDecl& param) -> std::string {
  // Every formal is a value parameter: an `input` by value (LRM 13.5.1), a
  // `ref` / `const ref` whose `RefType` already renders as `(const) Ref<T>` so
  // the reference value carries the aliasing (LRM 13.5.2). `output` / `inout`
  // are not parameters -- they ride the completion payload.
  return std::format("{} {}", RenderTypeAsCpp(unit, s, param.type), param.name);
}

// The C++ specifier this method's dispatch role prefixes its declaration
// with: `virtual` when the method introduces a new dispatch slot on this
// class, empty otherwise; the source of virtualness for an override is the
// slot the base already declares, which the `override` suffix records
// separately.
auto VirtualPrefix(const mir::MethodDecl& m) -> std::string_view {
  if (!m.virtual_dispatch.has_value()) return "";
  if (std::holds_alternative<mir::IntroducesVirtualSlot>(*m.virtual_dispatch)) {
    return "virtual ";
  }
  return "";
}

// The trailing specifier attached after the return type when this method
// fills an inherited dispatch slot: `override` records that the base's slot
// resolves through this implementation, so a name-only compilation cannot
// silently disagree with the intended override target.
auto OverrideSuffix(const mir::MethodDecl& m) -> std::string_view {
  if (!m.virtual_dispatch.has_value()) return "";
  if (std::holds_alternative<mir::IntroducesVirtualSlot>(*m.virtual_dispatch)) {
    return "";
  }
  return " override";
}

// The renderer for a class instance method: a C++ instance member function.
// `this` is what MIR calls `self`, seeded through a one-line adapter so the
// body's expressions resolve receiver-relative references uniformly. The
// method's dispatch role decorates the declaration with `virtual` or
// `override` where applicable.
auto RenderMethod(
    const ScopeView* parent_struct_view, const mir::CompilationUnit& unit,
    const mir::Class& s, const mir::MethodDecl& m, std::size_t indent)
    -> std::string {
  const ScopeView body_view = (parent_struct_view == nullptr)
                                  ? ScopeView::ForRoot(unit, s, m.code)
                                  : parent_struct_view->WithClass(s, m.code);

  const std::string ret = RenderTypeAsCpp(unit, s, m.code.result_type);
  const mir::LocalId self_local = m.code.params[0];
  const auto& self_decl = m.code.locals.Get(self_local);
  const std::string self_type = RenderTypeAsCpp(unit, s, self_decl.type);

  std::string sig = std::format("{}auto {}(", VirtualPrefix(m), m.name);
  for (std::size_t i = 1; i < m.code.params.size(); ++i) {
    if (i != 1) sig += ", ";
    sig += RenderMethodParam(unit, s, m.code.locals.Get(m.code.params[i]));
  }
  sig += std::format(") -> {}{}", ret, OverrideSuffix(m));

  std::string out;
  out += std::format("{}{} {{\n", Indent(indent), sig);
  out += std::format(
      "{}{} {} = this;\n", Indent(indent + 1), self_type, self_decl.name);
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

  const std::string ret = RenderTypeAsCpp(unit, s, a.code.result_type);
  std::string sig = std::format("static auto {}(", a.name);
  for (std::size_t i = 0; i < a.code.params.size(); ++i) {
    if (i != 0) sig += ", ";
    sig += RenderMethodParam(unit, s, a.code.locals.Get(a.code.params[i]));
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
  const auto& ctor_code = mir::GetConstructorCode(s);
  const auto render_typed_name = [&](mir::TypeId type, std::string_view name) {
    return std::format("{} {}", RenderTypeAsCpp(unit, s, type), name);
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

auto VisibilityKeyword(mir::MethodVisibility visibility) -> std::string_view {
  switch (visibility) {
    case mir::MethodVisibility::kPublic:
      return "public";
    case mir::MethodVisibility::kInternal:
      return "private";
  }
  throw InternalError("VisibilityKeyword: unknown MethodVisibility");
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
      parent_view.WithClass(s, mir::GetConstructorCode(s)).WithBlock(c.body);
  return std::format(
      "\n{0}static constexpr {1} {2} = {3};\n", Indent(indent),
      RenderTypeAsCpp(parent_view.Unit(), s, c.type), c.name,
      RenderExpr(view, view.Expr(c.value)));
}

auto RenderScopeAsClass(
    const mir::CompilationUnit& unit, const mir::Class& s, std::size_t indent,
    const ScopeView* parent_struct_view) -> std::string {
  // `this_anchor` is bound to `s.constructor` so it doubles as the view for
  // rendering the constructor body. Children's bodies use it as their enclosing
  // class (one hop above the child).
  const ScopeView this_anchor =
      (parent_struct_view == nullptr)
          ? ScopeView::ForRoot(unit, s, mir::GetConstructorCode(s))
          : parent_struct_view->WithClass(s, mir::GetConstructorCode(s));

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

  // Each method declares its access -- a class instance method is the object's
  // public callable surface, a scope's processes and lifecycle hooks are
  // internal -- and the access specifier follows that stated visibility,
  // coalescing a run of methods that share one. The ctor lives in the same
  // method storage but is emitted separately with C++ mem-init-list syntax,
  // so it is skipped here.
  std::optional<mir::MethodVisibility> open_section;
  for (std::size_t i = 0; i < s.methods.size(); ++i) {
    const mir::MethodId mid{static_cast<std::uint32_t>(i)};
    if (mid == s.constructor.method) continue;
    const auto& sub = s.methods.Get(mid);
    if (open_section != sub.visibility) {
      open_section = sub.visibility;
      out += std::format(
          "\n{} {}:\n", Indent(indent), VisibilityKeyword(sub.visibility));
    }
    out += "\n";
    out += RenderMethod(parent_struct_view, unit, s, sub, indent + 1);
  }

  // The class's runtime-callback adapters. Each renders as a static member
  // whose address decays to a plain function pointer for the runtime
  // callback table; the class never exposes them on its public surface.
  if (!s.abi_adapters.empty()) {
    if (open_section != mir::MethodVisibility::kInternal) {
      open_section = mir::MethodVisibility::kInternal;
      out += std::format(
          "\n{} {}:\n", Indent(indent),
          VisibilityKeyword(mir::MethodVisibility::kInternal));
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

  out += std::format("{}}};\n", Indent(indent));
  return out;
}

// The `extern "C"` declarations for every DPI-C import the unit calls: each
// import's foreign symbol as a C-linkage prototype over its ABI carrier types
// (LRM 35.4 / 35.5.6). An output / inout argument crosses by pointer, so its
// carrier type gains a trailing `*` (a scalar `int` -> `int*`, a `const char*`
// string -> `const char**`); an input argument crosses by value. Emitted before
// the classes that call them so a foreign call resolves against a declared
// signature; the definitions come from the user's linked C. Empty when the unit
// declares no import.
auto RenderForeignImportDeclarations(const mir::CompilationUnit& unit)
    -> std::string {
  std::string out;
  for (std::size_t i = 0; i < unit.classes.size(); ++i) {
    const mir::ClassId id{static_cast<std::uint32_t>(i)};
    if (!unit.classes.IsDefined(id)) continue;
    for (const auto& callable : unit.GetClass(id).static_callables) {
      std::string params;
      for (std::size_t p = 0; p < callable.params.size(); ++p) {
        if (p != 0) params += ", ";
        const bool writes_back =
            support::DpiDirectionWritesBack(callable.params[p].direction);
        if (const auto* scalar = std::get_if<support::ScalarCarrier>(
                &callable.params[p].carrier)) {
          // A by-value scalar crosses by value for input, by pointer for a
          // writeback direction.
          params += std::string{DpiScalarCarrierCppType(scalar->abi)};
          if (writes_back) params += "*";
        } else {
          // A canonical vector always crosses by pointer to its chunk buffer;
          // an input is read-only (`const`).
          const auto& vec =
              std::get<support::VectorCarrier>(callable.params[p].carrier);
          if (!writes_back) params += "const ";
          params += vec.four_state ? "svLogicVecVal*" : "svBitVecVal*";
        }
      }
      out += std::format(
                 R"(extern "C" {} {}({});)",
                 DpiScalarCarrierCppType(callable.ret_abi),
                 callable.external.foreign_name, params) +
             "\n";
    }
  }
  return out;
}

auto CollectExternalUnitNames(const mir::CompilationUnit& unit)
    -> std::vector<std::string> {
  std::vector<std::string> names;
  for (const auto& t : unit.types) {
    const auto* ext = std::get_if<mir::ExternalUnitObjectType>(&t.data);
    if (ext == nullptr) {
      continue;
    }
    if (std::ranges::find(names, ext->unit_name) == names.end()) {
      names.push_back(ext->unit_name);
    }
  }
  return names;
}

// A DPI-C export's foreign-linkage entry point (LRM 35.5): a free `extern "C"`
// definition foreign code calls. It recovers the exported method's receiver
// from the running design, then runs the marshaling body -- an ordinary
// statement render, so every carrier conversion and the method call come from
// MIR. Emitted after the class it dispatches into so that class is complete.
auto RenderForeignExportWrapper(
    const mir::CompilationUnit& unit, const mir::Class& s,
    const mir::ForeignExportWrapper& w) -> std::string {
  const ScopeView body_view = ScopeView::ForRoot(unit, s, w.code);
  const mir::LocalDecl& self_decl = w.code.locals.Get(w.self_local);
  const std::string self_type = RenderTypeAsCpp(unit, s, self_decl.type);

  std::string sig = std::format(
      "extern \"C\" {} {}(", RenderTypeAsCpp(unit, s, w.code.result_type),
      w.foreign_name);
  for (std::size_t i = 0; i < w.code.params.size(); ++i) {
    if (i != 0) sig += ", ";
    sig += RenderMethodParam(unit, s, w.code.locals.Get(w.code.params[i]));
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

auto RenderScopeHeaderFile(
    const mir::CompilationUnit& unit, const mir::Class& s) -> std::string {
  std::string out;
  out += "#pragma once\n";
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
  out += "#include \"lyra/value/unpacked_array.hpp\"\n";
  out += "#include \"lyra/value/dynamic_array.hpp\"\n";
  out += "#include \"lyra/value/union.hpp\"\n";
  for (const auto& name : CollectExternalUnitNames(unit)) {
    out += std::format("#include \"{}.hpp\"\n", ToCppName(name));
  }
  out += "\n";
  if (const std::string foreign_decls = RenderForeignImportDeclarations(unit);
      !foreign_decls.empty()) {
    out += foreign_decls;
    out += "\n";
  }
  bool any_enum = false;
  for (std::size_t i = 0; i < unit.types.size(); ++i) {
    const mir::TypeId type_id{static_cast<std::uint32_t>(i)};
    const auto* enum_type =
        std::get_if<mir::EnumType>(&unit.types.Get(type_id).data);
    if (enum_type == nullptr) continue;
    any_enum = true;
    const auto class_name = RenderEnumClassName(s, type_id);
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
  // SV `typedef <target> <alias>;` -> C++ `using <alias> = <target>;`. Skip
  // aliases whose name already matches the emitted class (the first typedef
  // for a given target supplies the class name itself).
  bool any_alias = false;
  for (const auto& alias : s.type_aliases) {
    std::string target = RenderTypeAsCpp(unit, s, alias.target);
    if (alias.name == target) continue;
    out += std::format("using {} = {};\n", alias.name, target);
    any_alias = true;
  }
  if (any_alias) {
    out += "\n";
  }

  // A SystemVerilog class is a free-standing registry object with no structural
  // parent, so it is not reached by the scope tree's `contained` walk. Emit
  // every non-tree-node class before the scope tree that may reference it
  // through a handle or `new`. (The scope objects -- the module and its
  // generate scopes -- are runtime tree nodes emitted by the tree walk below.)
  //
  // C++ requires a base class to be a complete type at the point of a derived
  // class's declaration, so an intra-unit base must render before its derived
  // class. Registry order is set by the interning walk, which may reach the
  // derived class first (via `Derived h;` before `class Base` is seen), so
  // the emission order climbs each class's base chain first.
  std::vector<bool> emitted(unit.classes.size(), false);
  const std::function<void(mir::ClassId)> emit_class =
      [&](mir::ClassId id) -> void {
    if (emitted[id.value]) return;
    if (!unit.classes.IsDefined(id)) return;
    const mir::Class& cls = unit.GetClass(id);
    if (cls.is_scope_tree_node) {
      return;
    }
    if (cls.base.has_value()) {
      if (const auto* intra = std::get_if<mir::IntraUnitClassRef>(&*cls.base)) {
        emit_class(intra->class_id);
      }
    }
    if (emitted[id.value]) return;
    emitted[id.value] = true;
    out += RenderScopeAsClass(unit, cls, 0, nullptr);
    out += "\n";
  };
  for (std::size_t i = 0; i < unit.classes.size(); ++i) {
    emit_class(mir::ClassId{static_cast<std::uint32_t>(i)});
  }

  out += RenderScopeAsClass(unit, s, 0, nullptr);
  out += RenderForeignExportWrappers(unit);
  return out;
}

auto RenderHostMain(const mir::CompilationUnit& root) -> std::string {
  const auto& root_class = root.GetClass(root.root);
  const std::string root_cpp_name = ToCppName(root_class.name);
  const std::string segment_cpp =
      RenderTypeAsCpp(root, root_class, root.builtins.hierarchy_segment);

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
  const auto& root = unit.GetClass(unit.root);
  return {
      .relpath = std::format("{}.hpp", ToCppName(root.name)),
      .content = RenderScopeHeaderFile(unit, root)};
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
