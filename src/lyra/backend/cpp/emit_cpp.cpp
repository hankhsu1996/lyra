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
#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/scope_view.hpp"
#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/mir/base_contract.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/field.hpp"
#include "lyra/mir/param.hpp"
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

auto RenderParamField(
    const mir::CompilationUnit& unit, const mir::Class& owner_class,
    const mir::ParamDecl& param, std::size_t indent) -> std::string {
  return std::format(
      "{}const {} {};\n", Indent(indent),
      RenderTypeAsCpp(unit, owner_class, param.type), param.name);
}

auto CtorParamName(std::size_t index) -> std::string {
  return std::format("param{}", index);
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

// The C++ name of a runtime-base virtual. The mapping from the resolved
// override reference to the emitted token lives only here, so the override
// relation stays a declaration reference everywhere else.
auto RuntimeVirtualName(mir::RuntimeMethod method) -> std::string_view {
  switch (method) {
    case mir::RuntimeMethod::kResolve:
      return "ResolveState";
    case mir::RuntimeMethod::kInitialize:
      return "InitializeState";
    case mir::RuntimeMethod::kActivate:
      return "CreateProcesses";
    case mir::RuntimeMethod::kTimePrecisionPower:
      return "TimePrecisionPower";
    case mir::RuntimeMethod::kDefName:
      return "DefName";
  }
  throw InternalError("RuntimeVirtualName: unknown RuntimeMethod");
}

// Whether a method's receiver is a read-only borrow (an immutable `&self`),
// read off `self`'s own parameter type. The C++ override shim is `const`
// exactly when its receiver is read-only.
auto ReceiverIsReadOnly(
    const mir::CompilationUnit& unit, const mir::MethodDecl& m) -> bool {
  if (m.code.params.empty()) {
    return false;
  }
  const auto& self_type =
      unit.types.Get(m.code.locals.Get(m.code.params[0]).type).data;
  const auto* ptr = std::get_if<mir::PointerType>(&self_type);
  return ptr != nullptr && ptr->mutability == mir::Mutability::kReadOnly;
}

// The one renderer for every callable body. A body renders uniformly as a
// static function over the explicit receiver `self`: the result type (whose
// `void` and coroutine cases are ordinary types, handled in `RenderTypeAsCpp`),
// the name, every parameter (`self` is `params[0]`, rendered like any other),
// and the body (a plain statement render, including any `co_return` that is
// itself a body statement). A method that overrides a runtime-base hook also
// emits the thin virtual shim the engine dispatches through; it forwards to the
// static body over `this`, the same plumbing the constructor uses for `init`.
auto RenderMethod(
    const ScopeView* parent_struct_view, const mir::CompilationUnit& unit,
    const mir::Class& s, const mir::MethodDecl& m, std::size_t indent)
    -> std::string {
  const ScopeView body_view = (parent_struct_view == nullptr)
                                  ? ScopeView::ForRoot(unit, s, m.code)
                                  : parent_struct_view->WithClass(s, m.code);

  std::string ret = RenderTypeAsCpp(unit, s, m.code.result_type);

  std::string sig = std::format("static auto {}(", m.name);
  for (std::size_t i = 0; i < m.code.params.size(); ++i) {
    if (i != 0) sig += ", ";
    sig += RenderMethodParam(unit, s, m.code.locals.Get(m.code.params[i]));
  }
  sig += std::format(") -> {}", ret);

  std::string out;
  out += std::format("{}{} {{\n", Indent(indent), sig);
  out += RenderBlockStatements(body_view, indent + 1);
  out += std::format("{}}}\n", Indent(indent));
  if (m.overrides.has_value()) {
    const std::string_view hook = std::visit(
        [](const mir::RuntimeLibraryMethodRef& ref) {
          return RuntimeVirtualName(ref.method);
        },
        *m.overrides);
    // A void hook discards; a value hook returns. `return <void-expr>;` is
    // legal in a void function, so the same forward serves both with no branch.
    // The shim is `const` exactly when its receiver is a read-only borrow.
    out += std::format(
        "{}{} {}(){} override {{ return {}(this); }}\n", Indent(indent), ret,
        hook, ReceiverIsReadOnly(unit, m) ? " const" : "", m.name);
  }
  return out;
}

// Joins string parts with ", " for emission of ctor sig args, base init
// forwards, and member-init list entries. Local to RenderConstructor; the
// rest of the file does not yet need a shared helper.
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
    const ScopeView& scope_view, const mir::Class& s,
    const std::string& base_class, std::size_t indent) -> std::string {
  // The render iterates every ctor entry in MIR-stated order without
  // switching on what each type means: ctor_prefix_params forward to the
  // base by name, structural params bind to same-named fields.
  // Each type goes through the single RenderTypeAsCpp dispatch, so the
  // C++ form of every type lives in exactly one place. Base forwarding is
  // plain pass-through (no std::move) -- C++ may copy a HierarchySegment
  // once at construction, which the iteration-time budget can absorb.
  const auto& unit = scope_view.Unit();
  const auto render_typed_name = [&](mir::TypeId type, std::string_view name) {
    return std::format("{} {}", RenderTypeAsCpp(unit, s, type), name);
  };

  std::vector<std::string> sig_args;
  std::vector<std::string> base_forwards;
  std::vector<std::string> field_inits;
  sig_args.reserve(s.ctor_prefix_params.size() + s.params.size());
  base_forwards.reserve(s.ctor_prefix_params.size());
  field_inits.reserve(s.params.size());

  for (std::size_t i = 0; i < s.ctor_prefix_params.size(); ++i) {
    const auto& p =
        s.ctor_prefix_params.Get(mir::ParamId{static_cast<std::uint32_t>(i)});
    sig_args.push_back(render_typed_name(p.type, p.name));
    base_forwards.push_back(p.name);
  }
  for (std::size_t i = 0; i < s.params.size(); ++i) {
    const auto& p = s.params.Get(mir::ParamId{static_cast<std::uint32_t>(i)});
    const auto param_name = CtorParamName(i);
    sig_args.push_back(render_typed_name(p.type, param_name));
    field_inits.push_back(std::format("{}({})", p.name, param_name));
  }

  std::vector<std::string> init_parts;
  init_parts.reserve(1 + field_inits.size());
  if (!s.ctor_prefix_params.empty()) {
    init_parts.push_back(
        std::format("{}({})", base_class, JoinCommaSeparated(base_forwards)));
  }
  for (auto& f : field_inits) {
    init_parts.push_back(std::move(f));
  }

  const std::string sig =
      init_parts.empty()
          ? std::format("{}({})", s.name, JoinCommaSeparated(sig_args))
          : std::format(
                "{}({}) : {}", s.name, JoinCommaSeparated(sig_args),
                JoinCommaSeparated(init_parts));

  // The C++ constructor is the one shape that is not an ordinary method:
  // it has no result type and runs a member-init list before its body, so
  // it cannot run a virtual override during construction. It is an
  // allocation shell that forwards to the base and the fields, then
  // kicks off the body -- a static worker over `self`, the same shape
  // every method uses. An empty body needs no kickoff.
  if (s.constructor.body.root_stmts.empty()) {
    return std::format("{}{} {{}}\n", Indent(indent), sig);
  }
  return std::format(
      "{0}{1} {{ init(this); }}\n"
      "{0}static auto init({2}* self) -> void {{\n"
      "{3}"
      "{0}}}\n",
      Indent(indent), sig, s.name,
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

auto RenderScopeAsClass(
    const mir::CompilationUnit& unit, const mir::Class& s, std::size_t indent,
    const ScopeView* parent_struct_view) -> std::string {
  // `this_anchor` is bound to `s.constructor` so it doubles as the view for
  // rendering the constructor body. Children's bodies use it as their enclosing
  // class (one hop above the child).
  const ScopeView this_anchor =
      (parent_struct_view == nullptr)
          ? ScopeView::ForRoot(unit, s, s.constructor)
          : parent_struct_view->WithClass(s, s.constructor);

  const std::optional<mir::BaseContract> base =
      s.base.has_value()
          ? std::optional{mir::ResolveBaseContract(unit, *s.base)}
          : std::nullopt;

  std::string out;
  out += Indent(indent) + "class " + s.name + " final";
  if (base.has_value()) {
    out += " : public " + RenderTypeAsCpp(unit, s, base->renderable);
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

  // A tree node forwards to its runtime base; a plain object (a class) has no
  // base but still runs its constructor body to initialize its members. Either
  // way the constructor is emitted when there is a base to forward to or a body
  // to run; a baseless object with an empty body keeps the implicit default.
  if (base.has_value()) {
    out += RenderConstructor(
        this_anchor, s, RenderTypeAsCpp(unit, s, base->renderable), indent + 1);
  } else if (!s.constructor.body.root_stmts.empty()) {
    out += RenderConstructor(this_anchor, s, "", indent + 1);
  }

  // Members follow the constructor and methods. They are public so cross-unit
  // references can reach them directly.
  if (!s.params.empty() || !s.fields.empty()) {
    out += "\n";
  }
  for (const auto& p : s.params) {
    out += RenderParamField(unit, s, p, indent + 1);
  }
  if (!s.params.empty() && !s.fields.empty()) {
    out += "\n";
  }
  out += RenderFieldList(this_anchor, s.fields, indent + 1);

  if (s.methods.empty()) {
    out += std::format("{}}};\n", Indent(indent));
    return out;
  }

  // Each method declares its access -- a class instance method is the object's
  // public callable surface, a scope's processes and lifecycle hooks are
  // internal -- and the access specifier follows that stated visibility,
  // coalescing a run of methods that share one.
  std::optional<mir::MethodVisibility> open_section;
  for (const auto& sub : s.methods) {
    if (open_section != sub.visibility) {
      open_section = sub.visibility;
      out += std::format(
          "\n{} {}:\n", Indent(indent), VisibilityKeyword(sub.visibility));
    }
    out += "\n";
    out += RenderMethod(parent_struct_view, unit, s, sub, indent + 1);
  }

  out += std::format("{}}};\n", Indent(indent));
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
    out += std::format("#include \"{}.hpp\"\n", name);
  }
  out += "\n";
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
  for (std::size_t i = 0; i < unit.classes.size(); ++i) {
    const mir::ClassId id{static_cast<std::uint32_t>(i)};
    if (!unit.classes.IsDefined(id)) continue;
    const mir::Class& cls = unit.GetClass(id);
    if (cls.base.has_value() &&
        mir::ResolveBaseContract(unit, *cls.base).is_runtime_tree_node) {
      continue;
    }
    out += RenderScopeAsClass(unit, cls, 0, nullptr);
    out += "\n";
  }

  out += RenderScopeAsClass(unit, s, 0, nullptr);
  return out;
}

auto RenderHostMain(std::span<const TopInstance> tops) -> std::string {
  std::string out;
  out += "#include <vector>\n";
  out += "\n";
  out += "#include \"lyra/runtime/engine.hpp\"\n";
  out += "#include \"lyra/runtime/simulation_entry.hpp\"\n";
  for (const auto& top : tops) {
    out += std::format(
        "#include \"{}.hpp\"\n", top.unit->GetClass(top.unit->root).name);
  }
  out += "\n";
  out += "auto main() -> int {\n";
  out += "  lyra::runtime::Engine engine;\n";
  for (std::size_t i = 0; i < tops.size(); ++i) {
    // A top instance's structural identity is fixed at this construction
    // site: the segment carries the top's source name with no per-dimension
    // indices, which $root then reads back when binding the design. The
    // segment type name comes from the canonical RenderTypeAsCpp mapping
    // so this harness file does not repeat a type literal already owned
    // by MIR's builtin TypeId table.
    const auto& unit = *tops[i].unit;
    const auto& top_class = unit.GetClass(unit.root);
    const std::string segment_cpp =
        RenderTypeAsCpp(unit, top_class, unit.builtins.hierarchy_segment);
    out += std::format(
        "  {0} top{1}{{nullptr, {2}{{\"{3}\", {{}}}}, engine.Services()}};\n",
        top_class.name, i, segment_cpp, tops[i].name);
  }
  out += "  std::vector<lyra::runtime::TopBinding> tops = {\n";
  for (std::size_t i = 0; i < tops.size(); ++i) {
    out += std::format("      {{&top{}}},\n", i);
  }
  out += "  };\n";
  out += "  engine.BindDesign(tops);\n";
  out += "  return lyra::runtime::RunSimulation(engine);\n";
  out += "}\n";
  return out;
}

}  // namespace

auto EmitCppDeclarations(const mir::CompilationUnit& unit) -> CppArtifact {
  const auto& root = unit.GetClass(unit.root);
  return {
      .relpath = std::format("{}.hpp", root.name),
      .content = RenderScopeHeaderFile(unit, root)};
}

auto EmitCppHostMain(std::span<const TopInstance> tops) -> CppArtifact {
  return {.relpath = "main.cpp", .content = RenderHostMain(tops)};
}

auto EmitCpp(
    std::span<const mir::CompilationUnit> units,
    std::span<const TopInstance> tops) -> CppArtifactSet {
  CppArtifactSet set;
  for (const auto& unit : units) {
    set.files.push_back(EmitCppDeclarations(unit));
  }
  set.files.push_back(EmitCppHostMain(tops));
  return set;
}

}  // namespace lyra::backend::cpp
