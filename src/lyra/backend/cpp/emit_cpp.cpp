#include <algorithm>
#include <cstddef>
#include <format>
#include <span>
#include <string>
#include <string_view>
#include <utility>
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
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/param.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderField(
    const ScopeView& ctor_view, const mir::MemberDecl& var, std::size_t indent)
    -> diag::Result<std::string> {
  const auto& unit = ctor_view.Unit();
  auto type_or = RenderTypeAsCpp(unit, ctor_view.Class(), var.type);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  // An upward reference is an ExternUp member: its constructor takes the
  // symbol payload (ancestor, by-name tail, leaf signal) rather than a value
  // initializer; it registers itself and relocates in the resolve phase.
  if (const auto* er =
          std::get_if<mir::ExternalRefType>(&unit.GetType(var.type).data)) {
    std::string tail = "{";
    for (std::size_t i = 0; i < er->tail.size(); ++i) {
      if (i != 0) tail += ", ";
      tail += "{\"" + er->tail[i].name + "\", {";
      for (std::size_t j = 0; j < er->tail[i].indices.size(); ++j) {
        if (j != 0) tail += ", ";
        tail += std::to_string(er->tail[i].indices[j]);
      }
      tail += "}}";
    }
    tail += "}";
    const std::string match = er->match == mir::ExternalRefMatch::kDefName
                                  ? "lyra::runtime::UpwardMatch::kDefName"
                                  : "lyra::runtime::UpwardMatch::kScopeName";
    return Indent(indent) + *type_or + " " + var.name + "{this, \"" +
           er->ancestor + "\", " + match + ", " + tail + ", \"" + er->signal +
           "\"};\n";
  }
  return Indent(indent) + *type_or + " " + var.name + "{};\n";
}

auto RenderParamField(
    const mir::CompilationUnit& unit, const mir::Class& owner_class,
    const mir::ParamDecl& param, std::size_t indent)
    -> diag::Result<std::string> {
  auto type_or = RenderTypeAsCpp(unit, owner_class, param.type);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  return Indent(indent) + "const " + *type_or + " " + param.name + ";\n";
}

auto CtorParamName(std::size_t index) -> std::string {
  return "param" + std::to_string(index);
}

auto RenderMethodParam(
    const mir::CompilationUnit& unit, const mir::Class& s,
    const mir::MethodParam& param) -> diag::Result<std::string> {
  auto type_or = RenderTypeAsCpp(unit, s, param.type);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  // Every formal is a value parameter: an `input` by value (LRM 13.5.1), a
  // `ref` / `const ref` whose `RefType` already renders as `(const) Ref<T>` so
  // the reference value carries the aliasing (LRM 13.5.2). `output` / `inout`
  // are not parameters -- they ride the completion payload.
  return *type_or + " " + param.name;
}

// The one renderer for every method. A method's declaration is rendered from
// its fields: the result type (whose `void` and coroutine cases are ordinary
// types, handled in `RenderTypeAsCpp`), the name, the parameters, and the body
// (a plain statement render, including any `co_return` that is itself a body
// statement). The `form` decides how the receiver `self` is bound: a `kStatic`
// method takes it as an explicit first parameter and spells it `self`; a
// `kVirtual` method takes it implicitly, spells it `this`, and overrides a
// runtime-base slot.
auto RenderMethod(
    const ScopeView* parent_struct_view, const mir::CompilationUnit& unit,
    const mir::Class& s, const mir::MethodDecl& m, std::size_t indent)
    -> diag::Result<std::string> {
  const bool is_static = m.form == mir::MethodForm::kStatic;
  const ScopeView base_view =
      (parent_struct_view == nullptr)
          ? ScopeView::ForRoot(unit, s, m.root_block)
          : parent_struct_view->WithClass(s, m.root_block);
  const ScopeView body_view =
      base_view.WithSelfSpelling(is_static ? "self" : "this");

  auto ret_or = RenderTypeAsCpp(unit, s, m.result_type);
  if (!ret_or) return std::unexpected(std::move(ret_or.error()));

  std::string sig =
      std::string(is_static ? "static auto " : "auto ") + m.name + "(";
  bool first = true;
  if (is_static) {
    sig += s.name + "* self";
    first = false;
  }
  for (const auto& param : m.params) {
    auto param_or = RenderMethodParam(unit, s, param);
    if (!param_or) return std::unexpected(std::move(param_or.error()));
    if (!first) sig += ", ";
    sig += *param_or;
    first = false;
  }
  sig += ") -> " + *ret_or;
  if (!is_static) {
    sig += " override";
  }

  std::string out;
  out += Indent(indent) + sig + " {\n";
  auto body_or = RenderBlockStatements(body_view, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  out += *body_or;
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderConstructor(
    const ScopeView& scope_view, const mir::Class& s,
    const std::string& base_class, std::size_t indent)
    -> diag::Result<std::string> {
  std::string sig = s.name +
                    "(lyra::runtime::Scope* parent, std::string name, "
                    "lyra::runtime::RuntimeServices& services";
  std::string init_list = base_class + "(parent, std::move(name), services)";
  for (std::size_t i = 0; i < s.params.size(); ++i) {
    const auto& p = s.params.Get(mir::ParamId{static_cast<std::uint32_t>(i)});
    const auto param_name = CtorParamName(i);
    auto type_or = RenderTypeAsCpp(scope_view.Unit(), s, p.type);
    if (!type_or) return std::unexpected(std::move(type_or.error()));
    sig += ", " + *type_or + " " + param_name;
    init_list += ", " + p.name + "(" + param_name + ")";
  }
  sig += ") : " + init_list;

  // The C++ constructor is the one shape that is not an ordinary method: it has
  // no result type and runs a member-init list before its body, so it cannot
  // run a virtual override during construction. It is an allocation shell that
  // forwards to the base and the param members, then kicks off the construction
  // body -- a static worker over `self`, the same body shape every method uses.
  // An empty body needs no kickoff.
  if (s.constructor_block.root_stmts.empty()) {
    return Indent(indent) + sig + " {}\n";
  }
  std::string out;
  out += Indent(indent) + sig + " { init(this); }\n";
  out += Indent(indent) + "static auto init(" + s.name + "* self) -> void {\n";
  auto body_or = RenderBlockStatements(scope_view, indent + 1);
  if (!body_or) return std::unexpected(std::move(body_or.error()));
  out += *body_or;
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderProcessKindLiteral(mir::ProcessKind kind) -> std::string {
  switch (kind) {
    case mir::ProcessKind::kInitial:
      return "lyra::runtime::ProcessKind::kInitial";
    case mir::ProcessKind::kFinal:
      return "lyra::runtime::ProcessKind::kFinal";
  }
  throw InternalError("RenderProcessKindLiteral: unknown ProcessKind");
}

// Process activation: the constructor-time registration of each process body
// with the engine, by lifecycle kind. The bodies themselves are ordinary
// methods rendered by `RenderMethod`; this only wires their registration.
auto RenderCreateProcesses(const mir::Class& s, std::size_t indent)
    -> std::string {
  std::string out;
  out += Indent(indent) + "void CreateProcesses() override {\n";
  for (const auto& p : s.processes) {
    out += Indent(indent + 1) + "AddProcess(" +
           RenderProcessKindLiteral(p.kind) + ", " + p.code.name + "(this));\n";
  }
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderScopeAsClass(
    const mir::CompilationUnit& unit, const mir::Class& s, std::size_t indent,
    bool is_top_level, const ScopeView* parent_struct_view)
    -> diag::Result<std::string> {
  // `this_anchor` is bound to `s.constructor_block` so it doubles as the
  // view for rendering the constructor body. Children's bodies use it as
  // their enclosing class (one hop above the child).
  const ScopeView this_anchor =
      (parent_struct_view == nullptr)
          ? ScopeView::ForRoot(unit, s, s.constructor_block)
          : parent_struct_view->WithClass(s, s.constructor_block);

  // A unit-root scope is an instance; a nested generate block is a generate
  // scope. The base type carries the kind.
  const std::string base_class =
      is_top_level ? "lyra::runtime::Instance" : "lyra::runtime::GenScope";

  std::string out;
  out += Indent(indent) + "class " + s.name + " final : public " + base_class +
         " {\n";
  out += Indent(indent) + " public:\n";

  // The scope's own time precision (LRM 3.14.2). The engine takes the minimum
  // across the tree as the design-global tick (LRM 3.14.3); the runtime scales
  // a local-precision delay from this value to that tick.
  out += Indent(indent + 1) +
         "auto TimePrecisionPower() const -> std::int8_t override { return " +
         std::to_string(static_cast<int>(s.time_resolution.precision_power)) +
         "; }\n\n";

  for (const auto& child : s.nested_classes) {
    auto child_or =
        RenderScopeAsClass(unit, child, indent + 1, false, &this_anchor);
    if (!child_or) return std::unexpected(std::move(child_or.error()));
    out += *child_or;
  }
  if (!s.nested_classes.empty()) {
    out += "\n";
  }

  auto ctor_or = RenderConstructor(this_anchor, s, base_class, indent + 1);
  if (!ctor_or) return std::unexpected(std::move(ctor_or.error()));
  out += *ctor_or;

  // The resolve and initialize phases run after construction; each is a
  // virtual override the engine dispatches, present only when the scope has
  // work for that phase. They render through the one method renderer.
  if (s.resolve.has_value()) {
    out += "\n";
    auto body_or =
        RenderMethod(parent_struct_view, unit, s, *s.resolve, indent + 1);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    out += *body_or;
  }
  if (s.initialize.has_value()) {
    out += "\n";
    auto body_or =
        RenderMethod(parent_struct_view, unit, s, *s.initialize, indent + 1);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    out += *body_or;
  }

  // Child links are registered automatically at construction and walked by the
  // base; only a scope's own processes need a per-class override.
  if (!s.processes.empty()) {
    out += "\n";
    out += RenderCreateProcesses(s, indent + 1);
  }

  // A unit-root scope is a module; its name is the def-name an upward reference
  // matches when climbing the parent chain (LRM 23.8).
  if (is_top_level) {
    out += Indent(indent + 1) +
           "auto DefName() const -> std::string_view override { return \"" +
           s.name + "\"; }\n";
  }

  // Members follow the constructor and methods. They are public so cross-unit
  // references can reach them directly.
  if (!s.params.empty() || !s.members.empty()) {
    out += "\n";
  }
  for (const auto& p : s.params) {
    auto field_or = RenderParamField(unit, s, p, indent + 1);
    if (!field_or) return std::unexpected(std::move(field_or.error()));
    out += *field_or;
  }
  if (!s.params.empty() && !s.members.empty()) {
    out += "\n";
  }
  for (const auto& v : s.members) {
    auto field_or = RenderField(this_anchor, v, indent + 1);
    if (!field_or) return std::unexpected(std::move(field_or.error()));
    out += *field_or;
  }

  if (s.processes.empty() && s.methods.empty()) {
    out += Indent(indent) + "};\n";
    return out;
  }

  out += "\n";
  out += Indent(indent) + " private:\n";

  for (const auto& p : s.processes) {
    out += "\n";
    auto body_or =
        RenderMethod(parent_struct_view, unit, s, p.code, indent + 1);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    out += *body_or;
  }

  for (const auto& sub : s.methods) {
    out += "\n";
    auto body_or = RenderMethod(parent_struct_view, unit, s, sub, indent + 1);
    if (!body_or) return std::unexpected(std::move(body_or.error()));
    out += *body_or;
  }

  out += Indent(indent) + "};\n";
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
    const mir::CompilationUnit& unit, const mir::Class& s)
    -> diag::Result<std::string> {
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
  out += "#include \"lyra/runtime/extern_up.hpp\"\n";
  out += "#include \"lyra/runtime/file_table.hpp\"\n";
  out += "#include \"lyra/runtime/finish.hpp\"\n";
  out += "#include \"lyra/runtime/fork.hpp\"\n";
  out += "#include \"lyra/runtime/named_event.hpp\"\n";
  out += "#include \"lyra/runtime/process_kind.hpp\"\n";
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
  for (const auto& name : CollectExternalUnitNames(unit)) {
    out += "#include \"" + name + ".hpp\"\n";
  }
  out += "\n";
  bool any_enum = false;
  for (std::size_t i = 0; i < unit.types.size(); ++i) {
    const auto* enum_type = std::get_if<mir::EnumType>(&unit.types[i].data);
    if (enum_type == nullptr) continue;
    any_enum = true;
    const auto class_name =
        RenderEnumClassName(s, mir::TypeId{static_cast<std::uint32_t>(i)});
    const auto& base = enum_type->base;
    const char* signed_lit =
        base.signedness == mir::Signedness::kSigned ? "true" : "false";
    const char* four_state_lit =
        base.atom != mir::BitAtom::kBit ? "true" : "false";
    out += std::format(
        "class {} final : public lyra::value::Enum<{}> {{\n", class_name,
        class_name);
    out += " public:\n";
    out += "  using Enum::Enum;\n";
    out += std::format(
        "  static constexpr lyra::value::PackedType kBase{{.width = {}, "
        ".is_signed = {}, .is_four_state = {}}};\n",
        base.BitWidth(), signed_lit, four_state_lit);
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
    auto target_or = RenderTypeAsCpp(unit, s, alias.target);
    if (!target_or) return std::unexpected(std::move(target_or.error()));
    if (alias.name == *target_or) continue;
    out += std::format("using {} = {};\n", alias.name, *target_or);
    any_alias = true;
  }
  if (any_alias) {
    out += "\n";
  }
  auto class_or = RenderScopeAsClass(unit, s, 0, true, nullptr);
  if (!class_or) return std::unexpected(std::move(class_or.error()));
  out += *class_or;
  return out;
}

auto RenderHostMain(std::span<const TopInstance> tops) -> std::string {
  std::string out;
  out += "#include <vector>\n";
  out += "\n";
  out += "#include \"lyra/runtime/engine.hpp\"\n";
  out += "#include \"lyra/runtime/simulation_entry.hpp\"\n";
  for (const auto& top : tops) {
    out += "#include \"" + top.unit->top_class.name + ".hpp\"\n";
  }
  out += "\n";
  out += "auto main() -> int {\n";
  out += "  lyra::runtime::Engine engine;\n";
  for (std::size_t i = 0; i < tops.size(); ++i) {
    out += "  " + tops[i].unit->top_class.name + " top" + std::to_string(i) +
           "{nullptr, \"" + tops[i].name + "\", engine.Services()};\n";
  }
  out += "  std::vector<lyra::runtime::TopBinding> tops = {\n";
  for (std::size_t i = 0; i < tops.size(); ++i) {
    out += "      {&top" + std::to_string(i) + "},\n";
  }
  out += "  };\n";
  out += "  engine.BindDesign(tops);\n";
  out += "  return lyra::runtime::RunSimulation(engine);\n";
  out += "}\n";
  return out;
}

}  // namespace

auto EmitCppDeclarations(const mir::CompilationUnit& unit)
    -> diag::Result<std::vector<CppArtifact>> {
  std::vector<CppArtifact> headers;
  const auto& root = unit.top_class;
  auto content_or = RenderScopeHeaderFile(unit, root);
  if (!content_or) return std::unexpected(std::move(content_or.error()));
  headers.push_back(
      {.relpath = root.name + ".hpp", .content = *std::move(content_or)});
  return headers;
}

auto EmitCppHostMain(std::span<const TopInstance> tops) -> CppArtifact {
  return {.relpath = "main.cpp", .content = RenderHostMain(tops)};
}

auto EmitCpp(
    std::span<const mir::CompilationUnit> units,
    std::span<const TopInstance> tops) -> diag::Result<CppArtifactSet> {
  CppArtifactSet set;
  for (const auto& unit : units) {
    auto headers_or = EmitCppDeclarations(unit);
    if (!headers_or) return std::unexpected(std::move(headers_or.error()));
    for (auto& h : *headers_or) {
      set.files.push_back(std::move(h));
    }
  }
  set.files.push_back(EmitCppHostMain(tops));
  return set;
}

}  // namespace lyra::backend::cpp
