#include <algorithm>
#include <cstddef>
#include <format>
#include <span>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/backend/cpp/render_expr.hpp"
#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/backend/cpp/string_literal.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/diag/diagnostic.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/structural_param.hpp"
#include "lyra/mir/structural_scope.hpp"
#include "lyra/mir/structural_var.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderField(
    const RenderContext& ctor_ctx, const mir::StructuralVarDecl& var,
    std::size_t indent) -> diag::Result<std::string> {
  const auto& unit = ctor_ctx.Unit();
  auto type_or = RenderTypeAsCpp(unit, ctor_ctx.StructuralScope(), var.type);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  // An owned-child member (a unique_ptr, or a vector of them) is filled in the
  // constructor body; the field itself is just default-constructed storage, so
  // it needs no initializer.
  if (mir::GetOwnedChildLeaf(unit, var.type).has_value()) {
    return Indent(indent) + *type_or + " " + var.name + ";\n";
  }
  auto value_expr_or = RenderExpr(ctor_ctx, ctor_ctx.Expr(var.initializer));
  if (!value_expr_or) {
    return std::unexpected(std::move(value_expr_or.error()));
  }
  // Var<T> forwards its single payload through a variadic-of-1 ctor to T's
  // copy ctor. Plain T uses copy-init because `T n(args);` at class scope
  // would parse as a member-function declaration.
  if (IsObservableScalarType(unit.GetType(var.type))) {
    return Indent(indent) + "lyra::runtime::Var<" + *type_or + "> " + var.name +
           "{" + *value_expr_or + "};\n";
  }
  return Indent(indent) + *type_or + " " + var.name + " = " + *value_expr_or +
         ";\n";
}

auto RenderParamField(
    const mir::CompilationUnit& unit, const mir::StructuralScope& owner_scope,
    const mir::StructuralParamDecl& param, std::size_t indent)
    -> diag::Result<std::string> {
  auto type_or = RenderTypeAsCpp(unit, owner_scope, param.type);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  return Indent(indent) + "const " + *type_or + " " + param.name + ";\n";
}

auto CtorParamName(std::size_t index) -> std::string {
  return "param" + std::to_string(index);
}

auto RenderConstructor(
    const RenderContext& scope_ctx, const mir::StructuralScope& s,
    const std::string& base_class, std::size_t indent)
    -> diag::Result<std::string> {
  std::string sig = s.name + "(lyra::runtime::Scope* parent, std::string name";
  std::string init_list = base_class + "(parent, std::move(name))";
  for (std::size_t i = 0; i < s.structural_params.size(); ++i) {
    const auto& p = s.structural_params[i];
    const auto param_name = CtorParamName(i);
    auto type_or = RenderTypeAsCpp(scope_ctx.Unit(), s, p.type);
    if (!type_or) return std::unexpected(std::move(type_or.error()));
    sig += ", " + *type_or + " " + param_name;
    init_list += ", " + p.name + "(" + param_name + ")";
  }
  sig += ") : " + init_list;

  if (s.constructor_scope.root_stmts.empty()) {
    return Indent(indent) + sig + " {}\n";
  }
  std::string out;
  out += Indent(indent) + sig + " {\n";
  auto rendered_or = RenderProceduralScopeStatements(scope_ctx, indent + 1);
  if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
  out += *rendered_or;
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderProcessMethodName(std::size_t index) -> std::string {
  return "process_" + std::to_string(index);
}

auto RenderProcessMethod(
    const RenderContext* parent_struct_ctx, const mir::CompilationUnit& unit,
    const mir::StructuralScope& s, const mir::Process& process,
    std::size_t index, std::size_t indent) -> diag::Result<std::string> {
  std::string out;
  out += Indent(indent) + "auto " + RenderProcessMethodName(index) +
         "() -> lyra::runtime::Coroutine {\n";
  const RenderContext proc_ctx =
      ((parent_struct_ctx == nullptr)
           ? RenderContext::ForRoot(unit, s, process.root_procedural_scope)
           : parent_struct_ctx->WithStructuralScope(
                 s, process.root_procedural_scope))
          .WithCoroutine(true);
  auto rendered_or = RenderProceduralScopeStatements(proc_ctx, indent + 1);
  if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
  out += *rendered_or;
  out += Indent(indent + 1) + "co_return;\n";
  out += Indent(indent) + "}\n";
  return out;
}

// A value-returning function renders its result type; a void function or task
// renders C++ `void`.
auto RenderSubroutineResultType(
    const mir::CompilationUnit& unit, const mir::StructuralScope& s,
    mir::TypeId result_type) -> diag::Result<std::string> {
  if (std::holds_alternative<mir::VoidType>(unit.GetType(result_type).data)) {
    return std::string{"void"};
  }
  return RenderTypeAsCpp(unit, s, result_type);
}

auto RenderSubroutineMethod(
    const RenderContext* parent_struct_ctx, const mir::CompilationUnit& unit,
    const mir::StructuralScope& s, const mir::StructuralSubroutineDecl& sub,
    std::size_t indent) -> diag::Result<std::string> {
  // A task may suspend on timing controls, so it is a coroutine enabled with
  // `co_await`; a function executes in zero time and renders its result type
  // (LRM 13.3 / 13.4).
  const bool is_task = sub.kind == mir::SubroutineKind::kTask;
  std::string return_type;
  if (is_task) {
    return_type = "lyra::runtime::Coroutine";
  } else {
    auto result_or = RenderSubroutineResultType(unit, s, sub.result_type);
    if (!result_or) return std::unexpected(std::move(result_or.error()));
    return_type = *std::move(result_or);
  }

  std::string sig = return_type + " " + sub.name + "(";
  for (std::size_t i = 0; i < sub.params.size(); ++i) {
    const auto& param = sub.params[i];
    auto type_or = RenderTypeAsCpp(unit, s, param.type);
    if (!type_or) return std::unexpected(std::move(type_or.error()));
    if (i != 0) sig += ", ";
    // An `input` formal is a by-value copy (LRM 13.5.1). An `output` / `inout`
    // formal binds to the caller's writeback temp by reference (`T&`) so body
    // writes flow back at the copy-out. A `ref` / `const ref` formal aliases
    // the actual's cell through a `Ref<T>` so writes route through that cell's
    // update-event path (LRM 13.5.2); `const ref` is `const Ref<T>`, which
    // makes `Set` ill-formed and enforces the read-only promise.
    switch (param.direction) {
      case mir::ParamDirection::kInput:
        sig += *type_or + " " + param.name;
        break;
      case mir::ParamDirection::kOutput:
      case mir::ParamDirection::kInOut:
        sig += *type_or + "& " + param.name;
        break;
      case mir::ParamDirection::kRef:
        sig += "lyra::runtime::Ref<" + *type_or + "> " + param.name;
        break;
      case mir::ParamDirection::kConstRef:
        sig += "const lyra::runtime::Ref<" + *type_or + "> " + param.name;
        break;
    }
  }
  sig += ")";

  const std::string frame_field =
      sub.static_locals.empty() ? std::string{} : sub.name + "__static";
  const RenderContext sub_ctx =
      (parent_struct_ctx == nullptr
           ? RenderContext::ForRoot(unit, s, sub.root_procedural_scope)
           : parent_struct_ctx->WithStructuralScope(
                 s, sub.root_procedural_scope))
          .WithStaticFrame(frame_field)
          .WithCoroutine(is_task);

  std::string out;
  // LRM 13.3.1 static locals: one per-instance frame member, default-evaluated
  // once at construction; the body reads and writes it across activations.
  if (!sub.static_locals.empty()) {
    out += Indent(indent) + "struct " + sub.name + "_StaticFrame {\n";
    for (const auto& sl : sub.static_locals) {
      const auto& var = sub.root_procedural_scope.vars.at(sl.var.value);
      auto type_or = RenderTypeAsCpp(unit, s, var.type);
      if (!type_or) return std::unexpected(std::move(type_or.error()));
      auto init_or =
          RenderExpr(sub_ctx, sub.root_procedural_scope.GetExpr(sl.init));
      if (!init_or) return std::unexpected(std::move(init_or.error()));
      out += Indent(indent + 1) + *type_or + " " + var.name + " = " + *init_or +
             ";\n";
    }
    out += Indent(indent) + "} " + frame_field + "{};\n";
  }

  out += Indent(indent) + sig + " {\n";
  auto rendered_or = RenderProceduralScopeStatements(sub_ctx, indent + 1);
  if (!rendered_or) return std::unexpected(std::move(rendered_or.error()));
  out += *rendered_or;
  if (is_task) {
    out += Indent(indent + 1) + "co_return;\n";
  }
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

auto RenderCreateProcesses(const mir::StructuralScope& s, std::size_t indent)
    -> std::string {
  std::string out;
  out += Indent(indent) + "void CreateProcesses() override {\n";
  for (std::size_t i = 0; i < s.processes.size(); ++i) {
    const auto& p = s.processes[i];
    out += Indent(indent + 1) + "AddProcess(" +
           RenderProcessKindLiteral(p.kind) + ", " +
           RenderProcessMethodName(i) + "());\n";
  }
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderScopeAsClass(
    const mir::CompilationUnit& unit, const mir::StructuralScope& s,
    std::size_t indent, bool is_top_level,
    const RenderContext* parent_struct_ctx) -> diag::Result<std::string> {
  // `this_anchor` is bound to `s.constructor_scope` so it doubles as the
  // ctx for rendering the constructor body. Children's bodies use it as
  // their structural parent (one hop above the child).
  const RenderContext this_anchor =
      (parent_struct_ctx == nullptr)
          ? RenderContext::ForRoot(unit, s, s.constructor_scope)
          : parent_struct_ctx->WithStructuralScope(s, s.constructor_scope);

  // A unit-root scope is an instance; a nested generate block is a generate
  // scope. The base type carries the kind.
  const std::string base_class =
      is_top_level ? "lyra::runtime::Instance" : "lyra::runtime::GenScope";

  std::string out;
  out += Indent(indent) + "class " + s.name + " final : public " + base_class +
         " {\n";
  out += Indent(indent) + " public:\n";

  // The scope's own time precision (LRM 3.14.2). The engine takes the minimum
  // across the tree as the design-global tick (LRM 3.14.3); a delay in this
  // scope scales from this precision to that tick.
  out += Indent(indent + 1) +
         "static constexpr std::int8_t kTimePrecisionPower = " +
         std::to_string(static_cast<int>(s.time_resolution.precision_power)) +
         ";\n";
  out += Indent(indent + 1) +
         "auto TimePrecisionPower() const -> std::int8_t override {\n";
  out += Indent(indent + 2) + "return kTimePrecisionPower;\n";
  out += Indent(indent + 1) + "}\n\n";

  // The scope's own time unit (LRM 3.14.2). $time / $realtime read here to
  // scale the design-global tick back to this design element's unit (LRM
  // 20.3); an unqualified reference in a process or subroutine body resolves
  // to the lexically enclosing scope's value.
  out += Indent(indent + 1) + "static constexpr std::int8_t kTimeUnitPower = " +
         std::to_string(static_cast<int>(s.time_resolution.unit_power)) +
         ";\n\n";

  for (const auto& child : s.child_structural_scopes) {
    auto child_or =
        RenderScopeAsClass(unit, child, indent + 1, false, &this_anchor);
    if (!child_or) return std::unexpected(std::move(child_or.error()));
    out += *child_or;
  }
  if (!s.child_structural_scopes.empty()) {
    out += "\n";
  }

  auto ctor_or = RenderConstructor(this_anchor, s, base_class, indent + 1);
  if (!ctor_or) return std::unexpected(std::move(ctor_or.error()));
  out += *ctor_or;
  // Child links are registered automatically at construction and walked by the
  // base; only a scope's own processes need a per-class override.
  if (!s.processes.empty()) {
    out += "\n";
    out += RenderCreateProcesses(s, indent + 1);
  }

  // Members follow the constructor and methods. They are public so cross-unit
  // references can reach them directly (see reference_resolution.md).
  if (!s.structural_params.empty() || !s.structural_vars.empty()) {
    out += "\n";
  }
  for (const auto& p : s.structural_params) {
    auto field_or = RenderParamField(unit, s, p, indent + 1);
    if (!field_or) return std::unexpected(std::move(field_or.error()));
    out += *field_or;
  }
  if (!s.structural_params.empty() && !s.structural_vars.empty()) {
    out += "\n";
  }
  for (const auto& v : s.structural_vars) {
    auto field_or = RenderField(this_anchor, v, indent + 1);
    if (!field_or) return std::unexpected(std::move(field_or.error()));
    out += *field_or;
  }

  // A cross-unit reference slot holds a resolved pointer into a child's member;
  // the constructor points it at `&child->member`. It mirrors that member's
  // storage: a `Var<T>*` for an observable scalar, a plain `T*` otherwise.
  for (std::size_t i = 0; i < s.cross_unit_refs.size(); ++i) {
    const auto& cu = s.cross_unit_refs[i];
    auto type_or = RenderTypeAsCpp(unit, s, cu.type);
    if (!type_or) return std::unexpected(std::move(type_or.error()));
    const std::string slot =
        CrossUnitRefSlotName(static_cast<std::uint32_t>(i));
    if (IsObservableScalarType(unit.GetType(cu.type))) {
      out += Indent(indent + 1) + "lyra::runtime::Var<" + *type_or + ">* " +
             slot + " = nullptr;\n";
    } else {
      out += Indent(indent + 1) + *type_or + "* " + slot + " = nullptr;\n";
    }
  }

  if (s.processes.empty() && s.structural_subroutines.empty()) {
    out += Indent(indent) + "};\n";
    return out;
  }

  out += "\n";
  out += Indent(indent) + " private:\n";

  for (std::size_t i = 0; i < s.processes.size(); ++i) {
    out += "\n";
    auto method_or = RenderProcessMethod(
        parent_struct_ctx, unit, s, s.processes[i], i, indent + 1);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    out += *method_or;
  }

  for (const auto& sub : s.structural_subroutines) {
    out += "\n";
    auto method_or =
        RenderSubroutineMethod(parent_struct_ctx, unit, s, sub, indent + 1);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    out += *method_or;
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
    const mir::CompilationUnit& unit, const mir::StructuralScope& s)
    -> diag::Result<std::string> {
  std::string out;
  out += "#pragma once\n";
  out += "#include <array>\n";
  out += "#include <cmath>\n";
  out += "#include <cstdint>\n";
  out += "#include <functional>\n";
  out += "#include <memory>\n";
  out += "#include <span>\n";
  out += "#include <string>\n";
  out += "#include <vector>\n";
  out += "#include \"lyra/runtime/coroutine.hpp\"\n";
  out += "#include \"lyra/runtime/delay.hpp\"\n";
  out += "#include \"lyra/runtime/file_io.hpp\"\n";
  out += "#include \"lyra/runtime/finish.hpp\"\n";
  out += "#include \"lyra/runtime/fork.hpp\"\n";
  out += "#include \"lyra/runtime/io.hpp\"\n";
  out += "#include \"lyra/runtime/named_event.hpp\"\n";
  out += "#include \"lyra/runtime/process_kind.hpp\"\n";
  out += "#include \"lyra/runtime/runtime_services.hpp\"\n";
  out += "#include \"lyra/runtime/scope.hpp\"\n";
  out += "#include \"lyra/runtime/scan.hpp\"\n";
  out += "#include \"lyra/runtime/sformat.hpp\"\n";
  out += "#include \"lyra/runtime/sim_time.hpp\"\n";
  out += "#include \"lyra/runtime/timescale.hpp\"\n";
  out += "#include \"lyra/runtime/var.hpp\"\n";
  out += "#include \"lyra/value/enum.hpp\"\n";
  out += "#include \"lyra/value/format.hpp\"\n";
  out += "#include \"lyra/value/packed_type.hpp\"\n";
  out += "#include \"lyra/value/packed.hpp\"\n";
  out += "#include \"lyra/value/packed_array.hpp\"\n";
  out += "#include \"lyra/value/packed_bitwise.hpp\"\n";
  out += "#include \"lyra/value/packed_convert.hpp\"\n";
  out += "#include \"lyra/value/packed_reduction.hpp\"\n";
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
  out += "#include <exception>\n";
  out += "#include <iostream>\n";
  out += "#include <vector>\n";
  out += "\n";
  out += "#include \"lyra/runtime/engine.hpp\"\n";
  for (const auto& top : tops) {
    out += "#include \"" + top.unit->structural_scope.name + ".hpp\"\n";
  }
  out += "\n";
  out += "auto main() -> int {\n";
  for (std::size_t i = 0; i < tops.size(); ++i) {
    out += "  " + tops[i].unit->structural_scope.name + " top" +
           std::to_string(i) + "{nullptr, \"" + tops[i].name + "\"};\n";
  }
  out += "  lyra::runtime::Engine engine;\n";
  out += "  std::vector<lyra::runtime::TopBinding> tops = {\n";
  for (std::size_t i = 0; i < tops.size(); ++i) {
    out += "      {&top" + std::to_string(i) + "},\n";
  }
  out += "  };\n";
  out += "  engine.BindDesign(tops);\n";
  out += "  try {\n";
  out += "    return engine.Run();\n";
  out += "  } catch (const std::exception& e) {\n";
  out += "    std::cerr << e.what() << \"\\n\";\n";
  out += "    return 1;\n";
  out += "  }\n";
  out += "}\n";
  return out;
}

}  // namespace

auto EmitCppDeclarations(const mir::CompilationUnit& unit)
    -> diag::Result<std::vector<CppArtifact>> {
  std::vector<CppArtifact> headers;
  const auto& root = unit.structural_scope;
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
