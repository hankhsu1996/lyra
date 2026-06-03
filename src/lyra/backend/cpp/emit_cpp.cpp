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
    std::size_t indent) -> diag::Result<std::string> {
  std::string sig = s.name + "(";
  std::string init_list;
  for (std::size_t i = 0; i < s.structural_params.size(); ++i) {
    if (i != 0) {
      sig += ", ";
      init_list += ", ";
    }
    const auto& p = s.structural_params[i];
    const auto param_name = CtorParamName(i);
    auto type_or = RenderTypeAsCpp(scope_ctx.Unit(), s, p.type);
    if (!type_or) return std::unexpected(std::move(type_or.error()));
    sig += *type_or + " " + param_name;
    init_list += p.name + "(" + param_name + ")";
  }
  sig += ")";
  if (!init_list.empty()) {
    sig += " : " + init_list;
  }

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
         "() -> lyra::runtime::ProcessCoroutine {\n";
  const RenderContext proc_ctx =
      (parent_struct_ctx == nullptr)
          ? RenderContext::ForRoot(unit, s, process.root_procedural_scope)
          : parent_struct_ctx->WithStructuralScope(
                s, process.root_procedural_scope);
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
  auto result_or = RenderSubroutineResultType(unit, s, sub.result_type);
  if (!result_or) return std::unexpected(std::move(result_or.error()));

  std::string sig = *result_or + " " + sub.name + "(";
  for (std::size_t i = 0; i < sub.params.size(); ++i) {
    const auto& param = sub.params[i];
    auto type_or = RenderTypeAsCpp(unit, s, param.type);
    if (!type_or) return std::unexpected(std::move(type_or.error()));
    if (i != 0) sig += ", ";
    // An `input` formal is a by-value copy (LRM 13.5.1); an `output` / `inout`
    // formal binds to the caller's writeback temp by reference so the body
    // writes flow back at the copy-out. ref / const ref are rejected upstream.
    if (param.direction == mir::ParamDirection::kInput) {
      sig += *type_or + " " + param.name;
    } else {
      sig += *type_or + "& " + param.name;
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
          .WithStaticFrame(frame_field);

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

auto RenderBind(
    const mir::CompilationUnit& unit, const mir::StructuralScope& s,
    std::size_t indent, bool is_top_level) -> std::string {
  std::string out;
  out += Indent(indent) + "void Bind(lyra::runtime::RuntimeBindContext& ctx)" +
         (is_top_level ? " override {\n" : " {\n");
  out += Indent(indent + 1) + "services_ = &ctx.Services();\n";
  for (std::size_t i = 0; i < s.processes.size(); ++i) {
    const auto& p = s.processes[i];
    out += Indent(indent + 1) + "ctx.AddProcess(\n";
    out += Indent(indent + 2) + RenderProcessKindLiteral(p.kind) + ",\n";
    out += Indent(indent + 2) + RenderProcessMethodName(i) + "());\n";
  }
  // An owned-object var binds its child under this scope. The scope kind is
  // read off the owning type: an external-unit instance is a module instance;
  // an intra-unit owned object is a generate scope.
  for (const auto& v : s.structural_vars) {
    if (const auto ext_unit = mir::GetExternalUnitName(unit, v.type);
        ext_unit.has_value()) {
      out += Indent(indent + 1) + "if (" + v.name + ") {\n";
      out += Indent(indent + 2) + "auto child = ctx.CreateChildScope(\n";
      out += Indent(indent + 3) + "\"" + v.name + "\",\n";
      out += Indent(indent + 3) +
             "lyra::runtime::RuntimeScopeKind::kModuleInstance);\n";
      out += Indent(indent + 2) + v.name + "->Bind(child);\n";
      out += Indent(indent + 1) + "}\n";
      continue;
    }
    const auto target = mir::GetOwnedObjectTarget(unit, v.type);
    if (!target.has_value()) {
      continue;
    }
    const auto& child_scope = s.GetChildStructuralScope(*target);

    if (mir::IsVectorOfOwningObjectType(unit, v.type)) {
      out += Indent(indent + 1) + "for (std::size_t idx = 0; idx < " + v.name +
             ".size(); ++idx) {\n";
      out += Indent(indent + 2) + "auto child = ctx.CreateChildScope(\n";
      out += Indent(indent + 3) + "\"" + child_scope.name +
             "[\" + std::to_string(idx) + \"]\",\n";
      out += Indent(indent + 3) +
             "lyra::runtime::RuntimeScopeKind::kGenerateScope);\n";
      out += Indent(indent + 2) + v.name + "[idx]->Bind(child);\n";
      out += Indent(indent + 1) + "}\n";
    } else {
      out += Indent(indent + 1) + "if (" + v.name + ") {\n";
      out += Indent(indent + 2) + "auto child = ctx.CreateChildScope(\n";
      out += Indent(indent + 3) + "\"" + child_scope.name + "\",\n";
      out += Indent(indent + 3) +
             "lyra::runtime::RuntimeScopeKind::kGenerateScope);\n";
      out += Indent(indent + 2) + v.name + "->Bind(child);\n";
      out += Indent(indent + 1) + "}\n";
    }
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

  std::string out;
  out += Indent(indent) + "class " + s.name;
  if (is_top_level) {
    out += " final : public lyra::runtime::Module";
  }
  out += " {\n";
  out += Indent(indent) + " public:\n";

  for (const auto& child : s.child_structural_scopes) {
    auto child_or =
        RenderScopeAsClass(unit, child, indent + 1, false, &this_anchor);
    if (!child_or) return std::unexpected(std::move(child_or.error()));
    out += *child_or;
  }
  if (!s.child_structural_scopes.empty()) {
    out += "\n";
  }

  for (const auto& p : s.structural_params) {
    auto field_or = RenderParamField(unit, s, p, indent + 1);
    if (!field_or) return std::unexpected(std::move(field_or.error()));
    out += *field_or;
  }
  if (!s.structural_params.empty()) {
    out += "\n";
  }

  for (const auto& v : s.structural_vars) {
    auto field_or = RenderField(this_anchor, v, indent + 1);
    if (!field_or) return std::unexpected(std::move(field_or.error()));
    out += *field_or;
  }
  if (!s.structural_vars.empty()) {
    out += "\n";
  }

  auto ctor_or = RenderConstructor(this_anchor, s, indent + 1);
  if (!ctor_or) return std::unexpected(std::move(ctor_or.error()));
  out += *ctor_or;
  out += "\n";
  out += RenderBind(unit, s, indent + 1, is_top_level);

  out += "\n";
  out += Indent(indent) + " private:\n";
  out += Indent(indent + 1) + "lyra::runtime::RuntimeServices* services_{};\n";
  out += "\n";

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
  out += "#include <memory>\n";
  out += "#include <span>\n";
  out += "#include <string>\n";
  out += "#include <vector>\n";
  out += "#include \"lyra/runtime/bind_context.hpp\"\n";
  out += "#include \"lyra/runtime/delay.hpp\"\n";
  out += "#include \"lyra/runtime/file_io.hpp\"\n";
  out += "#include \"lyra/runtime/finish.hpp\"\n";
  out += "#include \"lyra/runtime/io.hpp\"\n";
  out += "#include \"lyra/runtime/module.hpp\"\n";
  out += "#include \"lyra/runtime/named_event.hpp\"\n";
  out += "#include \"lyra/runtime/process.hpp\"\n";
  out += "#include \"lyra/runtime/process_kind.hpp\"\n";
  out += "#include \"lyra/runtime/runtime_scope_kind.hpp\"\n";
  out += "#include \"lyra/runtime/runtime_services.hpp\"\n";
  out += "#include \"lyra/runtime/scan.hpp\"\n";
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
           std::to_string(i) + ";\n";
  }
  out += "  lyra::runtime::Engine engine;\n";
  out += "  std::vector<lyra::runtime::TopBinding> tops = {\n";
  for (std::size_t i = 0; i < tops.size(); ++i) {
    out += "      {\"" + tops[i].name + "\", &top" + std::to_string(i) + "},\n";
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
