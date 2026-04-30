#include <cstddef>
#include <string>
#include <utility>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_context.hpp"
#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
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
    const mir::CompilationUnit& unit, const mir::StructuralScope& owner_scope,
    const mir::StructuralVarDecl& var, std::size_t indent)
    -> diag::Result<std::string> {
  auto type_or = RenderTypeAsCpp(unit, owner_scope, var.type);
  if (!type_or) return std::unexpected(std::move(type_or.error()));
  std::string init;
  const auto& ty = unit.GetType(var.type);
  if (ty.IsPackedArray() &&
      ty.AsPackedArray().form == mir::PackedArrayForm::kExplicit) {
    init = "{" + std::to_string(ty.AsPackedArray().BitWidth()) + "}";
  }
  return Indent(indent) + *type_or + " " + var.name + init + ";\n";
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
  if (process.kind != mir::ProcessKind::kInitial) {
    throw InternalError(
        "RenderProcessMethod: C++ emit is not yet supported for non-initial "
        "processes");
  }
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

auto RenderProcessKindLiteral(mir::ProcessKind kind) -> std::string {
  switch (kind) {
    case mir::ProcessKind::kInitial:
      return "lyra::runtime::ProcessKind::kInitial";
    case mir::ProcessKind::kFinal:
    case mir::ProcessKind::kAlways:
    case mir::ProcessKind::kAlwaysComb:
    case mir::ProcessKind::kAlwaysLatch:
    case mir::ProcessKind::kAlwaysFf:
      throw InternalError(
          "RenderProcessKindLiteral: C++ emit is not yet supported for "
          "non-initial processes");
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
  // Bind eligibility is inferred from storage type structure today; when
  // non-generate producers of `ObjectType` appear, switch to explicit scope
  // metadata.
  for (const auto& v : s.structural_vars) {
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
    auto field_or = RenderField(unit, s, v, indent + 1);
    if (!field_or) return std::unexpected(std::move(field_or.error()));
    out += *field_or;
  }
  if (!s.structural_vars.empty()) {
    out += "\n";
  }

  out += Indent(indent + 1) + "lyra::runtime::RuntimeServices* services_{};\n";
  out += "\n";

  auto ctor_or = RenderConstructor(this_anchor, s, indent + 1);
  if (!ctor_or) return std::unexpected(std::move(ctor_or.error()));
  out += *ctor_or;
  out += "\n";
  out += RenderBind(unit, s, indent + 1, is_top_level);

  for (std::size_t i = 0; i < s.processes.size(); ++i) {
    out += "\n";
    auto method_or = RenderProcessMethod(
        parent_struct_ctx, unit, s, s.processes[i], i, indent + 1);
    if (!method_or) return std::unexpected(std::move(method_or.error()));
    out += *method_or;
  }

  out += Indent(indent) + "};\n";
  return out;
}

auto RenderScopeHeaderFile(
    const mir::CompilationUnit& unit, const mir::StructuralScope& s)
    -> diag::Result<std::string> {
  std::string out;
  out += "#pragma once\n";
  out += "#include <array>\n";
  out += "#include <cstdint>\n";
  out += "#include <memory>\n";
  out += "#include <span>\n";
  out += "#include <string>\n";
  out += "#include <vector>\n";
  out += "#include \"lyra/runtime/bind_context.hpp\"\n";
  out += "#include \"lyra/runtime/delay.hpp\"\n";
  out += "#include \"lyra/runtime/format.hpp\"\n";
  out += "#include \"lyra/runtime/io.hpp\"\n";
  out += "#include \"lyra/runtime/module.hpp\"\n";
  out += "#include \"lyra/runtime/packed.hpp\"\n";
  out += "#include \"lyra/runtime/process.hpp\"\n";
  out += "#include \"lyra/runtime/process_kind.hpp\"\n";
  out += "#include \"lyra/runtime/runtime_scope_kind.hpp\"\n";
  out += "#include \"lyra/runtime/runtime_services.hpp\"\n";
  out += "\n";
  auto class_or = RenderScopeAsClass(unit, s, 0, true, nullptr);
  if (!class_or) return std::unexpected(std::move(class_or.error()));
  out += *class_or;
  return out;
}

auto RenderHostMain(const mir::StructuralScope& entry) -> std::string {
  std::string out;
  out += "#include \"lyra/runtime/engine.hpp\"\n";
  out += "#include \"" + entry.name + ".hpp\"\n";
  out += "\n";
  out += "auto main() -> int {\n";
  out += "  " + entry.name + " top;\n";
  out += "  lyra::runtime::Engine engine;\n";
  out += "  engine.BindRoot(\"top\", top);\n";
  out += "  return engine.Run();\n";
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

auto EmitCppHostMain(const mir::StructuralScope& entry_scope) -> CppArtifact {
  return {.relpath = "main.cpp", .content = RenderHostMain(entry_scope)};
}

auto EmitCpp(const mir::CompilationUnit& unit) -> diag::Result<CppArtifactSet> {
  CppArtifactSet set;
  auto headers_or = EmitCppDeclarations(unit);
  if (!headers_or) return std::unexpected(std::move(headers_or.error()));
  for (auto& h : *headers_or) {
    set.files.push_back(std::move(h));
  }
  set.files.push_back(EmitCppHostMain(unit.structural_scope));
  return set;
}

}  // namespace lyra::backend::cpp
