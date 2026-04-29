#include <cstddef>
#include <string>

#include "lyra/backend/cpp/api.hpp"
#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/backend/cpp/formatting.hpp"
#include "lyra/backend/cpp/render_stmt.hpp"
#include "lyra/backend/cpp/render_type.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderField(
    const mir::CompilationUnit& unit, const mir::ClassDecl& owner_class,
    const mir::MemberVar& member, std::size_t indent) -> std::string {
  return Indent(indent) + RenderTypeAsCpp(unit, owner_class, member.type) +
         " " + member.name + ";\n";
}

auto RenderConstructor(
    const mir::CompilationUnit& unit, const mir::ClassDecl& c,
    std::size_t indent) -> std::string {
  const auto& body = c.constructor;
  if (body.root_stmts.empty()) {
    return Indent(indent) + c.name + "() {}\n";
  }
  std::string out;
  out += Indent(indent) + c.name + "() {\n";
  out += RenderBody(unit, c, body, indent + 1);
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderProcessMethodName(std::size_t index) -> std::string {
  return "process_" + std::to_string(index);
}

auto RenderProcessMethod(
    const mir::CompilationUnit& unit, const mir::ClassDecl& c,
    const mir::Process& process, std::size_t index, std::size_t indent)
    -> std::string {
  std::string out;
  out += Indent(indent) + "auto " + RenderProcessMethodName(index) +
         "() -> lyra::runtime::ProcessCoroutine {\n";
  out += RenderBody(unit, c, process.body, indent + 1);
  out += Indent(indent + 1) + "co_return;\n";
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderProcessKindLiteral(mir::ProcessKind kind) -> std::string {
  switch (kind) {
    case mir::ProcessKind::kInitial:
      return "lyra::runtime::ProcessKind::kInitial";
  }
  throw InternalError("RenderProcessKindLiteral: unknown ProcessKind");
}

auto RenderBind(
    const mir::CompilationUnit& unit, const mir::ClassDecl& c,
    std::size_t indent, bool is_top_level) -> std::string {
  std::string out;
  out += Indent(indent) + "void Bind(lyra::runtime::RuntimeBindContext& ctx)" +
         (is_top_level ? " override {\n" : " {\n");
  out += Indent(indent + 1) + "services_ = &ctx.Services();\n";
  for (std::size_t i = 0; i < c.processes.size(); ++i) {
    const auto& p = c.processes[i];
    out += Indent(indent + 1) + "ctx.AddProcess(\n";
    out += Indent(indent + 2) + RenderProcessKindLiteral(p.kind) + ",\n";
    out += Indent(indent + 2) + RenderProcessMethodName(i) + "());\n";
  }
  // Every owning-object member (`OwningPtr<Object<T>>` or
  // `Vector<OwningPtr<Object<T>>>`) is treated as a runtime-bindable child
  // scope. The only producer of these types today is generate-arm child
  // class installation. When other producers of `ObjectType` appear
  // (e.g. owned process/module objects), bind eligibility must come from
  // explicit class metadata, not from storage type structure alone.
  for (const auto& m : c.member_vars) {
    const auto target = mir::GetOwnedObjectTarget(unit, m.type);
    if (!target.has_value()) {
      continue;
    }
    const auto& child_class = c.GetClass(*target);

    if (mir::IsVectorOfOwningObjectType(unit, m.type)) {
      out += Indent(indent + 1) + "for (std::size_t idx = 0; idx < " + m.name +
             ".size(); ++idx) {\n";
      out += Indent(indent + 2) + "auto child = ctx.CreateChildScope(\n";
      out += Indent(indent + 3) + "\"" + child_class.name +
             "[\" + std::to_string(idx) + \"]\",\n";
      out += Indent(indent + 3) +
             "lyra::runtime::RuntimeScopeKind::kGenerateScope);\n";
      out += Indent(indent + 2) + m.name + "[idx]->Bind(child);\n";
      out += Indent(indent + 1) + "}\n";
    } else {
      out += Indent(indent + 1) + "if (" + m.name + ") {\n";
      out += Indent(indent + 2) + "auto child = ctx.CreateChildScope(\n";
      out += Indent(indent + 3) + "\"" + child_class.name + "\",\n";
      out += Indent(indent + 3) +
             "lyra::runtime::RuntimeScopeKind::kGenerateScope);\n";
      out += Indent(indent + 2) + m.name + "->Bind(child);\n";
      out += Indent(indent + 1) + "}\n";
    }
  }
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderClassDecl(
    const mir::CompilationUnit& unit, const mir::ClassDecl& c,
    std::size_t indent, bool is_top_level) -> std::string {
  std::string out;
  out += Indent(indent) + "class " + c.name;
  if (is_top_level) {
    out += " final : public lyra::runtime::Module";
  }
  out += " {\n";
  out += Indent(indent) + " public:\n";

  for (const auto& child : c.classes) {
    out += RenderClassDecl(unit, child, indent + 1, false);
  }
  if (!c.classes.empty()) {
    out += "\n";
  }

  for (const auto& m : c.member_vars) {
    out += RenderField(unit, c, m, indent + 1);
  }
  if (!c.member_vars.empty()) {
    out += "\n";
  }

  out += Indent(indent + 1) + "lyra::runtime::RuntimeServices* services_{};\n";
  out += "\n";

  out += RenderConstructor(unit, c, indent + 1);
  out += "\n";
  out += RenderBind(unit, c, indent + 1, is_top_level);

  for (std::size_t i = 0; i < c.processes.size(); ++i) {
    out += "\n";
    out += RenderProcessMethod(unit, c, c.processes[i], i, indent + 1);
  }

  out += Indent(indent) + "};\n";
  return out;
}

auto RenderClassHeaderFile(
    const mir::CompilationUnit& unit, const mir::ClassDecl& c) -> std::string {
  std::string out;
  out += "#pragma once\n";
  out += "#include <array>\n";
  out += "#include <cstdint>\n";
  out += "#include <memory>\n";
  out += "#include <span>\n";
  out += "#include <string>\n";
  out += "#include <vector>\n";
  out += "#include \"lyra/runtime/bind_context.hpp\"\n";
  out += "#include \"lyra/runtime/convert.hpp\"\n";
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
  out += RenderClassDecl(unit, c, 0, true);
  return out;
}

auto RenderHostMain(const mir::ClassDecl& entry) -> std::string {
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
    -> std::vector<CppArtifact> {
  std::vector<CppArtifact> headers;
  headers.reserve(unit.classes.size());
  for (const auto& cls : unit.classes) {
    headers.push_back(
        {.relpath = cls.name + ".hpp",
         .content = RenderClassHeaderFile(unit, cls)});
  }
  return headers;
}

auto EmitCppHostMain(const mir::ClassDecl& entry_class) -> CppArtifact {
  return {.relpath = "main.cpp", .content = RenderHostMain(entry_class)};
}

auto EmitCpp(
    const mir::CompilationUnit& unit, const mir::ClassDecl& entry_class)
    -> CppArtifactSet {
  CppArtifactSet set;
  auto headers = EmitCppDeclarations(unit);
  for (auto& h : headers) {
    set.files.push_back(std::move(h));
  }
  set.files.push_back(EmitCppHostMain(entry_class));
  return set;
}

}  // namespace lyra::backend::cpp
