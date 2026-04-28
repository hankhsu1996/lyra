#include <cstddef>
#include <string>
#include <variant>

#include "formatting.hpp"
#include "lyra/backend/cpp/api.hpp"
#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/process.hpp"
#include "render_stmt.hpp"
#include "render_type.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderField(
    const mir::CompilationUnit& unit, const mir::ClassDecl& enclosing,
    const mir::MemberVar& member, std::size_t indent) -> std::string {
  return std::visit(
      Overloaded{
          [&](const mir::ValueMember& v) -> std::string {
            return Indent(indent) + RenderTypeAsCpp(unit, v.type) + " " +
                   member.name + ";\n";
          },
          [&](const mir::ChildClassMember& c) -> std::string {
            return Indent(indent) + enclosing.GetClass(c.target).Name() + "* " +
                   member.name + "{};\n";
          },
      },
      member.kind);
}

auto RenderConstructor(
    const mir::CompilationUnit& unit, const mir::ClassDecl& c,
    std::size_t indent) -> std::string {
  const auto& body = c.Constructor();
  if (body.root_stmts.empty()) {
    return Indent(indent) + c.Name() + "() {}\n";
  }
  std::string out;
  out += Indent(indent) + c.Name() + "() {\n";
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
         "() -> lyra::runtime::Process {\n";
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

auto RenderBind(const mir::ClassDecl& c, std::size_t indent, bool is_top_level)
    -> std::string {
  std::string out;
  out += Indent(indent) + "void Bind(lyra::runtime::RuntimeBindContext& ctx)" +
         (is_top_level ? " override {\n" : " {\n");
  out += Indent(indent + 1) + "engine_ = &ctx.GetEngine();\n";
  for (std::size_t i = 0; i < c.Processes().size(); ++i) {
    const auto& p = c.Processes()[i];
    out += Indent(indent + 1) + "ctx.AddProcess(\n";
    out += Indent(indent + 2) + RenderProcessKindLiteral(p.kind) + ",\n";
    out += Indent(indent + 2) + RenderProcessMethodName(i) + "());\n";
  }
  for (const auto& m : c.MemberVars()) {
    const auto* child = std::get_if<mir::ChildClassMember>(&m.kind);
    if (child == nullptr) {
      continue;
    }
    const auto& child_class = c.GetClass(child->target);
    out += Indent(indent + 1) + "if (" + m.name + " != nullptr) {\n";
    out += Indent(indent + 2) + "auto child = ctx.CreateChildScope(\n";
    out += Indent(indent + 3) + "\"" + child_class.Name() + "\",\n";
    out +=
        Indent(indent + 3) + "lyra::runtime::RuntimeScopeKind::kGenerate);\n";
    out += Indent(indent + 2) + m.name + "->Bind(child);\n";
    out += Indent(indent + 1) + "}\n";
  }
  out += Indent(indent) + "}\n";
  return out;
}

auto RenderClassDecl(
    const mir::CompilationUnit& unit, const mir::ClassDecl& c,
    std::size_t indent, bool is_top_level) -> std::string {
  std::string out;
  out += Indent(indent) + "class " + c.Name();
  if (is_top_level) {
    out += " final : public lyra::runtime::Module";
  }
  out += " {\n";
  out += Indent(indent) + " public:\n";

  for (const auto& child : c.Classes()) {
    out += RenderClassDecl(unit, child, indent + 1, false);
  }
  if (!c.Classes().empty()) {
    out += "\n";
  }

  for (const auto& m : c.MemberVars()) {
    out += RenderField(unit, c, m, indent + 1);
  }
  if (!c.MemberVars().empty()) {
    out += "\n";
  }

  out += Indent(indent + 1) + "lyra::runtime::Engine* engine_{};\n";
  out += "\n";

  out += RenderConstructor(unit, c, indent + 1);
  out += "\n";
  out += RenderBind(c, indent + 1, is_top_level);

  for (std::size_t i = 0; i < c.Processes().size(); ++i) {
    out += "\n";
    out += RenderProcessMethod(unit, c, c.Processes()[i], i, indent + 1);
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
  out += "#include <span>\n";
  out += "#include <string>\n";
  out += "#include \"lyra/runtime/bind_context.hpp\"\n";
  out += "#include \"lyra/runtime/engine.hpp\"\n";
  out += "#include \"lyra/runtime/format.hpp\"\n";
  out += "#include \"lyra/runtime/io.hpp\"\n";
  out += "#include \"lyra/runtime/module.hpp\"\n";
  out += "#include \"lyra/runtime/process.hpp\"\n";
  out += "#include \"lyra/runtime/process_kind.hpp\"\n";
  out += "#include \"lyra/runtime/runtime_scope_kind.hpp\"\n";
  out += "\n";
  out += RenderClassDecl(unit, c, 0, true);
  return out;
}

auto RenderHostMain(const mir::ClassDecl& entry) -> std::string {
  std::string out;
  out += "#include \"" + entry.Name() + ".hpp\"\n";
  out += "\n";
  out += "auto main() -> int {\n";
  out += "  " + entry.Name() + " top;\n";
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
  headers.reserve(unit.Classes().size());
  for (const auto& cls : unit.Classes()) {
    headers.push_back(
        {.relpath = cls.Name() + ".hpp",
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
