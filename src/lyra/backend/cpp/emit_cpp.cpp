#include <cstddef>
#include <string>

#include "formatting.hpp"
#include "lyra/backend/cpp/api.hpp"
#include "lyra/backend/cpp/artifact.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/member_var.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/support/internal_error.hpp"
#include "render_stmt.hpp"
#include "render_type.hpp"

namespace lyra::backend::cpp {

namespace {

auto RenderField(const mir::ClassDecl& c, const mir::MemberVar& member)
    -> std::string {
  return Indent(1) + RenderTypeAsCpp(c.GetType(member.type)) + " " +
         member.name + ";\n";
}

auto RenderConstructor(const mir::ClassDecl& c) -> std::string {
  const auto& body = c.Constructor();
  if (body.root_stmts.empty()) {
    return Indent(1) + c.Name() + "() {}\n";
  }
  std::string out;
  out += Indent(1) + c.Name() + "() {\n";
  out += RenderBody(c, body, 2);
  out += Indent(1) + "}\n";
  return out;
}

auto RenderProcessMethodName(std::size_t index) -> std::string {
  return "process_" + std::to_string(index);
}

auto RenderProcessMethod(
    const mir::ClassDecl& c, const mir::Process& process, std::size_t index)
    -> std::string {
  std::string out;
  out += Indent(1) + "auto " + RenderProcessMethodName(index) +
         "() -> lyra::runtime::Process {\n";
  out += RenderBody(c, process.body, 2);
  // This cut only handles zero-time `initial` bodies. Every body terminates
  // by falling off the end, so we always emit a trailing `co_return;` to make
  // the function a valid coroutine even when the MIR body is empty. When
  // delay/wait/event awaiters land, this trailing emission stays put; bodies
  // with `co_await` simply have suspension points before it.
  out += Indent(2) + "co_return;\n";
  out += Indent(1) + "}\n";
  return out;
}

auto RenderProcessKindLiteral(mir::ProcessKind kind) -> std::string {
  switch (kind) {
    case mir::ProcessKind::kInitial:
      return "lyra::runtime::ProcessKind::kInitial";
  }
  throw support::InternalError("RenderProcessKindLiteral: unknown ProcessKind");
}

auto RenderBind(const mir::ClassDecl& c) -> std::string {
  std::string out;
  out += Indent(1) +
         "void Bind(lyra::runtime::RuntimeBindContext& ctx) override {\n";
  for (std::size_t i = 0; i < c.Processes().size(); ++i) {
    const auto& p = c.Processes()[i];
    out += Indent(2) + "ctx.AddProcess(\n";
    out += Indent(3) + RenderProcessKindLiteral(p.kind) + ",\n";
    out += Indent(3) + RenderProcessMethodName(i) + "());\n";
  }
  out += Indent(1) + "}\n";
  return out;
}

auto RenderClassHeader(const mir::ClassDecl& c) -> std::string {
  std::string out;
  out += "#pragma once\n";
  out += "#include <cstdint>\n";
  out += "#include \"lyra/runtime/bind_context.hpp\"\n";
  out += "#include \"lyra/runtime/engine.hpp\"\n";
  out += "#include \"lyra/runtime/module.hpp\"\n";
  out += "#include \"lyra/runtime/process.hpp\"\n";
  out += "#include \"lyra/runtime/process_kind.hpp\"\n";
  out += "\n";
  out += "class " + c.Name() + " final : public lyra::runtime::Module {\n";
  out += " public:\n";

  for (const auto& m : c.MemberVars()) {
    out += RenderField(c, m);
  }
  if (!c.MemberVars().empty()) {
    out += "\n";
  }

  out += RenderConstructor(c);
  out += "\n";
  out += RenderBind(c);

  for (std::size_t i = 0; i < c.Processes().size(); ++i) {
    out += "\n";
    out += RenderProcessMethod(c, c.Processes()[i], i);
  }

  out += "};\n";
  return out;
}

auto RenderHostMain(const mir::ClassDecl& cls) -> std::string {
  std::string out;
  out += "#include \"" + cls.Name() + ".hpp\"\n";
  out += "\n";
  out += "auto main() -> int {\n";
  out += "  " + cls.Name() + " top;\n";
  out += "  lyra::runtime::Engine engine;\n";
  out += "  engine.BindRoot(\"top\", top);\n";
  out += "  return engine.Run();\n";
  out += "}\n";
  return out;
}

}  // namespace

auto EmitCpp(const mir::CompilationUnit& unit) -> CppArtifactSet {
  if (unit.Classes().size() != 1) {
    throw support::InternalError(
        "EmitCpp: this cut requires exactly one class in the unit");
  }
  CppArtifactSet set;
  const auto& cls = unit.Classes().front();
  set.files.push_back({cls.Name() + ".hpp", RenderClassHeader(cls)});
  set.files.push_back({"main.cpp", RenderHostMain(cls)});
  return set;
}

}  // namespace lyra::backend::cpp
