#include <cstddef>
#include <string>

#include "formatting.hpp"
#include "lyra/mir/class_decl.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/projection/cpp/api.hpp"
#include "render_stmt.hpp"
#include "render_type.hpp"

namespace lyra::projection::cpp {

namespace {

auto RenderField(const mir::ClassDecl& c, const mir::Member& member)
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
  out += Indent(1) + "void " + RenderProcessMethodName(index) + "() {\n";
  out += RenderBody(c, process.body, 2);
  out += Indent(1) + "}\n";
  return out;
}

auto RenderClass(const mir::ClassDecl& c) -> std::string {
  std::string out;
  out += "class " + c.Name() + " {\n";
  out += " public:\n";

  for (const auto& m : c.Members()) {
    out += RenderField(c, m);
  }
  if (!c.Members().empty()) {
    out += "\n";
  }

  out += RenderConstructor(c);

  for (std::size_t i = 0; i < c.Processes().size(); ++i) {
    out += "\n";
    out += RenderProcessMethod(c, c.Processes()[i], i);
  }

  out += "};\n";
  return out;
}

}  // namespace

auto ProjectCompilationUnitToCpp(const mir::CompilationUnit& unit)
    -> std::string {
  std::string out;
  bool first = true;
  for (const auto& cls : unit.Classes()) {
    if (!first) {
      out += "\n";
    }
    out += RenderClass(cls);
    first = false;
  }
  return out;
}

}  // namespace lyra::projection::cpp
