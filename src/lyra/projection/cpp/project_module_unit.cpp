#include <cstddef>
#include <string>
#include <variant>

#include "formatting.hpp"
#include "lyra/mir/member.hpp"
#include "lyra/mir/module_unit.hpp"
#include "lyra/mir/process.hpp"
#include "lyra/projection/cpp/api.hpp"
#include "lyra/support/overloaded.hpp"
#include "render_context.hpp"
#include "render_stmt.hpp"
#include "render_type.hpp"

namespace lyra::projection::cpp {

namespace {

auto RenderField(const mir::ModuleUnit& unit, const mir::Member& member)
    -> std::string {
  return Indent(1) + RenderTypeAsCpp(unit.GetType(member.type)) + " " +
         member.name + ";\n";
}

// Renderer-local naming. Naming is a projection concern; the IR carries no
// process names.
auto RenderProcessMethodName(std::size_t index) -> std::string {
  return "process_" + std::to_string(index);
}

auto RenderMethod(
    const mir::ModuleUnit& unit, const mir::Process& process, std::size_t index)
    -> std::string {
  const RenderContext ctx{unit, process};

  return std::visit(
      support::Overloaded{
          [&](const mir::Initial& p) -> std::string {
            std::string out;
            out +=
                Indent(1) + "void " + RenderProcessMethodName(index) + "() {\n";
            out += RenderProcessBody(ctx, p.body, 2);
            out += Indent(1) + "}\n";
            return out;
          },
      },
      process.data);
}

}  // namespace

auto ProjectModuleUnitToCpp(const mir::ModuleUnit& unit) -> std::string {
  std::string out;
  out += "class " + unit.Name() + " {\n";
  out += " public:\n";

  const auto& members = unit.Members();
  for (const auto& member : members) {
    out += RenderField(unit, member);
  }

  const auto& processes = unit.Processes();
  if (!members.empty() && !processes.empty()) {
    out += "\n";
  }

  for (std::size_t i = 0; i < processes.size(); ++i) {
    if (i != 0) {
      out += "\n";
    }
    out += RenderMethod(unit, processes[i], i);
  }

  out += "};\n";
  return out;
}

}  // namespace lyra::projection::cpp
