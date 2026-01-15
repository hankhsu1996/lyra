#include "lyra/mir/dumper.hpp"

#include <cassert>
#include <format>
#include <variant>

namespace lyra::mir {

Dumper::Dumper(const Arena* arena, std::ostream* out)
    : arena_(arena), out_(out) {
}

void Dumper::PrintIndent() {
  for (int i = 0; i < indent_; ++i) {
    *out_ << "  ";
  }
}

void Dumper::Indent() {
  ++indent_;
}

void Dumper::Dedent() {
  assert(indent_ > 0);
  --indent_;
}

void Dumper::Dump(const Design& design) {
  *out_ << "Design {\n";
  Indent();
  for (const auto& element : design.elements) {
    std::visit([this](const auto& elem) { Dump(elem); }, element);
  }
  Dedent();
  *out_ << "}\n";
}

void Dumper::Dump(const Module& module) {
  PrintIndent();
  *out_ << "Module {\n";
  Indent();

  for (ProcessId id : module.processes) {
    Dump(id);
  }
  for (FunctionId id : module.functions) {
    Dump(id);
  }

  Dedent();
  PrintIndent();
  *out_ << "}\n";
}

void Dumper::Dump(const Package& /*package*/) {
  PrintIndent();
  *out_ << "Package {}\n";
}

void Dumper::Dump(ProcessId id) {
  const Process& proc = (*arena_)[id];
  PrintIndent();

  const char* kind_str =
      proc.kind == ProcessKind::kOnce ? "process.once" : "process.loop";
  *out_ << kind_str << " {\n";
  Indent();

  for (BasicBlockId bb_id : proc.blocks) {
    Dump(bb_id);
  }

  Dedent();
  PrintIndent();
  *out_ << "}\n";
}

void Dumper::Dump(FunctionId id) {
  const Function& func = (*arena_)[id];
  PrintIndent();
  *out_ << "function {\n";
  Indent();

  for (BasicBlockId bb_id : func.blocks) {
    Dump(bb_id);
  }

  Dedent();
  PrintIndent();
  *out_ << "}\n";
}

void Dumper::Dump(BasicBlockId id) {
  const BasicBlock& bb = (*arena_)[id];
  PrintIndent();
  *out_ << std::format("bb{}: {{\n", bb.id.value);
  Indent();

  for (const Instruction& instr : bb.instructions) {
    PrintIndent();
    std::visit(
        [this](const auto& i) {
          using T = std::decay_t<decltype(i)>;
          if constexpr (std::is_same_v<T, Assign>) {
            *out_ << std::format("place[{}] = operand\n", i.target.value);
          } else if constexpr (std::is_same_v<T, Compute>) {
            *out_ << std::format(
                "place[{}] = rvalue(op={})\n", i.target.value,
                static_cast<int>(i.value.kind));
          }
        },
        instr);
  }

  PrintIndent();
  *out_ << std::format(
      "terminator: {}\n", static_cast<int>(bb.terminator.kind));

  Dedent();
  PrintIndent();
  *out_ << "}\n";
}

}  // namespace lyra::mir
