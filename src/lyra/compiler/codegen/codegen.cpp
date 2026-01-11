#include "lyra/compiler/codegen/codegen.hpp"

#include <algorithm>
#include <cstdint>
#include <format>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_set>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/mir/module.hpp"
#include "lyra/mir/package.hpp"
#include "lyra/mir/process.hpp"

namespace lyra::compiler {

namespace {

// Check if a module uses any array types (requiring <array> include)
auto UsesArrayType(const mir::Module& module) -> bool {
  auto is_array = [](const auto& item) {
    return item.variable.type.kind == common::Type::Kind::kUnpackedArray;
  };
  return std::ranges::any_of(module.variables, is_array) ||
         std::ranges::any_of(module.ports, is_array);
}

}  // namespace

auto Codegen::Generate(
    const mir::Module& module, bool skip_sdk_aliases, bool emit_file_header,
    bool emit_primary_template) -> std::string {
  out_.str("");
  indent_ = 0;
  port_symbols_.clear();
  used_type_aliases_ = TypeAlias::kNone;
  user_type_aliases_.clear();
  used_features_ = CodegenFeature::kNone;
  skip_sdk_aliases_ = skip_sdk_aliases;

  // Store timescale info for delay scaling
  timescale_ = module.timescale;
  if (module.timescale) {
    global_precision_power_ = module.timescale->precision_power;
  } else {
    global_precision_power_ = common::TimeScale::kDefaultPrecisionPower;
  }

  // Populate port symbols for identifier emission (output/inout ports only)
  // Input ports are public variables without underscore suffix
  for (const auto& port : module.ports) {
    if (port.direction != mir::PortDirection::kInput) {
      port_symbols_.insert(port.variable.symbol);
    }
  }

  // Two-phase generation: generate class first to collect feature usage,
  // then emit header with conditional includes, then append class.
  std::ostringstream class_out;
  class_out.swap(out_);
  EmitClass(module);
  std::string class_content = out_.str();
  out_.swap(class_out);  // Restore empty output stream

  // Emit SDK includes only for first module in file
  if (emit_file_header) {
    EmitHeader(module, UsesArrayType(module));
  }

  // Emit primary template forward declaration if requested
  if (emit_primary_template && !module.parameters.empty()) {
    EmitPrimaryTemplateDecl(module);
  }

  // Append class content
  out_ << class_content;

  return out_.str();
}

auto Codegen::DelayMultiplier() const -> uint64_t {
  if (timescale_) {
    return timescale_->DelayMultiplier(global_precision_power_);
  }
  return common::TimeScale::Default().DelayMultiplier(global_precision_power_);
}

void Codegen::EmitHeader(const mir::Module& module, bool uses_arrays) {
  // System headers
  bool has_system_headers = false;
  if (uses_arrays) {
    Line("#include <array>");
    has_system_headers = true;
  }
  if ((used_features_ & CodegenFeature::kCmath) != CodegenFeature::kNone) {
    Line("#include <cmath>");
    has_system_headers = true;
  }
  if ((used_features_ & CodegenFeature::kDisplay) != CodegenFeature::kNone) {
    Line("#include <iostream>");
    Line("#include <print>");
    has_system_headers = true;
  }
  if ((used_features_ & CodegenFeature::kMemIo) != CodegenFeature::kNone) {
    has_system_headers = true;
  }
  if (has_system_headers) {
    Line("");
  }
  // Lyra SDK header
  Line("#include <lyra/sdk/sdk.hpp>");
  if ((used_features_ & CodegenFeature::kMemIo) != CodegenFeature::kNone) {
    Line("#include <lyra/sdk/mem_io.hpp>");
  }
  if ((used_features_ & CodegenFeature::kPlusargs) != CodegenFeature::kNone) {
    Line("#include <lyra/sdk/plusargs.hpp>");
  }

  // Include headers for submodule types
  std::unordered_set<std::string> included;
  for (const auto& submod : module.submodules) {
    if (!included.contains(submod.module_type)) {
      Line("#include \"" + submod.module_type + ".hpp\"");
      included.insert(submod.module_type);
    }
  }

  Line("");

  // Emit using directives for package imports
  for (const auto& pkg_name : module.wildcard_imports) {
    Line(std::format("using namespace {};", pkg_name));
  }
  for (const auto& [pkg, sym] : module.explicit_imports) {
    Line(std::format("using {}::{};", pkg, sym));
  }
  if (!module.wildcard_imports.empty() || !module.explicit_imports.empty()) {
    Line("");
  }
}

void Codegen::EmitTypeAliases() {
  bool any_emitted = false;

  // Emit SDK type aliases unless they're already at file scope (from
  // packages.hpp)
  if (!skip_sdk_aliases_) {
    if ((used_type_aliases_ & TypeAlias::kBit) != TypeAlias::kNone) {
      Line("template <std::size_t N, bool S = false>");
      Line("using Bit = lyra::sdk::Bit<N, S>;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kInt) != TypeAlias::kNone) {
      Line("using Int = lyra::sdk::Int;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kLongInt) != TypeAlias::kNone) {
      Line("using LongInt = lyra::sdk::LongInt;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kShortInt) != TypeAlias::kNone) {
      Line("using ShortInt = lyra::sdk::ShortInt;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kByte) != TypeAlias::kNone) {
      Line("using Byte = lyra::sdk::Byte;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kReal) != TypeAlias::kNone) {
      Line("using Real = double;");
      any_emitted = true;
    }
    if ((used_type_aliases_ & TypeAlias::kShortReal) != TypeAlias::kNone) {
      Line("using ShortReal = float;");
      any_emitted = true;
    }
  }

  // Emit user-defined type aliases (typedef) - always needed for local typedefs
  for (const auto& [name, def] : user_type_aliases_) {
    Line(std::format("using {} = {};", name, def));
    any_emitted = true;
  }

  if (any_emitted) {
    Line("");
  }
}

void Codegen::EmitTimescaleConstants(const mir::Module& module) {
  // Only emit timescale constants that were actually used
  bool any_emitted = false;

  if ((used_features_ & CodegenFeature::kTimeDivisor) !=
      CodegenFeature::kNone) {
    Line(
        "static constexpr uint64_t kTimeDivisor = " +
        std::to_string(DelayMultiplier()) + ";");
    any_emitted = true;
  }
  if ((used_features_ & CodegenFeature::kModuleUnitPower) !=
      CodegenFeature::kNone) {
    int8_t module_unit_power = timescale_
                                   ? timescale_->unit_power
                                   : common::TimeScale::kDefaultUnitPower;
    Line(
        "static constexpr int8_t kModuleUnitPower = " +
        std::to_string(static_cast<int>(module_unit_power)) + ";");
    any_emitted = true;
  }
  if ((used_features_ & CodegenFeature::kModulePrecisionPower) !=
      CodegenFeature::kNone) {
    int8_t module_precision_power =
        timescale_ ? timescale_->precision_power
                   : common::TimeScale::kDefaultPrecisionPower;
    Line(
        "static constexpr int8_t kModulePrecisionPower = " +
        std::to_string(static_cast<int>(module_precision_power)) + ";");
    any_emitted = true;
  }
  if ((used_features_ & CodegenFeature::kModuleName) != CodegenFeature::kNone) {
    Line(
        "static constexpr std::string_view kModuleName = \"" + module.name +
        "\";");
    any_emitted = true;
  }
  if ((used_features_ & CodegenFeature::kTimescaleStr) !=
      CodegenFeature::kNone) {
    auto ts = timescale_.value_or(common::TimeScale::Default());
    Line(
        "static constexpr std::string_view kTimescaleStr = \"" + ts.ToString() +
        "\";");
    any_emitted = true;
  }

  if (any_emitted) {
    Line("");
  }
}

void Codegen::EmitPrimaryTemplateDecl(const mir::Module& module) {
  Indent();
  out_ << "template <";
  bool first = true;
  for (const auto& param : module.parameters) {
    if (!first) {
      out_ << ", ";
    }
    first = false;
    out_ << ToCppRawType(param.type) << " " << param.name;
  }
  out_ << ">\n";
  Line("class " + module.name + ";");
  out_ << "\n";
}

void Codegen::EmitClass(const mir::Module& module) {
  // Two-phase generation: first generate body to collect type alias and feature
  // usage, then emit class with only the aliases/constants that were used.

  // Phase 1: Save current output stream and generate body to a temporary buffer
  std::ostringstream saved_out;
  saved_out.swap(out_);
  int saved_indent = indent_;
  indent_ = 1;  // Body is indented inside class

  // Generate body content (this populates used_type_aliases_ and
  // used_features_)
  indent_--;
  Line(" public:");
  indent_++;

  // Input port member variables (public, for parent to drive)
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      std::string type_str = ToCppType(port.variable.type);
      Line(type_str + " " + std::string(port.variable.symbol->name) + ";");
    }
  }

  // Constructor with port parameters (output/inout ports only)
  Indent();
  out_ << module.name << "(";

  // Generate port parameters (output/inout ports only)
  bool first = true;
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      continue;  // Input ports are public variables, not constructor parameters
    }
    if (!first) {
      out_ << ", ";
    }
    first = false;
    std::string type_str = ToCppType(port.variable.type);
    out_ << type_str << "& " << port.variable.symbol->name;
  }

  out_ << ") : Module(\"" << module.name << "\")";

  // Port initializers (output/inout ports only)
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      continue;  // Input ports are public variables, not reference members
    }
    out_ << ", " << port.variable.symbol->name << "_("
         << port.variable.symbol->name << ")";
  }

  // Submodule initializers - pass output port bindings as references
  for (const auto& submod : module.submodules) {
    out_ << ", " << submod.instance_name << "_(";
    bool first = true;
    for (const auto& binding : submod.output_bindings) {
      if (!first) {
        out_ << ", ";
      }
      first = false;
      EmitExpression(*binding.signal);
    }
    out_ << ")";
  }

  out_ << " {\n";
  indent_++;
  for (const auto& process : module.processes) {
    Line("RegisterProcess(&" + module.name + "::" + process->name + ");");
  }
  // Register child modules for hierarchical execution
  for (const auto& submod : module.submodules) {
    Line("RegisterChild(&" + submod.instance_name + "_);");
  }
  indent_--;
  Line("}");
  Line("");

  // User-defined functions (helper methods, emitted before processes)
  for (const auto& function : module.functions) {
    EmitFunction(function);
  }

  // Process methods
  for (const auto& process : module.processes) {
    EmitProcess(*process);
  }

  // Member variables
  EmitVariables(module.variables);

  // Output/inout port reference members (must be before submodules for
  // initialization order - C++ initializes members in declaration order)
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      continue;  // Input ports declared as public variables above
    }
    std::string type_str = ToCppType(port.variable.type);
    Line(type_str + "& " + std::string(port.variable.symbol->name) + "_;");
  }

  // Submodule members (after port references so they can use them)
  for (const auto& submod : module.submodules) {
    Indent();
    out_ << submod.module_type;
    // Add template arguments if the submodule has parameter overrides
    if (!submod.parameter_overrides.empty()) {
      out_ << "<";
      bool first = true;
      for (const auto& override : submod.parameter_overrides) {
        if (!first) {
          out_ << ", ";
        }
        first = false;
        EmitConstantExpression(*override.value);
      }
      out_ << ">";
    }
    out_ << " " << submod.instance_name << "_;\n";
  }

  // Phase 2: Now emit the class with conditional type aliases, then append body
  std::string body = out_.str();
  out_.swap(saved_out);  // Restore original output stream
  indent_ = saved_indent;

  // Emit explicit specialization marker if parameterized
  if (!module.parameters.empty()) {
    Line("template <>");
  }

  // Emit class declaration (with template args if parameterized)
  Indent();
  out_ << "class " << module.name;
  if (!module.parameters.empty()) {
    out_ << "<";
    bool first = true;
    for (const auto& param : module.parameters) {
      if (!first) {
        out_ << ", ";
      }
      first = false;
      if (param.default_value) {
        EmitConstantExpression(*param.default_value);
      }
    }
    out_ << ">";
  }
  out_ << " : public lyra::sdk::Module {\n";
  indent_++;

  // Emit only the type aliases that were actually used
  EmitTypeAliases();

  // Emit only the timescale constants that were actually used
  EmitTimescaleConstants(module);

  // Append the body content
  out_ << body;

  // Close the class
  indent_--;
  Line("};");
}

void Codegen::EmitVariables(const std::vector<mir::ModuleVariable>& variables) {
  for (const auto& mod_var : variables) {
    std::string type_str = ToCppType(mod_var.variable.type);
    Indent();
    out_ << type_str << " " << mod_var.variable.symbol->name;
    if (mod_var.initializer) {
      out_ << " = ";
      EmitExpression(*mod_var.initializer);
    } else {
      out_ << "{}";
    }
    out_ << ";\n";
  }
}

void Codegen::EmitProcess(const mir::Process& process) {
  Line("auto " + process.name + "() -> lyra::sdk::Task {");
  indent_++;
  if (process.body) {
    EmitStatement(*process.body);
  }
  Line("co_return;");
  indent_--;
  Line("}");
  Line("");
}

void Codegen::EmitFunction(const mir::FunctionDefinition& function) {
  Indent();
  out_ << "auto " << function.name << "(";

  // Parameters
  bool first = true;
  for (const auto& param : function.parameters) {
    if (!first) {
      out_ << ", ";
    }
    first = false;
    out_ << ToCppType(param.variable.type) << " "
         << param.variable.symbol->name;
  }

  out_ << ") -> " << ToCppType(function.return_type) << " {\n";
  indent_++;

  // Body (local variables are emitted via VariableDeclarationStatement)
  if (function.body) {
    EmitStatement(*function.body);
  }

  indent_--;
  Line("}");
  Line("");
}

auto Codegen::GeneratePackages(
    const std::vector<std::unique_ptr<mir::Package>>& packages) -> std::string {
  out_.str("");
  indent_ = 0;
  used_type_aliases_ = TypeAlias::kNone;
  user_type_aliases_.clear();

  // Generate package namespaces
  for (const auto& pkg : packages) {
    Line(std::format("namespace {} {{", pkg->name));
    indent_++;

    for (const auto& type_decl : pkg->types) {
      // Emit typedef
      std::string cpp_type = ToCppType(type_decl.type);
      Line(std::format("using {} = {};", type_decl.name, cpp_type));

      // If enum, emit enum constants as constexpr values
      for (const auto& member : type_decl.members) {
        Line(
            std::format(
                "inline constexpr {} {}{{{}}};", type_decl.name, member.name,
                member.value));
      }
    }

    // Emit package parameters as constexpr
    for (const auto& param : pkg->parameters) {
      std::string type_str = ToCppType(param.variable.type);
      Indent();
      out_ << "inline constexpr " << type_str << " "
           << param.variable.symbol->name << "{";
      EmitExpression(*param.initializer);
      out_ << "};\n";
    }

    // Emit package variables
    for (const auto& var : pkg->variables) {
      std::string type_str = ToCppType(var.variable.type);
      Indent();
      out_ << "inline " << type_str << " " << var.variable.symbol->name;
      if (var.initializer) {
        out_ << "{";
        EmitExpression(*var.initializer);
        out_ << "}";
      } else {
        out_ << "{}";
      }
      out_ << ";\n";
    }

    // Emit package functions
    for (const auto& func : pkg->functions) {
      EmitFunction(func);
    }

    indent_--;
    Line(std::format("}}  // namespace {}", pkg->name));
    Line("");
  }

  // Generate header with all SDK type aliases at file scope.
  // We emit ALL aliases (not just those used by packages) because modules that
  // include packages.hpp will skip their own SDK aliases, relying on these.
  std::ostringstream header;
  header << "#pragma once\n\n";
  header << "#include <lyra/sdk/sdk.hpp>\n\n";

  // Always emit all SDK type aliases so modules can rely on them
  header << "template <std::size_t N, bool S = false>\n";
  header << "using Bit = lyra::sdk::Bit<N, S>;\n";
  header << "using Int = lyra::sdk::Int;\n";
  header << "using LongInt = lyra::sdk::LongInt;\n";
  header << "using ShortInt = lyra::sdk::ShortInt;\n";
  header << "using Byte = lyra::sdk::Byte;\n";
  header << "using Real = double;\n";
  header << "using ShortReal = float;\n";
  header << "\n";

  return header.str() + out_.str();
}

auto Codegen::GenerateModuleHeader(
    const mir::Module& module, bool has_packages, bool emit_file_header,
    bool emit_primary_template) -> std::string {
  // When packages exist, skip SDK type aliases in the class - they're already
  // at file scope in packages.hpp
  std::string code =
      Generate(module, has_packages, emit_file_header, emit_primary_template);

  std::ostringstream header;
  if (emit_file_header) {
    header << "#pragma once\n\n";
    if (has_packages) {
      header << "#include \"packages.hpp\"\n\n";
    }
  } else {
    // For appended specializations, just add a blank line separator
    header << "\n";
  }
  header << code;
  return header.str();
}

}  // namespace lyra::compiler
