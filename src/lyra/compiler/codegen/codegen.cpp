#include "lyra/compiler/codegen/codegen.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <format>
#include <memory>
#include <string>
#include <string_view>
#include <unordered_set>
#include <utility>
#include <vector>

#include "lyra/common/type.hpp"
#include "lyra/compiler/codegen/utils.hpp"
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

// Count template parameters (packed/integral types that can affect signal
// types)
auto CountTemplateParams(const mir::Module& module) -> size_t {
  return static_cast<size_t>(std::ranges::count_if(
      module.parameters,
      [](const auto& p) { return common::IsTemplateParamType(p.type); }));
}

// Check if module should use a params struct (2+ template params)
auto UseParamsStruct(const mir::Module& module) -> bool {
  return CountTemplateParams(module) >= 2;
}

// Check if module has any template parameters
auto HasTemplateParams(const mir::Module& module) -> bool {
  return CountTemplateParams(module) > 0;
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

  // Build escape map for C++ keyword collision handling
  BuildEscapeMap(module);

  // Populate constructor param symbols for identifier emission
  // Constructor params are non-template types stored as class members
  constructor_param_symbols_.clear();
  for (const auto& param : module.parameters) {
    if (!common::IsTemplateParamType(param.type)) {
      constructor_param_symbols_.insert(param.symbol);
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

  // Emit params struct and primary template forward declaration if requested
  if (emit_primary_template && !module.parameters.empty()) {
    EmitParamsStruct(module);
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
  if (!HasTemplateParams(module)) {
    return;  // No template params, no forward declaration needed
  }

  bool use_params_struct = UseParamsStruct(module);

  Indent();
  out_ << "template <";
  bool first = true;

  if (use_params_struct) {
    // Use params struct for 2+ template params
    out_ << module.name << "_params params = {}";
  } else {
    // Individual template params (packed/integral types only)
    for (const auto& param : module.parameters) {
      if (!common::IsTemplateParamType(param.type)) {
        continue;  // Skip constructor params
      }
      if (!first) {
        out_ << ", ";
      }
      first = false;
      out_ << ToCppRawType(param.type) << " " << param.name;
    }
  }

  out_ << ">\n";
  Line("class " + module.name + ";");
  out_ << "\n";
}

void Codegen::EmitParamsStruct(const mir::Module& module) {
  if (!UseParamsStruct(module)) {
    return;  // No struct needed for <2 template params
  }

  out_ << "struct " << module.name << "_params {\n";

  // Emit template parameters (packed/integral) with their defaults
  for (const auto& param : module.parameters) {
    if (!common::IsTemplateParamType(param.type)) {
      continue;  // Skip constructor params
    }
    out_ << "  " << ToCppRawType(param.type) << " " << param.name;
    if (param.default_value) {
      out_ << " = ";
      EmitConstantExpression(*param.default_value);
    }
    out_ << ";\n";
  }

  // Add comparison operator for NTTP (required for C++20 structural type)
  out_ << "  constexpr auto operator<=>(const " << module.name
       << "_params&) const = default;\n";
  out_ << "};\n\n";
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
      Line(type_str + " " + Escape(port.variable.symbol->name) + ";");
    }
  }

  // Constructor with parameters
  Indent();
  out_ << module.name << "(";

  bool first = true;

  // Constructor params (non-template types: string, unpacked struct, etc.)
  for (const auto& param : module.parameters) {
    if (common::IsTemplateParamType(param.type)) {
      continue;  // Template params, not constructor params
    }
    if (!first) {
      out_ << ", ";
    }
    first = false;
    out_ << ToCppType(param.type) << " " << param.name;
    if (param.default_value) {
      out_ << " = ";
      EmitConstantExpression(*param.default_value);
    }
  }

  // Port parameters (output/inout ports only)
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      continue;  // Input ports are public variables, not constructor parameters
    }
    if (!first) {
      out_ << ", ";
    }
    first = false;
    std::string type_str = ToCppType(port.variable.type);
    out_ << type_str << "& " << Escape(port.variable.symbol->name);
  }

  out_ << ") : Module(\"" << module.name << "\")";

  // Constructor param initializers
  for (const auto& param : module.parameters) {
    if (common::IsTemplateParamType(param.type)) {
      continue;  // Template params, no member storage
    }
    out_ << ", " << param.name << "_(" << param.name << ")";
  }

  // Port initializers (output/inout ports only)
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      continue;  // Input ports are public variables, not reference members
    }
    out_ << ", " << Escape(port.variable.symbol->name) << "_("
         << Escape(port.variable.symbol->name) << ")";
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
  if (!module.processes.empty()) {
    std::string call = "RegisterProcesses<" + module.name + ">(";
    for (size_t i = 0; i < module.processes.size(); ++i) {
      if (i > 0) {
        call += ", ";
      }
      call += "&" + module.name + "::" + Escape(module.processes[i]->name);
    }
    call += ");";
    Line(call);
  }
  if (!module.submodules.empty()) {
    std::string call = "RegisterChildren(";
    for (size_t i = 0; i < module.submodules.size(); ++i) {
      if (i > 0) {
        call += ", ";
      }
      call += "&" + module.submodules[i].instance_name + "_";
    }
    call += ");";
    Line(call);
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

  // Generate block struct types and members
  EmitGenerateBlockArrays(module.generate_block_arrays);
  EmitGenerateBlocks(module.generate_blocks);

  // Regular member variables (non-generate-block)
  EmitVariables(module.variables);

  // Constructor param members (non-template types: string, unpacked struct)
  for (const auto& param : module.parameters) {
    if (common::IsTemplateParamType(param.type)) {
      continue;  // Template params, no member storage
    }
    Line(ToCppType(param.type) + " " + param.name + "_;");
  }

  // Output/inout port reference members (must be before submodules for
  // initialization order - C++ initializes members in declaration order)
  for (const auto& port : module.ports) {
    if (port.direction == mir::PortDirection::kInput) {
      continue;  // Input ports declared as public variables above
    }
    std::string type_str = ToCppType(port.variable.type);
    Line(type_str + "& " + Escape(port.variable.symbol->name) + "_;");
  }

  // Submodule members (after port references so they can use them)
  for (const auto& submod : module.submodules) {
    if (submod.parameter_overrides.empty()) {
      // No template args needed
      Line(submod.module_type + " " + submod.instance_name + "_;");
      continue;
    }

    // Count template param overrides to decide on params struct
    size_t template_param_count = 0;
    for (const auto& override : submod.parameter_overrides) {
      if (common::IsTemplateParamType(override.value->type)) {
        template_param_count++;
      }
    }

    bool use_params_struct = template_param_count >= 2;

    if (use_params_struct) {
      // Emit static constexpr config variable for this child
      std::string config_name = submod.instance_name + "_params_";
      Indent();
      out_ << "static constexpr " << submod.module_type << "_params "
           << config_name << "{";
      bool first_param = true;
      for (const auto& override : submod.parameter_overrides) {
        if (!common::IsTemplateParamType(override.value->type)) {
          continue;  // Constructor params don't go in params struct
        }
        if (!first_param) {
          out_ << ", ";
        }
        first_param = false;
        out_ << "." << override.parameter_name << " = ";
        EmitConstantExpression(*override.value);
      }
      out_ << "};\n";

      // Emit member with type: Module<config_name>
      Indent();
      out_ << submod.module_type << "<" << config_name << "> "
           << submod.instance_name << "_;\n";
    } else if (template_param_count > 0) {
      // Use inline template args (for 1 template param)
      Indent();
      out_ << submod.module_type << "<";
      bool first = true;
      for (const auto& override : submod.parameter_overrides) {
        if (!common::IsTemplateParamType(override.value->type)) {
          continue;  // Skip constructor params
        }
        if (!first) {
          out_ << ", ";
        }
        first = false;
        EmitConstantExpression(*override.value);
      }
      out_ << "> " << submod.instance_name << "_;\n";
    } else {
      // No template params - just the type name
      Line(submod.module_type + " " + submod.instance_name + "_;");
    }
    // TODO(hankhsu): Handle constructor param overrides for submodules
  }

  // Phase 2: Now emit the class with conditional type aliases, then append body
  std::string body = out_.str();
  out_.swap(saved_out);  // Restore original output stream
  indent_ = saved_indent;

  // Emit explicit specialization marker if has template params
  if (HasTemplateParams(module)) {
    Line("template <>");
  }

  // Emit class declaration (with template args if has template params)
  Indent();
  out_ << "class " << module.name;
  if (HasTemplateParams(module)) {
    bool use_params_struct = UseParamsStruct(module);

    out_ << "<";

    if (use_params_struct) {
      // Use params struct with designated initializers
      out_ << module.name << "_params{";
      bool first_param = true;
      for (const auto& param : module.parameters) {
        if (!common::IsTemplateParamType(param.type)) {
          continue;  // Skip constructor params
        }
        if (!first_param) {
          out_ << ", ";
        }
        first_param = false;
        out_ << "." << param.name << " = ";
        if (param.default_value) {
          EmitConstantExpression(*param.default_value);
        }
      }
      out_ << "}";
    } else {
      // Individual template param values
      bool first = true;
      for (const auto& param : module.parameters) {
        if (!common::IsTemplateParamType(param.type)) {
          continue;  // Skip constructor params
        }
        if (!first) {
          out_ << ", ";
        }
        first = false;
        if (param.default_value) {
          EmitConstantExpression(*param.default_value);
        }
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
    out_ << type_str << " " << Escape(mod_var.variable.symbol->name);
    if (mod_var.initializer) {
      out_ << " = ";
      EmitExpression(*mod_var.initializer);
    } else {
      out_ << "{}";
    }
    out_ << ";\n";
  }
}

void Codegen::EmitGenerateBlockStruct(
    const std::string& name,
    const std::vector<mir::ModuleVariable>& variables) {
  std::string struct_name = name + "_s";
  Line("struct " + struct_name + " {");
  indent_++;

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

  indent_--;
  Line("};");
}

void Codegen::EmitGenerateBlockArrays(
    const std::vector<mir::GenerateBlockArray>& generate_block_arrays) {
  for (const auto& gen_block : generate_block_arrays) {
    EmitGenerateBlockStruct(gen_block.name, gen_block.variables);

    // Emit array member
    Indent();
    out_ << "std::array<" << gen_block.name << "_s, " << gen_block.size << "> "
         << gen_block.name << "_{};\n";
  }
}

void Codegen::EmitGenerateBlocks(
    const std::vector<mir::GenerateBlock>& generate_blocks) {
  for (const auto& gen_block : generate_blocks) {
    EmitGenerateBlockStruct(gen_block.name, gen_block.variables);

    // Emit single instance member (not an array)
    Indent();
    out_ << gen_block.name << "_s " << gen_block.name << "_{};\n";
  }
}

void Codegen::EmitProcess(const mir::Process& process) {
  temp_counter_ = 0;  // Reset for unique temp names within this function
  Line("auto " + Escape(process.name) + "() -> lyra::sdk::Task {");
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
  temp_counter_ = 0;  // Reset for unique temp names within this function
  Indent();
  out_ << "auto " << Escape(function.name) << "(";

  // Parameters
  bool first = true;
  for (const auto& param : function.parameters) {
    if (!first) {
      out_ << ", ";
    }
    first = false;
    out_ << ToCppType(param.variable.type) << " "
         << Escape(param.variable.symbol->name);
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
    // Build escape map for this package
    BuildEscapeMap(*pkg);

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
           << Escape(param.variable.symbol->name) << "{";
      EmitExpression(*param.initializer);
      out_ << "};\n";
    }

    // Emit package variables
    for (const auto& var : pkg->variables) {
      std::string type_str = ToCppType(var.variable.type);
      Indent();
      out_ << "inline " << type_str << " " << Escape(var.variable.symbol->name);
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

auto Codegen::GenerateAllModules(
    const std::vector<std::unique_ptr<mir::Module>>& modules, bool has_packages)
    -> std::vector<ModuleOutput> {
  std::vector<ModuleOutput> outputs;

  // Track emitted signatures to deduplicate (per-instance modules share
  // signatures)
  std::unordered_set<std::string> emitted_signatures;
  // Track which module names have been written (for template specializations)
  std::unordered_set<std::string> written_modules;

  for (const auto& mir : modules) {
    // Skip if this signature was already emitted
    if (emitted_signatures.contains(mir->signature)) {
      continue;
    }
    emitted_signatures.insert(mir->signature);

    bool is_first_in_file = !written_modules.contains(mir->name);
    written_modules.insert(mir->name);

    bool emit_file_header = is_first_in_file;
    bool emit_primary_template = is_first_in_file;
    std::string content = GenerateModuleHeader(
        *mir, has_packages, emit_file_header, emit_primary_template);

    outputs.push_back({
        .filename = mir->name + ".hpp",
        .content = std::move(content),
        .append = !is_first_in_file,
    });
  }

  return outputs;
}

void Codegen::BuildEscapeMap(const mir::Module& module) {
  escape_map_.clear();

  // Collect all identifier names in this module
  std::unordered_set<std::string_view> all_names;

  // Port names
  for (const auto& port : module.ports) {
    all_names.insert(port.variable.symbol->name);
  }

  // Module variable names
  for (const auto& var : module.variables) {
    all_names.insert(var.variable.symbol->name);
  }

  // Function names and their parameter names
  for (const auto& func : module.functions) {
    all_names.insert(func.name);
    for (const auto& param : func.parameters) {
      all_names.insert(param.variable.symbol->name);
    }
  }

  // Process names
  for (const auto& process : module.processes) {
    all_names.insert(process->name);
  }

  // Submodule instance names
  for (const auto& submod : module.submodules) {
    all_names.insert(submod.instance_name);
  }

  // Build escape map for keywords
  for (const auto& name : all_names) {
    if (codegen::IsCppKeyword(name)) {
      escape_map_[std::string(name)] =
          codegen::EscapeIdentifier(name, all_names);
    }
  }
}

void Codegen::BuildEscapeMap(const mir::Package& package) {
  escape_map_.clear();

  // Collect all identifier names in this package
  std::unordered_set<std::string_view> all_names;

  // Parameter names
  for (const auto& param : package.parameters) {
    all_names.insert(param.variable.symbol->name);
  }

  // Variable names
  for (const auto& var : package.variables) {
    all_names.insert(var.variable.symbol->name);
  }

  // Function names and their parameter names
  for (const auto& func : package.functions) {
    all_names.insert(func.name);
    for (const auto& param : func.parameters) {
      all_names.insert(param.variable.symbol->name);
    }
  }

  // Build escape map for keywords
  for (const auto& name : all_names) {
    if (codegen::IsCppKeyword(name)) {
      escape_map_[std::string(name)] =
          codegen::EscapeIdentifier(name, all_names);
    }
  }
}

auto Codegen::Escape(std::string_view name) const -> std::string {
  // Check if name has a pre-computed escape (for collision handling)
  auto it = escape_map_.find(std::string(name));
  if (it != escape_map_.end()) {
    return it->second;
  }
  // Fall back to simple escaping for local variables not in the map
  return codegen::EscapeIdentifier(name);
}

auto Codegen::EscapeQualified(std::string_view qualified_name) const
    -> std::string {
  // Find the last "::" separator
  auto pos = qualified_name.rfind("::");
  if (pos == std::string_view::npos) {
    // No qualifier - use the collision-aware Escape
    return Escape(qualified_name);
  }
  // Has qualifier - escape only the function name part
  std::string_view qualifier =
      qualified_name.substr(0, pos + 2);  // Include "::"
  std::string_view name = qualified_name.substr(pos + 2);
  return std::string(qualifier) + Escape(name);
}

}  // namespace lyra::compiler
