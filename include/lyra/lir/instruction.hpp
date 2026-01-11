#pragma once

#include <cassert>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/ranges.h>

#include "lyra/common/trigger.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::lir {

// Forward declaration for callee pointer
struct Function;

enum class InstructionKind {
  // Memory operations
  kLiteral,
  kLoadVariable,
  kStoreVariable,
  kStoreVariableNonBlocking,
  kLoadElement,              // Load from array/struct: base[index]
  kStoreElement,             // Store to array/struct: base[index] = value
  kStoreElementNonBlocking,  // NBA to array/struct: base[index] <= value
  kLoadPackedBits,           // Extract bits from packed: vec[offset+:width]
  kStorePackedBits,  // Insert bits to packed: vec[offset+:width] = value
  kCreateAggregate,  // Create default-initialized struct/array in temp
  kNewDynamicArray,  // Allocate/resize dynamic array: new[size] or
                     // new[size](init)

  // Move operation
  kMove,

  // Unary operations
  kUnaryPlus,
  kUnaryMinus,
  kUnaryLogicalNot,
  kUnaryBitwiseNot,

  // Reduction operations
  kReductionAnd,
  kReductionNand,
  kReductionOr,
  kReductionNor,
  kReductionXor,
  kReductionXnor,

  // Binary operations
  kBinaryAdd,
  kBinarySubtract,
  kBinaryMultiply,
  kBinaryDivide,
  kBinaryModulo,
  kBinaryEqual,
  kBinaryNotEqual,
  kBinaryLessThan,
  kBinaryLessThanEqual,
  kBinaryGreaterThan,
  kBinaryGreaterThanEqual,

  // Power operation
  kBinaryPower,

  // Bitwise operations
  kBinaryBitwiseAnd,
  kBinaryBitwiseOr,
  kBinaryBitwiseXor,
  kBinaryBitwiseXnor,

  // Logical operations
  kBinaryLogicalAnd,
  kBinaryLogicalOr,

  // Shift operations
  kBinaryLogicalShiftLeft,
  kBinaryLogicalShiftRight,
  kBinaryArithmeticShiftLeft,
  kBinaryArithmeticShiftRight,

  // Type operations
  kConversion,

  // Concatenation
  kConcatenation,

  // Control flow
  kComplete,
  kWaitEvent,
  kDelay,
  kSystemCall,
  kMethodCall,  // Generic method call (enum methods, future: string/class
                // methods)
  kJump,
  kBranch,

  // Function call/return
  kCall,    // Call user-defined function
  kReturn,  // Return from function

  // Closure captures (persistent local variables for closures like $monitor)
  kLoadCapture,   // Load from closure's captures map
  kStoreCapture,  // Store to closure's captures map
};

// Enum member info for method call runtime helpers
struct EnumMemberInfo {
  std::string name;
  int64_t value;
};

struct Instruction {
  InstructionKind kind{};

  // Destination temporary name, or empty if not applicable
  std::optional<TempRef> result{};
  std::optional<common::Type> result_type{};

  // Operand values used by the instruction
  std::vector<Operand> operands{};

  // System call name (for kSystemCall)
  std::string system_call_name{};
  std::vector<SymbolRef> output_targets{};

  // Function call (for kCall)
  std::string called_function_name{};  // For error messages and ToString()
  const Function* callee{nullptr};     // Resolved by LinkFunctionCalls

  // Event name
  std::vector<common::Trigger> wait_triggers{};

  // Hierarchical access: instance path + target symbol
  // instance_path: symbols for instance traversal (e.g., u_child instance
  // symbol) target_symbol: the final variable symbol to access
  std::vector<SymbolRef> instance_path{};
  SymbolRef target_symbol{nullptr};

  // Method call: method name and metadata
  // For enum methods: method_name is "next", "prev", or "name"
  // operands[0] = receiver
  std::string method_name{};
  int64_t method_step{1};  // For next(N)/prev(N), default 1
  std::vector<EnumMemberInfo> enum_members{};

  // For $monitor: name of synthesized check function.
  std::string monitor_check_function_name{};

  // For kLoadCapture/kStoreCapture: capture variable name
  std::string capture_name{};

  // For display-like system calls: explicit format string expression.
  // Separated from operands at MIR level, lowered to optional operand in LIR.
  // nullopt means no explicit format string (auto-format mode).
  // Also used for mem_io tasks ($readmemh, etc.) where it indicates filename.
  std::optional<Operand> format_operand{};

  // True if format_operand (or first operand for mem_io) is a string literal.
  // For display tasks: enables compile-time format parsing.
  // For mem_io tasks: enables filename extraction from integral literal.
  bool format_string_is_literal{false};

  // Source location for severity tasks ($fatal, $error, $warning, $info).
  // Propagated from MIR SystemCallExpression.
  std::optional<std::string> source_file{};
  std::optional<uint32_t> source_line{};

  static auto Basic(
      InstructionKind kind, TempRef result, std::vector<Operand> operands)
      -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .operands = std::move(operands)};
  }

  static auto Basic(InstructionKind kind, TempRef result, Operand operand)
      -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .operands = {std::move(operand)}};
  }

  static auto Basic(InstructionKind kind, TempRef result, TempRef operand)
      -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .operands = {Operand::Temp(operand)}};
  }

  static auto Basic(InstructionKind kind, TempRef result, SymbolRef operand)
      -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .operands = {Operand::Variable(operand)}};
  }

  static auto Basic(InstructionKind kind, TempRef result, LiteralRef operand)
      -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .operands = {Operand::Literal(operand)}};
  }

  static auto WithType(
      InstructionKind kind, TempRef result, TempRef operand,
      common::Type result_type) -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .result_type = std::move(result_type),
        .operands = {Operand::Temp(operand)}};
  }

  static auto StoreVariable(
      SymbolRef variable, TempRef value, bool is_non_blocking) -> Instruction {
    return Instruction{
        .kind = is_non_blocking ? InstructionKind::kStoreVariableNonBlocking
                                : InstructionKind::kStoreVariable,
        .operands = {Operand::Variable(variable), Operand::Temp(value)}};
  }

  // Load element from array/struct: result = base[index]
  // base can be variable or temp (polymorphic)
  static auto LoadElement(
      TempRef result, Operand base, TempRef index, common::Type element_type)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kLoadElement,
        .result = result,
        .result_type = std::move(element_type),
        .operands = {std::move(base), Operand::Temp(index)}};
  }

  // Store element to array/struct: base[index] = value
  // base can be variable or temp (polymorphic)
  // Sensitivity tracking only triggered if base is variable
  static auto StoreElement(
      Operand base, TempRef index, TempRef value, bool is_non_blocking = false)
      -> Instruction {
    return Instruction{
        .kind = is_non_blocking ? InstructionKind::kStoreElementNonBlocking
                                : InstructionKind::kStoreElement,
        .operands = {
            std::move(base), Operand::Temp(index), Operand::Temp(value)}};
  }

  // Load bits from packed vector: result = value[bit_offset +: width]
  // bit_offset is pre-computed (index * element_width for arrays, or literal
  // for struct fields). Width comes from result_type.
  static auto LoadPackedBits(
      TempRef result, TempRef value, TempRef bit_offset,
      common::Type result_type) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kLoadPackedBits,
        .result = result,
        .result_type = std::move(result_type),
        .operands = {Operand::Temp(value), Operand::Temp(bit_offset)}};
  }

  // Store bits to packed vector: variable[bit_offset +: width] = value
  // bit_offset is pre-computed. Width comes from slice_type.
  static auto StorePackedBits(
      SymbolRef variable, TempRef bit_offset, TempRef value,
      common::Type slice_type) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kStorePackedBits,
        .result_type = std::move(slice_type),
        .operands = {
            Operand::Variable(variable), Operand::Temp(bit_offset),
            Operand::Temp(value)}};
  }

  // Create default-initialized aggregate (struct or array) in temp
  static auto CreateAggregate(TempRef result, common::Type aggregate_type)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kCreateAggregate,
        .result = result,
        .result_type = std::move(aggregate_type),
        .operands = {}};
  }

  // Create new dynamic array: result = new[size] or new[size](init)
  // operands[0] = size temp
  // operands[1] = init array temp (optional)
  static auto NewDynamicArray(
      TempRef result, TempRef size, common::Type result_type,
      std::optional<TempRef> init = std::nullopt) -> Instruction {
    std::vector<Operand> ops = {Operand::Temp(size)};
    if (init) {
      ops.push_back(Operand::Temp(*init));
    }
    return Instruction{
        .kind = InstructionKind::kNewDynamicArray,
        .result = result,
        .result_type = std::move(result_type),
        .operands = std::move(ops)};
  }

  // Store to hierarchical target: instance.signal = value
  // instance_path: instance symbols to traverse
  // target: the variable symbol to write
  static auto StoreHierarchical(
      std::vector<SymbolRef> instance_path, SymbolRef target, TempRef value,
      bool is_non_blocking) -> Instruction {
    assert(!instance_path.empty() && "Must have at least one instance");
    assert(target != nullptr && "Must have target symbol");
    return Instruction{
        .kind = is_non_blocking ? InstructionKind::kStoreVariableNonBlocking
                                : InstructionKind::kStoreVariable,
        .operands = {Operand::Temp(value)},
        .instance_path = std::move(instance_path),
        .target_symbol = target};
  }

  // Load from hierarchical target: result = instance.signal
  // instance_path: instance symbols to traverse
  // target: the variable symbol to read
  static auto LoadHierarchical(
      TempRef result, std::vector<SymbolRef> instance_path, SymbolRef target,
      common::Type result_type) -> Instruction {
    assert(!instance_path.empty() && "Must have at least one instance");
    assert(target != nullptr && "Must have target symbol");
    return Instruction{
        .kind = InstructionKind::kLoadVariable,
        .result = result,
        .result_type = std::move(result_type),
        .instance_path = std::move(instance_path),
        .target_symbol = target};
  }

  static auto WaitEvent(std::vector<common::Trigger> triggers) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kWaitEvent,
        .wait_triggers = std::move(triggers)};
  }

  static auto Delay(Operand delay) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kDelay, .operands = {std::move(delay)}};
  }

  // System call instruction
  // For system tasks (no return value): result = std::nullopt
  // For system functions (return value): result = temp to store result
  static auto SystemCall(
      std::string name, std::vector<Operand> operands,
      std::optional<TempRef> result = std::nullopt,
      std::optional<common::Type> result_type = std::nullopt) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kSystemCall,
        .result = result,
        .result_type = result_type,
        .operands = std::move(operands),
        .system_call_name = std::move(name)};
  }

  static auto SystemCall(
      std::string name, std::vector<TempRef> args,
      std::optional<TempRef> result = std::nullopt,
      std::optional<common::Type> result_type = std::nullopt) -> Instruction {
    std::vector<Operand> operands;
    for (auto& arg : args) {
      operands.push_back(Operand::Temp(arg));
    }
    return SystemCall(
        std::move(name), std::move(operands), result, std::move(result_type));
  }

  // Method call instruction (enum methods, future: string/class methods)
  // For enum methods: receiver is operands[0]
  static auto MethodCall(
      std::string method, TempRef receiver, TempRef result,
      common::Type result_type, int64_t step,
      std::vector<EnumMemberInfo> members) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kMethodCall,
        .result = result,
        .result_type = std::move(result_type),
        .operands = {Operand::Temp(receiver)},
        .method_name = std::move(method),
        .method_step = step,
        .enum_members = std::move(members)};
  }

  // System call for $monitor with check function name
  static auto SystemCallWithMonitor(
      std::string name, std::vector<TempRef> args, std::string check_function)
      -> Instruction {
    std::vector<Operand> operands;
    for (auto& arg : args) {
      operands.push_back(Operand::Temp(arg));
    }
    return Instruction{
        .kind = InstructionKind::kSystemCall,
        .operands = std::move(operands),
        .system_call_name = std::move(name),
        .monitor_check_function_name = std::move(check_function)};
  }

  static auto Complete() -> Instruction {
    return Instruction{
        .kind = InstructionKind::kComplete,
        .result = std::nullopt,
        .result_type = std::nullopt,
        .operands = {}};
  }

  static auto Jump(LabelRef label) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kJump, .operands = {Operand::Label(label)}};
  }

  static auto Branch(
      TempRef condition, LabelRef true_label, LabelRef false_label)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kBranch,
        .operands = {
            Operand::Temp(condition), Operand::Label(true_label),
            Operand::Label(false_label)}};
  }

  // Concatenation: result = {op0, op1, ...}
  // Operands are ordered MSB to LSB (first operand is most significant)
  static auto Concatenation(
      TempRef result, std::vector<TempRef> operands, common::Type result_type)
      -> Instruction {
    std::vector<Operand> ops;
    ops.reserve(operands.size());
    for (auto& op : operands) {
      ops.push_back(Operand::Temp(op));
    }
    return Instruction{
        .kind = InstructionKind::kConcatenation,
        .result = result,
        .result_type = std::move(result_type),
        .operands = std::move(ops)};
  }

  // User-defined function call: result = function(args...)
  // Arguments are stored in operands, result (if any) in result field
  static auto Call(
      std::string function_name, std::vector<Operand> arguments,
      std::optional<TempRef> result_temp,
      std::optional<common::Type> result_type_val) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kCall,
        .result = result_temp,
        .result_type = result_type_val,
        .operands = std::move(arguments),
        .called_function_name = std::move(function_name)};
  }

  // Return from function: return value (if any)
  // Return value operand (if any) is stored in operands[0]
  static auto Return(std::optional<TempRef> value = std::nullopt)
      -> Instruction {
    Instruction instr;
    instr.kind = InstructionKind::kReturn;
    if (value) {
      instr.operands.push_back(Operand::Temp(*value));
    }
    return instr;
  }

  /// Load value from closure's captures.
  /// result: temp to store the loaded value
  /// name: capture variable name
  static auto LoadCapture(
      TempRef result, std::string name, common::Type value_type)
      -> Instruction {
    Instruction instr;
    instr.kind = InstructionKind::kLoadCapture;
    instr.result = result;
    instr.result_type = std::move(value_type);
    instr.capture_name = std::move(name);
    return instr;
  }

  /// Store value to closure's captures.
  /// value: temp holding the value to store
  /// name: capture variable name
  static auto StoreCapture(TempRef value, std::string name) -> Instruction {
    Instruction instr;
    instr.kind = InstructionKind::kStoreCapture;
    instr.operands.push_back(Operand::Temp(value));
    instr.capture_name = std::move(name);
    return instr;
  }

  [[nodiscard]] auto ToString() const -> std::string {
    switch (kind) {
      // Memory operations
      case InstructionKind::kLiteral:
        return fmt::format(
            "lit   {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kLoadVariable:
        if (!instance_path.empty()) {
          return fmt::format(
              "load  {}, {}", result.value(),
              common::FormatHierarchicalPath(instance_path, target_symbol));
        }
        return fmt::format(
            "load  {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kStoreVariable:
        if (!instance_path.empty()) {
          return fmt::format(
              "store {}, {}",
              common::FormatHierarchicalPath(instance_path, target_symbol),
              operands[0].ToString());
        }
        return fmt::format(
            "store {}, {}", operands[0].ToString(), operands[1].ToString());

      case InstructionKind::kStoreVariableNonBlocking:
        if (!instance_path.empty()) {
          return fmt::format(
              "nba   {}, {}",
              common::FormatHierarchicalPath(instance_path, target_symbol),
              operands[0].ToString());
        }
        return fmt::format(
            "nba   {}, {}", operands[0].ToString(), operands[1].ToString());

      case InstructionKind::kLoadElement:
        return fmt::format(
            "ldel  {}, {}[{}]", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kStoreElement:
        return fmt::format(
            "stel  {}[{}], {}", operands[0].ToString(), operands[1].ToString(),
            operands[2].ToString());

      case InstructionKind::kStoreElementNonBlocking:
        return fmt::format(
            "stelnb {}[{}], {}", operands[0].ToString(), operands[1].ToString(),
            operands[2].ToString());

      case InstructionKind::kLoadPackedBits:
        return fmt::format(
            "ldpb  {}, {}[{}+:]", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kStorePackedBits:
        return fmt::format(
            "stpb  {}[{}+:], {}", operands[0].ToString(),
            operands[1].ToString(), operands[2].ToString());

      case InstructionKind::kCreateAggregate:
        return fmt::format("crag  {}", result.value());

      case InstructionKind::kNewDynamicArray:
        if (operands.size() > 1) {
          return fmt::format(
              "newarr {}, new[{}]({})", result.value(), operands[0].ToString(),
              operands[1].ToString());
        }
        return fmt::format(
            "newarr {}, new[{}]", result.value(), operands[0].ToString());

      case InstructionKind::kMove:
        return fmt::format(
            "move  {}, {}", result.value(), operands[0].ToString());

      // Unary operations
      case InstructionKind::kUnaryPlus:
        return fmt::format(
            "plus  {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kUnaryMinus:
        return fmt::format(
            "minus {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kUnaryLogicalNot:
        return fmt::format(
            "not.l {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kUnaryBitwiseNot:
        return fmt::format(
            "not.b {}, {}", result.value(), operands[0].ToString());

      // Reduction operations
      case InstructionKind::kReductionAnd:
        return fmt::format(
            "and.r {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kReductionNand:
        return fmt::format(
            "nand.r {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kReductionOr:
        return fmt::format(
            "or.r  {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kReductionNor:
        return fmt::format(
            "nor.r {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kReductionXor:
        return fmt::format(
            "xor.r {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kReductionXnor:
        return fmt::format(
            "xnor.r {}, {}", result.value(), operands[0].ToString());

      // Binary operations
      case InstructionKind::kBinaryAdd:
        return fmt::format(
            "add   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinarySubtract:
        return fmt::format(
            "sub   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryMultiply:
        return fmt::format(
            "mul   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryDivide:
        return fmt::format(
            "div   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryModulo:
        return fmt::format(
            "mod   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryEqual:
        return fmt::format(
            "eq    {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryNotEqual:
        return fmt::format(
            "neq   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLessThan:
        return fmt::format(
            "lt    {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLessThanEqual:
        return fmt::format(
            "lteq  {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryGreaterThan:
        return fmt::format(
            "gt    {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryGreaterThanEqual:
        return fmt::format(
            "gteq  {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryPower:
        return fmt::format(
            "pow   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryBitwiseAnd:
        return fmt::format(
            "and.b {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryBitwiseOr:
        return fmt::format(
            "or.b  {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryBitwiseXor:
        return fmt::format(
            "xor.b {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryBitwiseXnor:
        return fmt::format(
            "xnor.b {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLogicalAnd:
        return fmt::format(
            "and.l {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLogicalOr:
        return fmt::format(
            "or.l  {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLogicalShiftLeft:
        return fmt::format(
            "shl   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryLogicalShiftRight:
        return fmt::format(
            "shr   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryArithmeticShiftLeft:
        return fmt::format(
            "sal   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kBinaryArithmeticShiftRight:
        return fmt::format(
            "sar   {}, {}, {}", result.value(), operands[0].ToString(),
            operands[1].ToString());

      case InstructionKind::kConversion:
        return fmt::format(
            "cvt   {}, {}", result.value(), operands[0].ToString());

      case InstructionKind::kConcatenation: {
        std::string args;
        for (size_t i = 0; i < operands.size(); ++i) {
          if (i > 0) {
            args += ", ";
          }
          args += operands[i].ToString();
        }
        return fmt::format("cat   {}, {{{}}}", result.value(), args);
      }

      case InstructionKind::kWaitEvent: {
        std::string result = "wait  ";
        for (size_t i = 0; i < wait_triggers.size(); ++i) {
          if (i > 0) {
            result += " or ";
          }
          result += wait_triggers[i].ToString();
        }
        return result;
      }

      case InstructionKind::kComplete:
        return "complete";

      case InstructionKind::kDelay:
        return fmt::format("delay {}", operands[0].ToString());

      case InstructionKind::kSystemCall: {
        std::string args;
        for (size_t i = 0; i < operands.size(); ++i) {
          if (i > 0) {
            args += ", ";
          }
          args += operands[i].ToString();
        }
        for (const auto& target : output_targets) {
          if (!args.empty()) {
            args += ", ";
          }
          args += "out:" + std::string(target->name);
        }
        if (args.empty()) {
          return fmt::format("call  {}", system_call_name);
        }
        return fmt::format("call  {} {}", system_call_name, args);
      }

      case InstructionKind::kMethodCall:
        // Show step parameter only for next/prev with non-default step
        // name() method doesn't take a step parameter
        if (method_step != 1 && method_name != "name") {
          return fmt::format(
              "mcall {}, {}.{}({})", result.value(), operands[0].ToString(),
              method_name, method_step);
        }
        return fmt::format(
            "mcall {}, {}.{}()", result.value(), operands[0].ToString(),
            method_name);

      case InstructionKind::kJump:
        if (operands.size() == 1) {
          return fmt::format("jump  {}", operands[0].ToString());
        } else {
          return "(invalid jump)";
        }

      case InstructionKind::kBranch:
        if (operands.size() == 3) {
          return fmt::format(
              "br    {}, {}, {}", operands[0].ToString(),
              operands[1].ToString(), operands[2].ToString());
        } else {
          return "(invalid branch)";
        }

      case InstructionKind::kCall:
        if (result) {
          std::string args;
          for (size_t i = 0; i < operands.size(); ++i) {
            if (i > 0) {
              args += ", ";
            }
            args += operands[i].ToString();
          }
          return fmt::format(
              "call  {}, {}({})", result.value(), called_function_name, args);
        } else {
          std::string args;
          for (size_t i = 0; i < operands.size(); ++i) {
            if (i > 0) {
              args += ", ";
            }
            args += operands[i].ToString();
          }
          return fmt::format("call  {}({})", called_function_name, args);
        }

      case InstructionKind::kReturn:
        if (operands.empty()) {
          return "ret";
        } else {
          return fmt::format("ret   {}", operands[0].ToString());
        }

      case InstructionKind::kLoadCapture:
        return fmt::format("load_capture {}, {}", result.value(), capture_name);

      case InstructionKind::kStoreCapture:
        return fmt::format(
            "store_capture {}, {}", capture_name, operands[0].ToString());
    }
  }
};

inline auto operator<<(std::ostream& os, const lyra::lir::Instruction& instr)
    -> std::ostream& {
  return os << instr.ToString();
}

}  // namespace lyra::lir

template <>
struct fmt::formatter<lyra::lir::Instruction> {
  template <typename ParseContext>
  constexpr auto parse(ParseContext& ctx) {
    return ctx.begin();
  }

  template <typename FormatContext>
  auto format(const lyra::lir::Instruction& instr, FormatContext& ctx) const {
    return fmt::format_to(ctx.out(), "{}", instr.ToString());
  }
};
