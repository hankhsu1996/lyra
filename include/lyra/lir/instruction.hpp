#pragma once

#include <cassert>
#include <cstdint>
#include <optional>
#include <string>
#include <vector>

#include <fmt/core.h>
#include <fmt/ranges.h>

#include "lyra/common/symbol.hpp"
#include "lyra/common/trigger.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/operand.hpp"

namespace lyra::lir {

using SymbolRef = common::SymbolRef;

// Forward declaration for callee pointer
struct Function;

/// Storage kind for container allocation (resolved at lowering time)
enum class StorageKind : uint8_t {
  kVector,  // std::vector - arrays, structs, unions
  kDeque,   // std::deque - queues
};

/// Value-semantic intrinsic operation kinds.
/// Unlike kIntrinsicCall (reference-semantic), these operate on pure values.
enum class IntrinsicOpKind : uint8_t {
  kEnumNext,  // Navigate to next enum value with step
  kEnumPrev,  // Navigate to previous enum value with step
  kEnumName,  // Get string name of enum value
};

enum class InstructionKind {
  // Memory operations
  kConstant,
  kResolveVar,    // Produces Pointer<T> from symbol
  kLoad,          // Dereference Pointer<T> to get T
  kStore,         // Write T through Pointer<T>
  kStoreNBA,      // Non-blocking store through Pointer<T>
  kResolveIndex,  // Pointer<Array<T>> + index -> Pointer<T>
  kResolveField,  // Pointer<Struct> + field_id -> Pointer<FieldT>
  kResolveSlice,  // Pointer<T> + offset + width -> SliceRef<U>
  kLoadSlice,     // SliceRef<T> -> T (extract bits from storage)
  kStoreSlice,    // SliceRef<T> + T -> void (read-modify-write)
  kStoreElement,  // Store to array/struct: base[index] = value
  kExtractBits,   // Extract bits from value (rvalue): result =
                  // value[offset+:width]
  kAllocate,      // Allocate container with storage kind resolved at lowering

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
  kIntrinsicCall,  // Reference-semantic: method on container (fn pointer
                   // dispatch)
  kIntrinsicOp,    // Value-semantic: pure operation on values (enum switch)
  kJump,
  kBranch,

  // Function call/return
  kCall,    // Call user-defined function
  kReturn,  // Return from function

  // Closure captures (persistent local variables for closures like $monitor)
  kLoadCapture,   // Load from closure's captures map
  kStoreCapture,  // Store to closure's captures map
};

/// Category of instruction for dispatch
enum class InstructionCategory : uint8_t {
  kMemory,      // Load, store, aggregate operations
  kArithmetic,  // Unary, binary, reduction operations
  kType,        // Conversion, concatenation
  kControl,     // Control flow, calls, events
};

/// Get the category of an instruction kind
constexpr auto GetInstructionCategory(InstructionKind kind)
    -> InstructionCategory {
  switch (kind) {
    // Memory operations
    case InstructionKind::kConstant:
    case InstructionKind::kResolveVar:
    case InstructionKind::kLoad:
    case InstructionKind::kStore:
    case InstructionKind::kStoreNBA:
    case InstructionKind::kResolveIndex:
    case InstructionKind::kResolveField:
    case InstructionKind::kResolveSlice:
    case InstructionKind::kLoadSlice:
    case InstructionKind::kStoreSlice:
    case InstructionKind::kStoreElement:
    case InstructionKind::kExtractBits:
    case InstructionKind::kAllocate:
    case InstructionKind::kMove:
      return InstructionCategory::kMemory;

    // Arithmetic operations
    case InstructionKind::kUnaryPlus:
    case InstructionKind::kUnaryMinus:
    case InstructionKind::kUnaryLogicalNot:
    case InstructionKind::kUnaryBitwiseNot:
    case InstructionKind::kReductionAnd:
    case InstructionKind::kReductionNand:
    case InstructionKind::kReductionOr:
    case InstructionKind::kReductionNor:
    case InstructionKind::kReductionXor:
    case InstructionKind::kReductionXnor:
    case InstructionKind::kBinaryAdd:
    case InstructionKind::kBinarySubtract:
    case InstructionKind::kBinaryMultiply:
    case InstructionKind::kBinaryDivide:
    case InstructionKind::kBinaryModulo:
    case InstructionKind::kBinaryEqual:
    case InstructionKind::kBinaryNotEqual:
    case InstructionKind::kBinaryLessThan:
    case InstructionKind::kBinaryLessThanEqual:
    case InstructionKind::kBinaryGreaterThan:
    case InstructionKind::kBinaryGreaterThanEqual:
    case InstructionKind::kBinaryPower:
    case InstructionKind::kBinaryBitwiseAnd:
    case InstructionKind::kBinaryBitwiseOr:
    case InstructionKind::kBinaryBitwiseXor:
    case InstructionKind::kBinaryBitwiseXnor:
    case InstructionKind::kBinaryLogicalAnd:
    case InstructionKind::kBinaryLogicalOr:
    case InstructionKind::kBinaryLogicalShiftLeft:
    case InstructionKind::kBinaryLogicalShiftRight:
    case InstructionKind::kBinaryArithmeticShiftLeft:
    case InstructionKind::kBinaryArithmeticShiftRight:
      return InstructionCategory::kArithmetic;

    // Type operations
    case InstructionKind::kConversion:
    case InstructionKind::kConcatenation:
      return InstructionCategory::kType;

    // Control flow operations
    case InstructionKind::kComplete:
    case InstructionKind::kWaitEvent:
    case InstructionKind::kDelay:
    case InstructionKind::kSystemCall:
    case InstructionKind::kIntrinsicCall:
    case InstructionKind::kIntrinsicOp:
    case InstructionKind::kJump:
    case InstructionKind::kBranch:
    case InstructionKind::kCall:
    case InstructionKind::kReturn:
    case InstructionKind::kLoadCapture:
    case InstructionKind::kStoreCapture:
      return InstructionCategory::kControl;
  }
  // Unreachable - all cases covered
  return InstructionCategory::kControl;
}

// Enum member info for method call runtime helpers
struct EnumMemberInfo {
  std::string name;
  int64_t value;
};

/// Instruction operand storage conventions:
///
/// - `operands`: Mixed operand kinds (Temp, Variable, Constant, Label).
///   Used by instructions that interact with memory, literals, or control flow.
///
/// - `temp_operands`: Pure value operands (TempRef only).
///   Enforces SSA form at the type level. Used by value-semantic operations.
struct Instruction {
  InstructionKind kind{};

  // Destination temporary name, or empty if not applicable
  std::optional<TempRef> result{};
  std::optional<common::Type> result_type{};

  // Mixed-kind operands (Temp, Variable, Constant, Label)
  std::vector<Operand> operands{};

  // Pure value operands (TempRef only) for value-semantic operations
  std::vector<TempRef> temp_operands{};

  // Control flow labels (dedicated fields, not operands)
  std::optional<LabelRef> jump_target{};   // For kJump
  std::optional<LabelRef> branch_true{};   // For kBranch
  std::optional<LabelRef> branch_false{};  // For kBranch

  // Constant reference (dedicated field, not operand)
  std::optional<ConstantRef> constant{};  // For kConstant

  // Symbol for kResolveVar (dedicated field, not operand)
  std::optional<SymbolRef> symbol{};

  // Delay amount (dedicated field, not operand)
  std::optional<ConstantRef> delay_amount{};  // For kDelay

  // System call name (for kSystemCall)
  std::string system_call_name{};
  std::vector<SymbolRef> output_targets{};

  // Function call (for kCall)
  std::string called_function_name{};  // For error messages and ToString()
  const Function* callee{nullptr};     // Resolved by LinkFunctionCalls

  // Event name
  std::vector<common::Trigger> wait_triggers{};

  // Intrinsic call: type-erased function pointer for builtin methods.
  // Uses void* because LIR cannot depend on interpreter types (layering).
  // At execution time, interpreter casts to IntrinsicFn and calls.
  // operands[0] = receiver, operands[1..] = args
  void* intrinsic_fn{nullptr};  // NOLINT(google-runtime-int)

  // For kIntrinsicOp: which value-semantic operation to perform
  IntrinsicOpKind op_kind{IntrinsicOpKind::kEnumNext};

  // For kIntrinsicOp: type context required to interpret the operation.
  // The opcode determines how this type is used (e.g., enum member lookup).
  std::optional<common::Type> type_context{};

  // Pre-computed lower bound for array index adjustment
  int32_t lower_bound{0};

  // For kAllocate: storage type resolved at lowering time
  StorageKind storage_kind{StorageKind::kVector};

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

  static auto WithType(
      InstructionKind kind, TempRef result, TempRef operand,
      common::Type result_type) -> Instruction {
    return Instruction{
        .kind = kind,
        .result = std::move(result),
        .result_type = std::move(result_type),
        .operands = {Operand::Temp(operand)}};
  }

  static auto Constant(TempRef result, ConstantRef constant_ref)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kConstant,
        .result = result,
        .constant = constant_ref,
    };
  }

  // Create a pointer to a variable.
  // The result temp's type should be Pointer<T> where T is the variable's type.
  static auto ResolveVar(TempRef result, SymbolRef sym) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kResolveVar,
        .result = result,
        .symbol = sym,
    };
  }

  // Load value through a pointer
  static auto Load(TempRef result, TempRef pointer) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kLoad,
        .result = result,
        .temp_operands = {pointer},
    };
  }

  // Store value through a pointer (blocking)
  static auto Store(TempRef pointer, TempRef value) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kStore,
        .temp_operands = {pointer, value},
    };
  }

  // Store value through a pointer (non-blocking assignment)
  static auto StoreNBA(TempRef pointer, TempRef value) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kStoreNBA,
        .temp_operands = {pointer, value},
    };
  }

  // Resolve array element pointer: base_ptr[index] -> Pointer<T>
  // base_ptr must be Pointer<Array<T>> or Pointer<Queue<T>>
  static auto ResolveIndex(TempRef result, TempRef base_ptr, TempRef index)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kResolveIndex,
        .result = result,
        .temp_operands = {base_ptr, index},
    };
  }

  // Resolve struct field pointer: base_ptr.field_id -> Pointer<FieldT>
  // base_ptr must be Pointer<Struct> or Pointer<Union>
  // field_id is stored in lower_bound field (repurposed for field index)
  static auto ResolveField(TempRef result, TempRef base_ptr, size_t field_id)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kResolveField,
        .result = result,
        .temp_operands = {base_ptr},
        .lower_bound = static_cast<int32_t>(field_id),
    };
  }

  // Resolve bit slice: base_ptr[offset +: width] -> SliceRef<T>
  // Creates a slice reference for packed bit access
  static auto ResolveSlice(
      TempRef result, TempRef base_ptr, TempRef offset, TempRef width)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kResolveSlice,
        .result = result,
        .temp_operands = {base_ptr, offset, width},
    };
  }

  // Load value through a slice reference (extract bits from storage)
  static auto LoadSlice(TempRef result, TempRef slice_ref) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kLoadSlice,
        .result = result,
        .temp_operands = {slice_ref},
    };
  }

  // Store value through a slice reference (read-modify-write)
  static auto StoreSlice(TempRef slice_ref, TempRef value) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kStoreSlice,
        .temp_operands = {slice_ref, value},
    };
  }

  // Store element to array/struct: base[index] = value
  // Store to array/struct element in-place.
  // base is temp (SSA value); index is element index or field id.
  static auto StoreElement(Operand base, TempRef index, TempRef value)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kStoreElement,
        .operands = {
            std::move(base), Operand::Temp(index), Operand::Temp(value)}};
  }

  // Extract bits from value (rvalue): result = value[bit_offset +: width]
  // For non-addressable expressions like (a+b)[7:4].
  // Width comes from result_type.
  static auto ExtractBits(
      TempRef result, TempRef value, TempRef bit_offset,
      common::Type result_type) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kExtractBits,
        .result = result,
        .result_type = std::move(result_type),
        .temp_operands = {value, bit_offset}};
  }

  // Allocate container with storage kind resolved at lowering time.
  // For fixed-size: operands empty (size from type)
  // For dynamic: operands[0] = size, operands[1] = init (optional)
  static auto Allocate(
      TempRef result, common::Type type, StorageKind storage,
      std::optional<TempRef> size = std::nullopt,
      std::optional<TempRef> init = std::nullopt) -> Instruction {
    std::vector<Operand> ops;
    if (size) {
      ops.push_back(Operand::Temp(*size));
      if (init) {
        ops.push_back(Operand::Temp(*init));
      }
    }
    Instruction instr;
    instr.kind = InstructionKind::kAllocate;
    instr.result = result;
    instr.result_type = std::move(type);
    instr.operands = std::move(ops);
    instr.storage_kind = storage;
    return instr;
  }

  static auto WaitEvent(std::vector<common::Trigger> triggers) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kWaitEvent,
        .wait_triggers = std::move(triggers)};
  }

  static auto Delay(ConstantRef delay) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kDelay,
        .delay_amount = delay,
    };
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

  // Builtin method call instruction (resolved function pointer dispatch)
  // temp_operands[0] = receiver, temp_operands[1..] = method arguments
  // All args are TempRef, enforcing SSA form
  // fn is a type-erased BuiltinMethodFn pointer
  static auto IntrinsicCall(
      void* fn, TempRef receiver, std::vector<TempRef> args, TempRef result,
      common::Type result_type) -> Instruction {
    std::vector<TempRef> temp_ops;
    temp_ops.reserve(1 + args.size());
    temp_ops.push_back(receiver);
    for (auto& arg : args) {
      temp_ops.push_back(std::move(arg));
    }
    return Instruction{
        .kind = InstructionKind::kIntrinsicCall,
        .result = result,
        .result_type = std::move(result_type),
        .temp_operands = std::move(temp_ops),
        .intrinsic_fn = fn};
  }

  // Value-semantic intrinsic operation (no receiver identity)
  // All args are pure values (TempRef), enforcing SSA form
  static auto IntrinsicOp(
      IntrinsicOpKind op, std::vector<TempRef> args, TempRef result,
      common::Type result_type, common::Type type_context) -> Instruction {
    return Instruction{
        .kind = InstructionKind::kIntrinsicOp,
        .result = result,
        .result_type = std::move(result_type),
        .temp_operands = std::move(args),
        .op_kind = op,
        .type_context = std::move(type_context)};
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
        .kind = InstructionKind::kJump,
        .jump_target = label,
    };
  }

  static auto Branch(
      TempRef condition, LabelRef true_label, LabelRef false_label)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kBranch,
        .temp_operands = {condition},
        .branch_true = true_label,
        .branch_false = false_label,
    };
  }

  // Concatenation: result = {op0, op1, ...}
  // Operands are ordered MSB to LSB (first operand is most significant)
  // All operands are temps (value-semantic operation)
  static auto Concatenation(
      TempRef result, std::vector<TempRef> operands, common::Type result_type)
      -> Instruction {
    return Instruction{
        .kind = InstructionKind::kConcatenation,
        .result = result,
        .result_type = std::move(result_type),
        .temp_operands = std::move(operands)};
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
      case InstructionKind::kConstant:
        return fmt::format(
            "const {}, {}", result.value(), constant->ToString());

      case InstructionKind::kResolveVar:
        return fmt::format(
            "resolve_var {}, {}", result.value(), (*symbol)->name);

      case InstructionKind::kLoad:
        return fmt::format("load  {}, {}", result.value(), temp_operands[0]);

      case InstructionKind::kStore:
        return fmt::format("store {}, {}", temp_operands[0], temp_operands[1]);

      case InstructionKind::kStoreNBA:
        return fmt::format("nba   {}, {}", temp_operands[0], temp_operands[1]);

      case InstructionKind::kResolveIndex:
        return fmt::format(
            "resolve_index {}, {}[{}]", result.value(), temp_operands[0],
            temp_operands[1]);

      case InstructionKind::kResolveField:
        return fmt::format(
            "resolve_field {}, {}.{}", result.value(), temp_operands[0],
            lower_bound);

      case InstructionKind::kResolveSlice:
        return fmt::format(
            "resolve_slice {}, {}[{}+:{}]", result.value(), temp_operands[0],
            temp_operands[1], temp_operands[2]);

      case InstructionKind::kLoadSlice:
        return fmt::format(
            "load_slice {}, {}", result.value(), temp_operands[0]);

      case InstructionKind::kStoreSlice:
        return fmt::format(
            "store_slice {}, {}", temp_operands[0], temp_operands[1]);

      case InstructionKind::kStoreElement:
        return fmt::format(
            "stel  {}[{}], {}", operands[0].ToString(), operands[1].ToString(),
            operands[2].ToString());

      case InstructionKind::kExtractBits:
        return fmt::format(
            "extb  {}, {}[{}+:]", result.value(), temp_operands[0],
            temp_operands[1]);

      case InstructionKind::kAllocate: {
        const char* storage_str =
            storage_kind == StorageKind::kDeque ? "deque" : "vector";
        if (operands.empty()) {
          return fmt::format("alloc {}, {}", result.value(), storage_str);
        }
        if (operands.size() == 1) {
          return fmt::format(
              "alloc {}, {}[{}]", result.value(), storage_str,
              operands[0].ToString());
        }
        return fmt::format(
            "alloc {}, {}[{}]({})", result.value(), storage_str,
            operands[0].ToString(), operands[1].ToString());
      }

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
        for (size_t i = 0; i < temp_operands.size(); ++i) {
          if (i > 0) {
            args += ", ";
          }
          args += temp_operands[i].ToString();
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
        return fmt::format("delay {}", delay_amount->ToString());

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

      case InstructionKind::kIntrinsicCall: {
        // Build args string from temp_operands[1..]
        std::string args_str;
        for (size_t i = 1; i < temp_operands.size(); ++i) {
          if (i > 1) {
            args_str += ", ";
          }
          args_str += temp_operands[i].ToString();
        }
        return fmt::format(
            "bcall {}, {}({})", result.value(), temp_operands[0].ToString(),
            args_str);
      }

      case InstructionKind::kIntrinsicOp: {
        const char* op_name = "?";
        switch (op_kind) {
          case IntrinsicOpKind::kEnumNext:
            op_name = "enum_next";
            break;
          case IntrinsicOpKind::kEnumPrev:
            op_name = "enum_prev";
            break;
          case IntrinsicOpKind::kEnumName:
            op_name = "enum_name";
            break;
        }
        std::string args_str;
        for (size_t i = 0; i < temp_operands.size(); ++i) {
          if (i > 0) {
            args_str += ", ";
          }
          args_str += temp_operands[i].ToString();
        }
        return fmt::format(
            "iop   {}, {}({})", result.value(), op_name, args_str);
      }

      case InstructionKind::kJump:
        if (jump_target.has_value()) {
          return fmt::format("jump  {}", jump_target->ToString());
        } else {
          return "(invalid jump)";
        }

      case InstructionKind::kBranch:
        if (temp_operands.size() == 1 && branch_true.has_value() &&
            branch_false.has_value()) {
          return fmt::format(
              "br    {}, {}, {}", temp_operands[0].ToString(),
              branch_true->ToString(), branch_false->ToString());
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
