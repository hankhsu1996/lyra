#include "lyra/lowering/mir_to_lir/expression.hpp"

#include <algorithm>
#include <cassert>
#include <cstddef>
#include <cstdint>
#include <optional>
#include <string>
#include <utility>
#include <vector>

#include "lyra/common/literal.hpp"
#include "lyra/common/system_function.hpp"
#include "lyra/common/type.hpp"
#include "lyra/lir/context.hpp"
#include "lyra/lir/instruction.hpp"
#include "lyra/lir/operand.hpp"
#include "lyra/lowering/mir_to_lir/helpers.hpp"
#include "lyra/lowering/mir_to_lir/lir_builder.hpp"
#include "lyra/mir/expression.hpp"
#include "lyra/mir/operators.hpp"

namespace lyra::lowering::mir_to_lir {

using Type = common::Type;
using Literal = common::Literal;
using Operand = lir::Operand;
using TempRef = lir::TempRef;
using Instruction = lir::Instruction;
using IK = lir::InstructionKind;

// Forward declaration for synthesizing $monitor check processes
// format_literal: optional format string literal (nullptr if no format string)
// display_call: display variant to use ($display, $displayb, $displayo,
// $displayh)
auto SynthesizeMonitorCheckProcess(
    const std::vector<const mir::Expression*>& monitored_exprs,
    const mir::Expression* format_literal, const std::string& display_call,
    LirBuilder& builder) -> std::string;

auto LowerExpression(const mir::Expression& expression, LirBuilder& builder)
    -> lir::TempRef {
  switch (expression.kind) {
    case mir::Expression::Kind::kLiteral: {
      const auto& literal_expression =
          mir::As<mir::LiteralExpression>(expression);
      auto result =
          builder.AllocateTemp("lit", literal_expression.literal.type);
      auto literal = builder.InternLiteral(literal_expression.literal);
      auto instruction = Instruction::Basic(IK::kLiteral, result, literal);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kIdentifier: {
      const auto& identifier = mir::As<mir::IdentifierExpression>(expression);
      auto result = builder.AllocateTemp("load", identifier.type);
      auto instruction =
          Instruction::Basic(IK::kLoadVariable, result, identifier.symbol);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kEnumValue: {
      // Emit enum value as its integer constant
      const auto& enum_val = mir::As<mir::EnumValueExpression>(expression);
      auto result = builder.AllocateTemp("enum", enum_val.type);

      // Respect the enum's base type signedness
      const auto& integral_data =
          std::get<common::IntegralData>(enum_val.type.data);
      auto width = enum_val.type.GetBitWidth();
      auto literal =
          integral_data.is_signed
              ? builder.InternLiteral(
                    Literal::IntegralSigned(enum_val.value, width))
              : builder.InternLiteral(
                    Literal::IntegralUnsigned(
                        static_cast<uint64_t>(enum_val.value), width));

      auto instruction = Instruction::Basic(IK::kLiteral, result, literal);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kUnary: {
      const auto& unary = mir::As<mir::UnaryExpression>(expression);
      assert(unary.operand);
      auto operand = LowerExpression(*unary.operand, builder);
      return LowerUnaryExpression(unary, operand, builder);
    }

    case mir::Expression::Kind::kBinary: {
      const auto& binary = mir::As<mir::BinaryExpression>(expression);
      assert(binary.left && binary.right);
      auto lhs = LowerExpression(*binary.left, builder);
      auto rhs = LowerExpression(*binary.right, builder);
      return LowerBinaryExpression(binary, lhs, rhs, builder);
    }

    case mir::Expression::Kind::kTernary: {
      const auto& ternary = mir::As<mir::TernaryExpression>(expression);
      assert(ternary.condition);
      assert(ternary.true_expression);
      assert(ternary.false_expression);
      return LowerTernaryExpression(ternary, builder);
    }

    case mir::Expression::Kind::kAssignment: {
      const auto& assignment = mir::As<mir::AssignmentExpression>(expression);
      assert(assignment.value);
      auto value = LowerExpression(*assignment.value, builder);

      if (assignment.target.IsHierarchical()) {
        // Hierarchical assignment: child.signal = value
        auto instruction = Instruction::StoreHierarchical(
            assignment.target.instance_path, assignment.target.target_symbol,
            value, assignment.is_non_blocking);
        builder.AddInstruction(std::move(instruction));
        return value;
      }

      if (assignment.target.IsStructFieldAssignment()) {
        // Check if this is an unpacked struct/union field assignment
        if (assignment.target.base_type &&
            (assignment.target.base_type->IsUnpackedStruct() ||
             assignment.target.base_type->IsUnpackedUnion())) {
          // Unpacked struct/union field assignment: var.field = value
          // For struct: use field_index; For union: ALWAYS use 0
          size_t storage_index = assignment.target.base_type->IsUnpackedUnion()
                                     ? 0
                                     : *assignment.target.field_bit_offset;
          auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
          auto index_literal = builder.InternLiteral(
              common::Literal::Int(static_cast<int32_t>(storage_index)));
          builder.AddInstruction(
              Instruction::Basic(IK::kLiteral, index_temp, index_literal));

          auto instruction = Instruction::StoreElement(
              Operand::Variable(assignment.target.symbol), index_temp, value,
              assignment.is_non_blocking);
          builder.AddInstruction(std::move(instruction));
          return value;
        }

        // Packed struct field assignment: my_struct.field = value
        // Create literal for field bit offset
        auto offset_temp = builder.AllocateTemp("offset", common::Type::Int());
        auto offset_literal = builder.InternLiteral(
            common::Literal::Int(
                static_cast<int32_t>(*assignment.target.field_bit_offset)));
        builder.AddInstruction(
            Instruction::Basic(IK::kLiteral, offset_temp, offset_literal));

        // Create slice type with field width
        auto slice_type = common::Type::IntegralUnsigned(
            static_cast<uint32_t>(*assignment.target.field_bit_width));

        // Emit StorePackedBits instruction
        auto instruction = Instruction::StorePackedBits(
            assignment.target.symbol, offset_temp, value, slice_type);
        builder.AddInstruction(std::move(instruction));
        return value;
      }

      if (assignment.target.IsElementSelect()) {
        if (assignment.target.IsPacked()) {
          // Packed element assignment (possibly multi-dimensional)
          const auto& base_type = *assignment.target.base_type;
          size_t element_width = GetElementWidthAfterIndices(
              base_type, assignment.target.indices.size());
          auto composite_index = ComputeCompositeIndex(
              assignment.target.indices, base_type, builder);
          auto adjusted_index = AdjustForNonZeroLower(
              composite_index, base_type.GetElementLower(), builder);

          // Compute bit_offset = adjusted_index * element_width
          auto bit_offset = builder.AllocateTemp("bit_offset", Type::Int());
          auto width_literal = builder.InternLiteral(
              Literal::Int(static_cast<int32_t>(element_width)));
          auto width_temp = builder.AllocateTemp("width", Type::Int());
          builder.AddInstruction(
              Instruction::Basic(IK::kLiteral, width_temp, width_literal));
          builder.AddInstruction(
              Instruction::Basic(
                  IK::kBinaryMultiply, bit_offset,
                  {Operand::Temp(adjusted_index), Operand::Temp(width_temp)}));

          auto slice_type =
              Type::IntegralUnsigned(static_cast<uint32_t>(element_width));
          auto instruction = Instruction::StorePackedBits(
              assignment.target.symbol, bit_offset, value, slice_type);
          builder.AddInstruction(std::move(instruction));
        } else {
          // Unpacked array element assignment
          size_t num_indices = assignment.target.indices.size();

          // Lower all indices
          std::vector<TempRef> index_temps;
          for (const auto& idx_expr : assignment.target.indices) {
            index_temps.push_back(LowerExpression(*idx_expr, builder));
          }

          if (num_indices == 1) {
            // 1D: simple store
            auto instruction = Instruction::StoreElement(
                Operand::Variable(assignment.target.symbol), index_temps[0],
                value, assignment.is_non_blocking);
            builder.AddInstruction(std::move(instruction));
          } else {
            // Multi-dimensional: copy-modify-store pattern
            // For arr[i][j] = value: load arr[i], modify [j], store back
            const auto& base_type = *assignment.target.base_type;

            // Load intermediate arrays (all but the last index)
            std::vector<TempRef> intermediate_temps;
            const Type* current_type = &base_type;

            for (size_t i = 0; i < num_indices - 1; ++i) {
              const Type& element_type = current_type->GetElementType();
              auto temp = builder.AllocateTemp("arr", element_type);

              if (i == 0) {
                // First level: load from variable
                auto instr = Instruction::LoadElement(
                    temp, Operand::Variable(assignment.target.symbol),
                    index_temps[i], element_type);
                builder.AddInstruction(std::move(instr));
              } else {
                // Subsequent levels: load from temp
                auto instr = Instruction::LoadElement(
                    temp, Operand::Temp(intermediate_temps.back()),
                    index_temps[i], element_type);
                builder.AddInstruction(std::move(instr));
              }

              intermediate_temps.push_back(temp);
              current_type = &element_type;
            }

            // Store value to the innermost array using the last index
            auto store_instr = Instruction::StoreElement(
                Operand::Temp(intermediate_temps.back()), index_temps.back(),
                value);
            builder.AddInstruction(std::move(store_instr));

            // Store back intermediate arrays in reverse order (3D+).
            // For arr[i][j][k] = v (3D), this stores temp0[j] = temp1.
            // Store-backs may be redundant due to shared_ptr semantics in
            // RuntimeValue, but kept for correctness regardless of storage
            // implementation.
            auto num_intermediates =
                static_cast<int>(intermediate_temps.size());
            for (int i = num_intermediates - 1; i >= 1; --i) {
              auto instr = Instruction::StoreElement(
                  Operand::Temp(intermediate_temps[i - 1]), index_temps[i],
                  intermediate_temps[i]);
              builder.AddInstruction(std::move(instr));
            }

            // Store back to the base variable
            auto final_store = Instruction::StoreElement(
                Operand::Variable(assignment.target.symbol), index_temps[0],
                intermediate_temps[0], assignment.is_non_blocking);
            builder.AddInstruction(std::move(final_store));
          }
        }
        return value;
      }

      // Simple variable assignment
      auto instruction = Instruction::StoreVariable(
          assignment.target.symbol, value, assignment.is_non_blocking);
      builder.AddInstruction(std::move(instruction));
      return value;
    }

    case mir::Expression::Kind::kElementSelect: {
      const auto& select = mir::As<mir::ElementSelectExpression>(expression);
      assert(select.value);
      assert(select.selector);

      auto index = LowerExpression(*select.selector, builder);
      auto result = builder.AllocateTemp("elem", expression.type);

      // Check if this is bitvector type (value) or unpacked array (variable)
      if (select.value->type.IsBitvector()) {
        // Packed vector: lower the value, then select element/bit
        auto value = LowerExpression(*select.value, builder);
        int32_t lower = select.value->type.GetElementLower();
        auto adjusted_index = AdjustForNonZeroLower(index, lower, builder);

        // Compute bit_offset = adjusted_index * element_width
        size_t element_width = expression.type.GetBitWidth();
        auto bit_offset = builder.AllocateTemp("bit_offset", Type::Int());
        auto width_literal = builder.InternLiteral(
            Literal::Int(static_cast<int32_t>(element_width)));
        auto width_temp = builder.AllocateTemp("width", Type::Int());
        builder.AddInstruction(
            Instruction::Basic(IK::kLiteral, width_temp, width_literal));
        builder.AddInstruction(
            Instruction::Basic(
                IK::kBinaryMultiply, bit_offset,
                {Operand::Temp(adjusted_index), Operand::Temp(width_temp)}));

        auto instruction = Instruction::LoadPackedBits(
            result, value, bit_offset, expression.type);
        builder.AddInstruction(std::move(instruction));
        return result;
      }

      // Unpacked array element access
      if (select.value->kind == mir::Expression::Kind::kIdentifier) {
        // Direct variable access: arr[i]
        const auto& array_id =
            mir::As<mir::IdentifierExpression>(*select.value);
        auto instruction = Instruction::LoadElement(
            result, Operand::Variable(array_id.symbol), index, expression.type);
        builder.AddInstruction(std::move(instruction));
        return result;
      }

      // Nested access (e.g., arr[i][j]): recursively lower the array expression
      auto array_temp = LowerExpression(*select.value, builder);
      auto instruction = Instruction::LoadElement(
          result, Operand::Temp(array_temp), index, expression.type);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kConversion: {
      const auto& conversion = mir::As<mir::ConversionExpression>(expression);
      auto input = LowerExpression(*conversion.value, builder);
      auto result = builder.AllocateTemp("cvt", conversion.target_type);
      auto instruction = Instruction::WithType(
          IK::kConversion, result, input, conversion.target_type);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kSystemCall: {
      const auto& system_call = mir::As<mir::SystemCallExpression>(expression);

      // Supported system calls are validated in AST→MIR
      assert(common::IsSystemFunctionSupported(system_call.name));

      // Check if this is a $monitor variant that needs symbol tracking
      bool is_monitor =
          (system_call.name == "$monitor" || system_call.name == "$monitorb" ||
           system_call.name == "$monitoro" || system_call.name == "$monitorh");

      std::vector<Operand> operands;
      std::vector<TempRef> arguments;

      auto lower_system_call_operand =
          [&](const mir::Expression& argument) -> Operand {
        if (argument.kind == mir::Expression::Kind::kIdentifier) {
          const auto& ident = mir::As<mir::IdentifierExpression>(argument);
          return Operand::Variable(ident.symbol);
        }
        // For literal expressions, keep as literal operand to preserve metadata
        if (argument.kind == mir::Expression::Kind::kLiteral) {
          const auto& lit = mir::As<mir::LiteralExpression>(argument);
          auto literal_ref = builder.InternLiteral(lit.literal);
          return Operand::Literal(literal_ref);
        }
        auto temp = LowerExpression(argument, builder);
        return Operand::Temp(temp);
      };

      if (is_monitor) {
        // Format string is now in format_expr (separate from arguments)
        const mir::Expression* format_literal = nullptr;
        if (system_call.format_expr && system_call.format_expr_is_literal) {
          format_literal = system_call.format_expr->get();
        }

        // Collect pointers to monitored expressions for synthesis
        // All arguments are now monitored values (format string is separate)
        std::vector<const mir::Expression*> monitored_exprs;

        for (const auto& argument : system_call.arguments) {
          if (argument) {
            // Collect expression for check function synthesis
            monitored_exprs.push_back(argument.get());

            // Lower expression at call site for initial print
            TempRef result = LowerExpression(*argument, builder);
            arguments.push_back(result);
          }
        }

        // Determine display variant based on monitor variant
        // $monitor → $display, $monitorb → $displayb, etc.
        std::string display_call;
        if (system_call.name == "$monitor") {
          display_call = "$display";
        } else if (system_call.name == "$monitorb") {
          display_call = "$displayb";
        } else if (system_call.name == "$monitoro") {
          display_call = "$displayo";
        } else if (system_call.name == "$monitorh") {
          display_call = "$displayh";
        } else {
          display_call = "$display";  // fallback
        }

        // Synthesize the check process for periodic re-evaluation
        std::string check_process_name = SynthesizeMonitorCheckProcess(
            monitored_exprs, format_literal, display_call, builder);

        auto instruction = Instruction::SystemCallWithMonitor(
            system_call.name, std::move(arguments),
            std::move(check_process_name));
        instruction.format_string_is_literal = (format_literal != nullptr);
        if (format_literal != nullptr) {
          TempRef format_temp = LowerExpression(*format_literal, builder);
          instruction.format_operand = Operand::Temp(format_temp);
        }
        builder.AddInstruction(std::move(instruction));
        return builder.AllocateTemp("sys", system_call.type);
      }
      for (const auto& argument : system_call.arguments) {
        if (argument) {
          operands.push_back(lower_system_call_operand(*argument));
        }
      }

      // Add default argument (1) for $finish and $stop if not provided
      // $exit takes no arguments per LRM
      if ((system_call.name == "$finish" || system_call.name == "$stop") &&
          operands.empty() && !is_monitor) {
        auto temp = builder.AllocateTemp("sys", system_call.type);
        auto const_one = builder.InternLiteral(Literal::Int(1));
        auto instruction = Instruction::Basic(IK::kLiteral, temp, const_one);
        builder.AddInstruction(std::move(instruction));
        operands.push_back(Operand::Temp(temp));
      }

      // System functions return a value, system tasks do not
      bool is_function = common::IsSystemFunction(system_call.name);

      auto result = builder.AllocateTemp("sys", system_call.type);

      // Lower format_expr if present (for display-like tasks)
      std::optional<Operand> format_operand;
      if (system_call.format_expr) {
        auto format_temp = LowerExpression(**system_call.format_expr, builder);
        format_operand = Operand::Temp(format_temp);
      }

      // Collect output targets for system calls like $value$plusargs
      std::vector<lir::SymbolRef> output_targets;
      for (const auto& target : system_call.output_targets) {
        output_targets.push_back(target.symbol);
      }

      // Create instruction - functions get result temp, tasks don't
      auto instruction =
          is_function
              ? Instruction::SystemCall(
                    system_call.name, std::move(operands), result,
                    system_call.type)
              : Instruction::SystemCall(system_call.name, std::move(operands));
      instruction.format_operand = format_operand;
      instruction.format_string_is_literal = system_call.format_expr_is_literal;
      instruction.source_file = system_call.source_file;
      instruction.source_line = system_call.source_line;
      instruction.output_targets = std::move(output_targets);
      builder.AddInstruction(std::move(instruction));

      return result;
    }

    case mir::Expression::Kind::kRangeSelect: {
      const auto& range = mir::As<mir::RangeSelectExpression>(expression);
      assert(range.value);

      auto value = LowerExpression(*range.value, builder);

      // Compute LSB: for [7:4], LSB is 4
      int32_t lsb = std::min(range.left, range.right);

      // Adjust for non-zero-based ranges (e.g., bit [63:32])
      // Packed structs return 0 from GetElementLower() (always 0-based)
      if (range.value->type.IsBitvector()) {
        lsb -= range.value->type.GetElementLower();
      }

      // Create a literal for the LSB shift amount
      auto lsb_temp = builder.AllocateTemp("lsb", Type::Int());
      auto lsb_literal = builder.InternLiteral(Literal::Int(lsb));
      auto lsb_instruction =
          Instruction::Basic(IK::kLiteral, lsb_temp, lsb_literal);
      builder.AddInstruction(std::move(lsb_instruction));

      auto result = builder.AllocateTemp("slice", expression.type);
      auto instruction =
          Instruction::LoadPackedBits(result, value, lsb_temp, expression.type);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kIndexedRangeSelect: {
      const auto& indexed =
          mir::As<mir::IndexedRangeSelectExpression>(expression);
      assert(indexed.value);
      assert(indexed.start);

      auto value = LowerExpression(*indexed.value, builder);
      auto start = LowerExpression(*indexed.start, builder);

      TempRef lsb_temp;
      if (indexed.is_ascending) {
        // a[i+:4]: lsb = i (start index is the LSB)
        lsb_temp = start;
      } else {
        // a[i-:4]: lsb = i - width + 1
        auto offset_temp = builder.AllocateTemp("offset", Type::Int());
        auto offset_literal =
            builder.InternLiteral(Literal::Int(indexed.width - 1));
        builder.AddInstruction(
            Instruction::Basic(IK::kLiteral, offset_temp, offset_literal));

        lsb_temp = builder.AllocateTemp("lsb", Type::Int());
        builder.AddInstruction(
            Instruction::Basic(
                IK::kBinarySubtract, lsb_temp,
                {Operand::Temp(start), Operand::Temp(offset_temp)}));
      }

      // Adjust for non-zero-based ranges if needed
      int32_t lower = indexed.value->type.GetElementLower();
      lsb_temp = AdjustForNonZeroLower(lsb_temp, lower, builder);

      auto result = builder.AllocateTemp("slice", expression.type);
      builder.AddInstruction(
          Instruction::LoadPackedBits(
              result, value, lsb_temp, expression.type));
      return result;
    }

    case mir::Expression::Kind::kHierarchicalReference: {
      const auto& hier_ref =
          mir::As<mir::HierarchicalReferenceExpression>(expression);
      auto result = builder.AllocateTemp("hier", expression.type);
      auto instruction = Instruction::LoadHierarchical(
          result, hier_ref.instance_path, hier_ref.target_symbol,
          expression.type);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kConcatenation: {
      const auto& concat = mir::As<mir::ConcatenationExpression>(expression);
      std::vector<TempRef> operand_temps;
      operand_temps.reserve(concat.operands.size());
      for (const auto& operand : concat.operands) {
        operand_temps.push_back(LowerExpression(*operand, builder));
      }
      auto result = builder.AllocateTemp("cat", expression.type);
      auto instruction = Instruction::Concatenation(
          result, std::move(operand_temps), expression.type);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kReplication: {
      const auto& rep = mir::As<mir::ReplicationExpression>(expression);

      // Lower operand once (ensures single evaluation per LRM)
      auto operand_temp = LowerExpression(*rep.operand, builder);

      // Create vector with operand repeated count times
      std::vector<TempRef> operand_temps(rep.count, operand_temp);

      // Emit as concatenation instruction
      auto result = builder.AllocateTemp("rep", expression.type);
      auto instruction = Instruction::Concatenation(
          result, std::move(operand_temps), expression.type);
      builder.AddInstruction(std::move(instruction));
      return result;
    }

    case mir::Expression::Kind::kFunctionCall: {
      const auto& call = mir::As<mir::FunctionCallExpression>(expression);

      // Lower arguments
      std::vector<Operand> arg_operands;
      arg_operands.reserve(call.arguments.size());
      for (const auto& arg : call.arguments) {
        auto arg_temp = LowerExpression(*arg, builder);
        arg_operands.push_back(Operand::Temp(arg_temp));
      }

      // Allocate result temp if non-void
      std::optional<TempRef> result_temp;
      std::optional<common::Type> result_type;
      if (call.type.kind != common::Type::Kind::kVoid) {
        result_temp = builder.AllocateTemp("call", call.type);
        result_type = call.type;
      }

      // Emit call instruction (function_name is already qualified for package
      // functions)
      auto instr = Instruction::Call(
          call.function_name, std::move(arg_operands), result_temp,
          result_type);
      builder.AddInstruction(std::move(instr));

      if (result_temp) {
        return *result_temp;
      }
      // Void functions: LowerExpression requires a TempRef return, so we
      // allocate a dummy temp. The caller (typically ExpressionStatement)
      // discards it. This is wasteful but harmless - a proper fix would be
      // changing LowerExpression to return std::optional<TempRef>, which
      // requires updating many call sites.
      return builder.AllocateTemp("void", common::Type::Void());
    }

    case mir::Expression::Kind::kMemberAccess: {
      const auto& member = mir::As<mir::MemberAccessExpression>(expression);
      assert(member.value);

      // Check if this is an unpacked struct/union access
      if (member.value->type.IsUnpackedStruct() ||
          member.value->type.IsUnpackedUnion()) {
        auto value = LowerExpression(*member.value, builder);
        auto result = builder.AllocateTemp("field", expression.type);

        // For struct: bit_offset is reused as field_index
        // For union: ALWAYS use index 0 (shared storage)
        size_t storage_index =
            member.value->type.IsUnpackedUnion() ? 0 : member.bit_offset;

        // Emit literal for storage index
        auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
        auto index_literal = builder.InternLiteral(
            Literal::Int(static_cast<int32_t>(storage_index)));
        builder.AddInstruction(
            Instruction::Basic(IK::kLiteral, index_temp, index_literal));

        // Use unified LoadElement with temp operand
        builder.AddInstruction(
            Instruction::LoadElement(
                result, Operand::Temp(value), index_temp, expression.type));
        return result;
      }

      // Packed struct: use bit extraction
      auto value = LowerExpression(*member.value, builder);

      // Create a literal for the bit offset (LSB position)
      auto offset_temp = builder.AllocateTemp("offset", Type::Int());
      auto offset_literal = builder.InternLiteral(
          Literal::Int(static_cast<int32_t>(member.bit_offset)));
      builder.AddInstruction(
          Instruction::Basic(IK::kLiteral, offset_temp, offset_literal));

      // Use LoadPackedBits to extract the field bits
      auto result = builder.AllocateTemp("field", expression.type);
      builder.AddInstruction(
          Instruction::LoadPackedBits(
              result, value, offset_temp, expression.type));
      return result;
    }

    case mir::Expression::Kind::kNewArray: {
      const auto& new_arr = mir::As<mir::NewArrayExpression>(expression);
      auto size = LowerExpression(*new_arr.size_expr, builder);
      auto result = builder.AllocateTemp("new_array", expression.type);

      if (new_arr.init_expr) {
        auto init = LowerExpression(*new_arr.init_expr, builder);
        builder.AddInstruction(
            Instruction::NewDynamicArray(result, size, expression.type, init));
      } else {
        builder.AddInstruction(
            Instruction::NewDynamicArray(result, size, expression.type));
      }
      return result;
    }

    case mir::Expression::Kind::kMethodCall: {
      const auto& mc = mir::As<mir::MethodCallExpression>(expression);
      auto receiver = LowerExpression(*mc.receiver, builder);
      auto result = builder.AllocateTemp("method_call", expression.type);

      // Convert MIR enum members to LIR enum members (if present)
      std::vector<lir::EnumMemberInfo> lir_members;
      lir_members.reserve(mc.enum_members.size());
      for (const auto& m : mc.enum_members) {
        lir_members.push_back({.name = m.name, .value = m.value});
      }

      // Get step from args (for enum next/prev)
      int64_t step = 1;
      if (!mc.args.empty()) {
        const auto& step_expr = mir::As<mir::LiteralExpression>(*mc.args[0]);
        step = step_expr.literal.value.AsInt64();
      }

      builder.AddInstruction(
          Instruction::MethodCall(
              mc.method_name, receiver, result, expression.type, step,
              std::move(lir_members)));
      return result;
    }

    case mir::Expression::Kind::kUnpackedStructLiteral: {
      const auto& lit =
          mir::As<mir::UnpackedStructLiteralExpression>(expression);
      // Create struct literal by storing field values into a new struct
      auto result = builder.AllocateTemp("struct_lit", expression.type);
      builder.AddInstruction(
          Instruction::CreateAggregate(result, expression.type));

      for (size_t i = 0; i < lit.field_values.size(); ++i) {
        auto field_value = LowerExpression(*lit.field_values[i], builder);

        // Emit literal for field index
        auto index_temp = builder.AllocateTemp("idx", common::Type::Int());
        auto index_literal =
            builder.InternLiteral(Literal::Int(static_cast<int32_t>(i)));
        builder.AddInstruction(
            Instruction::Basic(IK::kLiteral, index_temp, index_literal));

        // Use unified StoreElement with temp operand
        builder.AddInstruction(
            Instruction::StoreElement(
                Operand::Temp(result), index_temp, field_value));
      }
      return result;
    }
  }
  // All expression kinds must be handled above - if we reach here, a new
  // expression kind was added without updating this function
  throw common::InternalError(
      "LowerExpression",
      "unhandled MIR expression kind in MIR-to-LIR lowering");
}

auto LowerUnaryExpression(
    const mir::UnaryExpression& expression, lir::TempRef operand,
    LirBuilder& builder) -> lir::TempRef {
  using Operator = mir::UnaryOperator;

  // Handle unary operations
  IK kind{};

  switch (expression.op) {
    case Operator::kPlus:
      // Unary plus is a no-op, just return the operand
      return operand;

    case Operator::kMinus:
      kind = IK::kUnaryMinus;
      break;

    case Operator::kLogicalNot:
      kind = IK::kUnaryLogicalNot;
      break;

    case Operator::kBitwiseNot:
      kind = IK::kUnaryBitwiseNot;
      break;

    // Reduction operations
    case Operator::kReductionAnd:
      kind = IK::kReductionAnd;
      break;

    case Operator::kReductionNand:
      kind = IK::kReductionNand;
      break;

    case Operator::kReductionOr:
      kind = IK::kReductionOr;
      break;

    case Operator::kReductionNor:
      kind = IK::kReductionNor;
      break;

    case Operator::kReductionXor:
      kind = IK::kReductionXor;
      break;

    case Operator::kReductionXnor:
      kind = IK::kReductionXnor;
      break;

    // Handle increment/decrement with helper function
    case Operator::kPreincrement:
    case Operator::kPostincrement:
    case Operator::kPredecrement:
    case Operator::kPostdecrement:
      return LowerIncrementDecrementExpression(expression, builder);
  }

  auto result = builder.AllocateTemp("una", expression.type);
  auto instruction = Instruction::Basic(kind, result, operand);
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerBinaryExpression(
    const mir::BinaryExpression& expression, TempRef lhs, TempRef rhs,
    LirBuilder& builder) -> TempRef {
  using Operator = mir::BinaryOperator;

  IK kind{};

  const bool is_lhs_string = lhs->type == Type::String();
  const bool is_rhs_string = rhs->type == Type::String();
  const bool is_string = is_lhs_string || is_rhs_string;

  // String operand restrictions and unsupported operators are validated in
  // AST→MIR. For strings, only equality/inequality are allowed.
  if (is_string) {
    assert(
        expression.op == Operator::kEquality ||
        expression.op == Operator::kInequality);
  }

  switch (expression.op) {
    case Operator::kAddition:
      kind = IK::kBinaryAdd;
      break;
    case Operator::kSubtraction:
      kind = IK::kBinarySubtract;
      break;
    case Operator::kMultiplication:
      kind = IK::kBinaryMultiply;
      break;
    case Operator::kDivision:
      kind = IK::kBinaryDivide;
      break;
    case Operator::kModulo:
      kind = IK::kBinaryModulo;
      break;
    case Operator::kEquality:
      kind = IK::kBinaryEqual;
      break;
    case Operator::kInequality:
      kind = IK::kBinaryNotEqual;
      break;
    case Operator::kLessThan:
      kind = IK::kBinaryLessThan;
      break;
    case Operator::kLessThanEqual:
      kind = IK::kBinaryLessThanEqual;
      break;
    case Operator::kGreaterThan:
      kind = IK::kBinaryGreaterThan;
      break;
    case Operator::kGreaterThanEqual:
      kind = IK::kBinaryGreaterThanEqual;
      break;
    case Operator::kPower:
      kind = IK::kBinaryPower;
      break;
    case Operator::kBitwiseAnd:
      kind = IK::kBinaryBitwiseAnd;
      break;
    case Operator::kBitwiseOr:
      kind = IK::kBinaryBitwiseOr;
      break;
    case Operator::kBitwiseXor:
      kind = IK::kBinaryBitwiseXor;
      break;
    case Operator::kBitwiseXnor:
      kind = IK::kBinaryBitwiseXnor;
      break;
    case Operator::kLogicalAnd:
      kind = IK::kBinaryLogicalAnd;
      break;
    case Operator::kLogicalOr:
      kind = IK::kBinaryLogicalOr;
      break;
    case Operator::kLogicalShiftLeft:
      kind = IK::kBinaryLogicalShiftLeft;
      break;
    case Operator::kLogicalShiftRight:
      kind = IK::kBinaryLogicalShiftRight;
      break;
    case Operator::kArithmeticShiftLeft:
      kind = IK::kBinaryArithmeticShiftLeft;
      break;
    case Operator::kArithmeticShiftRight:
      kind = IK::kBinaryArithmeticShiftRight;
      break;

    // Unsupported operators - rejected in AST→MIR
    case Operator::kLogicalImplication:
    case Operator::kLogicalEquivalence:
    case Operator::kCaseEquality:
    case Operator::kCaseInequality:
    case Operator::kWildcardEquality:
    case Operator::kWildcardInequality:
      assert(false && "unsupported operator should be rejected in AST→MIR");
  }

  auto result = builder.AllocateTemp("bin", expression.type);
  auto instruction = Instruction::Basic(
      kind, result, {Operand::Temp(lhs), Operand::Temp(rhs)});
  builder.AddInstruction(std::move(instruction));
  return result;
}

auto LowerTernaryExpression(
    const mir::TernaryExpression& expression, LirBuilder& builder) -> TempRef {
  auto condition = LowerExpression(*expression.condition, builder);

  // Use branches for proper short-circuit evaluation
  auto true_label = builder.MakeLabel("ternary.true");
  auto false_label = builder.MakeLabel("ternary.false");
  auto end_label = builder.MakeLabel("ternary.end");

  auto result = builder.AllocateTemp("ternary", expression.type);

  auto branch = Instruction::Branch(condition, true_label, false_label);
  builder.AddInstruction(std::move(branch));

  // True branch
  builder.StartBlock(true_label);
  auto true_value = LowerExpression(*expression.true_expression, builder);

  auto copy_true = Instruction::Basic(IK::kMove, result, true_value);
  builder.AddInstruction(std::move(copy_true));

  auto jump_to_end = Instruction::Jump(end_label);
  builder.AddInstruction(std::move(jump_to_end));
  builder.EndBlock();

  // False branch
  builder.StartBlock(false_label);
  auto false_value = LowerExpression(*expression.false_expression, builder);

  auto copy_false = Instruction::Basic(IK::kMove, result, false_value);
  builder.AddInstruction(std::move(copy_false));

  auto jump_to_end_from_false = Instruction::Jump(end_label);
  builder.AddInstruction(std::move(jump_to_end_from_false));
  builder.EndBlock();

  // End block - control rejoins here
  builder.StartBlock(end_label);

  return result;
}

auto LowerIncrementDecrementExpression(
    const mir::UnaryExpression& expression, LirBuilder& builder) -> TempRef {
  using lir::Instruction;
  using lir::InstructionKind;
  using Operator = mir::UnaryOperator;

  // Operand must be a variable reference - validated in AST→MIR
  assert(expression.operand->kind == mir::Expression::Kind::kIdentifier);

  const auto& identifier =
      mir::As<mir::IdentifierExpression>(*expression.operand);

  // Load the current value
  auto load_temp = builder.AllocateTemp("load", identifier.type);
  auto load_instruction =
      Instruction::Basic(IK::kLoadVariable, load_temp, identifier.symbol);
  builder.AddInstruction(std::move(load_instruction));

  // Create a literal instruction for the constant 1
  auto const_one_temp = builder.AllocateTemp("const", identifier.type);
  auto const_one = builder.InternLiteral(Literal::Int(1));
  auto const_instruction =
      Instruction::Basic(IK::kLiteral, const_one_temp, const_one);
  builder.AddInstruction(std::move(const_instruction));

  // Compute the new value (add/subtract 1)
  auto operation_temp = builder.AllocateTemp("op", identifier.type);
  IK operation_kind{};

  if (expression.op == Operator::kPreincrement ||
      expression.op == Operator::kPostincrement) {
    operation_kind = IK::kBinaryAdd;
  } else {
    operation_kind = IK::kBinarySubtract;
  }

  auto operation_instruction = Instruction::Basic(
      operation_kind, operation_temp,
      {Operand::Temp(load_temp), Operand::Temp(const_one_temp)});
  builder.AddInstruction(std::move(operation_instruction));

  // Store the updated value
  auto store_instruction =
      Instruction::StoreVariable(identifier.symbol, operation_temp, false);
  builder.AddInstruction(std::move(store_instruction));

  // Return either the old or new value based on pre/post operation
  if (expression.op == Operator::kPreincrement ||
      expression.op == Operator::kPredecrement) {
    // Pre-operations return the new value
    return operation_temp;
  }
  // Post-operations return the original value
  return load_temp;
}

auto SynthesizeMonitorCheckProcess(
    const std::vector<const mir::Expression*>& monitored_exprs,
    const mir::Expression* format_literal, const std::string& display_call,
    LirBuilder& builder) -> std::string {
  // Generate unique process name
  std::string process_name = builder.MakeSyntheticFunctionName("monitor_check");

  // Begin synthetic process (saves current process state)
  builder.BeginSyntheticProcess(process_name);

  builder.StartBlock(builder.MakeLabel("entry"));

  // Handle empty monitor case: $monitor() with no expressions
  // Just complete immediately - nothing to track for changes
  if (monitored_exprs.empty()) {
    builder.AddInstruction(Instruction::Complete());
    return builder.EndSyntheticProcess();
  }

  // Generate capture names for prev values (matches $monitor handler)
  auto make_capture_name = [](size_t i) {
    return "__capture_prev_" + std::to_string(i);
  };

  // Phase 1: Evaluate all monitored expressions
  std::vector<TempRef> current_temps;
  current_temps.reserve(monitored_exprs.size());
  for (const auto* expr : monitored_exprs) {
    current_temps.push_back(LowerExpression(*expr, builder));
  }

  // Phase 2: Load previous values from captures and compare
  std::vector<TempRef> change_flags;
  change_flags.reserve(monitored_exprs.size());

  for (size_t i = 0; i < monitored_exprs.size(); ++i) {
    const auto& expr_type = monitored_exprs[i]->type;

    // Load previous value from capture
    TempRef prev = builder.AllocateTemp("prev", expr_type);
    builder.AddInstruction(
        Instruction::LoadCapture(prev, make_capture_name(i), expr_type));

    // Compare: changed_i = (current != prev)
    TempRef changed = builder.AllocateTemp("chg", Type::Bool());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryNotEqual, changed,
            {Operand::Temp(current_temps[i]), Operand::Temp(prev)}));
    change_flags.push_back(changed);
  }

  // Phase 3: OR all change flags together
  TempRef any_changed = change_flags[0];
  for (size_t i = 1; i < change_flags.size(); ++i) {
    TempRef new_changed = builder.AllocateTemp("any", Type::Bool());
    builder.AddInstruction(
        Instruction::Basic(
            IK::kBinaryLogicalOr, new_changed,
            {Operand::Temp(any_changed), Operand::Temp(change_flags[i])}));
    any_changed = new_changed;
  }

  // Phase 4: Branch - if any changed, print and update; else return
  auto print_label = builder.MakeLabel("do_print");
  auto end_label = builder.MakeLabel("end");
  builder.AddInstruction(
      Instruction::Branch(any_changed, print_label, end_label));
  builder.EndBlock();

  // do_print block: emit $display and store new prev values
  builder.StartBlock(print_label);

  // Build operands for $display: current values (format string is separate)
  std::vector<Operand> display_operands;
  for (const auto& temp : current_temps) {
    display_operands.push_back(Operand::Temp(temp));
  }

  // Use the provided display variant ($display, $displayb, $displayo,
  // $displayh)
  auto display_instr =
      Instruction::SystemCall(display_call, std::move(display_operands));
  if (format_literal != nullptr) {
    TempRef fmt_temp = LowerExpression(*format_literal, builder);
    display_instr.format_operand = Operand::Temp(fmt_temp);
  }
  display_instr.format_string_is_literal = (format_literal != nullptr);
  builder.AddInstruction(std::move(display_instr));

  // Store current values as new prev values in captures
  for (size_t i = 0; i < current_temps.size(); ++i) {
    builder.AddInstruction(
        Instruction::StoreCapture(current_temps[i], make_capture_name(i)));
  }

  builder.AddInstruction(Instruction::Jump(end_label));
  builder.EndBlock();

  // end block: complete (process terminator)
  builder.StartBlock(end_label);
  builder.AddInstruction(Instruction::Complete());

  // End synthetic process (restores process state, adds process to module)
  return builder.EndSyntheticProcess();
}

}  // namespace lyra::lowering::mir_to_lir
