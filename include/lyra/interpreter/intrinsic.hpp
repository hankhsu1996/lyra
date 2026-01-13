#pragma once

#include <span>
#include <string_view>

#include "lyra/common/type.hpp"
#include "lyra/interpreter/runtime_value.hpp"

namespace lyra::lir {
struct Instruction;
}

namespace lyra::interpreter {

using IntrinsicFn = auto (*)(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicArraySize(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicArrayDelete(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicQueueSize(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicQueueDelete(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicQueuePushBack(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicQueuePushFront(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicQueuePopBack(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicQueuePopFront(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicQueueInsert(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicEnumNext(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicEnumPrev(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicEnumName(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

/// Index read intrinsics - args[0] = index, returns element
auto IntrinsicArrayIndex(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicQueueIndex(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

/// Field access intrinsic - args[0] = field_id, returns field value
auto IntrinsicFieldGet(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

/// Index store intrinsics - args[0] = index, args[1] = value, returns modified
/// receiver
auto IntrinsicArrayStore(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

auto IntrinsicQueueStore(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

/// Field store intrinsic - args[0] = field_id, args[1] = value, returns
/// modified receiver
auto IntrinsicFieldSet(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue;

/// Pure enum operations for kIntrinsicOp (O(1) via precomputed index)
auto ExecuteEnumNext(
    int64_t value, int64_t step, const common::EnumData& enum_data,
    size_t bit_width) -> RuntimeValue;

auto ExecuteEnumPrev(
    int64_t value, int64_t step, const common::EnumData& enum_data,
    size_t bit_width) -> RuntimeValue;

auto ExecuteEnumName(int64_t value, const common::EnumData& enum_data)
    -> RuntimeValue;

/// Resolves method intrinsic by receiver type and method name
auto ResolveIntrinsicMethod(
    common::Type::Kind receiver_kind, std::string_view method_name) -> void*;

/// Resolves index read intrinsic by receiver type (no string, type-safe)
auto ResolveIntrinsicIndexRead(common::Type::Kind receiver_kind) -> void*;

/// Resolves index write intrinsic by receiver type (no string, type-safe)
auto ResolveIntrinsicIndexWrite(common::Type::Kind receiver_kind) -> void*;

}  // namespace lyra::interpreter
