#include "lyra/interpreter/intrinsic.hpp"

#include <cstddef>
#include <cstdint>
#include <span>
#include <stdexcept>
#include <string>
#include <string_view>
#include <utility>

#include <fmt/format.h>

#include "lyra/common/type.hpp"
#include "lyra/interpreter/runtime_value.hpp"
#include "lyra/lir/instruction.hpp"

namespace lyra::interpreter {

auto IntrinsicArraySize(
    RuntimeValue receiver, std::span<const RuntimeValue> /*args*/,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  const auto& arr = receiver.AsArray();
  auto size = static_cast<int32_t>(arr.size());
  return RuntimeValue::IntegralSigned(size, 32);
}

auto IntrinsicArrayDelete(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  auto& arr = receiver.AsArray();
  if (!args.empty()) {
    auto index = args[0].AsNarrow().AsInt64();
    if (index >= 0 && static_cast<size_t>(index) < arr.size()) {
      arr.erase(arr.begin() + index);
    }
  } else {
    arr.clear();
  }
  return receiver;
}

auto IntrinsicQueueSize(
    RuntimeValue receiver, std::span<const RuntimeValue> /*args*/,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  const auto& queue = receiver.AsQueue();
  auto size = static_cast<int32_t>(queue.size());
  return RuntimeValue::IntegralSigned(size, 32);
}

auto IntrinsicQueueDelete(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  auto& queue = receiver.AsQueue();
  if (!args.empty()) {
    auto index = args[0].AsNarrow().AsInt64();
    if (index >= 0) {
      queue.erase(static_cast<size_t>(index));  // bounds check in BoundedQueue
    }
  } else {
    queue.clear();
  }
  return receiver;
}

auto IntrinsicQueuePushBack(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  auto& queue = receiver.AsQueue();
  queue.push_back(args[0]);  // bounds check in BoundedQueue
  return receiver;
}

auto IntrinsicQueuePushFront(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  auto& queue = receiver.AsQueue();
  queue.push_front(args[0]);  // bounds check in BoundedQueue
  return receiver;
}

auto IntrinsicQueuePopBack(
    RuntimeValue receiver, std::span<const RuntimeValue> /*args*/,
    const lir::Instruction& instr) -> RuntimeValue {
  auto& queue = receiver.AsQueue();
  if (queue.empty()) {
    return RuntimeValue::DefaultValueForType(*instr.result_type);
  }
  auto value = std::move(queue.back());
  queue.pop_back();
  return value;
}

auto IntrinsicQueuePopFront(
    RuntimeValue receiver, std::span<const RuntimeValue> /*args*/,
    const lir::Instruction& instr) -> RuntimeValue {
  auto& queue = receiver.AsQueue();
  if (queue.empty()) {
    return RuntimeValue::DefaultValueForType(*instr.result_type);
  }
  auto value = std::move(queue.front());
  queue.pop_front();
  return value;
}

auto IntrinsicQueueInsert(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  auto& queue = receiver.AsQueue();
  auto index = args[0].AsNarrow().AsInt64();
  const auto& item = args[1];
  if (index >= 0) {
    queue.insert(
        static_cast<size_t>(index), item);  // bounds check in BoundedQueue
  }
  return receiver;
}

auto IntrinsicEnumNext(
    RuntimeValue receiver, std::span<const RuntimeValue> /*args*/,
    const lir::Instruction& instr) -> RuntimeValue {
  int64_t current_value = receiver.AsNarrow().AsInt64();
  const auto& members = instr.enum_members;

  size_t current_pos = 0;
  bool found = false;
  for (size_t i = 0; i < members.size(); ++i) {
    if (members[i].value == current_value) {
      current_pos = i;
      found = true;
      break;
    }
  }

  if (found) {
    auto step = static_cast<size_t>(instr.method_step);
    size_t target_pos = (current_pos + step) % members.size();
    return RuntimeValue::IntegralSigned(
        members[target_pos].value, instr.result_type->GetBitWidth());
  }
  return RuntimeValue::IntegralSigned(0, instr.result_type->GetBitWidth());
}

auto IntrinsicEnumPrev(
    RuntimeValue receiver, std::span<const RuntimeValue> /*args*/,
    const lir::Instruction& instr) -> RuntimeValue {
  int64_t current_value = receiver.AsNarrow().AsInt64();
  const auto& members = instr.enum_members;

  size_t current_pos = 0;
  bool found = false;
  for (size_t i = 0; i < members.size(); ++i) {
    if (members[i].value == current_value) {
      current_pos = i;
      found = true;
      break;
    }
  }

  if (found) {
    size_t step = static_cast<size_t>(instr.method_step) % members.size();
    size_t target_pos = (current_pos + members.size() - step) % members.size();
    return RuntimeValue::IntegralSigned(
        members[target_pos].value, instr.result_type->GetBitWidth());
  }
  return RuntimeValue::IntegralSigned(0, instr.result_type->GetBitWidth());
}

auto IntrinsicEnumName(
    RuntimeValue receiver, std::span<const RuntimeValue> /*args*/,
    const lir::Instruction& instr) -> RuntimeValue {
  int64_t current_value = receiver.AsNarrow().AsInt64();
  const auto& members = instr.enum_members;

  for (const auto& member : members) {
    if (member.value == current_value) {
      return RuntimeValue::String(member.name);
    }
  }
  return RuntimeValue::String("");
}

auto ExecuteEnumNext(
    int64_t value, int64_t step, const common::EnumData& enum_data,
    size_t bit_width) -> RuntimeValue {
  size_t pos = enum_data.GetIndex(value);
  size_t target = (pos + static_cast<size_t>(step)) % enum_data.members.size();
  return RuntimeValue::IntegralSigned(
      enum_data.members[target].value, bit_width);
}

auto ExecuteEnumPrev(
    int64_t value, int64_t step, const common::EnumData& enum_data,
    size_t bit_width) -> RuntimeValue {
  size_t pos = enum_data.GetIndex(value);
  size_t n = enum_data.members.size();
  size_t target = (pos + n - (static_cast<size_t>(step) % n)) % n;
  return RuntimeValue::IntegralSigned(
      enum_data.members[target].value, bit_width);
}

auto ExecuteEnumName(int64_t value, const common::EnumData& enum_data)
    -> RuntimeValue {
  size_t pos = enum_data.GetIndex(value);
  return RuntimeValue::String(enum_data.members[pos].name);
}

auto IntrinsicArrayIndex(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue {
  auto sv_index = args[0].AsNarrow().AsInt64();
  int32_t lower_bound = instr.lower_bound;
  auto actual_idx = static_cast<size_t>(sv_index - lower_bound);

  const auto& arr = receiver.AsArray();
  if (actual_idx >= arr.size()) {
    throw std::runtime_error(
        fmt::format(
            "array index {} out of bounds (size {})", sv_index, arr.size()));
  }
  return arr[actual_idx];
}

auto IntrinsicQueueIndex(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  // Queues always have lower_bound 0
  auto index = args[0].AsNarrow().AsInt64();
  auto actual_idx = static_cast<size_t>(index);

  const auto& queue = receiver.AsQueue();
  if (actual_idx >= queue.size()) {
    throw std::runtime_error(
        fmt::format(
            "queue index {} out of bounds (size {})", index, queue.size()));
  }
  return queue[actual_idx];
}

auto IntrinsicFieldGet(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  auto field_id = static_cast<size_t>(args[0].AsNarrow().AsInt64());
  return receiver.GetField(field_id);
}

auto IntrinsicArrayStore(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& instr) -> RuntimeValue {
  auto sv_index = args[0].AsNarrow().AsInt64();
  int32_t lower_bound = instr.lower_bound;
  auto actual_idx = static_cast<size_t>(sv_index - lower_bound);
  const auto& value = args[1];

  auto& arr = receiver.AsArray();
  if (actual_idx >= arr.size()) {
    throw std::runtime_error(
        fmt::format(
            "array index {} out of bounds (size {})", sv_index, arr.size()));
  }
  arr[actual_idx] = value;
  return receiver;
}

auto IntrinsicQueueStore(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  // Queues always have lower_bound 0
  auto index = args[0].AsNarrow().AsInt64();
  auto actual_idx = static_cast<size_t>(index);
  const auto& value = args[1];

  auto& queue = receiver.AsQueue();
  if (actual_idx >= queue.size()) {
    throw std::runtime_error(
        fmt::format(
            "queue index {} out of bounds (size {})", index, queue.size()));
  }
  queue[actual_idx] = value;
  return receiver;
}

auto IntrinsicFieldSet(
    RuntimeValue receiver, std::span<const RuntimeValue> args,
    const lir::Instruction& /*instr*/) -> RuntimeValue {
  auto field_id = static_cast<size_t>(args[0].AsNarrow().AsInt64());
  const auto& value = args[1];
  receiver.SetField(field_id, value);
  return receiver;
}

// NOLINTBEGIN(cppcoreguidelines-pro-type-reinterpret-cast)
auto ResolveIntrinsicMethod(
    common::Type::Kind receiver_kind, std::string_view method_name) -> void* {
  using Kind = common::Type::Kind;

  if (receiver_kind == Kind::kDynamicArray ||
      receiver_kind == Kind::kUnpackedArray) {
    if (method_name == "size") {
      return reinterpret_cast<void*>(&IntrinsicArraySize);
    }
    if (method_name == "delete") {
      return reinterpret_cast<void*>(&IntrinsicArrayDelete);
    }
  }

  if (receiver_kind == Kind::kQueue) {
    if (method_name == "size") {
      return reinterpret_cast<void*>(&IntrinsicQueueSize);
    }
    if (method_name == "delete") {
      return reinterpret_cast<void*>(&IntrinsicQueueDelete);
    }
    if (method_name == "push_back") {
      return reinterpret_cast<void*>(&IntrinsicQueuePushBack);
    }
    if (method_name == "push_front") {
      return reinterpret_cast<void*>(&IntrinsicQueuePushFront);
    }
    if (method_name == "pop_back") {
      return reinterpret_cast<void*>(&IntrinsicQueuePopBack);
    }
    if (method_name == "pop_front") {
      return reinterpret_cast<void*>(&IntrinsicQueuePopFront);
    }
    if (method_name == "insert") {
      return reinterpret_cast<void*>(&IntrinsicQueueInsert);
    }
  }

  if (receiver_kind == Kind::kEnum) {
    if (method_name == "next") {
      return reinterpret_cast<void*>(&IntrinsicEnumNext);
    }
    if (method_name == "prev") {
      return reinterpret_cast<void*>(&IntrinsicEnumPrev);
    }
    if (method_name == "name") {
      return reinterpret_cast<void*>(&IntrinsicEnumName);
    }
  }

  return nullptr;
}

auto ResolveIntrinsicIndexRead(common::Type::Kind receiver_kind) -> void* {
  using Kind = common::Type::Kind;

  if (receiver_kind == Kind::kDynamicArray ||
      receiver_kind == Kind::kUnpackedArray) {
    return reinterpret_cast<void*>(&IntrinsicArrayIndex);
  }
  if (receiver_kind == Kind::kQueue) {
    return reinterpret_cast<void*>(&IntrinsicQueueIndex);
  }
  if (receiver_kind == Kind::kUnpackedStruct ||
      receiver_kind == Kind::kUnpackedUnion) {
    return reinterpret_cast<void*>(&IntrinsicFieldGet);
  }
  return nullptr;
}

auto ResolveIntrinsicIndexWrite(common::Type::Kind receiver_kind) -> void* {
  using Kind = common::Type::Kind;

  if (receiver_kind == Kind::kDynamicArray ||
      receiver_kind == Kind::kUnpackedArray) {
    return reinterpret_cast<void*>(&IntrinsicArrayStore);
  }
  if (receiver_kind == Kind::kQueue) {
    return reinterpret_cast<void*>(&IntrinsicQueueStore);
  }
  if (receiver_kind == Kind::kUnpackedStruct ||
      receiver_kind == Kind::kUnpackedUnion) {
    return reinterpret_cast<void*>(&IntrinsicFieldSet);
  }
  return nullptr;
}
// NOLINTEND(cppcoreguidelines-pro-type-reinterpret-cast)

}  // namespace lyra::interpreter
