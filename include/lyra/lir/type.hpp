#pragma once

#include <cstdint>
#include <optional>
#include <string>
#include <variant>
#include <vector>

#include "lyra/lir/class_id.hpp"
#include "lyra/lir/type_id.hpp"

namespace lyra::lir {

// The LIR type graph. Each type is a LIR-owned identity translated from a
// generic-PL MIR type at MIR-to-LIR; it carries no reference back to MIR. LIR
// types continue MIR's type universe -- the ownership, wrapper, and object
// decisions MIR fixed are read here, not re-decided. They name the logical
// shape a value has during execution; physical size, alignment, and offsets are
// derived below LIR.

enum class BitAtom : std::uint8_t { kBit, kLogic, kReg };

enum class Signedness : std::uint8_t { kSigned, kUnsigned };

enum class PackedArrayForm : std::uint8_t {
  kExplicit,
  kByte,
  kShortInt,
  kInt,
  kLongInt,
  kInteger,
  kTime,
};

enum class PointerOwnership : std::uint8_t { kUnique, kShared, kBorrowed };

enum class Mutability : std::uint8_t { kMutable, kReadOnly };

enum class RuntimeLibraryKind : std::uint8_t {
  kPrintItem,
  kPrintLiteralItem,
  kPrintValueItem,
  kFormatSpec,
  kFormatArg,
  kChannelCancellation,
  kTimeFormat,
  kHierarchySegment,
  kDpiBitBuffer,
  kDpiLogicBuffer,
  kDpiBitChunk,
  kDpiLogicChunk,
  kTrigger,
};

struct PackedRange {
  std::int64_t left;
  std::int64_t right;
};

struct PackedArrayType {
  BitAtom atom;
  Signedness signedness;
  std::vector<PackedRange> dims;
  PackedArrayForm form;
};

struct EnumMember {
  std::string name;
  std::int64_t value;
};

struct EnumType {
  PackedArrayType base;
  std::vector<EnumMember> members;
};

struct UnpackedArrayType {
  TypeId element_type;
  std::uint64_t size;
};

struct DynamicArrayType {
  TypeId element_type;
};

struct QueueType {
  TypeId element_type;
  std::optional<std::uint64_t> max_bound;
};

struct AssociativeArrayType {
  TypeId element_type;
  TypeId key_type;
};

struct WildcardIndexType {};

struct StringType {};

// A borrowed, NUL-terminated machine string (C `const char*`): raw character
// storage the value does not own, distinct from the owning `StringType`.
struct MachineCStringType {};

struct MachineIntType {
  std::uint32_t bit_width;
  Signedness signedness;
};

// A primitive machine float (C `float` / `double`), distinct from `RealType`,
// which is a simulation value reached through a value wrapper.
struct MachineFloatType {
  std::uint32_t bit_width;
};

struct EventType {};
struct RealType {};
struct ShortRealType {};
struct RealTimeType {};
struct ChandleType {};
struct VoidType {};

struct ObjectType {
  ClassId class_id;
};

struct ExternalUnitObjectType {
  std::string unit_name;
};

struct ExternalClassType {
  std::string qualified_name;
};

struct RuntimeEffectsType {};
struct FilesType {};
struct DiagnosticType {};

struct RuntimeLibraryType {
  RuntimeLibraryKind kind;
};

struct CoroutineType {
  TypeId payload;
};

struct RefType {
  TypeId pointee;
  Mutability mutability;
};

struct PointerType {
  TypeId pointee;
  PointerOwnership ownership;
  Mutability mutability;
};

struct ManagedRefType {
  TypeId pointee;
};

struct VectorType {
  TypeId element;
};

struct TupleType {
  std::vector<TypeId> elements;
};

struct UnionType {
  std::vector<TypeId> elements;
};

// The sealed endpoint of a cross-instance reference -- a resolution node
// wrapping the referenced value type.
struct ResolvedType {
  TypeId value;
};

// The write capability for a net: a handle to one of a resolution node's driver
// slots.
struct DriverType {
  TypeId value;
};

struct ObservableType {
  TypeId value;
};

using TypeData = std::variant<
    PackedArrayType, EnumType, UnpackedArrayType, DynamicArrayType, QueueType,
    AssociativeArrayType, WildcardIndexType, StringType, MachineCStringType,
    MachineIntType, MachineFloatType, EventType, RealType, ShortRealType,
    RealTimeType, ChandleType, VoidType, ObjectType, ExternalUnitObjectType,
    ExternalClassType, RuntimeEffectsType, FilesType, DiagnosticType,
    RuntimeLibraryType, CoroutineType, RefType, PointerType, ManagedRefType,
    VectorType, TupleType, UnionType, ResolvedType, DriverType, ObservableType>;

struct Type {
  TypeData data;
};

}  // namespace lyra::lir
