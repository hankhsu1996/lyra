#pragma once

#include <cstddef>
#include <cstdint>
#include <string_view>
#include <type_traits>

#include "lyra/support/per_operand.hpp"

namespace lyra::support {

// What a value crosses as between generated code and a function of the
// runtime library, which is compiled before the program and so takes and
// answers machine values only. It is coarse enough that two sides disagreeing
// is a defect rather than a spelling difference: signedness is no part of it,
// since how the bits are read is the operation's own business at either end.
enum class MachineKind : std::uint8_t {
  kVoid,
  kAddress,
  kTruth,
  kInt8,
  kInt32,
  kInt64,
  kFloat,
  kDouble,
  // A structure passed whole, which crosses as its own fields.
  kAggregate,
};

// The machine kind a C++ parameter or answer of type `T` crosses as, as the
// host's C++ compiler passes it. The set is closed, so a type outside it fails
// to compile here rather than being classified by whichever arm its shape
// resembles.
template <typename T>
constexpr auto MachineKindOf() -> MachineKind {
  if constexpr (std::is_void_v<T>) {
    return MachineKind::kVoid;
  } else if constexpr (std::is_pointer_v<T> || std::is_reference_v<T>) {
    return MachineKind::kAddress;
  } else if constexpr (std::is_same_v<T, bool>) {
    return MachineKind::kTruth;
  } else if constexpr (std::is_same_v<T, float>) {
    return MachineKind::kFloat;
  } else if constexpr (std::is_same_v<T, double>) {
    return MachineKind::kDouble;
  } else if constexpr (std::is_class_v<T>) {
    return MachineKind::kAggregate;
  } else {
    static_assert(
        std::is_integral_v<T> || std::is_enum_v<T>,
        "a function of the library takes or answers a type no generated code "
        "could");
    if constexpr (sizeof(T) == 1) {
      return MachineKind::kInt8;
    } else if constexpr (sizeof(T) == 4) {
      return MachineKind::kInt32;
    } else {
      static_assert(sizeof(T) == 8);
      return MachineKind::kInt64;
    }
  }
}

// How a message names `kind`.
[[nodiscard]] auto MachineKindName(MachineKind kind) -> std::string_view;

// What a function crossing between generated code and the library takes and
// what it answers.
class MachineSignature {
 public:
  // The most values any function of the library takes.
  static constexpr std::size_t kMostTaken = 11;

  constexpr MachineSignature(
      MachineKind returns, std::initializer_list<MachineKind> takes)
      : returns_(returns), takes_(takes) {
  }

  [[nodiscard]] constexpr auto Returns() const -> MachineKind {
    return returns_;
  }

  [[nodiscard]] constexpr auto Takes() const -> std::span<const MachineKind> {
    return takes_.All();
  }

 private:
  MachineKind returns_;
  PerOperand<MachineKind, kMostTaken> takes_;
};

// The machine signature of the C++ function type, or function pointer type,
// `Function`. Whether a function can raise is no part of how it is called, so
// one that cannot is read the same way.
template <typename Function>
struct MachineSignatureOf;

template <typename R, typename... Params>
struct MachineSignatureOf<R(Params...)> {
  static constexpr auto Get() -> MachineSignature {
    return MachineSignature(MachineKindOf<R>(), {MachineKindOf<Params>()...});
  }
};

template <typename R, typename... Params>
struct MachineSignatureOf<R(Params...) noexcept>
    : MachineSignatureOf<R(Params...)> {};

template <typename Function>
struct MachineSignatureOf<Function*> : MachineSignatureOf<Function> {};

}  // namespace lyra::support
