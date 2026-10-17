#include "lyra/value/library_value_types.hpp"

#include <concepts>
#include <cstddef>
#include <cstdint>
#include <memory>
#include <type_traits>
#include <utility>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/simulation_error.hpp"
#include "lyra/value/any_value.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/element_type.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/index_order.hpp"
#include "lyra/value/integral.hpp"
#include "lyra/value/integral_words.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/reduction.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/string.hpp"
#include "lyra/value/value_type.hpp"

namespace lyra::value {

void LacksOperation() {
  throw InternalError(
      "value type: an operation is asked of a type the language does not "
      "define it for -- please report this as a bug");
}

namespace {

// LRM 6.24.3: a real and a chandle are the kinds that hold no stream of bits,
// so a streaming conversion of one is a program the front end rejected. Every
// other kind is one a program may stream.
template <typename T>
inline constexpr bool kIsBitStreamType = true;
template <typename Host>
inline constexpr bool kIsBitStreamType<RealValue<Host>> = false;
template <>
inline constexpr bool kIsBitStreamType<Chandle> = false;

// A value of `T` holds parts ordered by position, each a value of a type the
// value states and handed out where it lies.
template <typename T>
concept HoldsParts = requires(const T& c, T& m, std::size_t position) {
  { c.Count() } -> std::same_as<std::size_t>;
  { c.ElementType() } -> std::same_as<const ElementType&>;
  { c.ElementAt(position) } -> std::same_as<const void*>;
  { m.ElementAt(position) } -> std::same_as<void*>;
};

// A value of `T` written into a stream of bits, the stream being an integral
// value (LRM 6.24.3).
template <typename T>
concept WritesBitStream =
    requires(const T& value, const IntegralView& written, std::uint64_t at) {
      { value.WriteToStream(written, at) } -> std::same_as<std::uint64_t>;
    };

// A question about a value's stream of bits the language defines for a kind
// and Lyra does not yet answer, which a legal program can ask.
[[noreturn]] void StreamNotYetSupported() {
  throw SimulationError(
      "reading this value as a stream of bits, or building one from it, is "
      "not yet supported on this backend; please open an issue asking for "
      "support");
}

// The parts of a value of `T`, each answered with what `T` itself does.
template <HoldsParts T>
inline constexpr PartsWitnessTable kPartsWitnessTableOf{
    .count = [](const ValueType&, const void* value) -> std::size_t {
      return static_cast<const T*>(value)->Count();
    },
    .type = [](const ValueType&, const void* value) -> const ValueType& {
      return static_cast<const T*>(value)->ElementType().Type();
    },
    .at = [](const ValueType&, const void* value,
             std::size_t position) -> const void* {
      return static_cast<const T*>(value)->ElementAt(position);
    },
    .ref_at = [](const ValueType&, void* value, std::size_t position) -> void* {
      return static_cast<T*>(value)->ElementAt(position);
    }};

// What `T` answers each operation of its table with, which is what `T` itself
// does: C++ states a type's operations as its own members, so the table is
// made from them rather than written. Each reads the value as a `T`, and none
// reads anything off the type it is handed, which is `T`'s alone.
template <LyraValue T>
struct ValueWitnessesOf {
  static auto Of(const void* value) -> const T& {
    return *static_cast<const T*>(value);
  }

  static constexpr auto Copy = [](const ValueType&, const void* value,
                                  void* out) {
    std::construct_at(static_cast<T*>(out), Of(value));
  };
  static constexpr auto Move = [](const ValueType&, void* value,
                                  void* out) noexcept {
    std::construct_at(static_cast<T*>(out), std::move(*static_cast<T*>(value)));
  };
  static constexpr auto Destroy = [](const ValueType&, void* value) noexcept {
    std::destroy_at(static_cast<T*>(value));
  };
  static constexpr auto Assign = [](const ValueType&, void* storage,
                                    const void* value) {
    *static_cast<T*>(storage) = Of(value);
  };

  static constexpr auto Equal = [](const ValueType&, const void* lhs,
                                   const void* rhs) -> FourStateBit {
    return AnswerScalar(Of(lhs) == Of(rhs));
  };
  static constexpr auto CaseEqual = [](const ValueType&, const void* lhs,
                                       const void* rhs) -> bool {
    if constexpr (CaseEqualComparable<T>) {
      return Of(lhs).CaseEqual(Of(rhs)).IsTruthy();
    } else {
      LacksOperation();
    }
  };
  static constexpr auto BitIdentical = [](const ValueType&, const void* lhs,
                                          const void* rhs) -> bool {
    return Of(lhs).IsBitIdentical(Of(rhs));
  };
  static constexpr auto HasUnknown = [](const ValueType&,
                                        const void* value) -> bool {
    return Of(value).HasUnknown();
  };

  // A value held with the type it was written in is of no type of its own, so
  // there is no value of it before anything writes one (LRM 7.8.1).
  static constexpr auto BuildDefault = [](const ValueType&, void* out) {
    if constexpr (std::is_same_v<T, AnyValue>) {
      LacksOperation();
    } else {
      std::construct_at(static_cast<T*>(out));
    }
  };

  static constexpr auto BitstreamWidth = [](const ValueType&, const void* value,
                                            void* out) {
    if constexpr (requires { Of(value).BitstreamWidth(); }) {
      std::construct_at(static_cast<Int*>(out), Of(value).BitstreamWidth());
    } else if constexpr (kIsBitStreamType<T>) {
      StreamNotYetSupported();
    } else {
      LacksOperation();
    }
  };
  static constexpr auto CountBits = [](const ValueType&, const void* value,
                                       const ConstIntegralView& control_bits,
                                       void* out) {
    if constexpr (requires { Of(value).CountBits(control_bits); }) {
      std::construct_at(
          static_cast<Int*>(out), Of(value).CountBits(control_bits));
    } else if constexpr (kIsBitStreamType<T>) {
      StreamNotYetSupported();
    } else {
      LacksOperation();
    }
  };
  static constexpr auto WriteToStream =
      [](const ValueType&, const void* value, const IntegralView& stream,
         std::uint64_t filled) -> std::uint64_t {
    if constexpr (WritesBitStream<T>) {
      return Of(value).WriteToStream(stream, filled);
    } else if constexpr (kIsBitStreamType<T>) {
      StreamNotYetSupported();
    } else {
      LacksOperation();
    }
  };
  static constexpr auto ReadFromStream =
      [](const ValueType&, const ConstIntegralView&, std::uint64_t,
         void*) -> std::uint64_t {
    if constexpr (kIsBitStreamType<T>) {
      StreamNotYetSupported();
    } else {
      LacksOperation();
    }
  };

  static constexpr auto ResolveTriState = [](const ValueType&, const void* lhs,
                                             const void* rhs, void* out) {
    if constexpr (NetResolvable<T>) {
      std::construct_at(static_cast<T*>(out), Of(lhs).ResolveTriState(Of(rhs)));
    } else {
      LacksOperation();
    }
  };
  static constexpr auto ResolveWiredAnd = [](const ValueType&, const void* lhs,
                                             const void* rhs, void* out) {
    if constexpr (NetResolvable<T>) {
      std::construct_at(static_cast<T*>(out), Of(lhs).ResolveWiredAnd(Of(rhs)));
    } else {
      LacksOperation();
    }
  };
  static constexpr auto ResolveWiredOr = [](const ValueType&, const void* lhs,
                                            const void* rhs, void* out) {
    if constexpr (NetResolvable<T>) {
      std::construct_at(static_cast<T*>(out), Of(lhs).ResolveWiredOr(Of(rhs)));
    } else {
      LacksOperation();
    }
  };
  static constexpr auto Dominating = [](const ValueType&, const void* stronger,
                                        const void* weaker, void* out) {
    if constexpr (NetResolvable<T>) {
      std::construct_at(
          static_cast<T*>(out), Of(stronger).Dominating(Of(weaker)));
    } else {
      LacksOperation();
    }
  };
  static constexpr auto FilledLike = [](const ValueType&, const void* prototype,
                                        const void* fill, void* out) {
    if constexpr (NetResolvable<T>) {
      std::construct_at(
          static_cast<T*>(out),
          FilledAs(Of(prototype), *static_cast<const Logic*>(fill)));
    } else {
      LacksOperation();
    }
  };

  // An index type's own order where it keys an associative array (LRM 7.8),
  // which for a handle is no SV operator at all; otherwise the type's `<`.
  static constexpr auto OrderBefore = [](const ValueType&, const void* lhs,
                                         const void* rhs) -> bool {
    if constexpr (requires { typename AssocKeyTraits<T>::Less; }) {
      return typename AssocKeyTraits<T>::Less{}(Of(lhs), Of(rhs));
    } else if constexpr (requires { Of(lhs) < Of(rhs); }) {
      return static_cast<bool>(Of(lhs) < Of(rhs));
    } else {
      LacksOperation();
    }
  };
  static constexpr auto IsTrue = [](const ValueType&,
                                    const void* value) -> bool {
    if constexpr (requires { Of(value).IsTruthy(); }) {
      return Of(value).IsTruthy();
    } else {
      LacksOperation();
    }
  };
  static constexpr auto Reduce = [](const ValueType&, Reduction reduction,
                                    const void* lhs, const void* rhs,
                                    void* out) {
    std::construct_at(
        static_cast<T*>(out), Reduced(reduction, Of(lhs), Of(rhs)));
  };

  static constexpr const PartsWitnessTable* kParts =
      []() -> const PartsWitnessTable* {
    if constexpr (HoldsParts<T>) {
      return &kPartsWitnessTableOf<T>;
    } else {
      return nullptr;
    }
  }();
};

// The library's type of `T`: its numbers and its table.
template <LyraValue T>
constexpr auto LibraryValueType() -> ValueType {
  return ValueType(
      kValueWitnessTableFor<ValueWitnessesOf<T>>,
      static_cast<std::uint32_t>(sizeof(T)),
      static_cast<std::uint32_t>(alignof(T)), ValueTypeKind::kOther);
}

}  // namespace

extern "C" {
constinit const ValueType lyra_rt_wildcard_index_value_type =
    LibraryValueType<AnyValue>();
constinit const ValueType lyra_rt_string_value_type =
    LibraryValueType<String>();
constinit const ValueType lyra_rt_real_value_type = LibraryValueType<Real>();
constinit const ValueType lyra_rt_shortreal_value_type =
    LibraryValueType<ShortReal>();
constinit const ValueType lyra_rt_chandle_value_type =
    LibraryValueType<Chandle>();
constinit const ValueType lyra_rt_empty_value_type = LibraryValueType<Empty>();
constinit const ValueType lyra_rt_dynarray_value_type =
    LibraryValueType<RuntimeDynamicArray>();
constinit const ValueType lyra_rt_queue_value_type =
    LibraryValueType<RuntimeQueue>();
constinit const ValueType lyra_rt_assocarray_value_type =
    LibraryValueType<RuntimeAssociativeArray>();
constinit const ValueType lyra_rt_managedref_value_type =
    LibraryValueType<ObjectRef>();
}

}  // namespace lyra::value
