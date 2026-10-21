#pragma once

#include <string_view>

#include "lyra/base/function_ref.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/support/callable_bounds.hpp"
#include "lyra/value/any.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/value_type.hpp"
#include "lyra/value/value_witnesses_of.hpp"

namespace lyra::runtime {

// A reference to a value of any type, as Rust's `&mut dyn Any` is: what a
// `ref` argument of `dynamic_type` stands for (LRM 15.4.5, 13.5.2), apart from
// the value of any type as `function_ref` is apart from `std::function`. A
// value of its identity is received in two steps: it is written into storage
// of the type the reference was made at, which whoever made the reference
// holds, and the closure that maker lends then carries it on to where the
// reference stands for, as any write through a `ref` lands.
class AnyReference {
 public:
  // Refers to nothing, and so receives nothing: what a frame holds before the
  // argument is bound, and what a call stating only what it reads is handed.
  AnyReference() = default;
  AnyReference(
      void* incoming, const value::ValueType& type, const char* identity,
      base::FunctionRef<support::EffectSignature> carry)
      : incoming_(incoming), type_(&type), identity_(identity), carry_(carry) {
  }

  // Writes `value` where this stands, where the two are of one identity, and
  // answers whether it did: a message is received only into a variable of a
  // type equivalent to its own (LRM 15.4.5).
  [[nodiscard]] auto Receive(const value::Any& value) const -> bool {
    if (identity_ == nullptr || value.Identity() != identity_) {
      return false;
    }
    type_->Assign(incoming_, value.HeldValue());
    carry_();
    return true;
  }

 private:
  void* incoming_ = nullptr;
  const value::ValueType* type_ = nullptr;
  const char* identity_ = nullptr;
  base::FunctionRef<support::EffectSignature> carry_;
};

// A value of any type holding a copy of `value`, kept with its type and the
// identity it is tested by (LRM 15.4.9).
template <value::LyraValue T>
[[nodiscard]] auto MakeAny(const T& value, const char* identity) -> value::Any {
  return value::Any::CopyOf(value::kValueTypeOf<T>, &value, identity);
}

// A reference to a value of any type, of the identity `identity` names, which
// receives into what `incoming` refers to and is then carried on by `carry`.
template <value::LyraValue T>
[[nodiscard]] auto SeeAsAny(
    VariableRef<T> incoming, const char* identity,
    base::FunctionRef<support::EffectSignature> carry) -> AnyReference {
  return {incoming.Erased().storage, value::kValueTypeOf<T>, identity, carry};
}

// Writes `value` where `into` stands, where the two are of one identity, and
// answers whether it did.
inline auto ReceiveAny(const AnyReference& into, const value::Any& value)
    -> bool {
  return into.Receive(value);
}

}  // namespace lyra::runtime
