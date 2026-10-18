#pragma once

#include <concepts>
#include <memory>

#include "lyra/base/function_ref.hpp"
#include "lyra/support/callable_bounds.hpp"

namespace lyra::runtime {

// What every closure's definition begins with: the body ending the closure,
// the one thing any holder may ask of a closure whatever its signature, as
// every rustc vtable begins with the entries any `dyn` type has before the
// methods of its trait.
//
// The captures lie in the closure value, after what this library keeps there,
// where the code building the closure placed them and fills them. So of the
// value this states only the body ending it, as a class's definition states
// its destructor; where the value lies and how large it is are the building
// code's to say.
struct ClosureCommonEntries {
  void (*end)(void* self) = nullptr;
};

// The immutable definition of one closure: how it is ended, then the body a
// call runs, entered on the closure value its captures live in at the one
// signature it has, as the call method of a rustc `dyn FnMut(A) -> R` vtable
// is. Held once and shared by every value built from it, the way a class's
// definition is shared by its values, and emitted by the unit declaring it as
// a constant laid out as C lays a structure out. Every signature takes and
// answers machine values only, so nothing here knows a type of the program.
//
// A body started where it is made -- a fork's branch, a task enabled with
// arguments to copy back, an update waiting for its event -- has no call and
// is never held as a closure of no known type, so its definition is the
// common entries alone.
template <typename Signature>
struct ClosureDefinition;

template <typename R, typename... Params>
struct ClosureDefinition<R(Params...)> {
  ClosureCommonEntries common;
  R (*call)(void* self, Params... params) = nullptr;
};

// The part of every closure this library keeps, at its start: the definition
// it is ended and called through. Whatever the library is handed to run or ask
// later, it holds as this part, without knowing the closure's type; the
// captures lie after it, laid out by the code that built it. This is the shape
// of a C++ object holding its class's table address, rather than the address
// pair a Rust `Box<dyn>` is, so a box of a closure is one address wherever it
// is held, as a box of anything else is.
//
// The value never moves: a coroutine body's frame reads the captures through
// the value it was entered on for as long as it runs, and whatever keeps the
// closure is handed the box it was made in instead. Code that lays a closure
// out itself makes this part by storing the definition's address at the
// closure's start, as a constructor stores a class's table address.
class ClosureValue {
 public:
  ClosureValue(const ClosureValue&) = delete;
  auto operator=(const ClosureValue&) -> ClosureValue& = delete;
  ClosureValue(ClosureValue&&) = delete;
  auto operator=(ClosureValue&&) -> ClosureValue& = delete;

  // Ends the closure where it lies, captures included.
  void End();

  [[nodiscard]] auto Definition() const -> const ClosureCommonEntries* {
    return definition_;
  }

 protected:
  // The part of a closure of a known type this library keeps, made by that
  // closure's own construction.
  explicit ClosureValue(const ClosureCommonEntries* definition);

  template <typename Signature>
  explicit ClosureValue(const ClosureDefinition<Signature>* definition);

  ~ClosureValue() = default;

 private:
  const ClosureCommonEntries* definition_;
};

// Ends a closure of no known type held through a box, and gives back the
// storage it was made in: its end comes from the definition it opens with, as
// a Rust `Box<dyn>` is dropped through its vtable.
struct EndClosure {
  void operator()(ClosureValue* closure) const noexcept;
};

namespace detail {

// The definition `closure` was made from, which the holder knows to be at
// `Signature` because that is the signature it was handed the closure at. The
// common entries are the definition's first member, so the two share an
// address.
template <typename Signature>
auto DefinitionAt(const ClosureValue& closure)
    -> const ClosureDefinition<Signature>& {
  const void* definition = closure.Definition();
  return *static_cast<const ClosureDefinition<Signature>*>(definition);
}

}  // namespace detail

// A closure kept past the body that built it, called at `Signature`, as a Rust
// `Box<dyn FnMut(A) -> R>` is: the box that body made it in, which stays where
// it was made while the box is handed on.
template <typename Signature>
class OwnedClosure;

template <typename R, typename... Params>
class OwnedClosure<R(Params...)> {
 public:
  // Takes over the box `closure` was made in, whose definition is at this
  // signature.
  explicit OwnedClosure(ClosureValue* closure);
  ~OwnedClosure();
  OwnedClosure(const OwnedClosure&) = delete;
  auto operator=(const OwnedClosure&) -> OwnedClosure& = delete;
  OwnedClosure(OwnedClosure&&) noexcept;
  auto operator=(OwnedClosure&&) noexcept -> OwnedClosure&;

  auto operator()(Params... params) -> R {
    return detail::DefinitionAt<R(Params...)>(*box_).call(
        box_.get(), params...);
  }

 private:
  std::unique_ptr<ClosureValue, EndClosure> box_;
};

template <typename Signature>
ClosureValue::ClosureValue(const ClosureDefinition<Signature>* definition)
    : ClosureValue(&definition->common) {
}

template <typename R, typename... Params>
OwnedClosure<R(Params...)>::OwnedClosure(ClosureValue* closure)
    : box_(closure) {
}

template <typename R, typename... Params>
OwnedClosure<R(Params...)>::~OwnedClosure() = default;

template <typename R, typename... Params>
OwnedClosure<R(Params...)>::OwnedClosure(OwnedClosure&&) noexcept = default;

template <typename R, typename... Params>
auto OwnedClosure<R(Params...)>::operator=(OwnedClosure&&) noexcept
    -> OwnedClosure& = default;

// The signatures the library calls a closure at are fixed by its own entries,
// so what a closure needs of this library at each is compiled once, here,
// rather than in every unit that builds a closure.
extern template ClosureValue::ClosureValue(
    const ClosureDefinition<support::EffectSignature>* definition);
extern template ClosureValue::ClosureValue(
    const ClosureDefinition<support::ConditionSignature>* definition);
extern template ClosureValue::ClosureValue(
    const ClosureDefinition<support::WatchSignature>* definition);
extern template ClosureValue::ClosureValue(
    const ClosureDefinition<support::OrderSignature>* definition);
extern template class OwnedClosure<support::EffectSignature>;
extern template class OwnedClosure<support::ConditionSignature>;
extern template class OwnedClosure<support::WatchSignature>;

// The closure the library was handed a borrow of, of no known type, read as
// one called at `Signature`, as a Rust `&dyn Fn` is: where the closure lies,
// and the call of its definition.
template <typename Signature>
[[nodiscard]] auto Borrow(ClosureValue& closure)
    -> base::FunctionRef<Signature> {
  return {&closure, detail::DefinitionAt<Signature>(closure).call};
}

namespace detail {

// The signature a closure's call operator is entered at.
template <typename Member>
struct CallOperator;

template <typename Closure, typename R, typename... Params>
struct CallOperator<R (Closure::*)(Params...)> {
  using Signature = R(Params...);
};

template <typename Closure, typename R, typename... Params>
struct CallOperator<R (Closure::*)(Params...) const> {
  using Signature = R(Params...);
};

// The closure `self` is the library's part of.
template <typename Closure>
auto ClosureAt(void* self) -> Closure* {
  return static_cast<Closure*>(static_cast<ClosureValue*>(self));
}

template <typename Closure>
void EndAt(void* self) {
  std::destroy_at(ClosureAt<Closure>(self));
}

template <typename Closure, typename Signature>
struct CallAt;

template <typename Closure, typename R, typename... Params>
struct CallAt<Closure, R(Params...)> {
  static auto Call(void* self, Params... params) -> R {
    return (*ClosureAt<Closure>(self))(params...);
  }
};

}  // namespace detail

// The signature the closure type `Closure` is called at: its call operator's,
// whether that operator only reads the closure or also writes it.
template <typename Closure>
using SignatureOf =
    typename detail::CallOperator<decltype(&Closure::operator())>::Signature;

// A closure of a known type made one of no known type called at `Signature`,
// as Rust's unsizing coercion makes a `Box<dyn FnMut>` of a `Box<T>` and a
// `&dyn FnMut` of a `&T`: the closure stays where it was made, and is called,
// and for a box ended, from then on through its definition. A box is taken
// over; a borrow is called through the call its type has, which is known here.
template <typename Signature, std::derived_from<ClosureValue> Closure>
[[nodiscard]] auto Unsize(std::unique_ptr<Closure> box)
    -> OwnedClosure<Signature> {
  static_assert(
      std::same_as<Signature, SignatureOf<Closure>>,
      "a closure is held at the signature its body has");
  return OwnedClosure<Signature>(box.release());
}

template <typename Signature, std::derived_from<ClosureValue> Closure>
[[nodiscard]] auto Unsize(Closure& closure) -> base::FunctionRef<Signature> {
  static_assert(
      std::same_as<Signature, SignatureOf<Closure>>,
      "a closure is held at the signature its body has");
  return {
      static_cast<ClosureValue*>(&closure),
      &detail::CallAt<Closure, Signature>::Call};
}

// The definition of the closure type `Closure`: the body ending it, and the
// call its call operator is entered through, at that operator's signature. A
// closure with no call operator is entered where it is started, and its
// definition is the common entries alone.
template <std::derived_from<ClosureValue> Closure>
consteval auto DefinitionOf() {
  constexpr ClosureCommonEntries kCommon{.end = &detail::EndAt<Closure>};
  if constexpr (requires { &Closure::operator(); }) {
    using Signature = SignatureOf<Closure>;
    return ClosureDefinition<Signature>{
        .common = kCommon, .call = &detail::CallAt<Closure, Signature>::Call};
  } else {
    return kCommon;
  }
}

}  // namespace lyra::runtime
