#pragma once

namespace lyra::base {

template <typename Signature>
class FunctionRef;

// A borrowed callable whose type is erased: where it lies, and the function
// that calls it there, as LLVM's `function_ref` and Rust's `&dyn Fn` are. It
// owns nothing, so it is held only while what it refers to is alive. Whoever
// lends one supplies the function, which knows the callable's type; nothing
// here does.
template <typename R, typename... Params>
class FunctionRef<R(Params...)> {
 public:
  using Callback = R (*)(void* callable, Params... params);

  // Refers to nothing, as LLVM's empty `function_ref` does: what a slot holds
  // before one is lent into it. Calling it is an error.
  FunctionRef() = default;
  FunctionRef(void* callable, Callback callback)
      : callable_(callable), callback_(callback) {
  }

  auto operator()(Params... params) const -> R {
    return callback_(callable_, params...);
  }

 private:
  void* callable_ = nullptr;
  Callback callback_ = nullptr;
};

}  // namespace lyra::base
