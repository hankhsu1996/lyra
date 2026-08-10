#include "lyra/backend/cpp/place_access.hpp"

#include <format>
#include <string>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/mir/type.hpp"

namespace lyra::backend::cpp {

namespace {

// The runtime handle a store reaches subscribers through. It is execution
// context rather than program data, so it is reached where the access is
// realized instead of travelling through the program as an operand.
constexpr std::string_view kAmbientRuntime = "lyra::runtime::current_runtime()";

// The runtime library gives every capability wrapper the same access surface,
// so which wrapper a place holds does not change how the access is spelled --
// only whether that access exists at all. Each entry below is therefore one
// shared form plus the wrappers the access is not defined on.
void RequireCapabilityWrapper(const mir::Type& ty, std::string_view where) {
  if (!mir::IsCapabilityWrapperType(ty)) {
    throw InternalError(
        std::format(
            "{}: type is not a capability wrapper, so it represents no storage "
            "to reach through",
            where));
  }
}

}  // namespace

auto RenderLoadThrough(const mir::Type& wrapper_type, std::string_view wrapper)
    -> std::string {
  RequireCapabilityWrapper(wrapper_type, "RenderLoadThrough");
  // A driver carries one contribution to a net's resolution, and SystemVerilog
  // gives no way to name it: a design reads the net, whose value is the fold of
  // every driver (LRM 6.5).
  if (std::holds_alternative<mir::DriverType>(wrapper_type.data)) {
    throw InternalError(
        "RenderLoadThrough: a net driver's contribution is not readable; a "
        "read of the net reads its resolved value");
  }
  return std::format("({}).Get()", wrapper);
}

auto RenderLendThrough(const mir::Type& wrapper_type, std::string_view wrapper)
    -> std::string {
  RequireCapabilityWrapper(wrapper_type, "RenderLendThrough");
  // A net's resolved value is the fold of its drivers, so nothing writes it or
  // holds it by reference; a value reaches a net only through one of those
  // drivers (LRM 6.5).
  if (std::holds_alternative<mir::ResolvedType>(wrapper_type.data)) {
    throw InternalError(
        "RenderLendThrough: a net's resolved value takes no store; the "
        "destination is one of its drivers");
  }
  return std::format("(*({}).Mutate({}))", wrapper, kAmbientRuntime);
}

}  // namespace lyra::backend::cpp
