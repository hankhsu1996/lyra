#pragma once

#include <cstdint>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/float_width.hpp"
#include "lyra/support/layout.hpp"
#include "lyra/support/value_domain.hpp"
#include "lyra/value/integral.hpp"

namespace lyra::value {

// A value of the design held as its own bytes, laid out by its type alone:
// an integral value of a width and the states each bit holds, a real, which
// is the IEEE 754 value of its width (LRM 6.12), and a reference owning
// nothing, which is the address it holds (LRM 6.14, 25.9).
struct IntegralScalar {
  std::uint64_t width;
  StateDomain states;
};
struct RealScalar {
  support::FloatWidth width;
};
struct UnmanagedRefScalar {};
using Scalar = std::variant<IntegralScalar, RealScalar, UnmanagedRefScalar>;

// How a scalar is laid out, and the domain a cell or a container holding one
// holds it in. Every layer laying a scalar out or naming that domain reads
// this, and the library's class for each checks it is exactly this layout.
struct ScalarFacts {
  support::Layout layout;
  support::ValueDomain domain;
};

constexpr auto FactsOf(const Scalar& scalar) -> ScalarFacts {
  return std::visit(
      Overloaded{
          [](const IntegralScalar& integral) {
            return ScalarFacts{
                .layout =
                    {.size = IntegralBytesFor(integral.width, integral.states),
                     .align = IntegralAlignFor(integral.width)},
                .domain = IntegralDomainFor(integral.width, integral.states)};
          },
          // A real is laid out as the C float of its width.
          [](const RealScalar& real) {
            switch (real.width) {
              case support::FloatWidth::k32:
                return ScalarFacts{
                    .layout = {.size = 4, .align = 4},
                    .domain = support::ValueDomain::kShortReal};
              case support::FloatWidth::k64:
                return ScalarFacts{
                    .layout = {.size = 8, .align = 8},
                    .domain = support::ValueDomain::kReal};
            }
            throw InternalError("value: unknown float width");
          },
          [](const UnmanagedRefScalar&) {
            return ScalarFacts{
                .layout = {.size = sizeof(void*), .align = alignof(void*)},
                .domain = support::ValueDomain::kUnmanagedRef};
          }},
      scalar);
}

}  // namespace lyra::value
