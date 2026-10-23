#include "lyra/runtime/constraint_circuit.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <span>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"

namespace lyra::runtime {

ConstraintCircuit::ConstraintCircuit() {
  nodes_.emplace_back(ConstantNode{});
}

auto ConstraintCircuit::Input() -> CircuitLiteral {
  const auto node = static_cast<std::uint32_t>(nodes_.size());
  nodes_.emplace_back(
      PrimaryInput{.index = static_cast<std::uint32_t>(input_nodes_.size())});
  input_nodes_.push_back(node);
  return CircuitLiteral{node << 1U};
}

auto ConstraintCircuit::And(CircuitLiteral a, CircuitLiteral b)
    -> CircuitLiteral {
  if (a == kFalseLiteral || b == kFalseLiteral || a == b.Negated()) {
    return kFalseLiteral;
  }
  if (a == kTrueLiteral || a == b) {
    return b;
  }
  if (b == kTrueLiteral) {
    return a;
  }
  if (b.code < a.code) {
    std::swap(a, b);
  }
  const std::uint64_t key = (std::uint64_t{a.code} << 32U) | b.code;
  const auto [it, inserted] =
      gates_.try_emplace(key, static_cast<std::uint32_t>(nodes_.size()));
  if (inserted) {
    nodes_.emplace_back(AndGate{.left = a, .right = b});
  }
  return CircuitLiteral{it->second << 1U};
}

auto ConstraintCircuit::Or(CircuitLiteral a, CircuitLiteral b)
    -> CircuitLiteral {
  return And(a.Negated(), b.Negated()).Negated();
}

auto ConstraintCircuit::Xor(CircuitLiteral a, CircuitLiteral b)
    -> CircuitLiteral {
  return Or(And(a, b.Negated()), And(a.Negated(), b));
}

auto ConstraintCircuit::Ite(
    CircuitLiteral condition, CircuitLiteral then_bit, CircuitLiteral else_bit)
    -> CircuitLiteral {
  if (then_bit == else_bit) {
    return then_bit;
  }
  return Or(And(condition, then_bit), And(condition.Negated(), else_bit));
}

auto ConstraintCircuit::Constant(const std::vector<bool>& bits) -> CircuitBits {
  CircuitBits out;
  out.reserve(bits.size());
  for (const bool bit : bits) {
    out.push_back(bit ? kTrueLiteral : kFalseLiteral);
  }
  return out;
}

auto ConstraintCircuit::Resize(
    const CircuitBits& value, std::size_t width, bool sign_extend)
    -> CircuitBits {
  CircuitBits out(
      value.begin(), value.begin() + static_cast<std::ptrdiff_t>(
                                         std::min(width, value.size())));
  const CircuitLiteral fill =
      sign_extend && !value.empty() ? value.back() : kFalseLiteral;
  out.resize(width, fill);
  return out;
}

auto ConstraintCircuit::Not(const CircuitBits& a) -> CircuitBits {
  CircuitBits out;
  out.reserve(a.size());
  for (const CircuitLiteral bit : a) {
    out.push_back(bit.Negated());
  }
  return out;
}

auto ConstraintCircuit::And(const CircuitBits& a, const CircuitBits& b)
    -> CircuitBits {
  CircuitBits out(a.size());
  for (std::size_t i = 0; i < a.size(); ++i) {
    out[i] = And(a[i], b[i]);
  }
  return out;
}

auto ConstraintCircuit::Or(const CircuitBits& a, const CircuitBits& b)
    -> CircuitBits {
  CircuitBits out(a.size());
  for (std::size_t i = 0; i < a.size(); ++i) {
    out[i] = Or(a[i], b[i]);
  }
  return out;
}

auto ConstraintCircuit::Xor(const CircuitBits& a, const CircuitBits& b)
    -> CircuitBits {
  CircuitBits out(a.size());
  for (std::size_t i = 0; i < a.size(); ++i) {
    out[i] = Xor(a[i], b[i]);
  }
  return out;
}

auto ConstraintCircuit::Add(const CircuitBits& a, const CircuitBits& b)
    -> CircuitBits {
  CircuitBits out(a.size());
  CircuitLiteral carry = kFalseLiteral;
  for (std::size_t i = 0; i < a.size(); ++i) {
    const CircuitLiteral half = Xor(a[i], b[i]);
    out[i] = Xor(half, carry);
    carry = Or(And(a[i], b[i]), And(half, carry));
  }
  return out;
}

auto ConstraintCircuit::Neg(const CircuitBits& a) -> CircuitBits {
  CircuitBits one(a.size(), kFalseLiteral);
  if (!one.empty()) {
    one[0] = kTrueLiteral;
  }
  return Add(Not(a), one);
}

auto ConstraintCircuit::Sub(const CircuitBits& a, const CircuitBits& b)
    -> CircuitBits {
  // a + ~b + 1, the carry into the lowest bit standing for the one.
  CircuitBits out(a.size());
  CircuitLiteral carry = kTrueLiteral;
  for (std::size_t i = 0; i < a.size(); ++i) {
    const CircuitLiteral nb = b[i].Negated();
    const CircuitLiteral half = Xor(a[i], nb);
    out[i] = Xor(half, carry);
    carry = Or(And(a[i], nb), And(half, carry));
  }
  return out;
}

auto ConstraintCircuit::Mul(const CircuitBits& a, const CircuitBits& b)
    -> CircuitBits {
  const std::size_t width = a.size();
  CircuitBits product(width, kFalseLiteral);
  for (std::size_t i = 0; i < width; ++i) {
    CircuitBits partial(width, kFalseLiteral);
    for (std::size_t j = 0; i + j < width; ++j) {
      partial[i + j] = And(a[j], b[i]);
    }
    product = Add(product, partial);
  }
  return product;
}

namespace {

struct DivRem {
  CircuitBits quotient;
  CircuitBits remainder;
};

// Restoring division, one quotient bit per dividend bit from the top: the
// partial remainder takes the next dividend bit, and the divisor is taken
// away wherever it fits.
auto UnsignedDivRem(
    ConstraintCircuit& c, const CircuitBits& a, const CircuitBits& b)
    -> DivRem {
  const std::size_t width = a.size();
  const CircuitBits divisor = ConstraintCircuit::Resize(b, width + 1, false);
  CircuitBits remainder(width + 1, kFalseLiteral);
  CircuitBits quotient(width, kFalseLiteral);
  for (std::size_t step = width; step-- > 0;) {
    CircuitBits shifted(width + 1);
    shifted[0] = a[step];
    for (std::size_t i = 1; i <= width; ++i) {
      shifted[i] = remainder[i - 1];
    }
    const CircuitLiteral fits = c.Less(shifted, divisor, false).Negated();
    quotient[step] = fits;
    remainder = c.Select(fits, c.Sub(shifted, divisor), shifted);
  }
  return {
      .quotient = std::move(quotient),
      .remainder = ConstraintCircuit::Resize(remainder, width, false)};
}

auto DivideAndRemainder(
    ConstraintCircuit& c, const CircuitBits& a, const CircuitBits& b,
    bool is_signed) -> DivRem {
  if (a.empty()) {
    return {};
  }
  const CircuitBits zero(a.size(), kFalseLiteral);
  const CircuitLiteral by_zero = c.Equal(b, zero);
  if (!is_signed) {
    DivRem result = UnsignedDivRem(c, a, b);
    return {
        .quotient = c.Select(by_zero, zero, result.quotient),
        .remainder = c.Select(by_zero, zero, result.remainder)};
  }
  const CircuitLiteral a_negative = a.back();
  const CircuitLiteral b_negative = b.back();
  const CircuitBits a_magnitude = c.Select(a_negative, c.Neg(a), a);
  const CircuitBits b_magnitude = c.Select(b_negative, c.Neg(b), b);
  const DivRem magnitude = UnsignedDivRem(c, a_magnitude, b_magnitude);
  // LRM 11.4.2: a quotient truncates toward zero, and a remainder takes the
  // sign of the first operand.
  const CircuitBits quotient = c.Select(
      c.Xor(a_negative, b_negative), c.Neg(magnitude.quotient),
      magnitude.quotient);
  const CircuitBits remainder =
      c.Select(a_negative, c.Neg(magnitude.remainder), magnitude.remainder);
  return {
      .quotient = c.Select(by_zero, zero, quotient),
      .remainder = c.Select(by_zero, zero, remainder)};
}

}  // namespace

auto ConstraintCircuit::Div(
    const CircuitBits& a, const CircuitBits& b, bool is_signed) -> CircuitBits {
  return DivideAndRemainder(*this, a, b, is_signed).quotient;
}

auto ConstraintCircuit::Rem(
    const CircuitBits& a, const CircuitBits& b, bool is_signed) -> CircuitBits {
  return DivideAndRemainder(*this, a, b, is_signed).remainder;
}

namespace {

// Shifts `a` by the amount `amount` states, one stage per bit of the amount:
// a stage whose distance reaches past the value moves everything out.
auto Shift(
    ConstraintCircuit& c, const CircuitBits& a, const CircuitBits& amount,
    bool left, CircuitLiteral fill) -> CircuitBits {
  const std::size_t width = a.size();
  CircuitBits value = a;
  CircuitLiteral beyond = kFalseLiteral;
  for (std::size_t k = 0; k < amount.size(); ++k) {
    if (k >= 63 || (std::size_t{1} << k) >= width) {
      beyond = c.Or(beyond, amount[k]);
      continue;
    }
    const std::size_t distance = std::size_t{1} << k;
    CircuitBits moved(width, fill);
    for (std::size_t i = 0; i < width; ++i) {
      if (left && i >= distance) {
        moved[i] = value[i - distance];
      } else if (!left && i + distance < width) {
        moved[i] = value[i + distance];
      }
    }
    value = c.Select(amount[k], moved, value);
  }
  return c.Select(beyond, CircuitBits(width, fill), value);
}

}  // namespace

auto ConstraintCircuit::ShiftLeft(
    const CircuitBits& a, const CircuitBits& amount) -> CircuitBits {
  return Shift(*this, a, amount, true, kFalseLiteral);
}

auto ConstraintCircuit::ShiftRight(
    const CircuitBits& a, const CircuitBits& amount, bool arithmetic)
    -> CircuitBits {
  const CircuitLiteral fill =
      arithmetic && !a.empty() ? a.back() : kFalseLiteral;
  return Shift(*this, a, amount, false, fill);
}

auto ConstraintCircuit::Equal(const CircuitBits& a, const CircuitBits& b)
    -> CircuitLiteral {
  CircuitLiteral equal = kTrueLiteral;
  for (std::size_t i = 0; i < a.size(); ++i) {
    equal = And(equal, Xor(a[i], b[i]).Negated());
  }
  return equal;
}

auto ConstraintCircuit::Less(
    const CircuitBits& a, const CircuitBits& b, bool is_signed)
    -> CircuitLiteral {
  // From the lowest bit up: below the bits read so far a is less where this
  // bit decides it, or where the bits agree and the lower ones did. A signed
  // comparison is the unsigned one with the sign bits read the other way.
  CircuitLiteral less = kFalseLiteral;
  for (std::size_t i = 0; i < a.size(); ++i) {
    const bool flip = is_signed && i + 1 == a.size();
    const CircuitLiteral ai = flip ? a[i].Negated() : a[i];
    const CircuitLiteral bi = flip ? b[i].Negated() : b[i];
    less = Or(And(ai.Negated(), bi), And(Xor(ai, bi).Negated(), less));
  }
  return less;
}

auto ConstraintCircuit::ReduceAnd(const CircuitBits& a) -> CircuitLiteral {
  CircuitLiteral out = kTrueLiteral;
  for (const CircuitLiteral bit : a) {
    out = And(out, bit);
  }
  return out;
}

auto ConstraintCircuit::ReduceOr(const CircuitBits& a) -> CircuitLiteral {
  CircuitLiteral out = kFalseLiteral;
  for (const CircuitLiteral bit : a) {
    out = Or(out, bit);
  }
  return out;
}

auto ConstraintCircuit::ReduceXor(const CircuitBits& a) -> CircuitLiteral {
  CircuitLiteral out = kFalseLiteral;
  for (const CircuitLiteral bit : a) {
    out = Xor(out, bit);
  }
  return out;
}

auto ConstraintCircuit::Select(
    CircuitLiteral condition, const CircuitBits& then_bits,
    const CircuitBits& else_bits) -> CircuitBits {
  CircuitBits out(then_bits.size());
  for (std::size_t i = 0; i < then_bits.size(); ++i) {
    out[i] = Ite(condition, then_bits[i], else_bits[i]);
  }
  return out;
}

auto ConstraintCircuit::IsInput(std::uint32_t node) const -> bool {
  return std::holds_alternative<PrimaryInput>(nodes_[node]);
}

auto ConstraintCircuit::InputIndex(std::uint32_t node) const -> std::uint32_t {
  const auto* input = std::get_if<PrimaryInput>(&nodes_[node]);
  if (input == nullptr) {
    throw InternalError(
        "ConstraintCircuit: only an input has an input position");
  }
  return input->index;
}

auto ConstraintCircuit::InputNode(std::uint32_t index) const -> std::uint32_t {
  return input_nodes_[index];
}

auto ConstraintCircuit::InputCount() const -> std::uint32_t {
  return static_cast<std::uint32_t>(input_nodes_.size());
}

auto ConstraintCircuit::NodeCount() const -> std::uint32_t {
  return static_cast<std::uint32_t>(nodes_.size());
}

auto ConstraintCircuit::FaninOf(std::uint32_t node) const
    -> std::pair<CircuitLiteral, CircuitLiteral> {
  const auto* gate = std::get_if<AndGate>(&nodes_[node]);
  if (gate == nullptr) {
    throw InternalError("ConstraintCircuit: only a gate has fanins");
  }
  return {gate->left, gate->right};
}

auto ConstraintCircuit::Support(std::span<const CircuitLiteral> roots) const
    -> std::vector<std::uint32_t> {
  std::vector<bool> seen(nodes_.size(), false);
  std::vector<std::uint32_t> stack;
  std::vector<std::uint32_t> inputs;
  for (const CircuitLiteral root : roots) {
    stack.push_back(root.Node());
  }
  while (!stack.empty()) {
    const std::uint32_t node = stack.back();
    stack.pop_back();
    if (node == 0 || seen[node]) {
      continue;
    }
    seen[node] = true;
    std::visit(
        Overloaded{
            [](const ConstantNode&) {},
            [&](const PrimaryInput& input) { inputs.push_back(input.index); },
            [&](const AndGate& gate) {
              stack.push_back(gate.left.Node());
              stack.push_back(gate.right.Node());
            }},
        nodes_[node]);
  }
  std::ranges::sort(inputs);
  return inputs;
}

auto ConstraintCircuit::Evaluate(
    CircuitLiteral literal, const std::vector<bool>& inputs) const -> bool {
  // A gate's inputs are made before it, so one pass in creation order sees
  // every fanin settled.
  const std::uint32_t last = literal.Node();
  std::vector<bool> value(last + 1, false);
  for (std::uint32_t node = 1; node <= last; ++node) {
    const auto holds = [&](CircuitLiteral l) {
      return value[l.Node()] != l.IsNegated();
    };
    value[node] = std::visit(
        Overloaded{
            [](const ConstantNode&) { return false; },
            [&](const PrimaryInput& input) -> bool {
              return inputs[input.index];
            },
            [&](const AndGate& gate) {
              return holds(gate.left) && holds(gate.right);
            }},
        nodes_[node]);
  }
  return value[last] != literal.IsNegated();
}

}  // namespace lyra::runtime
