#pragma once

#include <cstddef>
#include <cstdint>
#include <span>
#include <unordered_map>
#include <utility>
#include <variant>
#include <vector>

namespace lyra::runtime {

// A wire of a constraint circuit: a node, read as it is or negated. Node 0 is
// the constant false, so the literal 0 is false and 1 is true.
struct CircuitLiteral {
  std::uint32_t code = 0;

  [[nodiscard]] auto Node() const -> std::uint32_t {
    return code >> 1U;
  }
  [[nodiscard]] auto IsNegated() const -> bool {
    return (code & 1U) != 0;
  }
  [[nodiscard]] auto Negated() const -> CircuitLiteral {
    return CircuitLiteral{code ^ 1U};
  }
  [[nodiscard]] auto IsConstant() const -> bool {
    return Node() == 0;
  }

  auto operator==(const CircuitLiteral&) const -> bool = default;
};

inline constexpr CircuitLiteral kFalseLiteral{0};
inline constexpr CircuitLiteral kTrueLiteral{1};

// The bits of a value of a constraint, least significant first.
using CircuitBits = std::vector<CircuitLiteral>;

// What a constraint states, as a graph of two-input AND gates over the bits
// of the values chosen, each edge read as it is or negated (an and-inverter
// graph, as SAT-based and BDD-based checkers both take their input). A gate
// asked for twice with the same inputs is the same gate, so what two
// constraints state about one value is stated once.
class ConstraintCircuit {
 public:
  ConstraintCircuit();

  // A bit nothing decides yet, numbered by the order inputs are made.
  [[nodiscard]] auto Input() -> CircuitLiteral;

  [[nodiscard]] auto And(CircuitLiteral a, CircuitLiteral b) -> CircuitLiteral;
  [[nodiscard]] auto Or(CircuitLiteral a, CircuitLiteral b) -> CircuitLiteral;
  [[nodiscard]] auto Xor(CircuitLiteral a, CircuitLiteral b) -> CircuitLiteral;
  [[nodiscard]] auto Ite(
      CircuitLiteral condition, CircuitLiteral then_bit,
      CircuitLiteral else_bit) -> CircuitLiteral;

  // Arithmetic over the bits of values, each the width of its operands unless
  // it says otherwise, wrapping as SystemVerilog's two-state arithmetic does
  // (LRM 11.4). A signed operation reads its operands as two's complement.
  [[nodiscard]] static auto Constant(const std::vector<bool>& bits)
      -> CircuitBits;
  [[nodiscard]] static auto Resize(
      const CircuitBits& value, std::size_t width, bool sign_extend)
      -> CircuitBits;

  [[nodiscard]] static auto Not(const CircuitBits& a) -> CircuitBits;
  [[nodiscard]] auto And(const CircuitBits& a, const CircuitBits& b)
      -> CircuitBits;
  [[nodiscard]] auto Or(const CircuitBits& a, const CircuitBits& b)
      -> CircuitBits;
  [[nodiscard]] auto Xor(const CircuitBits& a, const CircuitBits& b)
      -> CircuitBits;

  [[nodiscard]] auto Add(const CircuitBits& a, const CircuitBits& b)
      -> CircuitBits;
  [[nodiscard]] auto Sub(const CircuitBits& a, const CircuitBits& b)
      -> CircuitBits;
  [[nodiscard]] auto Neg(const CircuitBits& a) -> CircuitBits;
  [[nodiscard]] auto Mul(const CircuitBits& a, const CircuitBits& b)
      -> CircuitBits;
  // A quotient or a remainder; division by zero answers zero for both, which a
  // two-state solver has to pick some value for where LRM 11.4.2 answers x.
  [[nodiscard]] auto Div(
      const CircuitBits& a, const CircuitBits& b, bool is_signed)
      -> CircuitBits;
  [[nodiscard]] auto Rem(
      const CircuitBits& a, const CircuitBits& b, bool is_signed)
      -> CircuitBits;

  // A shift by an amount of any width: what it shifts out is gone, and a right
  // shift that is arithmetic fills with the sign (LRM 11.4.10).
  [[nodiscard]] auto ShiftLeft(const CircuitBits& a, const CircuitBits& amount)
      -> CircuitBits;
  [[nodiscard]] auto ShiftRight(
      const CircuitBits& a, const CircuitBits& amount, bool arithmetic)
      -> CircuitBits;

  [[nodiscard]] auto Equal(const CircuitBits& a, const CircuitBits& b)
      -> CircuitLiteral;
  [[nodiscard]] auto Less(
      const CircuitBits& a, const CircuitBits& b, bool is_signed)
      -> CircuitLiteral;

  [[nodiscard]] auto ReduceAnd(const CircuitBits& a) -> CircuitLiteral;
  [[nodiscard]] auto ReduceOr(const CircuitBits& a) -> CircuitLiteral;
  [[nodiscard]] auto ReduceXor(const CircuitBits& a) -> CircuitLiteral;

  [[nodiscard]] auto Select(
      CircuitLiteral condition, const CircuitBits& then_bits,
      const CircuitBits& else_bits) -> CircuitBits;

  [[nodiscard]] auto IsInput(std::uint32_t node) const -> bool;
  // The position among the inputs of an input node.
  [[nodiscard]] auto InputIndex(std::uint32_t node) const -> std::uint32_t;
  [[nodiscard]] auto InputNode(std::uint32_t index) const -> std::uint32_t;
  [[nodiscard]] auto InputCount() const -> std::uint32_t;
  [[nodiscard]] auto NodeCount() const -> std::uint32_t;
  // The two inputs of a gate node.
  [[nodiscard]] auto FaninOf(std::uint32_t node) const
      -> std::pair<CircuitLiteral, CircuitLiteral>;

  // The inputs `roots` read, as input positions in increasing order.
  [[nodiscard]] auto Support(std::span<const CircuitLiteral> roots) const
      -> std::vector<std::uint32_t>;

  // The value of `literal` where input `i` holds `inputs[i]`.
  [[nodiscard]] auto Evaluate(
      CircuitLiteral literal, const std::vector<bool>& inputs) const -> bool;

 private:
  // Node 0, false, which every constant literal names.
  struct ConstantNode {};
  // An input, at its position among the inputs.
  struct PrimaryInput {
    std::uint32_t index;
  };
  // The conjunction of two literals.
  struct AndGate {
    CircuitLiteral left;
    CircuitLiteral right;
  };
  using Node = std::variant<ConstantNode, PrimaryInput, AndGate>;

  std::vector<Node> nodes_;
  std::vector<std::uint32_t> input_nodes_;
  std::unordered_map<std::uint64_t, std::uint32_t> gates_;
};

}  // namespace lyra::runtime
