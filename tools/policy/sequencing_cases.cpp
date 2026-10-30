// The cases `check_sequencing.py --self-test` holds the query to. Each case
// is the code under a comment opening `Reported:`, which the query has to
// report, or `In order:`, which it must not. The types stand in for Lyra's by
// name alone, since the query recognizes a builder by its name.

#include <cstdint>
#include <functional>
#include <utility>
#include <variant>

namespace lyra::mir {

struct ExprId {
  std::uint32_t value;
};

struct Arena {
  std::uint32_t added = 0;
  auto Add(int node) -> ExprId {
    added += static_cast<std::uint32_t>(node);
    return {.value = added};
  }
  [[nodiscard]] auto Get(ExprId id) const -> std::uint32_t {
    return id.value + added;
  }
};

struct Shape {
  std::uint32_t width;
};

struct Block {
  Arena exprs;
  Shape shape{};
  auto ReadLocal(int local) -> ExprId {
    return exprs.Add(local);
  }
  auto Declared() -> Shape& {
    return shape;
  }
};

struct BinaryExpr {
  ExprId lhs;
  ExprId rhs;
};

auto MakeBinary(Block& block, ExprId lhs, ExprId rhs) -> ExprId {
  return block.exprs.Add(static_cast<int>(lhs.value + rhs.value));
}

auto Literal(Block& block, int value) -> ExprId {
  return block.exprs.Add(value);
}

auto Pair(ExprId lhs, ExprId rhs) -> BinaryExpr {
  return {.lhs = lhs, .rhs = rhs};
}

auto Later(ExprId now, const std::function<ExprId()>& then) -> BinaryExpr {
  return {.lhs = now, .rhs = then()};
}

// Holds a block and appends to it from a method that is const to itself.
struct CodeGenFunction {
  Block* block;
  [[nodiscard]] auto Load(int local) const -> ExprId {
    return block->ReadLocal(local);
  }
};

template <class... Arms>
struct Overloaded : Arms... {
  using Arms::operator()...;
};

template <class... Arms>
Overloaded(Arms...) -> Overloaded<Arms...>;

auto Sum(std::uint32_t a, std::uint32_t b) -> std::uint32_t {
  return a + b;
}

auto Join(Block first, Block second) -> std::uint32_t {
  return first.exprs.added + second.exprs.added;
}

auto Choose(Block& block) -> std::variant<int, ExprId> {
  return block.ReadLocal(7);
}

auto Cases(Block& block) -> std::uint32_t {
  // Reported: two appends as arguments of one call.
  const ExprId direct =
      MakeBinary(block, block.ReadLocal(1), block.exprs.Add(2));
  // Reported: a helper taking the block counts as one.
  const BinaryExpr helped = Pair(Literal(block, 1), Literal(block, 2));
  // Reported: an append under an operator's operands.
  const std::uint32_t sum = block.ReadLocal(1).value + block.ReadLocal(2).value;
  // Reported: a lambda's body is not visible at the call, so calling one
  // counts.
  auto load = [&](int i) { return block.ReadLocal(i); };
  const BinaryExpr loaded = Pair(load(0), load(1));
  // Reported: a method of what holds a builder counts, const or not.
  const CodeGenFunction codegen{.block = &block};
  const BinaryExpr emitted = Pair(codegen.Load(1), codegen.Load(2));

  // In order: each operand bound in a statement of its own.
  const ExprId lhs = block.ReadLocal(1);
  const ExprId rhs = block.exprs.Add(2);
  const ExprId bound = MakeBinary(block, lhs, rhs);
  // In order: a braced initializer runs its clauses first to last.
  const BinaryExpr braced{.lhs = block.ReadLocal(1), .rhs = Literal(block, 2)};
  // In order: one append beside reads of what is already built.
  const ExprId one = MakeBinary(block, block.ReadLocal(1), rhs);
  // In order: an accessor answers what is there and appends nothing.
  const BinaryExpr read =
      Pair(Literal(block, 1), ExprId{block.Declared().width});
  // In order: a lambda handed over is not run where it is written.
  auto apply = [](auto f, ExprId at) { return f(at); };
  const ExprId applied = apply(
      [&](ExprId at) { return Literal(block, static_cast<int>(at.value)); },
      block.ReadLocal(3));
  // In order: so is one a parameter's type is built from.
  const BinaryExpr deferred =
      Later(block.ReadLocal(4), [&] { return Literal(block, 5); });
  // In order: a const method of what holds nodes reads them and appends
  // nothing.
  const std::uint32_t looked = Sum(block.exprs.Get(lhs), block.exprs.Get(rhs));
  // In order: nor does a set of lambdas built to be handed over run where it is
  // built.
  const ExprId visited = std::visit(
      Overloaded{
          [&](int at) { return Literal(block, at); },
          [&](ExprId at) { return Literal(block, static_cast<int>(at.value)); },
      },
      Choose(block));
  // In order: moving two blocks into a call reads neither and appends to
  // neither.
  Block first;
  Block second;
  const std::uint32_t joined = Join(std::move(first), std::move(second));
  return joined + direct.value + helped.lhs.value + sum + loaded.lhs.value +
         emitted.lhs.value + bound.value + braced.lhs.value + one.value +
         read.rhs.value + applied.value + deferred.rhs.value + looked +
         visited.value;
}

}  // namespace lyra::mir
