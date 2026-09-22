#include "lyra/mir/integral_constant_materialization.hpp"

#include <algorithm>
#include <cstdint>
#include <utility>
#include <vector>

#include "lyra/mir/expr.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/runtime_record.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_descriptor.hpp"
#include "lyra/support/builtin_fn.hpp"

namespace lyra::mir {

namespace {

// The constant's numeric value in one machine integer, sign-extended from the
// width its type declares. Only reached where that width fits the carrier and
// no bit is X or Z, so nothing is lost. An integral type is at least one bit
// wide and an entry's planes are sized from that width, so the low word is
// always there to read.
auto CarrierValue(const IntegralConstant& value, const PackedArrayType& shape)
    -> std::int64_t {
  const std::uint64_t width = shape.BitWidth();
  const std::uint64_t bits = value.value_words.front();
  if (shape.signedness == Signedness::kSigned && width < 64U) {
    const std::uint64_t sign_bit = std::uint64_t{1} << (width - 1U);
    if ((bits & sign_bit) != 0U) {
      const std::uint64_t mask = (std::uint64_t{1} << width) - 1U;
      return static_cast<std::int64_t>(bits | ~mask);
    }
  }
  return static_cast<std::int64_t>(bits);
}

// One plane of a constant's bits, as the run of machine words a factory reads
// it out of. An entry's planes are already the length its type calls for, so
// this hands them on as they stand -- including the empty run a two-state
// value's unknown plane is, which the factory refuses to receive as anything
// else.
auto WordPlane(
    RuntimeRecordBuilder& record, TypeId word,
    const std::vector<std::uint64_t>& words) -> ExprId {
  std::vector<ExprId> elements;
  elements.reserve(words.size());
  for (const std::uint64_t bits : words) {
    elements.push_back(record.Add(
        Expr{
            .data = MachineIntLiteral{.value = static_cast<std::int64_t>(bits)},
            .type = word}));
  }
  return record.MachineArray(word, std::move(elements));
}

auto FactoryCall(
    RuntimeRecordBuilder& record, TypeId type, support::BuiltinFn factory,
    std::vector<ExprId> arguments) -> ExprId {
  return record.Add(
      Expr{
          .data =
              CallExpr{
                  .callee = Direct{.target = factory},
                  .arguments = std::move(arguments)},
          .type = type});
}

}  // namespace

auto MaterializeIntegralConstant(
    const CompilationUnit& unit, IntegralConstantId constant) -> ValueBuild {
  // Both arrive by value: building the expression interns the types of the
  // word runs it names, and a reference into a pool does not survive its
  // growth.
  const IntegralConstantDecl decl = unit.integral_constants.Get(constant);
  const PackedArrayType shape = unit.types.Get(decl.type).PackedShape();

  ValueBuild built;
  RuntimeRecordBuilder record(unit, built.body.exprs);
  const ExprId packed_type =
      BuildTypeDescriptorRef(unit, built.body, decl.type);

  // A value narrow enough for one machine integer and carrying no unknown bit
  // is built from that integer; every other one is built from its planes. They
  // are two library entries rather than one told apart by how many operands
  // arrived, so which one is named here is the whole of the choice.
  const bool has_unknown = std::ranges::any_of(
      decl.value.state_words, [](std::uint64_t word) { return word != 0U; });
  if (shape.BitWidth() <= 64U && !has_unknown) {
    const ExprId carrier = record.MachineInt(CarrierValue(decl.value, shape));
    built.value = FactoryCall(
        record, decl.type, support::BuiltinFn::kFromInt,
        {carrier, packed_type});
  } else {
    const TypeId word = unit.builtins.machine_word;
    built.value = FactoryCall(
        record, decl.type, support::BuiltinFn::kFromWords,
        {WordPlane(record, word, decl.value.value_words),
         WordPlane(record, word, decl.value.state_words), packed_type});
  }
  return built;
}

}  // namespace lyra::mir
