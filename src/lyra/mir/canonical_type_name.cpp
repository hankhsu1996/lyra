#include "lyra/mir/canonical_type_name.hpp"

#include <cstdint>
#include <format>
#include <string>
#include <string_view>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/base/overloaded.hpp"
#include "lyra/mir/class.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/support/class_name.hpp"
#include "lyra/support/def_path.hpp"

namespace lyra::mir {

namespace {

// `type` written onto `out`: a letter for its kind, then each fact that fixes
// it ended by `;`, a name preceded by its length, then the types it is made
// of, each written the same way, so where each part ends is told by the text
// itself and no two types write alike.
void Write(const CompilationUnit& unit, TypeId type, std::string& out) {
  const auto number = [&](std::uint64_t value) {
    out += std::format("{};", value);
  };
  const auto named = [&](std::string_view name) {
    out += std::format("{}:{}", name.size(), name);
  };
  const auto of = [&](TypeId part) { Write(unit, part, out); };
  const auto all = [&](const std::vector<TypeId>& parts) {
    number(parts.size());
    for (const TypeId part : parts) {
      of(part);
    }
  };
  const auto kind = [&](char letter) { out += letter; };
  const auto integral = [&](const IntegralType& t) {
    number(t.bit_width);
    number(std::to_underlying(t.signedness));
    number(std::to_underlying(t.state_kind));
  };
  unit.types.Get(type).Visit(
      Overloaded{
          [&](const IntegralType& t) {
            kind('I');
            integral(t);
          },
          // An enumeration type is its base and its members (LRM 6.19); two
          // with the same are one type here.
          [&](const EnumType& t) {
            kind('e');
            integral(t.base);
            number(t.members.size());
            for (const EnumMember& member : t.members) {
              named(member.name);
              number(member.value.value_words.size());
              for (const std::uint64_t word : member.value.value_words) {
                number(word);
              }
              number(member.value.state_words.size());
              for (const std::uint64_t word : member.value.state_words) {
                number(word);
              }
            }
          },
          [&](const UnpackedArrayType& t) {
            kind('A');
            number(t.count);
            of(t.element_type);
          },
          [&](const ErasedValueType&) { kind('R'); },
          [&](const MachineCStringType&) { kind('c'); },
          [&](const MachineBoolType&) { kind('b'); },
          [&](const MachineIntType& t) {
            kind('i');
            number(BitsOf(t.width));
            number(std::to_underlying(t.signedness));
          },
          [&](const MachineFloatType& t) {
            kind('f');
            number(std::to_underlying(t.width));
          },
          [&](const MachineArrayType& t) {
            kind('a');
            number(t.size);
            of(t.element);
          },
          [&](const MachineSpanType& t) {
            kind('z');
            of(t.element);
          },
          [&](const MachineFunctionType& t) {
            kind('F');
            all(t.params);
            of(t.result);
          },
          [&](const RealType& t) {
            kind('r');
            number(std::to_underlying(t.width));
          },
          [&](const VoidType&) { kind('v'); },
          // A class is the unit defining it and its name there.
          [&](const ObjectType& t) {
            kind('O');
            const Class& cls = unit.GetClass(t.of);
            named(DefiningUnitOf(unit, cls.defining_unit));
            std::visit(
                Overloaded{
                    [&](const support::DefPath& path) {
                      named(support::DefPathIdentity(path));
                    },
                    [&](const support::UnnamedClass& unnamed) {
                      out += 'u';
                      number(unnamed.disambiguator);
                    }},
                cls.identity);
          },
          [&](const LibraryType& t) {
            kind('l');
            number(std::to_underlying(t.decl));
            number(t.args.size());
            for (const LibraryType::Arg& arg : t.args) {
              std::visit(
                  Overloaded{
                      [&](TypeId part) { of(part); },
                      [&](const support::QueueBound& bound) {
                        std::visit(
                            Overloaded{
                                [&](const support::UnboundedQueue&) {
                                  out += "-;";
                                },
                                [&](const support::BoundedQueue& most) {
                                  number(most.count);
                                }},
                            bound);
                      }},
                  arg);
            }
          },
          [&](const CoroutineType& t) {
            kind('T');
            of(t.payload);
          },
          [&](const VariableRefType& t) {
            kind('V');
            number(std::to_underlying(t.mutability));
            of(t.pointee);
          },
          [&](const ReferenceWrapperType& t) {
            kind('W');
            number(std::to_underlying(t.mutability));
            of(t.pointee);
          },
          [&](const PointerType& t) {
            kind('P');
            number(std::to_underlying(t.ownership));
            number(std::to_underlying(t.mutability));
            of(t.pointee);
          },
          [&](const ReferenceType& t) {
            kind('L');
            number(std::to_underlying(t.mutability));
            of(t.referent);
          },
          [&](const ManagedRefType& t) {
            kind('H');
            of(t.pointee);
          },
          [&](const UnmanagedRefType& t) {
            kind('C');
            of(t.pointee);
          },
          [&](const VectorType& t) {
            kind('Z');
            of(t.element);
          },
          [&](const TupleType& t) {
            kind('U');
            all(t.elements);
          },
          [&](const ObservableType& t) {
            kind('o');
            of(t.value);
          },
          [&](const ResolvedType& t) {
            kind('n');
            of(t.value);
          },
          [&](const DriverType& t) {
            kind('j');
            of(t.value);
          },
          [&](const OpenWriteType& t) {
            kind('p');
            of(t.value);
          },
          [&](const DesignationType& t) {
            kind('q');
            of(t.value);
          },
          [&](const BitsReferenceType& t) {
            kind('B');
            of(t.bits);
          },
          [&](const ObjectWriteType& t) {
            kind('w');
            of(t.object);
          },
          // A record is the declaration it names: the unit defining it and
          // where in that unit it is declared.
          [&](const RecordType& t) {
            kind('N');
            const TypeDeclarationRef declared =
                RecordDeclarationOf(unit, t.declaration);
            named(declared.unit_name);
            named(support::DefPathIdentity(declared.path));
          },
          // A closure is known only to the unit that made it, by its place
          // there.
          [&](const ClosureType& t) {
            kind('K');
            named(unit.name);
            number(t.closure_id.value);
          },
          [&](const ErasedCallableType& t) {
            kind('E');
            number(std::to_underlying(t.kind));
            all(t.params);
            of(t.result);
          },
          // A net no net type resolves is its value type's (LRM 6.6.7).
          [&](const UnresolvedType& t) {
            kind('X');
            of(t.value);
          }});
}

}  // namespace

auto CanonicalTypeName(const CompilationUnit& unit, TypeId type)
    -> std::string {
  std::string out;
  Write(unit, type, out);
  return out;
}

}  // namespace lyra::mir
