#include "lyra/backend/cpp/render_type.hpp"

#include <cstdint>
#include <optional>
#include <span>
#include <string_view>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/naming.hpp"
#include "lyra/backend/cpp/target_text.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/diag/source_manager.hpp"
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/mir/type_declaration_ref.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/float_width.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/value/enumeration.hpp"
#include "lyra/value/integral_words.hpp"

namespace lyra::backend::cpp {

auto BodyCleanupExtentCppType() -> std::string_view {
  return "lyra::runtime::ScopeExit";
}

auto SuspensionCppType() -> std::string_view {
  return "lyra::runtime::Suspension";
}

auto OverlaidReadCppFunction() -> std::string_view {
  return "lyra::value::ReadOverlaid";
}

namespace {

// Whether a value of `type` is an object of the root, named as its own class
// or as a class of the library it extends: what is ended through its class's
// definition.
auto IsObjectOfTheRoot(const mir::TypePool& types, mir::TypeId type) -> bool {
  const mir::Type& t = types.Get(type);
  return t.Is<mir::ObjectType>() || t.RuntimeClassOf().has_value();
}

// The spelling the library's declaration of `decl` gives it.
auto LibraryCppType(support::LibraryDecl decl) -> std::string_view {
  return support::DeclarationOf(decl).cpp_spelling;
}

void WriteTypeList(
    TargetText& out, const mir::CompilationUnit& unit,
    std::span<const mir::TypeId> types) {
  WriteSeparated(out, types, ", ", [&](mir::TypeId type) {
    Write(out, CppType(unit, type));
  });
}

auto MachineIntCppType(const mir::MachineIntType& m) -> std::string_view {
  const bool is_signed = m.signedness == mir::Signedness::kSigned;
  switch (m.width) {
    case mir::MachineIntWidth::k8:
      return is_signed ? "std::int8_t" : "std::uint8_t";
    case mir::MachineIntWidth::k16:
      return is_signed ? "std::int16_t" : "std::uint16_t";
    case mir::MachineIntWidth::k32:
      return is_signed ? "std::int32_t" : "std::uint32_t";
    case mir::MachineIntWidth::k64:
      return is_signed ? "std::int64_t" : "std::uint64_t";
  }
  throw InternalError("backend::cpp: unknown MachineIntWidth");
}

// The library's family of vectors of one signedness and state domain.
auto IntegralFamilyName(
    mir::Signedness signedness, mir::IntegralStateKind state)
    -> std::string_view {
  switch (state) {
    case mir::IntegralStateKind::kTwoState:
      switch (signedness) {
        case mir::Signedness::kSigned:
          return "SignedBitVector";
        case mir::Signedness::kUnsigned:
          return "BitVector";
      }
      break;
    case mir::IntegralStateKind::kFourState:
      switch (signedness) {
        case mir::Signedness::kSigned:
          return "SignedLogicVector";
        case mir::Signedness::kUnsigned:
          return "LogicVector";
      }
      break;
  }
  throw InternalError("backend::cpp: unknown integral signedness or states");
}

// An integral type is the library's value of its width, signedness and state
// domain, spelled as the three facts the type states: the vector of its width
// in the family of its signedness and domain. A type the language predefines
// under a name (LRM 6.11) is the same type as the vector it abbreviates, so it
// has the same spelling.
void WriteIntegralCppType(TargetText& out, const mir::IntegralType& t) {
  Write(
      out, "lyra::value::", IntegralFamilyName(t.signedness, t.state_kind), "<",
      t.bit_width, ">");
}

auto MachineFloatCppType(const mir::MachineFloatType& m) -> std::string_view {
  switch (m.width) {
    case support::FloatWidth::k32:
      return "float";
    case support::FloatWidth::k64:
      return "double";
  }
  throw InternalError("backend::cpp: unknown FloatWidth");
}

auto RealCppType(const mir::RealType& r) -> std::string_view {
  switch (r.width) {
    case support::FloatWidth::k32:
      return "lyra::value::ShortReal";
    case support::FloatWidth::k64:
      return "lyra::value::Real";
  }
  throw InternalError("backend::cpp: unknown FloatWidth");
}

}  // namespace

void WriteOne(TargetText& out, const CppConstQualifier& qualifier) {
  switch (qualifier.of) {
    case mir::Mutability::kReadOnly:
      out += " const";
      return;
    case mir::Mutability::kMutable:
      return;
  }
  throw InternalError("backend::cpp: unknown Mutability");
}

void WriteOne(TargetText& out, const CppType& spelling) {
  const mir::CompilationUnit& unit = spelling.Unit();
  const mir::TypeId type_id = spelling.Type();
  const auto type = [&unit](mir::TypeId t) { return CppType(unit, t); };
  // A struct or a union is named by its declaration, wherever it is declared.
  const auto record = [&](mir::RecordId declaration) {
    const mir::TypeDeclarationRef ref =
        mir::RecordDeclarationOf(unit, declaration);
    Write(out, CppRecordQualifiedName(ref.unit_name, ref.path));
  };
  unit.types.Get(type_id).Visit(
      Overloaded{
          [&](const mir::IntegralType& t) { WriteIntegralCppType(out, t); },
          // An enum holds a value of its base type; its member names do not
          // change how the value is stored.
          [&](const mir::EnumType& e) { WriteIntegralCppType(out, e.base); },
          // A value of no known type is held only through a reference, which
          // is spelled whole where it is.
          [&](const mir::ErasedValueType&) {
            throw InternalError(
                "backend::cpp: a value of no known type is named apart from "
                "the reference holding it -- please report this as a bug");
          },
          [&](const mir::MachineCStringType&) { out += "const char*"; },
          [&](const mir::MachineBoolType&) { out += "bool"; },
          [&](const mir::MachineIntType& m) { out += MachineIntCppType(m); },
          [&](const mir::MachineFloatType& m) {
            out += MachineFloatCppType(m);
          },
          [&](const mir::MachineArrayType& m) {
            Write(out, "std::array<", type(m.element), ", ", m.size, ">");
          },
          [&](const mir::MachineSpanType& m) {
            Write(out, "lyra::value::ConstantSpan<", type(m.element), ">");
          },
          [&](const mir::MachineFunctionType& m) {
            // A pointer to a function type, spelled so that a declaration
            // names it as it names any other type. The erased function type
            // is the runtime's alias.
            if (mir::IsErasedFunction(unit.types, type_id)) {
              out += "lyra::abi::ErasedEntry";
              return;
            }
            Write(out, "std::add_pointer_t<", type(m.result), "(");
            WriteTypeList(out, unit, m.params);
            out += ")>";
          },
          [&](const mir::RealType& r) { out += RealCppType(r); },
          [&](const mir::UnpackedArrayType& ua) {
            Write(
                out, "lyra::value::UnpackedArray<", type(ua.element_type), ", ",
                ua.Size(), ">");
          },
          // No C++ class is every object implementing an interface,
          // so an object seen as one is the root every object is.
          [&](const mir::ObjectType& o) {
            if (unit.GetClass(o.of).is_interface) {
              out += LibraryCppType(support::LibraryDecl::kObjectRoot);
              return;
            }
            Write(out, CppClassRef(unit, o.of));
          },
          [&](const mir::RecordType& r) { record(r.declaration); },
          // The class its declaration names, written over its arguments
          // where they make it different classes. A queue's bound (LRM
          // 7.10.5) is a value of the class template.
          [&](const mir::LibraryType& l) {
            const support::LibraryDeclData& declaration =
                support::DeclarationOf(l.decl);
            out += declaration.cpp_spelling;
            if (!declaration.cpp_spelling_takes_args) {
              return;
            }
            out += "<";
            WriteSeparated(
                out, l.args, ", ", [&](const mir::LibraryType::Arg& arg) {
                  std::visit(
                      Overloaded{
                          [&](mir::TypeId t) { Write(out, type(t)); },
                          [&](const support::QueueBound& bound) {
                            WriteNumber(
                                out, support::LibraryBoundOf(bound), 10);
                          }},
                      arg);
                });
            out += ">";
          },
          [&](const mir::CoroutineType& c) {
            Write(out, "lyra::runtime::Coroutine<", type(c.payload), ">");
          },
          [&](const mir::VariableRefType& r) {
            // A reference to a value of no known type is where that value is
            // received and what carries it on, as Rust's `&mut dyn Any` is a
            // pointer and its metadata.
            if (unit.types.Get(r.pointee).Is<mir::ErasedValueType>()) {
              out += "lyra::runtime::AnyReference";
              return;
            }
            Write(
                out, "lyra::runtime::VariableRef<", type(r.pointee), ">",
                CppConstQualifier{r.mutability});
          },
          // The runtime's reference is re-seatable, so it holds a reference
          // bound after its object is built as well. Binding writes the
          // wrapper itself, so it is never a const object whatever it may
          // write through.
          [&](const mir::ReferenceWrapperType& r) {
            Write(out, "lyra::runtime::VariableRef<", type(r.pointee), ">");
          },
          [&](const mir::VoidType&) { out += "void"; },
          // Whether the holder may write through it qualifies the pointee,
          // whoever owns it.
          [&](const mir::PointerType& p) {
            const CppConstQualifier pointee_qualifier{p.mutability};
            switch (p.ownership) {
              // A box of a closure of no known type is the library's, which
              // calls the closure at the signature the type states and ends
              // it, both through the definition it opens with.
              case mir::PointerOwnership::kUnique:
                if (unit.types.Get(p.pointee).Is<mir::ErasedCallableType>()) {
                  Write(
                      out, "lyra::runtime::OwnedClosure<",
                      CppCallSignature(unit, p.pointee), ">");
                  return;
                }
                // An object one owner holds alone is ended through its
                // class's definition, which knows the class it was created
                // as.
                if (IsObjectOfTheRoot(unit.types, p.pointee)) {
                  Write(
                      out, "lyra::runtime::OwnedObject<", type(p.pointee),
                      pointee_qualifier, ">");
                  return;
                }
                Write(
                    out, "std::unique_ptr<", type(p.pointee), pointee_qualifier,
                    ">");
                return;
              case mir::PointerOwnership::kShared:
                Write(
                    out, "std::shared_ptr<", type(p.pointee), pointee_qualifier,
                    ">");
                return;
              // A borrowed pointer is a plain pointer to the pointee's type, a
              // `Var<T>` included.
              case mir::PointerOwnership::kBorrowed:
                Write(out, type(p.pointee), pointee_qualifier, "*");
                return;
            }
            throw InternalError("backend::cpp: unknown PointerOwnership");
          },
          [&](const mir::ReferenceType& r) {
            // A borrow of a closure of no known type is where the closure lies
            // and how to call it, as Rust's `&dyn Fn` is, at the signature the
            // type states.
            if (unit.types.Get(r.referent).Is<mir::ErasedCallableType>()) {
              Write(
                  out, "lyra::base::FunctionRef<",
                  CppCallSignature(unit, r.referent), ">");
              return;
            }
            Write(out, type(r.referent), CppConstQualifier{r.mutability}, "&");
          },
          // Every object reference is one type, whatever class it is seen as.
          // The class is written where the reference is used, because two
          // units may hold one cell as different classes, and a type per
          // class would give that cell two types.
          [&](const mir::ManagedRefType&) { out += "lyra::value::ObjectRef"; },
          // One type too, for the same reason.
          [&](const mir::UnmanagedRefType&) {
            out += "lyra::value::UnmanagedRef";
          },
          [&](const mir::VectorType& v) {
            Write(out, "std::vector<", type(v.element), ">");
          },
          // A tuple is its components, so it is the library's product of them.
          [&](const mir::TupleType& t) {
            Write(out, CppTupleComponents{.unit = &unit, .of = t.elements});
          },
          [&](const mir::ObservableType& o) {
            Write(out, "lyra::runtime::Var<", type(o.value), ">");
          },
          [&](const mir::ResolvedType& r) {
            Write(out, "lyra::runtime::ResolvedNet<", type(r.value), ">");
          },
          [&](const mir::DriverType& d) {
            Write(out, "lyra::runtime::Driver<", type(d.value), ">");
          },
          // A write, and a place designated within it, live and end within
          // the expression that opens the write, so nothing ever holds one
          // under a name.
          [](const mir::OpenWriteType&) {
            throw InternalError(
                "backend::cpp: a write in progress ends with the expression "
                "that opens it, so nothing names its type -- please report "
                "this as a bug");
          },
          [](const mir::DesignationType&) {
            throw InternalError(
                "backend::cpp: a place designated within a write ends with "
                "the expression that opens the write, so nothing names its "
                "type -- please report this as a bug");
          },
          [](const mir::BitsReferenceType&) {
            throw InternalError(
                "backend::cpp: a reference to bits is written through where "
                "it is taken, so nothing names its type -- please report this "
                "as a bug");
          },
          // Named where it is opened, by constructing it on the object.
          [&](const mir::ObjectWriteType& w) {
            Write(out, "lyra::runtime::ObjectWrite<", type(w.object), ">");
          },
          [&](const mir::ClosureType& c) {
            Write(out, CppClosureName(c.closure_id));
          },
          // A closure of no known type is held only through a box or a borrow,
          // each of which is spelled whole above.
          [&](const mir::ErasedCallableType&) {
            throw InternalError(
                "backend::cpp: a closure of no known type is named apart from "
                "the box or the borrow holding it -- please report this as a "
                "bug");
          },
      });
}

namespace {

void WritePlane(TargetText& out, std::span<const std::uint64_t> words) {
  out += "{";
  WriteSeparated(out, words, ", ", [&](std::uint64_t word) {
    Write(
        out, CppMachineIntLiteral{
                 .value = static_cast<std::int64_t>(word),
                 .signedness = mir::Signedness::kUnsigned});
  });
  out += "}";
}

}  // namespace

void WriteOne(TargetText& out, const CppMachineIntLiteral& literal) {
  switch (literal.signedness) {
    case mir::Signedness::kUnsigned:
      out += "0x";
      WriteNumber(out, static_cast<std::uint64_t>(literal.value), 16);
      out += "ULL";
      return;
    case mir::Signedness::kSigned:
      WriteInt64Constant(out, literal.value);
      return;
  }
  throw InternalError("backend::cpp: unknown signedness");
}

auto PrecedenceOf(const CppMachineIntLiteral& literal) -> Precedence {
  switch (literal.signedness) {
    case mir::Signedness::kUnsigned:
      return Precedence::kPostfix;
    case mir::Signedness::kSigned:
      return literal.value < 0 ? Precedence::kPrefix : Precedence::kPostfix;
  }
  throw InternalError("backend::cpp: unknown signedness");
}

void WriteOne(TargetText& out, const CppIntegralLiteral& literal) {
  Write(out, literal.type, "::FromWords(");
  WritePlane(out, literal.value_words);
  out += ", ";
  WritePlane(out, literal.state_words);
  out += ")";
}

auto DerefSpellingAsCpp(const mir::CompilationUnit& unit, mir::TypeId type_id)
    -> DerefSpelling {
  const auto reaches_nothing = []() -> DerefSpelling {
    throw InternalError(
        "backend::cpp: a value of this type reaches nothing, so there is "
        "nothing for a dereference to open -- please report this as a bug");
  };
  return unit.types.Get(type_id).Visit(
      Overloaded{
          [&](const mir::PointerType&) -> DerefSpelling {
            return DerefByOperator{};
          },
          [&](const mir::ReferenceType&) -> DerefSpelling {
            return DerefImplicit{};
          },
          // A `ref` is dereferenced like a pointer, which reaches the cell it
          // is bound to (LRM 23.3.3.2).
          [&](const mir::VariableRefType&) -> DerefSpelling {
            return DerefByOperator{};
          },
          [&](const mir::ReferenceWrapperType&) -> DerefSpelling {
            return DerefByOperator{};
          },
          // A place designated within a write is dereferenced like a pointer,
          // which reaches it, where the write lands. The write itself names no
          // place.
          [&](const mir::DesignationType&) -> DerefSpelling {
            return DerefByOperator{};
          },
          [&](const mir::OpenWriteType&) { return reaches_nothing(); },
          [&](const mir::BitsReferenceType&) { return reaches_nothing(); },
          // A write in progress into an object is dereferenced to the object,
          // as a guard is to what it guards.
          [&](const mir::ObjectWriteType&) -> DerefSpelling {
            return DerefByOperator{};
          },
          [](const mir::ManagedRefType&) -> DerefSpelling {
            throw InternalError(
                "backend::cpp: a class handle is opened by the library's call, "
                "so no dereference reaches one -- please report this as a bug");
          },
          [&](const mir::UnmanagedRefType& u) -> DerefSpelling {
            return DerefAsClass{.pointee = u.pointee};
          },
          // Every other type is a value that reaches nothing, so there is
          // nothing to dereference.
          [&](const mir::IntegralType&) { return reaches_nothing(); },
          [&](const mir::EnumType&) { return reaches_nothing(); },
          [&](const mir::UnpackedArrayType&) { return reaches_nothing(); },
          [&](const mir::ErasedValueType&) { return reaches_nothing(); },
          [&](const mir::MachineCStringType&) { return reaches_nothing(); },
          [&](const mir::MachineBoolType&) { return reaches_nothing(); },
          [&](const mir::MachineIntType&) { return reaches_nothing(); },
          [&](const mir::MachineFloatType&) { return reaches_nothing(); },
          [&](const mir::MachineArrayType&) { return reaches_nothing(); },
          [&](const mir::MachineSpanType&) { return reaches_nothing(); },
          [&](const mir::MachineFunctionType&) { return reaches_nothing(); },
          [&](const mir::RealType&) { return reaches_nothing(); },
          [&](const mir::VoidType&) { return reaches_nothing(); },
          [&](const mir::ObjectType&) { return reaches_nothing(); },
          [&](const mir::LibraryType&) { return reaches_nothing(); },
          [&](const mir::CoroutineType&) { return reaches_nothing(); },
          [&](const mir::VectorType&) { return reaches_nothing(); },
          [&](const mir::TupleType&) { return reaches_nothing(); },
          [&](const mir::ObservableType&) { return reaches_nothing(); },
          [&](const mir::ResolvedType&) { return reaches_nothing(); },
          [&](const mir::DriverType&) { return reaches_nothing(); },
          [&](const mir::RecordType&) { return reaches_nothing(); },
          [&](const mir::ClosureType&) { return reaches_nothing(); },
          [&](const mir::ErasedCallableType&) { return reaches_nothing(); },
      });
}

auto ReceiverSpellingAsCpp(
    const mir::CompilationUnit& unit, mir::TypeId receiver_type)
    -> ReceiverSpelling {
  const auto holds_no_object = []() -> ReceiverSpelling {
    throw InternalError(
        "backend::cpp: a body's receiver is of a type that holds no object "
        "for the body to run against -- please report this as a bug");
  };
  return unit.types.Get(receiver_type)
      .Visit(
          Overloaded{
              [&](const mir::PointerType& p) -> ReceiverSpelling {
                switch (p.ownership) {
                  case mir::PointerOwnership::kBorrowed:
                    return ReceiverFromThis{
                        .object = p.mutability, .bound_from = "this"};
                  case mir::PointerOwnership::kUnique:
                    return ReceiverAsParameter{};
                  // A body shares its object with no one else, so nothing
                  // hands one over shared.
                  case mir::PointerOwnership::kShared:
                    return holds_no_object();
                }
                throw InternalError(
                    "ReceiverSpellingAsCpp: unknown PointerOwnership");
              },
              [&](const mir::ReferenceType& r) -> ReceiverSpelling {
                return ReceiverFromThis{
                    .object = r.mutability, .bound_from = "*this"};
              },
              [&](const mir::VariableRefType&) { return holds_no_object(); },
              [&](const mir::ReferenceWrapperType&) {
                return holds_no_object();
              },
              [&](const mir::DesignationType&) { return holds_no_object(); },
              [&](const mir::OpenWriteType&) { return holds_no_object(); },
              [&](const mir::BitsReferenceType&) { return holds_no_object(); },
              [&](const mir::ObjectWriteType&) { return holds_no_object(); },
              [&](const mir::ManagedRefType&) { return holds_no_object(); },
              [&](const mir::UnmanagedRefType&) { return holds_no_object(); },
              [&](const mir::IntegralType&) { return holds_no_object(); },
              [&](const mir::EnumType&) { return holds_no_object(); },
              [&](const mir::UnpackedArrayType&) { return holds_no_object(); },
              [&](const mir::ErasedValueType&) { return holds_no_object(); },
              [&](const mir::MachineCStringType&) { return holds_no_object(); },
              [&](const mir::MachineBoolType&) { return holds_no_object(); },
              [&](const mir::MachineIntType&) { return holds_no_object(); },
              [&](const mir::MachineFloatType&) { return holds_no_object(); },
              [&](const mir::MachineArrayType&) { return holds_no_object(); },
              [&](const mir::MachineSpanType&) { return holds_no_object(); },
              [&](const mir::MachineFunctionType&) {
                return holds_no_object();
              },
              [&](const mir::RealType&) { return holds_no_object(); },
              [&](const mir::VoidType&) { return holds_no_object(); },
              [&](const mir::ObjectType&) { return holds_no_object(); },
              [&](const mir::LibraryType&) { return holds_no_object(); },
              [&](const mir::CoroutineType&) { return holds_no_object(); },
              [&](const mir::VectorType&) { return holds_no_object(); },
              [&](const mir::TupleType&) { return holds_no_object(); },
              [&](const mir::ObservableType&) { return holds_no_object(); },
              [&](const mir::ResolvedType&) { return holds_no_object(); },
              [&](const mir::DriverType&) { return holds_no_object(); },
              [&](const mir::RecordType&) { return holds_no_object(); },
              [&](const mir::ClosureType&) { return holds_no_object(); },
              [&](const mir::ErasedCallableType&) { return holds_no_object(); },
          });
}

auto NullSpellingAsCpp(const mir::CompilationUnit& unit, mir::TypeId type)
    -> diag::Result<NullSpelling> {
  const mir::Type& t = unit.types.Get(type);
  if (t.Is<mir::ManagedRefType>() || t.Is<mir::UnmanagedRefType>()) {
    return NullAsEmptyValue{.type = type};
  }
  if (t.Is<mir::PointerType>() || t.Is<mir::MachineFunctionType>()) {
    return NullAsNullAddress{};
  }
  return diag::Fail(
      diag::DiagCode::kUnsupportedExpressionForm,
      "a null value of type '{}' is not yet supported with '--backend cpp'",
      TextOf(CppType(unit, type)));
}

void WriteNull(
    TargetText& out, const mir::CompilationUnit& unit,
    const NullSpelling& spelling) {
  std::visit(
      Overloaded{
          [&](const NullAsEmptyValue& empty) {
            Write(out, CppType(unit, empty.type), "{}");
          },
          [&](NullAsNullAddress) { out += "nullptr"; }},
      spelling);
}

void WriteOne(TargetText& out, const CppUnionMemberTag& tag) {
  Write(out, "std::in_place_index<", tag.member.value, ">");
}

void WriteOne(TargetText& out, const CppConstructorName& constructor) {
  const mir::CompilationUnit& unit = constructor.of.Unit();
  const mir::TypeId type_id = constructor.of.Type();
  // Most types are constructed by naming them, `T(args)`, with the name the
  // type mapping gives.
  const auto by_naming_itself = [&](const auto&) {
    Write(out, constructor.of);
  };
  unit.types.Get(type_id).Visit(
      Overloaded{
          // A shared pointer is made by the function that allocates and
          // constructs what it holds, `std::make_shared<T>(args)`. Storage a
          // unique pointer or a class handle owns is allocated by a
          // new-expression, and a borrowed pointer points at storage that
          // already exists, so neither is constructed.
          [&](const mir::PointerType& p) {
            switch (p.ownership) {
              case mir::PointerOwnership::kShared:
                Write(out, "std::make_shared<", CppType(unit, p.pointee), ">");
                return;
              case mir::PointerOwnership::kUnique:
                throw InternalError(
                    "backend::cpp: what a unique pointer owns is allocated by "
                    "a new-expression, so none is constructed");
              case mir::PointerOwnership::kBorrowed:
                throw InternalError(
                    "backend::cpp: a borrowed pointer is bound to storage "
                    "that already exists, so none is constructed");
            }
            throw InternalError("backend::cpp: unknown PointerOwnership");
          },
          [](const mir::ManagedRefType&) {
            throw InternalError(
                "backend::cpp: the object a class handle owns is allocated by "
                "a new-expression, so no handle is constructed");
          },
          [](const mir::ReferenceType&) {
            throw InternalError(
                "backend::cpp: a reference is bound to what already exists, so "
                "none is constructed");
          },
          // A sequence is built by the library function that takes its
          // elements, since the container type takes no element list.
          [&](const mir::VectorType& v) {
            Write(
                out, "lyra::runtime::MakeSequence<", CppType(unit, v.element),
                ">");
          },
          [&](const mir::IntegralType& t) { by_naming_itself(t); },
          [&](const mir::EnumType& t) { by_naming_itself(t); },
          [&](const mir::MachineCStringType& t) { by_naming_itself(t); },
          [&](const mir::MachineBoolType& t) { by_naming_itself(t); },
          [&](const mir::MachineIntType& t) { by_naming_itself(t); },
          [&](const mir::MachineFloatType& t) { by_naming_itself(t); },
          [&](const mir::MachineArrayType& t) { by_naming_itself(t); },
          [&](const mir::MachineSpanType& t) { by_naming_itself(t); },
          [&](const mir::MachineFunctionType& t) { by_naming_itself(t); },
          [&](const mir::UnmanagedRefType& t) { by_naming_itself(t); },
          [&](const mir::RealType& t) { by_naming_itself(t); },
          [&](const mir::UnpackedArrayType& t) { by_naming_itself(t); },
          [](const mir::ErasedValueType&) {
            throw InternalError(
                "backend::cpp: a value of no known type is held only through a "
                "reference, so none is constructed");
          },
          [&](const mir::ObjectType& t) { by_naming_itself(t); },
          [&](const mir::RecordType& t) { by_naming_itself(t); },
          [&](const mir::LibraryType& t) { by_naming_itself(t); },
          [&](const mir::CoroutineType& t) { by_naming_itself(t); },
          [&](const mir::VariableRefType& t) { by_naming_itself(t); },
          [&](const mir::ReferenceWrapperType& t) { by_naming_itself(t); },
          [&](const mir::VoidType& t) { by_naming_itself(t); },
          [&](const mir::TupleType& t) { by_naming_itself(t); },
          [&](const mir::ObservableType& t) { by_naming_itself(t); },
          [&](const mir::ResolvedType& t) { by_naming_itself(t); },
          [&](const mir::DriverType& t) { by_naming_itself(t); },
          [&](const mir::OpenWriteType& t) { by_naming_itself(t); },
          [&](const mir::DesignationType& t) { by_naming_itself(t); },
          [&](const mir::BitsReferenceType& t) { by_naming_itself(t); },
          [&](const mir::ObjectWriteType& t) { by_naming_itself(t); },
          [&](const mir::ClosureType& t) { by_naming_itself(t); },
          [](const mir::ErasedCallableType&) {
            throw InternalError(
                "backend::cpp: a closure of no known type is made by a cast "
                "of the box or the borrow holding it, so none is constructed");
          },
      });
}

void WriteOne(TargetText& out, const CppNewOwner& owner) {
  const mir::Type& type = owner.of.Unit().types.Get(owner.of.Type());
  if (const auto* pointer = type.As<mir::PointerType>()) {
    switch (pointer->ownership) {
      case mir::PointerOwnership::kUnique:
        Write(out, owner.of);
        return;
      case mir::PointerOwnership::kShared:
      case mir::PointerOwnership::kBorrowed:
        break;
    }
  }
  throw InternalError(
      "backend::cpp: storage a new-expression allocates for a value is owned "
      "by a unique pointer -- please report this as a bug");
}

namespace {

// The class an object of the type `built` is of.
auto ClassOfObject(const CppType& built) -> CppClassRef {
  const auto* object =
      built.Unit().types.Get(built.Type()).As<mir::ObjectType>();
  if (object == nullptr) {
    throw InternalError(
        "backend::cpp: a creation builds a value that is no object -- please "
        "report this as a bug");
  }
  return {built.Unit(), object->of};
}

// What text naming the class `id` needs to have the class declared. A class
// some other unit may name is declared forward by the text naming it, class and
// definition both, as the unit declaring it spells them; one nested in another
// is declared only inside that one, so the text includes the file defining that
// one; one no other unit names is declared in the unit's code file, the one
// file that names it.
void DeclareClass(
    TargetText& out, const mir::CompilationUnit& unit, mir::ClassId id) {
  const mir::Class& cls = unit.GetClass(id);
  const support::DefPath* path = support::PathOf(cls.identity);
  if (path == nullptr) return;
  if (IsNestedClass(*path)) {
    out.Require(ClassDefinitionFileOf(
        mir::DefiningUnitOf(unit, cls.defining_unit), *path));
    return;
  }
  out.DeclareForward(CppForwardDeclarationOf(unit, id));
}

}  // namespace

void WriteOne(TargetText& out, const CppUnconstructedObject& creation) {
  Write(
      out, "lyra::runtime::CreateUnconstructedObject<", creation.built, ">(&",
      CppDefinitionOf{.of = ClassOfObject(creation.built)}, ")");
}

void WriteOne(TargetText& out, const CppObjectCreation& creation) {
  const mir::Type& type = creation.of.Unit().types.Get(creation.of.Type());
  const auto head = [&](std::string_view function) {
    Write(
        out, function, "<", creation.built, ">(&",
        CppDefinitionOf{.of = ClassOfObject(creation.built)}, ", &",
        creation.built, "::", CppConstructorMemberName());
  };
  if (type.Is<mir::ManagedRefType>()) {
    head("lyra::runtime::CreateObject");
    return;
  }
  if (const auto* pointer = type.As<mir::PointerType>()) {
    switch (pointer->ownership) {
      case mir::PointerOwnership::kUnique:
        head("lyra::runtime::CreateOwned");
        return;
      case mir::PointerOwnership::kShared:
      case mir::PointerOwnership::kBorrowed:
        break;
    }
  }
  throw InternalError(
      "backend::cpp: an object a new-expression creates is owned by a unique "
      "pointer or a class handle -- please report this as a bug");
}

void WriteOne(TargetText& out, const CppObjectEnd& end) {
  Write(out, "lyra::runtime::EndObjectOf<", end.of, ">");
}

void WriteOne(TargetText& out, const CppDefinitionOf& definition) {
  const mir::CompilationUnit& unit = definition.of.Unit();
  const auto no_definition = [] {
    throw InternalError(
        "backend::cpp: a definition of a root every object or scope extends is "
        "named, which no unit declares -- please report this as a bug");
  };
  std::visit(
      Overloaded{
          [&](const mir::ClassId& id) {
            DeclareClass(out, unit, id);
            Write(out, DefinitionPath{.unit = &unit, .id = id});
          },
          [&](const mir::ObjectTreeRootRef&) { no_definition(); },
          [&](const mir::ClassObjectRootRef&) { no_definition(); },
          [&](const mir::ObjectRootRef&) { no_definition(); }},
      definition.of.Ref());
}

void WriteOne(TargetText& out, const CppEntryPointerType& type) {
  Write(
      out, "std::add_pointer_t<", CppType(*type.unit, type.code->result_type),
      "(", LibraryCppType(support::LibraryDecl::kObjectRoot), "*");
  for (const mir::LocalId param : type.code->ParamsAfterReceiver()) {
    Write(out, ", ", CppType(*type.unit, type.code->locals.Get(param).type));
  }
  out += ")>";
}

namespace {

// The table type the introducer of `behavior` declares, and the member of it
// for the behavior, `B::sv_table, &B::sv_table::f`.
void WriteTableAndMember(
    TargetText& out, const mir::CompilationUnit& unit,
    const mir::VirtualSlot& behavior) {
  const CppDispatchTableOf table{
      .of = CppClassRef(unit, mir::ClassRef{behavior.owner_class})};
  Write(
      out, table, ", &", table, "::",
      CppTableMemberName(
          unit, unit.GetClass(behavior.owner_class), behavior.slot));
}

}  // namespace

void WriteOne(TargetText& out, const CppDispatchCallee& callee) {
  out += "lyra::runtime::Dispatch<";
  WriteTableAndMember(out, *callee.unit, callee.behavior);
  out += ">";
}

void WriteOne(TargetText& out, const CppInterfaceDispatchCallee& callee) {
  out += "lyra::runtime::InterfaceDispatch<";
  WriteTableAndMember(out, *callee.unit, callee.requirement);
  Write(
      out, ", &",
      CppDefinitionOf{
          .of = CppClassRef(
              *callee.unit, mir::ClassRef{callee.requirement.owner_class})},
      ">");
}

void WriteOne(TargetText& out, const CppTupleComponents& components) {
  out += "lyra::value::Tuple<";
  WriteTypeList(out, *components.unit, components.of);
  out += ">";
}

void WriteOne(TargetText& out, const CppRecordBase& base) {
  const auto overlay = [&](std::string_view library_type) {
    out += library_type;
    WriteTypeList(out, *base.unit, base.of->elements);
    out += ">";
  };
  switch (base.of->kind) {
    case mir::RecordKind::kStruct:
      Write(
          out, CppTupleComponents{.unit = base.unit, .of = base.of->elements});
      return;
    case mir::RecordKind::kUnion:
      overlay("lyra::value::Union<");
      return;
    case mir::RecordKind::kTaggedUnion:
      overlay("lyra::value::TaggedUnion<");
      return;
  }
  throw InternalError("backend::cpp: unknown record kind");
}

auto CppClosureBase() -> std::string_view {
  return "lyra::runtime::ClosureValue";
}

auto CppClosureDefinitionOf() -> std::string_view {
  return "lyra::runtime::DefinitionOf";
}

void WriteOne(TargetText& out, const CppCallSignature& signature) {
  const mir::CompilationUnit& unit = signature.Unit();
  const auto& erased =
      unit.types.Get(signature.Erased()).Get<mir::ErasedCallableType>();
  Write(out, CppType(unit, erased.result), "(");
  WriteTypeList(out, unit, erased.params);
  out += ")";
}

void WriteOne(TargetText& out, const CppClassRef& ref) {
  const mir::CompilationUnit& unit = ref.Unit();
  std::visit(
      Overloaded{
          [&](const mir::ClassId& id) {
            DeclareClass(out, unit, id);
            Write(out, CppClassPath(unit, id));
          },
          // The tree's root is the library's scope class, spelled as a value
          // of that class's type is.
          [&](const mir::ObjectTreeRootRef&) {
            out += LibraryCppType(support::LibraryDecl::kScope);
          },
          [&](const mir::ClassObjectRootRef&) {
            out += LibraryCppType(support::LibraryDecl::kClassObject);
          },
          [&](const mir::ObjectRootRef&) {
            out += LibraryCppType(support::LibraryDecl::kObjectRoot);
          }},
      ref.Ref());
}

void WriteOne(TargetText& out, const CppBaseClass& base) {
  const mir::CompilationUnit& unit = base.of.Unit();
  std::visit(
      Overloaded{
          // A class with a path is defined in a file of the unit defining it,
          // which the text naming it as a base includes; one no other unit
          // names is defined in the unit's code file, ahead of the classes
          // deriving from it.
          [&](const mir::ClassId& id) {
            const mir::Class& cls = unit.GetClass(id);
            if (const support::DefPath* path = support::PathOf(cls.identity)) {
              out.Require(ClassDefinitionFileOf(
                  cls.defining_unit.value_or(unit.name), *path));
            }
          },
          [](const mir::ObjectTreeRootRef&) {},
          [](const mir::ClassObjectRootRef&) {},
          [](const mir::ObjectRootRef&) {}},
      base.of.Ref());
  Write(out, base.of);
}

void WriteOne(TargetText& out, const CppDispatchTableOf& table) {
  Write(out, CppBaseClass{.of = table.of}, "::", CppDispatchTableTypeName());
}

}  // namespace lyra::backend::cpp
