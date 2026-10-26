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
#include "lyra/mir/class_id.hpp"
#include "lyra/mir/class_ref.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/integral_constant.hpp"
#include "lyra/mir/type.hpp"
#include "lyra/mir/type_builders.hpp"
#include "lyra/support/def_path.hpp"
#include "lyra/support/runtime_class.hpp"
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

auto RuntimeClassCppType(support::RuntimeClass klass) -> std::string_view {
  switch (klass) {
    case support::RuntimeClass::kScope:
      return "lyra::runtime::Scope";
    case support::RuntimeClass::kObject:
      return "lyra::runtime::GcObject";
    case support::RuntimeClass::kClassObject:
      return "lyra::runtime::ClassObject";
    case support::RuntimeClass::kProcess:
      return "lyra::runtime::RuntimeProcess";
    case support::RuntimeClass::kSynchronizationObject:
      return "lyra::runtime::SynchronizationObject";
    case support::RuntimeClass::kConstraintProblem:
      return "lyra::runtime::ConstraintProblem";
  }
  throw InternalError("backend::cpp: unknown runtime class");
}

void WriteTypeList(
    TargetText& out, const mir::CompilationUnit& unit,
    std::span<const mir::TypeId> types) {
  WriteSeparated(out, types, ", ", [&](mir::TypeId type) {
    Write(out, CppType(unit, type));
  });
}

auto RuntimeLibraryCppType(mir::RuntimeLibraryKind kind) -> std::string_view {
  switch (kind) {
    case mir::RuntimeLibraryKind::kEnumeration:
      return "lyra::value::EnumerationMembers";
    case mir::RuntimeLibraryKind::kPrintItem:
      return "lyra::value::PrintItem";
    case mir::RuntimeLibraryKind::kPrintLiteralItem:
      return "lyra::value::PrintLiteralItem";
    case mir::RuntimeLibraryKind::kPrintValueItem:
      return "lyra::value::PrintValueItem";
    case mir::RuntimeLibraryKind::kCancellationTarget:
      return "lyra::runtime::CancellationTarget";
    case mir::RuntimeLibraryKind::kTimer:
      return "lyra::runtime::Timer";
    case mir::RuntimeLibraryKind::kDeferredEffects:
      return "lyra::runtime::DeferredEffects";
    case mir::RuntimeLibraryKind::kViolationReportKeeper:
      return "lyra::runtime::ViolationReportKeeper";
    case mir::RuntimeLibraryKind::kControlEffect:
      return "lyra::runtime::ControlEffect";
    case mir::RuntimeLibraryKind::kFormatSpec:
      return "lyra::value::FormatSpec";
    case mir::RuntimeLibraryKind::kFormatArg:
      return "lyra::value::FormatArg";
    case mir::RuntimeLibraryKind::kChannelCancellation:
      return "lyra::runtime::ChannelCancellation";
    case mir::RuntimeLibraryKind::kTimeFormat:
      return "lyra::value::TimeFormat";
    case mir::RuntimeLibraryKind::kHierarchySegment:
      return "lyra::runtime::HierarchySegment";
    case mir::RuntimeLibraryKind::kTrigger:
      return "lyra::runtime::Trigger";
    case mir::RuntimeLibraryKind::kObservation:
      return "lyra::runtime::Observation";
    case mir::RuntimeLibraryKind::kReadReport:
      return "lyra::runtime::ReadReport";
    case mir::RuntimeLibraryKind::kWait:
      return "lyra::runtime::Wait";
    case mir::RuntimeLibraryKind::kLeasedReference:
      return "lyra::runtime::LeasedReference";
    case mir::RuntimeLibraryKind::kObjectDefinition:
      return "lyra::runtime::ObjectDefinition";
    case mir::RuntimeLibraryKind::kScopeInfo:
      return "lyra::runtime::ScopeInfo";
    case mir::RuntimeLibraryKind::kScopeCallable:
      return "lyra::runtime::ScopeCallable";
    case mir::RuntimeLibraryKind::kConstructionTable:
      return "lyra::runtime::ConstructionTable";
    case mir::RuntimeLibraryKind::kInstantiation:
      return "lyra::runtime::Instantiation";
    case mir::RuntimeLibraryKind::kGeneratePlaces:
      return "lyra::runtime::GeneratePlaces";
    case mir::RuntimeLibraryKind::kDumpEntry:
      return "lyra::runtime::DumpEntry";
    case mir::RuntimeLibraryKind::kDumpBlock:
      return "lyra::runtime::DumpBlock";
    case mir::RuntimeLibraryKind::kBitVec:
      return "lyra::value::BitVec";
    case mir::RuntimeLibraryKind::kDpiBitBuffer:
      return "lyra::value::DpiBitBuffer";
    case mir::RuntimeLibraryKind::kDpiLogicBuffer:
      return "lyra::value::DpiLogicBuffer";
    case mir::RuntimeLibraryKind::kDpiBitChunk:
      return "svBitVecVal";
    case mir::RuntimeLibraryKind::kDpiLogicChunk:
      return "svLogicVecVal";
    case mir::RuntimeLibraryKind::kDpiOpenArray:
      return "lyra::value::DpiOpenArray";
    case mir::RuntimeLibraryKind::kDpiOpenArrayHandle:
      return "const svOpenArrayHandle";
  }
  throw InternalError("backend::cpp: unknown RuntimeLibraryKind");
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
    case mir::MachineFloatWidth::k32:
      return "float";
    case mir::MachineFloatWidth::k64:
      return "double";
  }
  throw InternalError("backend::cpp: unknown MachineFloatWidth");
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
          [&](const mir::StringType&) { out += "lyra::value::String"; },
          [&](const mir::AnyType&) { out += "lyra::value::Any"; },
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
          [&](const mir::MachineFunctionType& m) {
            // A pointer to a function type, spelled so that a declaration
            // names it as it names any other type. The erased function type
            // is the runtime's alias.
            if (mir::IsErasedFunction(unit.types, type_id)) {
              out += "lyra::runtime::ErasedEntry";
              return;
            }
            Write(out, "std::add_pointer_t<", type(m.result), "(");
            WriteTypeList(out, unit, m.params);
            out += ")>";
          },
          [&](const mir::ChandleType&) { out += "lyra::value::Chandle"; },
          [&](const mir::RealType&) { out += "lyra::value::Real"; },
          [&](const mir::ShortRealType&) { out += "lyra::value::ShortReal"; },
          [&](const mir::UnpackedArrayType& ua) {
            Write(
                out, "lyra::value::UnpackedArray<", type(ua.element_type), ", ",
                ua.Size(), ">");
          },
          [&](const mir::DynamicArrayType& da) {
            Write(
                out, "lyra::value::DynamicArray<", type(da.element_type), ">");
          },
          // LRM 7.10.5: the bound is part of the queue's type, and a queue
          // declaring none is spelled with a bound below zero.
          [&](const mir::QueueType& q) {
            const std::int64_t bound =
                q.max_bound.has_value()
                    ? static_cast<std::int64_t>(*q.max_bound)
                    : -1;
            Write(
                out, "lyra::value::Queue<", type(q.element_type), ", ", bound,
                ">");
          },
          [&](const mir::AssociativeArrayType& a) {
            Write(
                out, "lyra::value::AssociativeArray<", type(a.key_type), ", ",
                type(a.element_type), ">");
          },
          [&](const mir::WildcardIndexType&) {
            out += "lyra::value::WildcardKey";
          },
          [&](const mir::ObjectType& o) {
            Write(out, CppClassRef(unit, o.of));
          },
          [&](const mir::RecordType& r) { record(r.declaration); },
          [&](const mir::RuntimeClassType& e) {
            out += RuntimeClassCppType(e.which);
          },
          [&](const mir::RuntimeEffectsType&) {
            out += "lyra::runtime::RuntimeEffects&";
          },
          [&](const mir::FilesType&) { out += "lyra::runtime::FileTable&"; },
          [&](const mir::DiagnosticType&) {
            out += "lyra::runtime::DiagnosticDispatcher&";
          },
          [&](const mir::RuntimeLibraryType& r) {
            out += RuntimeLibraryCppType(r.kind);
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
          [&](const mir::WeakManagedRefType&) {
            out += "lyra::runtime::WeakObjectRef";
          },
          [&](const mir::VectorType& v) {
            Write(out, "std::vector<", type(v.element), ">");
          },
          // A tuple is its components, so it is the library's product of them.
          [&](const mir::TupleType& t) {
            Write(out, CppTupleComponents{.unit = &unit, .of = t.elements});
          },
          [&](const mir::EmptyType&) { out += "lyra::value::Empty"; },
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
          [&](const mir::SampledHistoryType& h) {
            Write(out, "lyra::runtime::SampledHistory<", type(h.value), ">");
          },
          [&](const mir::EvaluationAttemptsType&) {
            out += "lyra::runtime::EvaluationAttempts";
          },
          [&](const mir::LoopBlocksType&) {
            out += "lyra::runtime::LoopBlocks";
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
          // A generic definition is written as a function template, and its
          // type parameter as the template parameter.
          [&](const mir::TypeParameterType& t) {
            Write(out, CppTypeParameterName(t.index));
          },
      });
}

namespace {

void WritePlane(TargetText& out, std::span<const std::uint64_t> words) {
  out += "{";
  WriteSeparated(out, words, ", ", [&](std::uint64_t word) {
    out += "0x";
    WriteNumber(out, word, 16);
    out += "ULL";
  });
  out += "}";
}

}  // namespace

void WriteOne(TargetText& out, const CppIntegralLiteral& literal) {
  const mir::IntegralConstantDecl& constant =
      literal.Unit().integral_constants.Get(literal.Constant());
  Write(out, CppType(literal.Unit(), constant.type), "::FromWords(");
  WritePlane(out, constant.value.value_words);
  out += ", ";
  WritePlane(out, constant.value.state_words);
  out += ")";
}

// Every enumeration's table goes by one type name, so each table states how
// wide its base type is, whether that type has an unknown plane, and how many
// members it holds, which is what tells the C++ compiler which table type it
// is.
void WriteOne(TargetText& out, const CppEnumTableLiteral& literal) {
  const mir::CompilationUnit& unit = literal.Unit();
  const mir::EnumType& enumeration = unit.enum_tables.Get(literal.Table());
  bool four_state = false;
  switch (enumeration.base.state_kind) {
    case mir::IntegralStateKind::kTwoState:
      break;
    case mir::IntegralStateKind::kFourState:
      four_state = true;
      break;
  }
  std::vector<value::ConstPlanes> members;
  members.reserve(enumeration.members.size());
  for (const mir::EnumMember& member : enumeration.members) {
    members.push_back(
        value::ConstPlanes{
            .value = member.value.value_words,
            .unknown = member.value.state_words});
  }
  Write(out, CppType(unit, unit.builtins.enumeration), "<");
  WriteNumber(out, enumeration.base.bit_width, 10);
  out += four_state ? ", true, " : ", false, ";
  WriteNumber(out, static_cast<std::uint64_t>(enumeration.members.size()), 10);
  out += ">{";
  WritePlane(
      out, value::EnumerationPlanes(
               enumeration.base.bit_width, four_state, members));
  out += ", {";
  WriteSeparated(
      out, enumeration.members, ", ", [&](const mir::EnumMember& member) {
        Write(out, CppNameLiteral(member.name));
      });
  out += "}}";
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
          [&](const mir::ManagedRefType& m) -> DerefSpelling {
            return DerefThroughView{.pointee = m.pointee};
          },
          [&](const mir::WeakManagedRefType&) { return reaches_nothing(); },
          // Every other type is a value that reaches nothing, so there is
          // nothing to dereference.
          [&](const mir::IntegralType&) { return reaches_nothing(); },
          [&](const mir::EnumType&) { return reaches_nothing(); },
          [&](const mir::UnpackedArrayType&) { return reaches_nothing(); },
          [&](const mir::DynamicArrayType&) { return reaches_nothing(); },
          [&](const mir::QueueType&) { return reaches_nothing(); },
          [&](const mir::AssociativeArrayType&) { return reaches_nothing(); },
          [&](const mir::WildcardIndexType&) { return reaches_nothing(); },
          [&](const mir::AnyType&) { return reaches_nothing(); },
          [&](const mir::ErasedValueType&) { return reaches_nothing(); },
          [&](const mir::StringType&) { return reaches_nothing(); },
          [&](const mir::MachineCStringType&) { return reaches_nothing(); },
          [&](const mir::MachineBoolType&) { return reaches_nothing(); },
          [&](const mir::MachineIntType&) { return reaches_nothing(); },
          [&](const mir::MachineFloatType&) { return reaches_nothing(); },
          [&](const mir::MachineArrayType&) { return reaches_nothing(); },
          [&](const mir::MachineFunctionType&) { return reaches_nothing(); },
          [&](const mir::RealType&) { return reaches_nothing(); },
          [&](const mir::ShortRealType&) { return reaches_nothing(); },
          [&](const mir::ChandleType&) { return reaches_nothing(); },
          [&](const mir::VoidType&) { return reaches_nothing(); },
          [&](const mir::EmptyType&) { return reaches_nothing(); },
          [&](const mir::ObjectType&) { return reaches_nothing(); },
          [&](const mir::RuntimeClassType&) { return reaches_nothing(); },
          [&](const mir::RuntimeEffectsType&) { return reaches_nothing(); },
          [&](const mir::FilesType&) { return reaches_nothing(); },
          [&](const mir::DiagnosticType&) { return reaches_nothing(); },
          [&](const mir::RuntimeLibraryType&) { return reaches_nothing(); },
          [&](const mir::CoroutineType&) { return reaches_nothing(); },
          [&](const mir::VectorType&) { return reaches_nothing(); },
          [&](const mir::TupleType&) { return reaches_nothing(); },
          [&](const mir::ObservableType&) { return reaches_nothing(); },
          [&](const mir::ResolvedType&) { return reaches_nothing(); },
          [&](const mir::DriverType&) { return reaches_nothing(); },
          [&](const mir::SampledHistoryType&) { return reaches_nothing(); },
          [&](const mir::EvaluationAttemptsType&) { return reaches_nothing(); },
          [&](const mir::LoopBlocksType&) { return reaches_nothing(); },
          [&](const mir::RecordType&) { return reaches_nothing(); },
          [&](const mir::ClosureType&) { return reaches_nothing(); },
          [&](const mir::ErasedCallableType&) { return reaches_nothing(); },
          [&](const mir::TypeParameterType&) { return reaches_nothing(); },
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
              [&](const mir::WeakManagedRefType&) { return holds_no_object(); },
              [&](const mir::IntegralType&) { return holds_no_object(); },
              [&](const mir::EnumType&) { return holds_no_object(); },
              [&](const mir::UnpackedArrayType&) { return holds_no_object(); },
              [&](const mir::DynamicArrayType&) { return holds_no_object(); },
              [&](const mir::QueueType&) { return holds_no_object(); },
              [&](const mir::AssociativeArrayType&) {
                return holds_no_object();
              },
              [&](const mir::WildcardIndexType&) { return holds_no_object(); },
              [&](const mir::AnyType&) { return holds_no_object(); },
              [&](const mir::ErasedValueType&) { return holds_no_object(); },
              [&](const mir::StringType&) { return holds_no_object(); },
              [&](const mir::MachineCStringType&) { return holds_no_object(); },
              [&](const mir::MachineBoolType&) { return holds_no_object(); },
              [&](const mir::MachineIntType&) { return holds_no_object(); },
              [&](const mir::MachineFloatType&) { return holds_no_object(); },
              [&](const mir::MachineArrayType&) { return holds_no_object(); },
              [&](const mir::MachineFunctionType&) {
                return holds_no_object();
              },
              [&](const mir::RealType&) { return holds_no_object(); },
              [&](const mir::ShortRealType&) { return holds_no_object(); },
              [&](const mir::ChandleType&) { return holds_no_object(); },
              [&](const mir::VoidType&) { return holds_no_object(); },
              [&](const mir::EmptyType&) { return holds_no_object(); },
              [&](const mir::ObjectType&) { return holds_no_object(); },
              [&](const mir::RuntimeClassType&) { return holds_no_object(); },
              [&](const mir::RuntimeEffectsType&) { return holds_no_object(); },
              [&](const mir::FilesType&) { return holds_no_object(); },
              [&](const mir::DiagnosticType&) { return holds_no_object(); },
              [&](const mir::RuntimeLibraryType&) { return holds_no_object(); },
              [&](const mir::CoroutineType&) { return holds_no_object(); },
              [&](const mir::VectorType&) { return holds_no_object(); },
              [&](const mir::TupleType&) { return holds_no_object(); },
              [&](const mir::ObservableType&) { return holds_no_object(); },
              [&](const mir::ResolvedType&) { return holds_no_object(); },
              [&](const mir::DriverType&) { return holds_no_object(); },
              [&](const mir::SampledHistoryType&) { return holds_no_object(); },
              [&](const mir::EvaluationAttemptsType&) {
                return holds_no_object();
              },
              [&](const mir::LoopBlocksType&) { return holds_no_object(); },
              [&](const mir::RecordType&) { return holds_no_object(); },
              [&](const mir::ClosureType&) { return holds_no_object(); },
              [&](const mir::ErasedCallableType&) { return holds_no_object(); },
              [&](const mir::TypeParameterType&) { return holds_no_object(); },
          });
}

auto NullSpellingAsCpp(const mir::CompilationUnit& unit, mir::TypeId type)
    -> diag::Result<NullSpelling> {
  const mir::Type& t = unit.types.Get(type);
  if (t.Is<mir::ManagedRefType>() || t.Is<mir::ChandleType>()) {
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
          [&](const mir::WeakManagedRefType& t) { by_naming_itself(t); },
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
          [&](const mir::StringType& t) { by_naming_itself(t); },
          [&](const mir::MachineCStringType& t) { by_naming_itself(t); },
          [&](const mir::MachineBoolType& t) { by_naming_itself(t); },
          [&](const mir::MachineIntType& t) { by_naming_itself(t); },
          [&](const mir::MachineFloatType& t) { by_naming_itself(t); },
          [&](const mir::MachineArrayType& t) { by_naming_itself(t); },
          [&](const mir::MachineFunctionType& t) { by_naming_itself(t); },
          [&](const mir::ChandleType& t) { by_naming_itself(t); },
          [&](const mir::RealType& t) { by_naming_itself(t); },
          [&](const mir::ShortRealType& t) { by_naming_itself(t); },
          [&](const mir::UnpackedArrayType& t) { by_naming_itself(t); },
          [&](const mir::DynamicArrayType& t) { by_naming_itself(t); },
          [&](const mir::QueueType& t) { by_naming_itself(t); },
          [&](const mir::AssociativeArrayType& t) { by_naming_itself(t); },
          [&](const mir::WildcardIndexType& t) { by_naming_itself(t); },
          [&](const mir::AnyType& t) { by_naming_itself(t); },
          [](const mir::ErasedValueType&) {
            throw InternalError(
                "backend::cpp: a value of no known type is held only through a "
                "reference, so none is constructed");
          },
          [&](const mir::ObjectType& t) { by_naming_itself(t); },
          [&](const mir::RecordType& t) { by_naming_itself(t); },
          [&](const mir::RuntimeClassType& t) { by_naming_itself(t); },
          [&](const mir::RuntimeEffectsType& t) { by_naming_itself(t); },
          [&](const mir::FilesType& t) { by_naming_itself(t); },
          [&](const mir::DiagnosticType& t) { by_naming_itself(t); },
          [&](const mir::RuntimeLibraryType& t) { by_naming_itself(t); },
          [&](const mir::CoroutineType& t) { by_naming_itself(t); },
          [&](const mir::VariableRefType& t) { by_naming_itself(t); },
          [&](const mir::ReferenceWrapperType& t) { by_naming_itself(t); },
          [&](const mir::VoidType& t) { by_naming_itself(t); },
          [&](const mir::TupleType& t) { by_naming_itself(t); },
          [&](const mir::EmptyType& t) { by_naming_itself(t); },
          [&](const mir::ObservableType& t) { by_naming_itself(t); },
          [&](const mir::ResolvedType& t) { by_naming_itself(t); },
          [&](const mir::DriverType& t) { by_naming_itself(t); },
          [&](const mir::OpenWriteType& t) { by_naming_itself(t); },
          [&](const mir::DesignationType& t) { by_naming_itself(t); },
          [&](const mir::BitsReferenceType& t) { by_naming_itself(t); },
          [&](const mir::ObjectWriteType& t) { by_naming_itself(t); },
          [&](const mir::SampledHistoryType& t) { by_naming_itself(t); },
          [&](const mir::EvaluationAttemptsType& t) { by_naming_itself(t); },
          [&](const mir::LoopBlocksType& t) { by_naming_itself(t); },
          [&](const mir::ClosureType& t) { by_naming_itself(t); },
          [](const mir::ErasedCallableType&) {
            throw InternalError(
                "backend::cpp: a closure of no known type is made by a cast "
                "of the box or the borrow holding it, so none is constructed");
          },
          [](const mir::TypeParameterType&) {
            throw InternalError(
                "backend::cpp: a value of a type parameter is made only by "
                "the call its initializer is, which names no constructor");
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

void WriteOne(TargetText& out, const CppObjectCreation& creation) {
  const mir::Type& type = creation.of.Unit().types.Get(creation.of.Type());
  if (type.Is<mir::ManagedRefType>()) {
    out += "lyra::runtime::CreateObject";
    return;
  }
  if (const auto* pointer = type.As<mir::PointerType>()) {
    switch (pointer->ownership) {
      case mir::PointerOwnership::kUnique:
        out += "lyra::runtime::CreateOwned";
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
          // A class with a path is declared in a file of the unit defining it,
          // which the text naming it includes; one no other unit names is
          // declared in the unit's code file, the one file that names it.
          [&](const mir::ClassId& id) {
            const mir::Class& cls = unit.GetClass(id);
            if (cls.path.has_value()) {
              out.Require(ClassDeclarationFileOf(
                  cls.defining_unit.value_or(unit.name), *cls.path));
            }
            Write(out, CppClassPath(unit, id));
          },
          // The tree's root is the library's scope class, spelled as a value
          // of that class's type is.
          [&](const mir::ObjectTreeRootRef&) {
            out += RuntimeClassCppType(support::RuntimeClass::kScope);
          },
          [&](const mir::ManagedObjectRootRef&) {
            out += RuntimeClassCppType(support::RuntimeClass::kClassObject);
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
            if (cls.path.has_value()) {
              out.Require(ClassDefinitionFileOf(
                  cls.defining_unit.value_or(unit.name), *cls.path));
            }
          },
          [](const mir::ObjectTreeRootRef&) {},
          [](const mir::ManagedObjectRootRef&) {}},
      base.of.Ref());
  Write(out, base.of);
}

}  // namespace lyra::backend::cpp
