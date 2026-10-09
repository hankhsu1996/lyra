#include "lyra/runtime/object_layout.hpp"

#include <cstdint>
#include <string>
#include <string_view>
#include <type_traits>
#include <typeinfo>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/activation_value_cell.hpp"
#include "lyra/runtime/cancellation.hpp"
#include "lyra/runtime/closure.hpp"
#include "lyra/runtime/coroutine.hpp"
#include "lyra/runtime/evaluation_attempts.hpp"
#include "lyra/runtime/file_table.hpp"
#include "lyra/runtime/hierarchy_segment.hpp"
#include "lyra/runtime/named_event.hpp"
#include "lyra/runtime/net.hpp"
#include "lyra/runtime/object_change.hpp"
#include "lyra/runtime/object_ref.hpp"
#include "lyra/runtime/observation.hpp"
#include "lyra/runtime/open_write.hpp"
#include "lyra/runtime/read_report.hpp"
#include "lyra/runtime/runtime_process.hpp"
#include "lyra/runtime/sampled_history.hpp"
#include "lyra/runtime/scope.hpp"
#include "lyra/runtime/shared_pointer.hpp"
#include "lyra/runtime/trigger.hpp"
#include "lyra/runtime/var.hpp"
#include "lyra/value/chandle.hpp"
#include "lyra/value/concepts.hpp"
#include "lyra/value/dpi_canonical.hpp"
#include "lyra/value/dpi_open_array.hpp"
#include "lyra/value/empty.hpp"
#include "lyra/value/format.hpp"
#include "lyra/value/object_ref.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/runtime_associative_array.hpp"
#include "lyra/value/runtime_dynamic_array.hpp"
#include "lyra/value/runtime_queue.hpp"
#include "lyra/value/runtime_tagged_union.hpp"
#include "lyra/value/runtime_tuple.hpp"
#include "lyra/value/runtime_union.hpp"
#include "lyra/value/runtime_unpacked_array.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

namespace {

using support::MemberStorageKind;
using support::ObjectLayout;
using support::ValueDomain;

template <typename T>
constexpr auto Of() -> ObjectLayout {
  return ObjectLayout{
      .size = static_cast<std::uint32_t>(sizeof(T)),
      .align = static_cast<std::uint32_t>(alignof(T)),
      .ends_with_nothing_to_do = std::is_trivially_destructible_v<T>};
}

[[noreturn]] auto NotRealized() -> ObjectLayout {
  throw InternalError(
      "object layout: this member storage is not realized over this value "
      "domain -- please report this as a bug");
}

// The class the library holds a value of `domain` as, handed to `f` as its
// template argument. Every storage below is one template over that class, so
// this is the one place a domain names its class, and what a storage admits is
// asked of the class it is handed.
template <typename F>
auto WithValueClass(ValueDomain domain, F f) -> ObjectLayout {
  switch (domain) {
    case ValueDomain::kPacked:
      return f.template operator()<value::PackedArray>();
    case ValueDomain::kString:
      return f.template operator()<value::String>();
    case ValueDomain::kReal:
      return f.template operator()<value::Real>();
    case ValueDomain::kShortReal:
      return f.template operator()<value::ShortReal>();
    case ValueDomain::kChandle:
      return f.template operator()<value::Chandle>();
    case ValueDomain::kEmpty:
      return f.template operator()<value::Empty>();
    case ValueDomain::kTuple:
      return f.template operator()<value::RuntimeTuple>();
    case ValueDomain::kUnion:
      return f.template operator()<value::RuntimeUnion>();
    case ValueDomain::kTaggedUnion:
      return f.template operator()<value::RuntimeTaggedUnion>();
    case ValueDomain::kDynArray:
      return f.template operator()<value::RuntimeDynamicArray>();
    case ValueDomain::kUnpackedArray:
      return f.template operator()<value::RuntimeUnpackedArray>();
    case ValueDomain::kQueue:
      return f.template operator()<value::RuntimeQueue>();
    case ValueDomain::kAssocArray:
      return f.template operator()<value::RuntimeAssociativeArray>();
    case ValueDomain::kManagedRef:
      return f.template operator()<value::ObjectRef>();
  }
  throw InternalError("object layout: unknown value domain");
}

// A variable may be of every value class but the empty one, which is only ever
// a tagged union's payload (LRM 7.3.2).
template <typename T>
struct AdmitsVariable : std::bool_constant<!std::is_same_v<T, value::Empty>> {};

// A history is kept over every variable type but the chandle, whose value is
// the pointer it carries (LRM 6.14), which no sampled read can answer across.
template <typename T>
struct AdmitsHistory
    : std::bool_constant<
          AdmitsVariable<T>::value && !std::is_same_v<T, value::Chandle>> {};

// A net resolves only what LRM 6.7.1 admits as a net's data type.
template <typename T>
struct AdmitsNet : std::bool_constant<value::NetResolvable<T>> {};

// `Storage` over the value class of `domain`, where `Admits` holds of that
// class.
template <template <typename> class Storage, template <typename> class Admits>
auto StorageOver(ValueDomain domain) -> ObjectLayout {
  return WithValueClass(domain, []<typename T>() -> ObjectLayout {
    if constexpr (Admits<T>::value) {
      return Of<Storage<T>>();
    } else {
      return NotRealized();
    }
  });
}

}  // namespace

auto LayoutOf(ValueDomain domain) -> ObjectLayout {
  return WithValueClass(
      domain, []<typename T>() -> ObjectLayout { return Of<T>(); });
}

auto LayoutOf(support::LibraryObject object) -> ObjectLayout {
  switch (object) {
    case support::LibraryObject::kClosure:
      return Of<OwnedClosure>();
    case support::LibraryObject::kPrintItem:
      return Of<value::PrintItem>();
    case support::LibraryObject::kFormatSpec:
      return Of<value::FormatSpec>();
    case support::LibraryObject::kFormatArg:
      return Of<value::FormatArg>();
    case support::LibraryObject::kHierarchySegment:
      return Of<HierarchySegment>();
    case support::LibraryObject::kTrigger:
      return Of<Trigger>();
    case support::LibraryObject::kObservation:
      return Of<Observation>();
    case support::LibraryObject::kReadReport:
      return Of<ReadReport>();
    case support::LibraryObject::kWait:
      return Of<Wait>();
    case support::LibraryObject::kDpiBitBuffer:
      return Of<value::DpiBitBuffer>();
    case support::LibraryObject::kDpiLogicBuffer:
      return Of<value::DpiLogicBuffer>();
    case support::LibraryObject::kDpiOpenArray:
      return Of<value::DpiOpenArray>();
    case support::LibraryObject::kChannelCancellation:
      return Of<ChannelCancellation>();
    case support::LibraryObject::kExecution:
      return Of<Coroutine<void>>();
    case support::LibraryObject::kSharedPointer:
      return Of<SharedPointer>();
    case support::LibraryObject::kOpenWrite:
      return Of<OpenWrite>();
    case support::LibraryObject::kDesignation:
      return Of<ErasedDesignation>();
    case support::LibraryObject::kObjectWrite:
      return Of<ErasedObjectWrite>();
    case support::LibraryObject::kReference:
      return Of<ErasedReference>();
  }
  throw InternalError("object layout: unknown library object");
}

auto LayoutOf(const support::RuntimeObject& object) -> ObjectLayout {
  return std::visit(
      Overloaded{
          [](ValueDomain domain) { return LayoutOf(domain); },
          [](support::LibraryObject library) { return LayoutOf(library); }},
      object);
}

auto LayoutOf(support::DeclaredMemberStorage storage) -> ObjectLayout {
  switch (storage.kind) {
    case MemberStorageKind::kInlineValue:
      return StorageOver<std::type_identity_t, AdmitsVariable>(storage.domain);
    case MemberStorageKind::kValueCell:
      return StorageOver<ActivationValueCell, AdmitsVariable>(storage.domain);
    case MemberStorageKind::kObservableCell:
      return StorageOver<Var, AdmitsVariable>(storage.domain);
    case MemberStorageKind::kSampledHistory:
      return StorageOver<SampledHistory, AdmitsHistory>(storage.domain);
    case MemberStorageKind::kResolvedNet:
      return StorageOver<ResolvedNet, AdmitsNet>(storage.domain);
    case MemberStorageKind::kBorrowedHandle:
      return Of<void*>();
    case MemberStorageKind::kReference:
      return Of<ErasedReference>();
    case MemberStorageKind::kSharedPointer:
      return Of<SharedPointer>();
    case MemberStorageKind::kChannelCancellation:
      return Of<ChannelCancellation>();
    case MemberStorageKind::kNamedEvent:
      return Of<NamedEvent>();
    case MemberStorageKind::kCancellationTarget:
      return Of<CancellationTarget>();
    case MemberStorageKind::kEvaluationAttempts:
      return Of<EvaluationAttempts>();
  }
  throw InternalError("object layout: unknown member storage kind");
}

auto LayoutOf(support::RuntimeClass klass) -> ObjectLayout {
  switch (klass) {
    case support::RuntimeClass::kScope:
      return Of<Scope>();
    case support::RuntimeClass::kObject:
      return Of<GcObject>();
    case support::RuntimeClass::kProcess:
      return Of<RuntimeProcess>();
  }
  throw InternalError("object layout: unknown runtime class");
}

auto ClosureCapturesAt() -> std::uint64_t {
  return sizeof(ClosureValue);
}

// The Itanium ABI names a class's type information `_ZTI` followed by the
// class's mangled name, which is what its `type_info` reports as its name.
auto TypeInfoSymbolOf(support::RuntimeClass klass) -> std::string {
  switch (klass) {
    case support::RuntimeClass::kScope:
      return std::string("_ZTI") + typeid(Scope).name();
    case support::RuntimeClass::kObject:
      return std::string("_ZTI") + typeid(GcObject).name();
    case support::RuntimeClass::kProcess:
      throw InternalError(
          "object layout: a process is read as a class a value extends -- "
          "please report this as a bug");
  }
  throw InternalError("object layout: unknown runtime class");
}

// The Itanium mangling of the constructors and destructors below is written
// out, since neither has an address the language lets a program take. A wrong
// one fails every program's link, since every class a design declares extends
// one of these and calls both.
auto BaseObjectConstructorSymbolOf(support::RuntimeClass klass)
    -> std::string_view {
  switch (klass) {
    case support::RuntimeClass::kScope:
      return "_ZN4lyra7runtime5ScopeC2EPS1_NS0_16HierarchySegmentEPKNS0_"
             "16ObjectDefinitionE";
    case support::RuntimeClass::kObject:
      return "_ZN4lyra7runtime8GcObjectC2Ev";
    case support::RuntimeClass::kProcess:
      break;
  }
  throw InternalError(
      "object layout: a process is read as a class a value extends -- please "
      "report this as a bug");
}

auto BaseObjectDestructorSymbolOf(support::RuntimeClass klass)
    -> std::string_view {
  switch (klass) {
    case support::RuntimeClass::kScope:
      return "_ZN4lyra7runtime5ScopeD2Ev";
    case support::RuntimeClass::kObject:
      return "_ZN4lyra7runtime8GcObjectD2Ev";
    case support::RuntimeClass::kProcess:
      break;
  }
  throw InternalError(
      "object layout: a process is read as a class a value extends -- please "
      "report this as a bug");
}

// The Itanium mangling of each member function of `lyra::runtime::Scope`. A
// pointer to a member function names no symbol, so these are written out; a
// wrong one fails every program's link, since every class extending the scope
// that does not override one names it.
auto VirtualFunctionSymbolOf(support::LibraryVirtual function)
    -> std::string_view {
  switch (function) {
    case support::LibraryVirtual::kScopeResolve:
      return "_ZN4lyra7runtime5Scope10sv_resolveEv";
    case support::LibraryVirtual::kScopeInitialize:
      return "_ZN4lyra7runtime5Scope13sv_initializeEv";
    case support::LibraryVirtual::kScopeCreateProcesses:
      return "_ZN4lyra7runtime5Scope19sv_create_processesEv";
  }
  throw InternalError("object layout: unknown library virtual");
}

}  // namespace lyra::runtime
