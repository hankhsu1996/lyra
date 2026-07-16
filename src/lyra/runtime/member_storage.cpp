#include "lyra/runtime/member_storage.hpp"

#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/runtime/scope_program.hpp"
#include "lyra/value/packed_array.hpp"
#include "lyra/value/real.hpp"
#include "lyra/value/string.hpp"

namespace lyra::runtime {

MemberStorage::MemberStorage(MemberStorageDescriptor descriptor) {
  switch (descriptor.kind) {
    case MemberStorageKind::kBorrowedHandle:
      object_.emplace<void*>(nullptr);
      return;
    case MemberStorageKind::kObservableCell:
      switch (descriptor.domain) {
        case ValueDomain::kPacked:
          object_.emplace<Var<value::PackedArray>>();
          return;
        case ValueDomain::kString:
          object_.emplace<Var<value::String>>();
          return;
        case ValueDomain::kReal:
          object_.emplace<Var<value::Real>>();
          return;
        case ValueDomain::kShortReal:
          object_.emplace<Var<value::ShortReal>>();
          return;
        case ValueDomain::kNone:
          throw InternalError(
              "MemberStorage: an observable cell needs a value domain");
      }
  }
  throw InternalError("MemberStorage: unknown member storage kind");
}

auto MemberStorage::Address() -> void* {
  return std::visit(
      Overloaded{
          [](void*& handle) -> void* { return &handle; },
          [](auto& cell) -> void* { return &cell; }},
      object_);
}

}  // namespace lyra::runtime
