#include "lyra/support/runtime_object.hpp"

#include <optional>
#include <string_view>
#include <variant>

#include "lyra/base/internal_error.hpp"
#include "lyra/base/overloaded.hpp"
#include "lyra/support/library_decl.hpp"
#include "lyra/support/value_domain.hpp"

namespace lyra::support {

auto ObjectOf(LibraryDecl decl) -> std::optional<RuntimeObject> {
  using Object = std::optional<RuntimeObject>;
  return std::visit(
      Overloaded{
          [](const DomainObject& held) -> Object { return held.domain; },
          [&](const OwnObject& own) -> Object {
            if (!own.by_value.has_value()) {
              return std::nullopt;
            }
            return decl;
          },
          [](const StorageElsewhere&) -> Object { return std::nullopt; }},
      DeclarationOf(decl).holding);
}

namespace {

auto LibraryObjectName(LibraryObject object) -> std::string_view {
  switch (object) {
    case LibraryObject::kExecution:
      return "execution";
    case LibraryObject::kSharedPointer:
      return "shared_pointer";
    case LibraryObject::kOpenWrite:
      return "open_write";
    case LibraryObject::kDesignation:
      return "designation";
    case LibraryObject::kObjectWrite:
      return "object_write";
    case LibraryObject::kReference:
      return "reference";
    case LibraryObject::kAnyReference:
      return "any_reference";
  }
  throw InternalError("runtime object: unknown library object");
}

}  // namespace

auto RuntimeObjectName(const RuntimeObject& object) -> std::string_view {
  using Name = std::string_view;
  return std::visit(
      Overloaded{
          [](ValueDomain domain) -> Name { return ValueDomainName(domain); },
          [](LibraryDecl decl) -> Name { return OwnObjectEntries(decl); },
          [](LibraryObject library) -> Name {
            return LibraryObjectName(library);
          }},
      object);
}

}  // namespace lyra::support
