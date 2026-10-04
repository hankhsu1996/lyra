#include "lyra/support/runtime_class.hpp"

#include <array>
#include <cstdint>
#include <optional>
#include <span>
#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

auto RuntimeClassName(RuntimeClass klass) -> std::string_view {
  switch (klass) {
    case RuntimeClass::kScope:
      return "Scope";
    case RuntimeClass::kObject:
      return "Object";
    case RuntimeClass::kProcess:
      return "Process";
  }
  throw InternalError("RuntimeClassName: unknown runtime class");
}

auto DeclaringClassOf(LibraryVirtual function) -> RuntimeClass {
  switch (function) {
    case LibraryVirtual::kScopeResolve:
    case LibraryVirtual::kScopeInitialize:
    case LibraryVirtual::kScopeCreateProcesses:
      return RuntimeClass::kScope;
  }
  throw InternalError("DeclaringClassOf: unknown library virtual");
}

auto VirtualsDeclaredBy(RuntimeClass klass) -> std::span<const LibraryVirtual> {
  static constexpr std::array<LibraryVirtual, 3> kScopeVirtuals{
      LibraryVirtual::kScopeResolve, LibraryVirtual::kScopeInitialize,
      LibraryVirtual::kScopeCreateProcesses};
  switch (klass) {
    case RuntimeClass::kScope:
      return kScopeVirtuals;
    case RuntimeClass::kObject:
    case RuntimeClass::kProcess:
      return {};
  }
  throw InternalError("VirtualsDeclaredBy: unknown runtime class");
}

auto LibraryBaseOf(RuntimeClass klass) -> std::optional<RuntimeClass> {
  switch (klass) {
    case RuntimeClass::kScope:
      return RuntimeClass::kObject;
    case RuntimeClass::kObject:
    case RuntimeClass::kProcess:
      return std::nullopt;
  }
  throw InternalError("LibraryBaseOf: unknown runtime class");
}

auto OrdinalOf(LibraryVirtual function) -> std::uint32_t {
  switch (function) {
    case LibraryVirtual::kScopeResolve:
      return 0;
    case LibraryVirtual::kScopeInitialize:
      return 1;
    case LibraryVirtual::kScopeCreateProcesses:
      return 2;
  }
  throw InternalError("OrdinalOf: unknown library virtual");
}

auto LibraryVirtualName(LibraryVirtual function) -> std::string_view {
  switch (function) {
    case LibraryVirtual::kScopeResolve:
      return "sv_resolve";
    case LibraryVirtual::kScopeInitialize:
      return "sv_initialize";
    case LibraryVirtual::kScopeCreateProcesses:
      return "sv_create_processes";
  }
  throw InternalError("LibraryVirtualName: unknown library virtual");
}

}  // namespace lyra::support
