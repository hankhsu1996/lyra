#include "lyra/support/imported_runtime_class.hpp"

#include <array>
#include <optional>
#include <string_view>

#include "lyra/base/internal_error.hpp"

namespace lyra::support {

namespace {

constexpr std::array kEveryImportedRuntimeClass{ImportedRuntimeClass::kProcess};

}  // namespace

auto ImportedRuntimeClassName(ImportedRuntimeClass klass) -> std::string_view {
  switch (klass) {
    case ImportedRuntimeClass::kProcess:
      return "process";
  }
  throw InternalError("ImportedRuntimeClassName: unknown imported class");
}

auto ImportedRuntimeClassNamed(std::string_view name)
    -> std::optional<ImportedRuntimeClass> {
  for (const ImportedRuntimeClass klass : kEveryImportedRuntimeClass) {
    if (ImportedRuntimeClassName(klass) == name) {
      return klass;
    }
  }
  return std::nullopt;
}

}  // namespace lyra::support
