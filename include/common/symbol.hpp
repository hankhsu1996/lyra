#pragma once

#include <functional>

#include <slang/ast/Symbol.h>

namespace lyra::common {

using SymbolRef = std::reference_wrapper<const slang::ast::Symbol>;

}  // namespace lyra::common
