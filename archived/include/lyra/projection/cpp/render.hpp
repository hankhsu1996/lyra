#pragma once

#include <string>

#include "lyra/common/diagnostic/diagnostic.hpp"
#include "lyra/projection/cpp/model.hpp"
#include "lyra/xir/compilation_unit.hpp"

class TypeArena;
class ConstantArena;

namespace lyra::projection::cpp {

auto ProjectPerCU(
    const xir::CompilationUnit& cu, const TypeArena& type_arena,
    const ConstantArena& constant_arena) -> Result<PerCUProjection>;

auto ProjectHost(const xir::CompilationUnit& cu) -> HostProjection;

auto RenderPerCUHeader(const PerCUProjection& proj) -> std::string;

auto RenderInternalEntry(const HostProjection& host) -> std::string;

auto RenderHostEntry(const HostProjection& host) -> std::string;

}  // namespace lyra::projection::cpp
