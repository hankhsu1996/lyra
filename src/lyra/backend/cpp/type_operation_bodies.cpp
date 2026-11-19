#include "lyra/backend/cpp/type_operation_bodies.hpp"

#include <algorithm>
#include <cstddef>
#include <cstdint>
#include <deque>
#include <map>
#include <optional>
#include <set>
#include <string>
#include <utility>
#include <variant>
#include <vector>

#include "lyra/backend/cpp/render_type_operations.hpp"
#include "lyra/base/internal_error.hpp"
#include "lyra/mir/callable_code.hpp"
#include "lyra/mir/code_source.hpp"
#include "lyra/mir/compilation_unit.hpp"
#include "lyra/mir/expr.hpp"
#include "lyra/mir/stmt.hpp"
#include "lyra/mir/type_id.hpp"
#include "lyra/mir/type_interface.hpp"
#include "lyra/support/builtin_fn.hpp"
#include "lyra/support/value_operation.hpp"

namespace lyra::backend::cpp {

namespace {

// The free functions one code file defines for itself, each asked for once
// by its name, in the order first asked.
struct FileFunctions {
  std::set<std::string> asked;
  std::deque<mir::TypeOperationSource> pending;
  std::vector<mir::TypeOperationBody> built;
};

// One implementation the unit writes: the implementation, the body of each
// item, and the functions the type's code file defines for them.
struct ImplInProgress {
  mir::Impl impl;
  std::vector<mir::TypeOperationBody> items;
  FileFunctions functions;
};

// The bodies a unit needs, found as its code and the bodies built for it are
// read, each built once.
class Collector {
 public:
  Collector(const mir::CompilationUnit& unit, const BodyBuilder& build)
      : unit_(&unit), build_(&build) {
  }

  auto Run() && -> TypeOperationBodies {
    mir::ForEachCode(
        *unit_, [&](const mir::CodeSource&, const mir::CallableCode& code) {
          // A declaration states no body to read.
          if (code.body.has_value()) {
            ReadBlock(*code.body, unit_functions_);
          }
        });
    // Building a body may add a container type to the pool, which then has an
    // implementation of its own, so the passes run until none adds anything.
    while (NoteImpls() || BuildPending()) {
    }
    TypeOperationBodies bodies{.impls = {}, .functions = {}};
    for (auto& [name, of] : impls_) {
      // The type's code file is the type's alone, whatever order this unit
      // met its types in: functions by name.
      std::ranges::sort(
          of.functions.built, {}, [&](const mir::TypeOperationBody& function) {
            return TypeFunctionName(*unit_, function.source);
          });
      bodies.impls.push_back(
          ImplBodies{
              .self_type = of.impl.self_type,
              .items = std::move(of.items),
              .functions = std::move(of.functions.built)});
    }
    bodies.functions = std::move(unit_functions_.built);
    return bodies;
  }

 private:
  [[nodiscard]] auto Built(const mir::TypeOperationSource& source) const
      -> mir::CallableCode {
    std::optional<mir::CallableCode> built = (*build_)(source);
    if (!built.has_value()) {
      throw InternalError(
          "backend::cpp: a type states an operation MIR builds no body for "
          "-- please report this as a bug");
    }
    return *std::move(built);
  }

  // Builds every function each file was asked for since the last pass,
  // reading each for what it calls in turn; whether any was built.
  auto BuildPending() -> bool {
    bool any = BuildPending(unit_functions_);
    for (auto& [name, of] : impls_) {
      any = BuildPending(of.functions) || any;
    }
    return any;
  }
  auto BuildPending(FileFunctions& file) -> bool {
    bool any = false;
    while (!file.pending.empty()) {
      const mir::TypeOperationSource next = file.pending.front();
      file.pending.pop_front();
      mir::CallableCode code = Built(next);
      ReadBlock(code.Body(), file);
      file.built.push_back(
          mir::TypeOperationBody{.source = next, .code = std::move(code)});
      any = true;
    }
    return any;
  }

  // Each type the pool holds that no pass has met yet and that implements the
  // library's traits has its implementation's items built, each read for
  // what it calls in the type's code file. Whether any implementation was
  // added.
  auto NoteImpls() -> bool {
    const std::size_t impls_before = impls_.size();
    while (types_seen_ < unit_->types.size()) {
      const mir::TypeId type{.value = static_cast<std::uint32_t>(types_seen_)};
      ++types_seen_;
      std::optional<mir::Impl> impl = mir::ImplOf(*unit_, type);
      if (!impl.has_value()) {
        continue;
      }
      const auto [at, added] = impls_.try_emplace(TypeSymbolName(*unit_, type));
      if (!added) {
        continue;
      }
      ImplInProgress& of = at->second;
      of.impl = *std::move(impl);
      for (const support::ValueOperation item : of.impl.Items()) {
        const mir::TypeOperationSource source{.type = type, .answers = item};
        mir::CallableCode code = Built(source);
        ReadBlock(code.Body(), of.functions);
        of.items.push_back(
            mir::TypeOperationBody{.source = source, .code = std::move(code)});
      }
    }
    return impls_.size() != impls_before;
  }

  // A body `file`'s code calls: an item of a type's implementation, which the
  // type's own code file defines, or a free function `file` defines, asked
  // for once by name.
  void Need(const mir::TypeOperationSource& source, FileFunctions& file) {
    if (!IsNonMemberBody(*unit_, source)) {
      return;
    }
    if (file.asked.insert(TypeFunctionName(*unit_, source)).second) {
      file.pending.push_back(source);
    }
  }

  static auto SourceOf(const mir::TypeOperationTarget& target)
      -> mir::TypeOperationSource {
    return mir::TypeOperationSource{
        .type = target.type,
        .answers = target.answers,
        .arguments = target.arguments};
  }

  void ReadBlock(const mir::Block& block, FileFunctions& file) {
    for (const mir::ExprId id : block.exprs.Ids()) {
      const auto* call = std::get_if<mir::CallExpr>(&block.exprs.Get(id).data);
      if (call == nullptr) {
        continue;
      }
      const auto* direct = std::get_if<mir::Direct>(&call->callee);
      if (direct == nullptr) {
        continue;
      }
      if (const auto* target =
              std::get_if<mir::TypeOperationTarget>(&direct->target)) {
        Need(SourceOf(*target), file);
      }
      // A run written within a write is the array type's own slice write,
      // taken on the part the write designates.
      if (mir::DirectBuiltinFn(*call) ==
          support::BuiltinFn::kAssignDesignatedRun) {
        Need(SourceOf(mir::DesignatedRunWrite(*unit_, block, *call)), file);
      }
    }
    for (const mir::BlockId child : block.child_scopes.Ids()) {
      ReadBlock(block.child_scopes.Get(child), file);
    }
  }

  const mir::CompilationUnit* unit_;
  const BodyBuilder* build_;
  FileFunctions unit_functions_;
  std::size_t types_seen_ = 0;
  // Each implementation by its type's name, in a stable order.
  std::map<std::string, ImplInProgress> impls_;
};

}  // namespace

auto TypeOperationBodiesOf(
    const mir::CompilationUnit& unit, const BodyBuilder& build)
    -> TypeOperationBodies {
  return Collector(unit, build).Run();
}

}  // namespace lyra::backend::cpp
