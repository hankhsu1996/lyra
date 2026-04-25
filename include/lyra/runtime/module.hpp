#pragma once

namespace lyra::runtime {

class RuntimeBindContext;

class Module {
 public:
  Module() = default;
  virtual ~Module() = default;
  Module(const Module&) = delete;
  auto operator=(const Module&) -> Module& = delete;
  Module(Module&&) = delete;
  auto operator=(Module&&) -> Module& = delete;

  virtual void Bind(RuntimeBindContext& ctx) = 0;
};

}  // namespace lyra::runtime
