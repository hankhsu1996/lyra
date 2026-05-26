#pragma once

#include <cstdint>
#include <functional>
#include <vector>

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

  // Last-write-wins per site within a time slot: re-submit at the same site
  // overwrites the prior closure, which suppresses settle-time glitches.
  void SubmitObserved(std::uint32_t site_id, std::function<void()> fn);

  void DrainObserved();

 private:
  // Empty std::function == clean slot; no parallel dirty bitmap needed.
  std::vector<std::function<void()>> observed_pending_;
};

}  // namespace lyra::runtime
