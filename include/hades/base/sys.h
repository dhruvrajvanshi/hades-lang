#ifndef HADES_SYS
#define HADES_SYS

#include <filesystem>
#include <iostream>

namespace hades {
namespace fs = std::filesystem;

class Unimplemented: public std::exception {
public:
  const char* what() const noexcept override {
    return "Unimplemented";
  }
};

[[noreturn]]
inline auto unimplemented() -> void {
  throw Unimplemented();
}

} // namespace hades

#endif
