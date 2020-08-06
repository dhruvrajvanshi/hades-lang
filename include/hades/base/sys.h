#ifndef HADES_SYS
#define HADES_SYS

#include <filesystem>
#include <iostream>
#include "hades/base/data.h"
#include "cassert"

namespace hades {
namespace fs = std::filesystem;

class Unimplemented: public std::exception {
  String m_message;
public:
  Unimplemented(String message = "Unimplemented") noexcept;
  const char* what() const noexcept override {
    return "Unimplemented";
  }
};

[[noreturn]]
inline auto unimplemented(String message = "Unimplemented") -> void {
  throw Unimplemented(message);
}

} // namespace hades

#endif
