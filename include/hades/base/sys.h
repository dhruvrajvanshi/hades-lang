#ifndef HADES_SYS
#define HADES_SYS

#include "cassert"
#include "hades/base/data.h"
#include <filesystem>
#include <iostream>

namespace hades {
namespace fs = std::filesystem;

using Path = fs::path;

namespace path {

auto replace_extension(const Path &path, String &&new_extension) -> Path;

} // namespace path

class Unimplemented : public std::exception {
  String m_message;

public:
  Unimplemented(String message = "Unimplemented") noexcept;
  const char *what() const noexcept override { return "Unimplemented"; }
};

[[noreturn]] inline auto unimplemented(String message = "Unimplemented")
    -> void {
  throw Unimplemented(message);
}

} // namespace hades

#endif
