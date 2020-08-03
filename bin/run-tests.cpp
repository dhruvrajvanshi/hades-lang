#include "hades/core/Context.h"
#include "hades/sys.h"
#include "iostream"

namespace fs = hades::fs;
using namespace hades;

auto main(int argc, const char **argv) -> int {
  std::cout << "Running tests\n";
  auto test_directory = fs::path("test");
  auto test_build_directory = test_directory.parent_path().append("test_build");
  if (fs::exists(test_build_directory)) {
    fs::remove_all(test_build_directory);
  }
  fs::create_directory(test_build_directory);
  for (auto entry : fs::directory_iterator(test_directory)) {
    if (entry.path().extension() == ".hds") {
      auto filename_without_extension = entry.path().stem();
      auto stdout_file = entry.path();
      stdout_file.replace_extension(".stdout");

      SmallVec<String, 16> args {
          entry.path(),
          "--directories", test_directory,
          "--output", test_build_directory
      };

      auto ctx = hades::core::Context(args);
      auto exit_code = ctx.run();

      if (exit_code != 0) {
        std::cout << "[FAIL]: " << entry << "\n";
        continue;
      }
      std::cout << "[PASS]: " << entry << "\n";
    }
  }
  return 0;
}
