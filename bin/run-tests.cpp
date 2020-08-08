#include "hades/base/data.h"
#include "hades/base/sys.h"
#include "hades/context/Context.h"
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
    if (entry.path().string().find("bug") == String::npos) {
      continue;
    }
    if (entry.path().extension() != ".hds") {
      continue;
    }
    auto filename_without_extension = entry.path().stem();
    auto stdout_file = entry.path();
    stdout_file.replace_extension(".stdout");

    auto output_file_name = path::replace_extension(filename_without_extension, "");
    auto out_file = test_build_directory / output_file_name;

    Vec<String> args{
        "--sources",     entry.path(),        //
        "--directories", test_directory,      //
        "--output",      out_file  //
    };

    auto ctx_result = hades::core::Context::from_args(args);
    auto&& ctx = ctx_result.get_value();
    auto exit_code = ctx->run();

    assert(fs::exists(out_file) && "Expected an executable file to exist after compilation");

    if (exit_code != 0) {
      std::cout << "[FAIL]: " << entry << "\n";
      continue;
    }
    std::cout << "[PASS]: " << entry << "\n";
  }
  return 0;
}
