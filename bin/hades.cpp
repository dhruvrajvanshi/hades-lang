#include "hades/core/Context.h"

auto main(int argc, const char** argv) -> int {
    auto ctx = hades::core::Context(argc, argv);
    return ctx.run();
}