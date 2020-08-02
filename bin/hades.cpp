#include "llvm/IR/LLVMContext.h"
#include "hades/core/Context.h"

auto main() -> int {
    hades::core::Context ctx;
    ctx.run();
    return 0;
}