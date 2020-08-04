//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_REQUESTS_GETBUILDFLAGSREQUEST_H
#define HADES_REQUESTS_GETBUILDFLAGSREQUEST_H

#include "hades/ast/SourceFile.h"
#include "hades/core/CommandLineFlags.h"
#include "llvm/IR/LLVMContext.h"

namespace hades::requests {

template <typename Result>
class Request {
};

using ExitCode = uint32_t;
struct BuildObjectFileRequest: public Request<ExitCode> {
private:
  const CommandLineFlags* m_flags;
public:
  BuildObjectFileRequest(const CommandLineFlags* flags): m_flags(flags) {}
  auto flags() const noexcept -> const CommandLineFlags& { return *m_flags; }
};

struct GetParsedSourceFileRequest: public Request<SourceFile> {
  const fs::path* path;
};

struct CodegenRequest: public Request<llvm::Module> {};

} // namespace hades::requests

#endif // HADES_REQUESTs_GETBUILDFLAGSREQUEST_H
