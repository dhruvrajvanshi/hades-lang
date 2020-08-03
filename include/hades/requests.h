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

struct GetBuildFlagsRequest: public Request<Result<CommandLineFlags, FlagParseError>> {
  const Vec<String>* args;
  GetBuildFlagsRequest(const Vec<String>* args): args{args} {}
};

using ExitCode = uint32_t;
struct BuildObjectFileRequest: public Request<ExitCode> {
  const Vec<String>* args;
  BuildObjectFileRequest(const Vec<String>* args): args(args) {}
};

struct GetParsedSourceFileRequest: public Request<SourceFile> {
  const fs::path* path;
};

struct CodegenRequest: public Request<llvm::Module> {};

} // namespace hades::requests

#endif // HADES_REQUESTs_GETBUILDFLAGSREQUEST_H
