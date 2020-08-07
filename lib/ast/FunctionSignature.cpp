//
// Created by dhruv on 07/08/20.
//
#include "hades/ast/FunctionSignature.h"

namespace hades {

FunctionSignature::FunctionSignature(  //
    SourceLocation location,           //
    Identifier name,                   //
    FunctionSignature::Params params,  //
    Optional<const Type *> return_type //
    ) noexcept                         //
    : m_location(location),            //
      m_name(name),                    //
      m_params(params),                //
      m_return_type(return_type)       //
{}

auto FunctionSignature::add_param(FunctionSignature::Params &params,
                                  const Param *param) -> void {
  params.append({std::move(param)});
}

} // namespace hades
