//
// Created by dhruv on 07/08/20.
//
#include "hades/ast/FunctionSignature.h"

namespace hades {

FunctionSignature::FunctionSignature(   //
    SourceLocation location,            //
    Identifier name,                    //
    FunctionSignature::Params &&params, //
    Optional<const Type *> return_type  //
    ) noexcept                          //
    : m_location(location),             //
      m_name(name),                     //
      m_params(params),                 //
      m_return_type(return_type)        //
{}

auto FunctionSignature::location() const -> const SourceLocation & {
  return m_location;
}
auto FunctionSignature::params() const -> ArrayRef<const Param *> {
  return hades::ArrayRef<const Param *>(m_params);
}

auto FunctionSignature::return_type() const -> const Optional<const Type *> & {
  return m_return_type;
}

auto FunctionSignature::name() const -> const Identifier & {
  return m_name;
}

} // namespace hades
