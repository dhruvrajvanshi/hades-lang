//
// Created by dhruv on 07/08/20.
//
#include "hades/ast/FunctionSignature.h"

namespace hades {

FunctionSignature::FunctionSignature(   //
    SourceLocation location,            //
    Identifier name,                    //
    FunctionSignature::Params &&params, //
    Optional<const TypeAnnotation *> return_type  //
    ) noexcept                          //
    : m_location(location),             //
      m_name(name),                     //
      m_params(params),                 //
      m_return_type_annotation(return_type)        //
{}

auto FunctionSignature::location() const -> const SourceLocation & {
  return m_location;
}
auto FunctionSignature::params() const -> ArrayRef<const Param *> {
  return hades::ArrayRef<const Param *>(m_params);
}

auto FunctionSignature::return_type_annotation() const -> const Optional<const TypeAnnotation *> & {
  return m_return_type_annotation;
}

auto FunctionSignature::name() const -> const Identifier & {
  return m_name;
}

} // namespace hades
