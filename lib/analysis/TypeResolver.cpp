//
// Created by dhruv on 08/08/20.
//
#include "hades/analysis/TypeResolver.h"
#include "TypeResolverImpl.h"

namespace hades {

TypeResolver::TypeResolver() noexcept : m_impl{new TypeResolverImpl()} {}
TypeResolver::~TypeResolver() noexcept = default;

} // namespace hades
