//
// Created by dhruv on 08/08/20.
//

#ifndef HADES_TYPERESOLVER_H
#define HADES_TYPERESOLVER_H

#include "hades/base.h"

namespace hades {
class TypeResolverImpl;
class TypeResolver {
  UniquePtr<TypeResolverImpl> m_impl;
public:
  TypeResolver() noexcept;
  ~TypeResolver() noexcept;
  HADES_DEFAULT_MOVE(TypeResolver)
private:
  auto self() -> TypeResolverImpl&;
};

} // namespace hades

#endif // HADES_TYPERESOLVER_H
