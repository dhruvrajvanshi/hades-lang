//
// Created by dhruv on 06/08/20.
//

#include "hades/base/sys.h"

namespace hades {

Unimplemented::Unimplemented(std::string message) noexcept
    : m_message{std::move(message)} {}

} // namespace hades
