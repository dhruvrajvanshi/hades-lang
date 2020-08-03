//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_FLAGS_BUILDFLAGS_H
#define HADES_FLAGS_BUILDFLAGS_H

#include "hades/base/data.h"
#include "hades/base/sys.h"

namespace hades {

class FlagParseError {
  Vec<String> m_messages;

public:
  FlagParseError(Vec<String> messages) : m_messages(std::move(messages)) {}

  auto messages() const -> const Vec<String> & { return m_messages; }
};
class CommandLineFlags {
  Vec<fs::path> m_sources;
  Vec<fs::path> m_directories;
  fs::path m_output;

  CommandLineFlags(Vec<fs::path> sources, Vec<fs::path> directories,
                   fs::path output)
      : m_sources(std::move(sources)), m_directories(std::move(directories)),
        m_output(std::move(output)){};

public:
  static auto parse(const Vec<String> &)
      -> Result<CommandLineFlags, FlagParseError>;
};

auto operator<<(std::ostream &os, const FlagParseError &self)
    -> std::ostream &;

} // namespace hades

#endif // HADES_FLAGS_BUILDFLAGS_H
