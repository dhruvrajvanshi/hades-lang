//
// Created by dhruv on 03/08/20.
//

#ifndef HADES_FLAGS_BUILDFLAGS_H
#define HADES_FLAGS_BUILDFLAGS_H

#include "hades/base.h"

namespace hades {

class FlagParseError {
  Vec<String> m_messages;

public:
  FlagParseError(Vec<String> messages) : m_messages(std::move(messages)) {}
  HADES_DEFAULT_COPY(FlagParseError)
  HADES_DEFAULT_MOVE(FlagParseError)

  auto messages() const -> const Vec<String> & { return m_messages; }
};
static_assert(std::is_move_constructible_v<FlagParseError>);
static_assert(std::is_move_assignable_v<FlagParseError>);
static_assert(std::is_copy_constructible_v<FlagParseError>);
static_assert(std::is_copy_assignable_v<FlagParseError>);

class CommandLineFlags {
  Vec<fs::path> m_sources;
  Vec<fs::path> m_directories;
  fs::path m_output;

public:
  static auto parse(const Vec<String> &)
      -> Result<CommandLineFlags, FlagParseError>;

  CommandLineFlags(Vec<fs::path> sources, Vec<fs::path> directories,
                   fs::path output)
      : m_sources(std::move(sources)), m_directories(std::move(directories)),
        m_output(std::move(output)){};

  HADES_DEFAULT_MOVE(CommandLineFlags)
  HADES_DEFAULT_COPY(CommandLineFlags)
  auto sources() const noexcept -> const Vec<fs::path>&;
  auto directories() const noexcept -> const Vec<fs::path>&;
  auto output() const noexcept -> const fs::path&;
};

auto operator<<(std::ostream &os, const FlagParseError &self) -> std::ostream &;

} // namespace hades

#endif // HADES_FLAGS_BUILDFLAGS_H
