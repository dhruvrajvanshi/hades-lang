//
// Created by dhruv on 03/08/20.
//
#include "hades/core/CommandLineFlags.h"

namespace hades {

auto find_flag_range(const char *flag_name, const Vec<String> &flags)
-> Tuple<u32, u32>;

template <typename T>
auto single(const char *flag_name, const Vec<String> &flags) -> Optional<T> {
  auto [start, stop] = find_flag_range(flag_name, flags);
  if (start < flags.size()) {
    return optional::some(T(flags[start]));
  } else {
    return optional::none<T>();
  }
}

template <typename T>
auto many(const char *flag_name, const Vec<String> &flags) -> Vec<T> {
  auto result = Vec<T>();
  auto [start, stop] = find_flag_range(flag_name, flags);
  for (auto i = start; i < stop && i < flags.size(); i++) {
    result.emplace_back(flags[i]);
  }
  return result;
}

auto CommandLineFlags::parse(const Vec<String>& flags) -> Result<CommandLineFlags, FlagParseError> {
  auto output = single<fs::path>("--output", flags);
  auto directories = many<fs::path>("--directories", flags);
  auto sources = many<fs::path>("--sources", flags);
  auto errors = Vec<String>();

  if (!output.hasValue()) {
    errors.push_back("Missing flag: --output");
  }

  if (sources.empty()) {
    errors.push_back("Missing flags: --sources");
  }

  if (!errors.empty()) {
    return Result<CommandLineFlags, FlagParseError>(FlagParseError(errors));
  }

  auto result = CommandLineFlags(sources, directories, output.getValue());
  return Result<CommandLineFlags, FlagParseError>(result);
}

auto find_flag_range(const char *flag_name, const Vec<String> &flags)
-> Tuple<u32, u32> {
  auto start = 0;
  while (start < flags.size()) {
    if (flags[start] == flag_name) {
      start++;
      break;
    }
    start++;
  }
  if (start >= flags.size()) {
    return {start, start};
  }
  auto stop = start;
  while (stop < flags.size()) {
    if (flags[stop].starts_with("--")) {
      break;
    }
    stop++;
  }
  return {start, stop};
}
auto operator<<(std::ostream &os, const FlagParseError &self)
-> std::ostream & {
  for (auto message : self.messages()) {
    os << message << std::endl;
  }
  return os << std::endl;
}
} // namespace hades
