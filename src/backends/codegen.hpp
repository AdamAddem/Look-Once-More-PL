#pragma once
#include "edenlib/vectors/vector.hpp"
#include <filesystem>
#include <memory>
#include <string_view>

namespace LOM::PeepIR {
  struct TU;
}

namespace LOM {

class Backend {
protected:
  Backend() = default;
public:
  virtual std::filesystem::path createASMFile   (const std::filesystem::path &file) = 0;
  virtual std::filesystem::path createIRFile    (const std::filesystem::path &file) = 0;
  virtual std::filesystem::path createObjectFile(const std::filesystem::path &file) = 0;

  static std::unique_ptr<Backend> codegen(PeepIR::TU&&, std::filesystem::path const& file);
  static void linkObjects(eden::vector<std::filesystem::path> const& obj_paths);

  virtual ~Backend() = default;
};

#ifdef _WIN32
inline constexpr std::string_view obj_extension = ".obj";
#else
inline constexpr std::string_view obj_extension = ".o";
#endif

}