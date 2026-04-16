#pragma once
#include <filesystem>
#include <memory>
#include <string>
#include <vector>


namespace LOM::PeepMIR {
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

  static std::unique_ptr<Backend> codegen(PeepMIR::TU&&, const std::filesystem::path& file);
  static void linkObjects(const std::vector<std::filesystem::path>&  obj_paths);

  virtual ~Backend() = default;
};
}