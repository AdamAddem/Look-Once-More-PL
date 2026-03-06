#pragma once
#include <filesystem>
#include <memory>
#include <string>
#include <vector>

namespace Validation {
  struct ValidatedTU;
}

class Backend {
protected:
  Backend() = default;
public:
  virtual std::filesystem::path createASMFile   (const std::filesystem::path &file) = 0;
  virtual std::filesystem::path createIRFile    (const std::filesystem::path &file) = 0;
  virtual std::filesystem::path createObjectFile(const std::filesystem::path &file) = 0;

  static std::unique_ptr<Backend> codegen(const Validation::ValidatedTU&, const std::filesystem::path& file);
  static void linkObjects(const std::vector<std::filesystem::path>&  obj_paths);

  virtual ~Backend() = default;
};

