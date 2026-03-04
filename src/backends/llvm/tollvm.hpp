#pragma once
#include <filesystem>
#include <string>
#include <vector>

namespace Validation {
  struct ValidatedTU;
}

namespace ToLLVM {


//Each returns the filename they've created
std::filesystem::path createASMFile(Validation::ValidatedTU&&, const std::string &filename);
std::filesystem::path createIRFile(Validation::ValidatedTU&&, const std::string &filename);
std::filesystem::path createObjectFile(Validation::ValidatedTU&&, const std::string &filename);

void linkObjects(const std::vector<std::filesystem::path>& obj_paths);

}
