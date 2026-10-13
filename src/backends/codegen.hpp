#pragma once
#include <filesystem>
#include <memory>
#include <string_view>

namespace LOM::PeepIR { struct TU; }

namespace LOM {

class Backend {
protected:
  Backend() = default;
public:
  virtual void createASMFile   (std::filesystem::path const& file) = 0;
  virtual void createIRFile    (std::filesystem::path const& file) = 0;

  // returns the
  virtual std::filesystem::path createObjectFile(std::filesystem::path const& file) = 0;

  static std::unique_ptr<Backend> codegen(PeepIR::TU&, std::filesystem::path const& file);
  static void linkObjects(std::span<std::filesystem::path> obj_paths);

  virtual ~Backend() = default;
};

#ifdef _WIN32
inline constexpr std::string_view obj_extension = ".obj";
inline constexpr std::string_view asm_extension = ".asm";
#else
inline constexpr std::string_view obj_extension = ".o";
inline constexpr std::string_view asm_extension = ".s";
#endif

static constexpr std::string_view asm_folder = "build/asm/";
static constexpr std::string_view object_folder = "build/obj/";

}