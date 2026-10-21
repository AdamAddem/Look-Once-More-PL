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
  virtual void createASMFile() = 0;
  virtual void createIRFile() = 0;
  virtual void createObjectFile() = 0;

  static std::unique_ptr<Backend> codegen(PeepIR::TU&);
  static void linkObjects();

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