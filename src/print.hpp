#pragma once
#include "edenlib/shortened_macros.hpp"
#include "file.hpp"

#include "edenlib/typedefs.hpp"
#include <span>

namespace LOM {
namespace Parser { struct TU; }
namespace PeepIR { struct TU; }

  NoInlineCold void printParsed() noexcept;
  NoInlineCold void printPeeped() noexcept;
  NoInlineCold void printFileErrors(File file) noexcept;
}