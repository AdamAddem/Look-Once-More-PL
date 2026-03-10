#pragma once


enum class Mutability : bool {
  IMMUTABLE,
  MUTABLE,
};

enum class StealState : bool {
  NOT_STOLEN,
  STOLEN,
};

enum class PointerType {
  RAW,
  UNIQUE,
  VAGUE
};
