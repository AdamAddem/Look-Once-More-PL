#pragma once


enum class Mutability : bool {
  IMMUTABLE,
  MUTABLE,
};

enum class StealState : bool {
  NOT_STOLEN,
  STOLEN,
};

enum class PrimitiveType {
  I8, I16, I32, I64,
  U8, U16, U32, U64,
  F32, F64,
  BOOL, CHAR, STRING, DEVOID,

  RAW,
  UNIQUE,
  VAGUE
};

enum class PointerType {
  RAW,
  UNIQUE,
  VAGUE
};
