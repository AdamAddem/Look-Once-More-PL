#pragma once
#include "build_system/build.hpp"
#include "module/table_and_module_sync.hpp"

#include "edenlib/shortened_macros.hpp"
#include "edenlib/utils.hpp"
#include "edenlib/type_aliases.hpp"

#include <cassert>
#include <utility>

namespace LOM {

class PrimitiveType;
class ArrayType;
class CustomType;

struct PtrBits {
  constexpr PtrBits() noexcept = default;

#define pre Assume(ptr_level >= 1 and ptr_level <= 4);
  constexpr explicit PtrBits(u8_t ptr_level, bool l1_raw, bool l2_raw = false, bool l3_raw = false, bool l4_raw = false) noexcept { pre

    // set ptr level flag
    flags |= levelmask_for_level(ptr_level);

    // set ptr raw flags
    {
      auto const raw_flags = l1_raw_mask * u8_t(l1_raw) | l2_raw_mask * u8_t(l2_raw) | l3_raw_mask * u8_t(l3_raw) | l4_raw_mask * u8_t(l4_raw);
      flags |= raw_flags;
    }

  }
#undef pre

  InlineNodiscardCXPR u8_t pointer_level() const noexcept {
    u8_t const ptr_level_flags = flags & level_mask;
    return std::bit_width<u8_t>(ptr_level_flags);
  }

#define pre assert(ptr_level >= 1 and ptr_level <= pointer_level());
  InlineNodiscardCXPR bool isLevelRaw(u8_t ptr_level) const noexcept { pre return flags bitand rawmask_for_level(ptr_level); }
#undef pre

#define pre assert(pointer_level() >= 1);
  InlineNodiscardCXPR bool isTopLevelRaw() const noexcept { pre return isLevelRaw(pointer_level()); }
#undef pre

  // returns true if this has just become a pointer.
  // as in, pointer level is no longer 0
#define pre assert(pointer_level() < 4);
  InlineCXPR bool addLevel(bool is_level_raw) noexcept { pre
    auto const new_level = pointer_level() + 1;
    auto const new_level_flag = levelmask_for_level(new_level);
    u8_t const new_raw_flags = (flags & raw_mask) | (rawmask_for_level(new_level) * u8_t(is_level_raw));
    flags = new_level_flag bitor new_raw_flags;
    return new_level == 1;
  }
#undef pre

  // returns true if this is no longer a pointer
  // as in, pointer level is now 0
#define pre assert(pointer_level() >= 1);
  InlineCXPR bool removeLevel() noexcept { pre
    auto const old_level = pointer_level();
    auto const new_level = old_level - 1;
    auto const new_level_flag = levelmask_for_level(new_level);

    auto const rawmask_for_old_level = rawmask_for_level(old_level);
    auto const new_raw_flags = flags bitand rawmask_for_old_level;
    flags = new_level_flag bitor new_raw_flags;
    return new_level == 0;
  }
#undef pre

  InlineCXPR void removeAllLevels() noexcept { flags = 0; }

  InlineNodiscardCXPR bool operator==(PtrBits const&) const noexcept = default;
private:

  static constexpr u8_t l1_ptr_mask = 1 << 0;
  static constexpr u8_t l2_ptr_mask = 1 << 1;
  static constexpr u8_t l3_ptr_mask = 1 << 2;
  static constexpr u8_t l4_ptr_mask = 1 << 3;
  static constexpr u8_t level_mask = l1_ptr_mask | l2_ptr_mask | l3_ptr_mask | l4_ptr_mask;
  InlineNodiscardCXPR static u8_t levelmask_for_level(u8_t ptr_level) noexcept {
    auto const n = u32_t(ptr_level - 1) & u32_t(0b1111); // when ptr_level is 0, this will overflow then be truncated to 15
    auto const m = u8_t(1 << n);  // truncation is necessary to prevent shifting 1 by a large amount
    return m;
  }

  static constexpr u8_t l1_raw_mask = 1 << 4;
  static constexpr u8_t l2_raw_mask = 1 << 5;
  static constexpr u8_t l3_raw_mask = 1 << 6;
  static constexpr u8_t l4_raw_mask = 1 << 7;
  static constexpr u8_t raw_mask = l1_raw_mask | l2_raw_mask | l3_raw_mask | l4_raw_mask;
  InlineNodiscardCXPR static u8_t rawmask_for_level(u8_t ptr_level) noexcept { return 1 << (ptr_level + 3); }

  u8_t flags{};
};

struct Qualifiers {
  bool writable : 1 = false;
  bool nullable : 1 = false;
  u8_t _pad : 6 = 0;

  NodiscardCXPR bool operator==(Qualifiers const&) const noexcept = default;
}; static_assert(sizeof(Qualifiers) == 1, "If increased this would make stuff fatter than intended");

struct TypeID {
  enum class PointedType : u8_t {
    DEVOID, ERROR, PRIMITIVE, ARRAY, CUSTOM
  };
  using enum PointedType;
  
  PointedType derived{}; // the derived type of the most pointed thing. for a pointer to int, derived is primitive. for pointer to pointer to array, derived is array
  PtrBits ptr_specs{};
  u16_t module_id{};
  u32_t id{};

  InlineNodiscardCXPR sz_t pointer_level() const noexcept { return ptr_specs.pointer_level(); }
  InlineCXPR void addPointer(bool is_raw) noexcept { ptr_specs.addLevel(is_raw); }
  InlineCXPR void removePointer() noexcept { ptr_specs.removeLevel(); }

#define pre assert(isPointer());
  InlineNodiscardCXPR bool isTopLevelPtrRaw() const noexcept { pre return ptr_specs.isTopLevelRaw(); }
#undef pre

  InlineNodiscardCXPR bool isPointer()    const noexcept { return ptr_specs.pointer_level() != 0; }
  InlineNodiscardCXPR bool isDevoid()     const noexcept { return not isPointer() and derived == DEVOID;}
  InlineNodiscardCXPR bool isError()      const noexcept { return not isPointer() and derived == ERROR;}
  InlineNodiscardCXPR bool isPrimitive()  const noexcept { return not isPointer() and derived == PRIMITIVE; }
  InlineNodiscardCXPR bool isArray()      const noexcept { return not isPointer() and derived == ARRAY; }
  InlineNodiscardCXPR bool isCustom()     const noexcept { return not isPointer() and derived == CUSTOM; }

  NodiscardCXPR bool isBool()             const noexcept;
  NodiscardCXPR bool isIntegral()         const noexcept;
  NodiscardCXPR bool isUnsignedIntegral() const noexcept;
  NodiscardCXPR bool isSignedIntegral()   const noexcept;
  NodiscardCXPR bool isFloating()         const noexcept;
  NodiscardCXPR bool isArithmetic()       const noexcept;
  NodiscardCXPR sz_t bitwidth()           const noexcept;

  InlineNodiscardCXPR bool operator==(TypeID const&) const noexcept = default;

  // only use this when typeID truly refers to a primitive
  InlineNodiscardCXPR PrimitiveType const& getPrimitiveType() const noexcept;

  // if this is raw pointer to ref pointer to i32, returns TypeID(ref pointer to i32)
#define pre assert(isPointer());
  InlineNodiscardCXPR TypeID 
  getPointedTypeID() const noexcept { pre
    auto cpy = *this;
    cpy.ptr_specs.removeLevel();
    return cpy;
  }
#undef pre

  // if this is raw pointer to ref pointer to i32, returns TypeID(i32)
#define pre assert(isPointer());
  InlineNodiscardCXPR TypeID getMostPointedTypeID() const noexcept { pre
    auto cpy = *this;
    cpy.ptr_specs.removeAllLevels();
    return cpy;
  }
#undef pre

  [[nodiscard]] ArrayType const& getArrayType()     const noexcept;
  [[nodiscard]] CustomType const& getCustomType()    const noexcept;

  [[nodiscard]] bool coercibleTo(TypeID) const noexcept;
  [[nodiscard]] bool castableTo(TypeID) const noexcept;
  [[nodiscard]] bool sameAs(TypeID) const noexcept;

  NoInlineCold [[nodiscard]] std::string toString() const noexcept;
};

class PrimitiveType final {
friend class Module;
public:
  enum class PrimitiveTypeEnum : u8_t {
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64,
    CHAR, STRING, BOOL,
  };
  using enum PrimitiveTypeEnum;

private:
  PrimitiveTypeEnum primitive_type;
  bool is_smaller_literal{false}; // true only for unsigned literals which fit into both signed and unsigned of their size (ex: 5 is U8 but its value can fit in I8, 128 is U8 but cannot fit in I8)

  constexpr PrimitiveType(PrimitiveTypeEnum type, bool is_smaller_literal = false) noexcept : primitive_type(type), is_smaller_literal(is_smaller_literal) {}

  NodiscardCXPR static PrimitiveType const&
  getTypeFromID(u32_t id) {
    static constexpr PrimitiveType ts[] {
      {U8}, {U16}, {U32}, {U64},
      {I8}, {I16}, {I32}, {I64},
      {F32}, {F64},
      {CHAR}, {STRING}, {BOOL},

      {U8, true}, {U16, true}, {U32, true},
    };
    return ts[id];
  }

public:

#define pre Assume(typeID.derived == TypeID::PRIMITIVE);
  NodiscardCXPR static PrimitiveType const& getTypeFromID(TypeID typeID) { pre return getTypeFromID(typeID.id); }
#undef pre

  InlineNodiscardCXPR PrimitiveTypeEnum getUnderlyingPrimitiveType() const noexcept { return primitive_type; }

  InlineNodiscardCXPR bool isIntegral()         const noexcept { return eden::enumBetween(primitive_type, U8, I64); }
  InlineNodiscardCXPR bool isSignedIntegral()   const noexcept { return eden::enumBetween(primitive_type, I8, I64); }
  InlineNodiscardCXPR bool isUnsignedIntegral() const noexcept { return eden::enumBetween(primitive_type, U8, U64); }
  InlineNodiscardCXPR bool isFloating()         const noexcept { return eden::enumBetween(primitive_type, F32, F64); }
  InlineNodiscardCXPR bool isArithmetic()       const noexcept { return eden::enumBetween(primitive_type, PrimitiveTypeEnum{}, F64); }
  InlineNodiscardCXPR bool isBool()             const noexcept { return primitive_type == BOOL; }
  InlineNodiscardCXPR bool isChar()             const noexcept { return primitive_type == CHAR; }
  InlineNodiscardCXPR bool isString()           const noexcept { return primitive_type == STRING; }

  NodiscardCXPR sz_t bitwidth() const noexcept {
    switch (primitive_type) {
    case I8:  case U8:  case BOOL: case CHAR: return 8;
    case I16: case U16:           return 16;
    case I32: case U32: case F32: return 32;
    case I64: case U64: case F64: return 64;
    default: Unreachable("Invalid primitive type enum.");
    }
  }

#define type_singleton_decl(type_name) \
  [[nodiscard]] static consteval TypeID type_name ## ID() noexcept; \
  [[nodiscard]] static consteval PrimitiveType const& type_name () noexcept;

  type_singleton_decl(i8)
  type_singleton_decl(i16)
  type_singleton_decl(i32)
  type_singleton_decl(i64)

  type_singleton_decl(u8)
  type_singleton_decl(u16)
  type_singleton_decl(u32)
  type_singleton_decl(u64)

  type_singleton_decl(f32)
  type_singleton_decl(f64)

  type_singleton_decl(bool_)
  type_singleton_decl(char_)
  type_singleton_decl(string)
#undef type_singleton_decl

  NoInlineCold [[nodiscard]] std::string toString() const noexcept;
};

#define type_singleton(type_name, type_enum) \
  [[nodiscard]] consteval TypeID \
  PrimitiveType::type_name ## ID() noexcept \
  { return { .derived = TypeID::PRIMITIVE, .id = std::to_underlying(type_enum) }; } \
  [[nodiscard]] consteval PrimitiveType const& \
  PrimitiveType:: type_name() noexcept \
  { return PrimitiveType::getTypeFromID(PrimitiveType::type_name ## ID ()); }
  
  type_singleton(i8, I8)
  type_singleton(i16, I16)
  type_singleton(i32, I32)
  type_singleton(i64, I64)

  type_singleton(u8, U8)
  type_singleton(u16, U16)
  type_singleton(u32, U32)
  type_singleton(u64, U64)

  type_singleton(f32, F32)
  type_singleton(f64, F64)

  type_singleton(char_, CHAR)
  type_singleton(string, STRING)
  type_singleton(bool_, BOOL)
#undef type_singleton

InlineNodiscardCXPR bool TypeID::isBool()             const noexcept { return isPrimitive() and getPrimitiveType().isBool(); }
InlineNodiscardCXPR bool TypeID::isIntegral()         const noexcept { return isPrimitive() and getPrimitiveType().isIntegral(); }
InlineNodiscardCXPR bool TypeID::isSignedIntegral()   const noexcept { return isPrimitive() and getPrimitiveType().isSignedIntegral(); }
InlineNodiscardCXPR bool TypeID::isUnsignedIntegral() const noexcept { return isPrimitive() and getPrimitiveType().isUnsignedIntegral(); }
InlineNodiscardCXPR bool TypeID::isFloating()         const noexcept { return isPrimitive() and getPrimitiveType().isFloating(); }
InlineNodiscardCXPR bool TypeID::isArithmetic()       const noexcept { return isPrimitive() and getPrimitiveType().isArithmetic(); }

#define pre assert(isPrimitive());
InlineNodiscardCXPR PrimitiveType const& TypeID::getPrimitiveType() const noexcept { pre return PrimitiveType::getTypeFromID(*this); }
#undef pre

class ArrayType final {
  friend class Module;
  TypeID subtypeID;
  u64_t size;

#define pre Assume(array_size not_eq 0);
  constexpr explicit ArrayType(TypeID subtypeID, u64_t array_size) noexcept : subtypeID(subtypeID), size(array_size) { pre }
#undef pre

public:

  InlineNodiscardCXPR bool sameAs(ArrayType const& other) const noexcept {
    if (size != other.size) return false;
    return subtypeID.sameAs( other.subtypeID );
  }
  InlineNodiscardCXPR u64_t  getSize()      const noexcept { return size; }
  InlineNodiscardCXPR TypeID getSubtypeID() const noexcept { return subtypeID; }

  InlineNodiscardCXPR bool operator==(ArrayType const& other) const noexcept { return eden::areBitwiseEqual(this, &other); }

  NoInlineCold [[nodiscard]] std::string toString() const noexcept;
};

class SymbolTable;
class CustomType final {
  friend class SymbolTable; friend class Module;

  alignas(SYMBOL_TABLE_ALIGNMENT)
  std::byte symboltable_buff[SYMBOL_TABLE_SIZE];

  [[nodiscard]] SymbolTable& member_table() noexcept;
public:
  [[nodiscard]] SymbolTable const& member_table() const noexcept;

  CustomType() noexcept;
};

NodiscardCXPR sz_t
TypeID::bitwidth() const noexcept {
  if (isPointer()) return sizeof(void*) * 8;

  switch (derived) {
  case ERROR:     return sizeof(void*) * 8;
  case PRIMITIVE: return getPrimitiveType().bitwidth();

  case ARRAY: {
    auto const& array_type = getArrayType();
    return array_type.getSize() * array_type.getSubtypeID().bitwidth();
  }

  case CUSTOM:
  default:
    Unreachable("Unimplemented type to assess bitwidth.");
  }
}

inline constexpr TypeID devoidID{ .derived = TypeID::DEVOID, .id = 0 };
inline constexpr TypeID errorID{ .derived = TypeID::ERROR, .id = 1 };

NodiscardCXPR static TypeID
signedToLiteralTypeID(i64_t val) noexcept {
  if (std::in_range<i8_t>(val))  return { .derived = TypeID::PRIMITIVE, .id = u32_t(PrimitiveType::I8)  };
  if (std::in_range<i16_t>(val)) return { .derived = TypeID::PRIMITIVE, .id = u32_t(PrimitiveType::I16) };
  if (std::in_range<i32_t>(val)) return { .derived = TypeID::PRIMITIVE, .id = u32_t(PrimitiveType::I32) };
  return { .derived = TypeID::PRIMITIVE, .id = u32_t(PrimitiveType::I64) };
}

NodiscardCXPR static TypeID
unsignedToLiteralTypeID(u64_t val) noexcept {
  static constexpr auto literal_offset = (u32_t) std::to_underlying(PrimitiveType::BOOL) + 1;
  if (std::in_range<u8_t>(val))  return { .derived = TypeID::PRIMITIVE, .id = u32_t(PrimitiveType::U8) + u32_t(val <= u8_t(i8_max)) * literal_offset  };
  if (std::in_range<u16_t>(val)) return { .derived = TypeID::PRIMITIVE, .id = u32_t(PrimitiveType::U16) + u32_t(val <= u8_t(i16_max)) * literal_offset  };
  if (std::in_range<u32_t>(val)) return { .derived = TypeID::PRIMITIVE, .id = u32_t(PrimitiveType::U32) + u32_t(val <= u8_t(i16_max)) * literal_offset  };
  return { .derived = TypeID::PRIMITIVE, .id = u32_t(PrimitiveType::U64) };
}

}
