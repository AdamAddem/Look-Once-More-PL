#pragma once
#include "edenlib/bitwise_utils.hpp"
#include "edenlib/enum_utils.hpp"
#include "edenlib/macros.hpp"
#include "edenlib/typedefs.hpp"

#include "build_system/build.hpp"
#include "module/table_and_module_sync.hpp"
#include "settings.hpp"

#include <cassert>
#include <cstring>
#include <limits>
#include <span>
#include <utility>
#include <print>

namespace LOM {

class PrimitiveType;
class ArrayType;
class CustomType;

class Type {
  friend class Module;
public:
  enum class DerivedType : u8_t {DEVOID, ERROR, PRIMITIVE, ARRAY, CUSTOM};
  using enum DerivedType;
private:
  DerivedType derived_type{DEVOID};

  // should only be called for the error type
  consteval Type() : derived_type(ERROR) {}

protected: constexpr explicit Type(DerivedType derived_type) : derived_type(derived_type) {}
public:
  InlineNodiscardCXPR DerivedType getDerivedType() const noexcept { return derived_type; }

  struct Qualifiers {
    bool writable : 1 = false;
    bool nullable : 1 = false;
    u8_t _pad : 6 = 0;

    edenNodiscardCXPR bool operator==(Qualifiers const&) const noexcept = default;
  }; static_assert(sizeof(Qualifiers) == 1, "If increased this would make stuff fatter than intended");

  template<std::derived_from<Type> T>
  static consteval DerivedType corresponding_derived_type() {
    if      constexpr(eden::same_c<T, PrimitiveType>) return PRIMITIVE;
    else if constexpr(eden::same_c<T, ArrayType>)     return ARRAY;
    else if constexpr(eden::same_c<T, CustomType>)    return CUSTOM;
    else { static_assert(false); return {}; }
  }

  [[nodiscard]] static consteval Type const& devoid() noexcept  { static constexpr Type devoid{DEVOID}; return devoid; }
  [[nodiscard]] static consteval Type const& error()  noexcept  { static constexpr Type error;          return error; }

  InlineNodiscardCXPR bool isDevoid()     const noexcept { return derived_type == DEVOID;}
  InlineNodiscardCXPR bool isError()      const noexcept { return derived_type == ERROR;}
  InlineNodiscardCXPR bool isPrimitive()  const noexcept { return derived_type == PRIMITIVE;}
  InlineNodiscardCXPR bool isArray()      const noexcept { return derived_type == ARRAY;}
  InlineNodiscardCXPR bool isCustom()     const noexcept { return derived_type == CUSTOM;}

  NodiscardCXPR bool isBool()             const noexcept;
  NodiscardCXPR bool isIntegral()         const noexcept;
  NodiscardCXPR bool isUnsignedIntegral() const noexcept;
  NodiscardCXPR bool isSignedIntegral()   const noexcept;
  NodiscardCXPR bool isFloating()         const noexcept;
  NodiscardCXPR bool isArithmetic()       const noexcept;

  // InlineNodiscardCXPR PrimitiveType const& castToPrimitive() const noexcept;
  // InlineNodiscardCXPR ArrayType     const& castToArray()     const noexcept;
  // InlineNodiscardCXPR CustomType    const& castToCustom()    const noexcept;

  InlineNodiscardCXPR PrimitiveType const& castToPrimitive() const noexcept { Assume(derived_type == PRIMITIVE); return (PrimitiveType const&)(*this); }
  InlineNodiscardCXPR ArrayType     const& castToArray()     const noexcept { Assume(derived_type == ARRAY);     return (ArrayType     const&)(*this); }
  InlineNodiscardCXPR CustomType    const& castToCustom()    const noexcept { Assume(derived_type == CUSTOM);    return (CustomType    const&)(*this); }

  Type(const Type&) = delete;
  void operator=(const Type&) = delete;

  Type(Type&&) noexcept = default;
  Type& operator=(Type &&) noexcept = default;
};

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

struct TypeID {
  Type::DerivedType derived{}; // the derived type of the most pointed thing. for a pointer to int, derived is primitive. for pointer to pointer to array, derived is array
  PtrBits ptr_specs{};
  u16_t module_id{};
  u32_t id{};

  InlineNodiscardCXPR sz_t pointer_level() const noexcept { return ptr_specs.pointer_level(); }
  InlineCXPR void addPointer(bool is_raw) noexcept { ptr_specs.addLevel(is_raw); }
  InlineCXPR void removePointer() noexcept { ptr_specs.removeLevel(); }

#define pre Assume(isPointer());
  InlineNodiscardCXPR bool isTopLevelPtrRaw() const noexcept { pre return ptr_specs.isTopLevelRaw(); }
#undef pre

  InlineNodiscardCXPR bool isPointer()    const noexcept { return ptr_specs.pointer_level() != 0; }
  InlineNodiscardCXPR bool isDevoid()     const noexcept { return not isPointer() and derived == Type::DEVOID;}
  InlineNodiscardCXPR bool isError()      const noexcept { return not isPointer() and derived == Type::ERROR;}
  InlineNodiscardCXPR bool isPrimitive()  const noexcept { return not isPointer() and derived == Type::PRIMITIVE; }
  InlineNodiscardCXPR bool isArray()      const noexcept { return not isPointer() and derived == Type::ARRAY; }
  InlineNodiscardCXPR bool isCustom()     const noexcept { return not isPointer() and derived == Type::CUSTOM; }

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
#define pre Assume(isPointer());
  InlineNodiscardCXPR TypeID getPointedTypeID() const noexcept { pre
    auto cpy = *this;
    cpy.ptr_specs.removeLevel();
    return cpy;
  }
#undef pre

  // if this is raw pointer to ref pointer to i32, returns TypeID(i32)
#define pre Assume(isPointer());
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

class PrimitiveType final : public Type {
  friend class Module;
public:
  enum class PrimitiveTypeEnum : u8_t { // do not change order between I8 and BOOL, ParsePrimitiveType in parser relies on this
    U7, U15, U31, U63, // unsigned literal whos values are compatable with both signed and unsigned
    U8, U16, U32, U64,
    I8, I16, I32, I64,
    F32, F64,
    CHAR, STRING, BOOL,
  };
  using enum PrimitiveTypeEnum;
  
private:
  static constexpr sz_t num_types = 17; static_assert(std::to_underlying(BOOL) == 16);
  PrimitiveTypeEnum primitive_type;

  constexpr PrimitiveType(PrimitiveTypeEnum type) noexcept : Type(PRIMITIVE), primitive_type(type) {}

  NodiscardCXPR static PrimitiveType const&
  getTypeFromID(u32_t id) {
    static constexpr PrimitiveType ts[num_types] {
      {U7}, {U15}, {U31}, {U63},
      {U8}, {U16}, {U32}, {U64},
      {I8}, {I16}, {I32}, {I64},
      {F32}, {F64},
      {CHAR}, {STRING}, {BOOL},
    };
    return ts[id];
  }

public:

#define pre Assume(typeID.derived == Type::PRIMITIVE);
  NodiscardCXPR static PrimitiveType const& getTypeFromID(TypeID typeID) { pre return getTypeFromID(typeID.id); }
#undef pre

  InlineNodiscardCXPR PrimitiveTypeEnum getUnderlyingPrimitiveType() const noexcept { return primitive_type; }

  InlineNodiscardCXPR bool isIntegral()         const noexcept { return eden::enumBetween(primitive_type, U7, I64); }
  InlineNodiscardCXPR bool isSignedIntegral()   const noexcept { return eden::enumBetween(primitive_type, I8, I64); }
  InlineNodiscardCXPR bool isUnsignedIntegral() const noexcept { return eden::enumBetween(primitive_type, U7, U64); }
  InlineNodiscardCXPR bool isFloating()         const noexcept { return eden::enumBetween(primitive_type, F32, F64); }
  InlineNodiscardCXPR bool isArithmetic()       const noexcept { return eden::enumBetween(primitive_type, U7, F64); }
  InlineNodiscardCXPR bool isBool()             const noexcept { return primitive_type == BOOL; }
  InlineNodiscardCXPR bool isChar()             const noexcept { return primitive_type == CHAR; }
  InlineNodiscardCXPR bool isString()           const noexcept { return primitive_type == STRING; }

  NodiscardCXPR sz_t bitwidth() const noexcept {
    switch (primitive_type) {
    case I8:
    case U7:
    case U8:
    case BOOL:
    case CHAR:
      return 8;

    case I16:
    case U15:
    case U16:
      return 16;

    case I32:
    case U31:
    case U32:
    case F32:
      return 32;

    case I64:
    case U63:
    case U64:
    case F64:
      return 64;
    default:
      std::unreachable();
    }
  }

#define type_singleton_decl(type_name) \
  [[nodiscard]] static consteval TypeID type_name ## ID() noexcept; \
  [[nodiscard]] static consteval PrimitiveType const& type_name () noexcept;

  type_singleton_decl(i8)
  type_singleton_decl(i16)
  type_singleton_decl(i32)
  type_singleton_decl(i64)

  type_singleton_decl(u7)
  type_singleton_decl(u15)
  type_singleton_decl(u31)
  type_singleton_decl(u63)

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
  { return { .derived = Type::PRIMITIVE, .id = std::to_underlying(type_enum) }; } \
  [[nodiscard]] consteval PrimitiveType const& \
  PrimitiveType:: type_name() noexcept \
  { return PrimitiveType::getTypeFromID(PrimitiveType::type_name ## ID ()); }
  
  type_singleton(i8, I8)
  type_singleton(i16, I16)
  type_singleton(i32, I32)
  type_singleton(i64, I64)

  type_singleton(u7, U7)
  type_singleton(u15, U15)
  type_singleton(u31, U31)
  type_singleton(u63, U63)

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

InlineNodiscardCXPR bool TypeID::isBool()             const noexcept { return isPrimitive() and ( id == std::to_underlying(PrimitiveType::BOOL) ); }
InlineNodiscardCXPR bool TypeID::isIntegral()         const noexcept { return isPrimitive() and eden::enumBetween(PrimitiveType::PrimitiveTypeEnum{ (u8_t) id }, PrimitiveType::U7, PrimitiveType::I64); }
InlineNodiscardCXPR bool TypeID::isSignedIntegral()   const noexcept { return isPrimitive() and eden::enumBetween(PrimitiveType::PrimitiveTypeEnum{ (u8_t) id}, PrimitiveType::I8, PrimitiveType::I64); }
InlineNodiscardCXPR bool TypeID::isUnsignedIntegral() const noexcept { return isPrimitive() and eden::enumBetween(PrimitiveType::PrimitiveTypeEnum{ (u8_t) id}, PrimitiveType::U7, PrimitiveType::U64); }
InlineNodiscardCXPR bool TypeID::isFloating()         const noexcept { return isPrimitive() and eden::enumBetween(PrimitiveType::PrimitiveTypeEnum{ (u8_t) id}, PrimitiveType::F32, PrimitiveType::F64); }
InlineNodiscardCXPR bool TypeID::isArithmetic()       const noexcept { return isPrimitive() and eden::enumBetween(PrimitiveType::PrimitiveTypeEnum{ (u8_t) id}, PrimitiveType::U7, PrimitiveType::F64); }

#define pre Assume(isPrimitive());
InlineNodiscardCXPR PrimitiveType const& TypeID::getPrimitiveType() const noexcept { pre return PrimitiveType::getTypeFromID(*this); }
#undef pre

InlineNodiscardCXPR bool Type::isBool()             const noexcept { return this == &PrimitiveType::i8(); }
InlineNodiscardCXPR bool Type::isIntegral()         const noexcept { return derived_type == PRIMITIVE and (PrimitiveType const*)(this)->isIntegral();}
InlineNodiscardCXPR bool Type::isUnsignedIntegral() const noexcept { return derived_type == PRIMITIVE and (PrimitiveType const*)(this)->isUnsignedIntegral();}
InlineNodiscardCXPR bool Type::isSignedIntegral()   const noexcept { return derived_type == PRIMITIVE and (PrimitiveType const*)(this)->isSignedIntegral();}
InlineNodiscardCXPR bool Type::isFloating()         const noexcept { return this == &PrimitiveType::f32() or this == &PrimitiveType::f64(); }
InlineNodiscardCXPR bool Type::isArithmetic()       const noexcept { return derived_type == PRIMITIVE and (PrimitiveType const*)(this)->isArithmetic(); }

class ArrayType final : public Type {
  friend class Module;
  byte_t _pad[3]{};
  TypeID subtypeID;
  u32_t array_size_upper; // stupid but reduces alignment and shaves 4bytes
  u32_t array_size;

#define pre Assume(array_size not_eq 0);
  constexpr explicit ArrayType(TypeID subtypeID, u64_t array_size) noexcept : Type(ARRAY), subtypeID(subtypeID), array_size_upper(array_size >> 32), array_size(array_size) { pre }
#undef pre

public:

  InlineNodiscardCXPR bool sameAs(ArrayType const& other) const noexcept {
    if (getSize() != other.getSize()) return false;
    return subtypeID.sameAs( other.subtypeID );
  }

  InlineNodiscardCXPR u64_t  getSize()      const noexcept { return (u64_t(array_size_upper) << 32) | u64_t(array_size); }
  InlineNodiscardCXPR TypeID getSubtypeID() const noexcept { return subtypeID; }

#define pre Assume(&other != this);
  InlineNodiscardCXPR bool operator==(ArrayType const& other) const noexcept { pre return eden::are_bitwise_equal_restrict(this, &other); }
#undef pre

  NoInlineCold [[nodiscard]] std::string toString() const noexcept;
};

class SymbolTable;
class CustomType final : public Type {
  friend class SymbolTable; friend class Module;

  byte_t _pad[3]{};
  u32_t name_len;
  char const* name;

  alignas(SYMBOL_TABLE_ALIGNMENT)
  std::byte symboltable_buff[SYMBOL_TABLE_SIZE];

  [[nodiscard]] SymbolTable& member_table() noexcept;

public:

  explicit CustomType(std::string_view name) noexcept;

  [[nodiscard]] SymbolTable const& member_table() const noexcept;

  InlineNodiscardCXPR std::string_view nameof()   const noexcept { return {name, name_len}; }
  InlineNodiscardCXPR std::string      toString() const noexcept { return std::string(nameof()); }

  [[nodiscard]] std::string definitionToString() const noexcept;
};

NodiscardCXPR sz_t
TypeID::bitwidth() const noexcept {
  if (isPointer()) return sizeof(void*) * 8;

  switch (derived) { using enum Type::DerivedType;
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

inline constexpr TypeID devoidID{ .derived = Type::DEVOID, .id = 0 };
inline constexpr TypeID errorID{ .derived = Type::ERROR, .id = 1 };

NodiscardCXPR static TypeID getPrimitiveLiteralID(PrimitiveType::PrimitiveTypeEnum primitive_type) noexcept { return { .derived = Type::PRIMITIVE, .id = std::to_underlying(primitive_type) }; };

NodiscardCXPR static TypeID
signedToLiteralTypeID(i64_t val) noexcept {
  val = val < 0 ? (val * -1) - 1 : val;
  if (val <= std::numeric_limits<i8_t>::max())  return getPrimitiveLiteralID(PrimitiveType::I8);
  if (val <= std::numeric_limits<i16_t>::max()) return getPrimitiveLiteralID(PrimitiveType::I16);
  if (val <= std::numeric_limits<i32_t>::max()) return getPrimitiveLiteralID(PrimitiveType::I32);
  return getPrimitiveLiteralID(PrimitiveType::I64);
}

NodiscardCXPR static TypeID
unsignedToLiteralTypeID(u64_t val) noexcept {
  if (val <= i8_max) return getPrimitiveLiteralID(PrimitiveType::U7);
  if (val <= u8_max) return getPrimitiveLiteralID(PrimitiveType::U8);

  if (val <= i16_max) return getPrimitiveLiteralID(PrimitiveType::U15);
  if (val <= u16_max) return getPrimitiveLiteralID(PrimitiveType::U16);

  if (val <= i32_max) return getPrimitiveLiteralID(PrimitiveType::U31);
  if (val <= u32_max) return getPrimitiveLiteralID(PrimitiveType::U32);

  if (val <= i64_max) return getPrimitiveLiteralID(PrimitiveType::U63);
  return getPrimitiveLiteralID(PrimitiveType::U64);
}

};
