#pragma once
#include "edenlib/bitwise_utils.hpp"
#include "edenlib/enum_utils.hpp"
#include "edenlib/macros.hpp"
#include "edenlib/typedefs.hpp"

#include "build_system/build.hpp"
#include "module/table_and_module_sync.hpp"
#include "settings.hpp"

#include <cassert>
#include <limits>
#include <span>
#include <utility>

namespace LOM {

class PrimitiveType;
class PointerType;
class ArrayType;
class FunctionType;
class CustomType;

class Type {
  friend class Module;
public:
  enum class DerivedType : u8_t {DEVOID, ERROR, PRIMITIVE, POINTER, ARRAY, FUNCTION, CUSTOM};
  using enum DerivedType;
private:
  DerivedType derived_type{};

  static constexpr u8_t is_arithmetic_mask = 1 << 0;
  static constexpr u8_t is_callable_mask = 1 << 1;
  static constexpr u8_t RESERVED_MASK3 = 1 << 2;
  static constexpr u8_t RESERVED_MASK4 = 1 << 3;
  static constexpr u8_t RESERVED_MASK5 = 1 << 4;
  static constexpr u8_t RESERVED_MASK6 = 1 << 5;
  static constexpr u8_t RESERVED_MASK7 = 1 << 6;
  static constexpr u8_t RESERVED_MASK8 = 1 << 7;
  u8_t flags{}; // flags for quicker type checking

  // should only be called for the error type
  consteval Type() : derived_type(ERROR) { setArithmetic(); }

protected:
  constexpr void setArithmetic() noexcept {flags or_eq is_arithmetic_mask;}
  constexpr void setCallable()   noexcept {flags or_eq is_callable_mask;}

  constexpr explicit Type(DerivedType derived_type) : derived_type(derived_type) {}

public:
  edenInlineNodiscardCXPR DerivedType getDerivedType() const noexcept { return derived_type; }

  struct Qualifiers {
    bool writable;

    constexpr Qualifiers() noexcept : writable(false) {}
    constexpr explicit Qualifiers(eden::flags::DoNotInitialize) noexcept {}
    constexpr explicit Qualifiers(bool is_mutable) noexcept : writable(is_mutable) {}

    edenNodiscardCXPR bool
    operator ==(Qualifiers const&) const noexcept = default;
  }; static_assert(sizeof(Qualifiers) == 1, "If increased this would make stuff fatter than intended");

  template<std::derived_from<Type> T>
  static consteval DerivedType corresponding_derived_type() {
    if      constexpr(eden::same_c<T, PrimitiveType>) return PRIMITIVE;
    else if constexpr(eden::same_c<T, PointerType>)   return POINTER;
    else if constexpr(eden::same_c<T, ArrayType>)     return ARRAY;
    else if constexpr(eden::same_c<T, FunctionType>)  return FUNCTION;
    else if constexpr(eden::same_c<T, CustomType>)    return CUSTOM;
    else static_assert(false); return {};
  }

  [[nodiscard]] static consteval Type const& devoid() noexcept  { static constexpr Type devoid{DEVOID}; return devoid; }
  [[nodiscard]] static consteval Type const& error()  noexcept  { static constexpr Type error;          return error; }

  edenInlineNodiscardCXPR bool isDevoid()     const noexcept  { return derived_type == DEVOID;}
  edenInlineNodiscardCXPR bool isError()      const noexcept  { return derived_type == ERROR;}
  edenInlineNodiscardCXPR bool isPrimitive()  const noexcept  { return derived_type == PRIMITIVE;}
  edenInlineNodiscardCXPR bool isPointer()    const noexcept  { return derived_type == POINTER;}
  edenInlineNodiscardCXPR bool isArray()      const noexcept  { return derived_type == ARRAY;}
  edenInlineNodiscardCXPR bool isFunction()   const noexcept  { return derived_type == FUNCTION;}
  edenInlineNodiscardCXPR bool isCustom()     const noexcept  { return derived_type == CUSTOM;}
  edenInlineNodiscardCXPR bool isArithmetic() const noexcept  { return flags bitand is_arithmetic_mask;}
  edenInlineNodiscardCXPR bool isCallable()   const noexcept  { return flags bitand is_callable_mask;}

  edenNodiscardCXPR bool isBool()             const noexcept;
  edenNodiscardCXPR bool isIntegral()         const noexcept;
  edenNodiscardCXPR bool isUnsignedIntegral() const noexcept;
  edenNodiscardCXPR bool isSignedIntegral()   const noexcept;
  edenNodiscardCXPR bool isFloating()         const noexcept;
  edenNodiscardCXPR sz_t bitwidth()           const noexcept;

  edenInlineNodiscardCXPR PrimitiveType const*   castToPrimitive() const noexcept;
  edenInlineNodiscardCXPR PointerType   const*   castToPointer()   const noexcept;
  edenInlineNodiscardCXPR ArrayType     const*   castToArray()     const noexcept;
  edenInlineNodiscardCXPR FunctionType  const*   castToFunction()  const noexcept;
  edenInlineNodiscardCXPR CustomType    const*   castToCustom()    const noexcept;

  [[nodiscard]] std::string toString(Module const& owning_module) const noexcept;
  [[nodiscard]] bool coercibleTo(Type const* other) const noexcept;
  [[nodiscard]] bool castableTo(Type const* other) const noexcept;

  Type(const Type&) = delete;
  void operator=(const Type&) = delete;

  Type(Type&&) noexcept = default;
  Type& operator=(Type &&) noexcept = default;
};

struct TypeID {
  Type::DerivedType derived;
  byte_t _pad{};
  u16_t module_id;
  u32_t id;

  [[nodiscard]] Type const& getType() const noexcept;
  [[nodiscard]] Type const& getType(Module const& contained_module) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;
  [[nodiscard]] std::string toString(Module const& owning_module) const noexcept;
  edenInlineNodiscardCXPR bool operator==(TypeID const&) const = default;
};

struct QualifiedTypeID {
  Type::DerivedType derived;
  Type::Qualifiers qualifiers;
  u16_t module_id;
  u32_t id;

  constexpr QualifiedTypeID() noexcept = default;
  constexpr explicit QualifiedTypeID(TypeID typeID, Type::Qualifiers instance_qualifiers = {}) noexcept : derived(typeID.derived), qualifiers(instance_qualifiers), module_id(typeID.module_id), id(typeID.id) {}
  constexpr QualifiedTypeID(TypeID type_id, bool writable) noexcept : QualifiedTypeID(type_id) { qualifiers.writable = writable; }

  edenInlineCXPR void set(TypeID typeID) noexcept { derived = typeID.derived; module_id = typeID.module_id; id = typeID.id; }
  edenInlineNodiscardCXPR bool isUnqualified() const noexcept { return qualifiers == Type::Qualifiers{}; }
  edenInlineNodiscardCXPR TypeID toTypeID() const noexcept { return { .derived = derived, .module_id = module_id, .id = id }; }


  edenNoInlineCold [[nodiscard]] std::string toString(Module const& owning_module) const noexcept { return (qualifiers.writable ? "$ " : ": ") + toTypeID().toString(owning_module); }
  edenNoInlineCold [[nodiscard]] std::string toString() const noexcept { return toString(getModule(module_id)); }
}; static_assert(sizeof(QualifiedTypeID) == 8);

class PrimitiveType final : public Type {
  friend class Module;
public:
  enum class PrimitiveTypeEnum : u8_t {
    I8, I16, I32, I64,

    U7, U15, U31, U63, //unsigned literal whos values are compatable with both signed and unsigned
    U8, U16, U32, U64,

    F32, F64,
    BOOL, CHAR, STRING
  };
  using enum PrimitiveTypeEnum;
  
private:
  static constexpr sz_t num_types = 17; static_assert(std::to_underlying(STRING) == 16);
  PrimitiveTypeEnum primitive_type;

  constexpr PrimitiveType(PrimitiveTypeEnum type) noexcept : Type(PRIMITIVE), primitive_type(type) {
    eden::enumBetween(type, I8, F64) ? setArithmetic() : (void)0; 
  }

  static consteval std::array<PrimitiveType, num_types> const& make_arr() {
    static constexpr std::array<PrimitiveType, num_types> x {
      PrimitiveType{I8}, {I16}, {I32}, {I64},
      {PrimitiveType::U7}, {PrimitiveType::U15}, {PrimitiveType::U31}, {PrimitiveType::U63},
      {PrimitiveType::U8}, {PrimitiveType::U16}, {PrimitiveType::U32}, {PrimitiveType::U64},
      {PrimitiveType::F32}, {PrimitiveType::F64},
      {PrimitiveType::BOOL}, {PrimitiveType::CHAR}, {PrimitiveType::STRING}
    }; 
    return x;
  }

public:
  constexpr explicit operator QualifiedTypeID() const noexcept {
    QualifiedTypeID t;
    t.derived = PRIMITIVE;
    t.module_id = 0;
    t.id = std::to_underlying(primitive_type);
    return t;
  }

  edenInlineNodiscardCXPR PrimitiveTypeEnum         getUnderlyingPrimitiveType() const noexcept { return primitive_type; }
  edenInlineNodiscardCXPR bool isIntegral()         const noexcept { return eden::enumBetween(primitive_type, I8, U64); }
  edenInlineNodiscardCXPR bool isSignedIntegral()   const noexcept { return eden::enumBetween(primitive_type, I8, I64); }
  edenInlineNodiscardCXPR bool isUnsignedIntegral() const noexcept { return eden::enumBetween(primitive_type, U7, U64); }
  edenInlineNodiscardCXPR bool isFloating()         const noexcept { return eden::enumBetween(primitive_type, F32, F64); }
  edenInlineNodiscardCXPR bool isBool()             const noexcept { return primitive_type == BOOL; }
  edenInlineNodiscardCXPR bool isChar()             const noexcept { return primitive_type == CHAR; }
  edenInlineNodiscardCXPR bool isString()           const noexcept { return primitive_type == STRING; }

  edenNodiscardCXPR sz_t bitwidth() const noexcept {
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

  [[nodiscard]] bool coercibleTo(PrimitiveType const* other) const noexcept;
  [[nodiscard]] bool castableTo(PrimitiveType const* other) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;

#define type_singleton_decl(type_name) \
  [[nodiscard]] static consteval PrimitiveType const& type_name() noexcept;

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

  [[nodiscard]] static consteval PrimitiveType const& iptr_t() noexcept { return i64(); }
  [[nodiscard]] static consteval PrimitiveType const& uptr_t() noexcept { return u64(); }
};

#define type_singleton(type_name, type_enum) \
  [[nodiscard]] consteval PrimitiveType const& \
  PrimitiveType::type_name() noexcept \
  { return make_arr()[std::to_underlying(type_enum)]; }
  
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

  type_singleton(bool_, BOOL)
  type_singleton(char_, CHAR)
  type_singleton(string, STRING)
#undef type_singleton

edenInlineNodiscardCXPR bool Type::isBool()             const noexcept { return derived_type == PRIMITIVE and static_cast<PrimitiveType const*>(this)->isBool();    }
edenInlineNodiscardCXPR bool Type::isIntegral()         const noexcept { return derived_type == PRIMITIVE and static_cast<PrimitiveType const*>(this)->isIntegral();}
edenInlineNodiscardCXPR bool Type::isUnsignedIntegral() const noexcept { return derived_type == PRIMITIVE and static_cast<PrimitiveType const*>(this)->isUnsignedIntegral();}
edenInlineNodiscardCXPR bool Type::isSignedIntegral()   const noexcept { return derived_type == PRIMITIVE and static_cast<PrimitiveType const*>(this)->isSignedIntegral();}
edenInlineNodiscardCXPR bool Type::isFloating()         const noexcept { return derived_type == PRIMITIVE and static_cast<PrimitiveType const*>(this)->isFloating();}

class PointerType final : public Type {
  friend class Module;

  bool is_raw;
  DerivedType pointed_derived;
  u32_t pointed_id;

  constexpr PointerType(TypeID pointed_type, bool is_raw) noexcept
  : Type(POINTER), is_raw(is_raw), pointed_derived(pointed_type.derived), pointed_id(pointed_type.id) {}

public:

  [[nodiscard]] bool coercibleTo(PointerType const* other) const noexcept;
  [[nodiscard]] bool castableTo(PointerType const* other) const noexcept;
  [[nodiscard]] std::string toString(Module const& owning_module) const noexcept;

  edenInlineNodiscardCXPR TypeID pointedID(u16_t module_id) const noexcept { return { .derived = pointed_derived, .module_id = module_id, .id = pointed_id }; }

#define pre assert(&other not_eq this)
  edenInlineNodiscardCXPR bool operator==(PointerType const& other) const noexcept { return eden::are_bitwise_equal_restrict(this, &other); }
#undef pre
};

class ArrayType final : public Type {
  friend class Module;

  byte_t _pad{};
  DerivedType subtype_derived;
  u32_t subtype_id;
  u64_t array_size;

  constexpr explicit ArrayType(TypeID subtypeID, u64_t array_size) noexcept
  : Type(ARRAY), subtype_derived(subtypeID.derived),  subtype_id(subtypeID.id), array_size(array_size) { assert(array_size not_eq 0); }

public:

  edenInlineNodiscardCXPR u64_t  getSize()                          const noexcept { return array_size; }
  edenInlineNodiscardCXPR bool   coerciblTo(ArrayType const* other) const noexcept { return this == other; }
  edenInlineNodiscardCXPR bool   castableo(ArrayType const* other)  const noexcept { return this == other; }
  edenInlineNodiscardCXPR TypeID subtypeID(u16_t module_id)         const noexcept { return { .derived = subtype_derived, .module_id = module_id, .id = subtype_id }; }

  [[nodiscard]] std::string toString(Module const& owning_module) const noexcept;

#define pre assert(&other != this);
  edenInlineNodiscardCXPR bool operator==(ArrayType const& other) const noexcept { pre return eden::are_bitwise_equal_restrict(this, &other); }
#undef pre
};

class FunctionType final : public Type {
  friend class Module;

  bool is_variadic;
  u8_t num_parameters;
  DerivedType parameter_derived_types[Settings::MAX_FUNCTION_PARAMETERS]{};
  u32_t parameter_ids[Settings::MAX_FUNCTION_PARAMETERS]{};

  DerivedType return_derived_type;
  byte_t _pad[3]{};
  u32_t return_id;

  constexpr FunctionType(std::span<DerivedType const> parameter_derived_types, std::span<u32_t const> parameter_ids, TypeID returnID, bool is_variadic) noexcept
  : Type(FUNCTION), is_variadic(is_variadic), num_parameters(parameter_ids.size()), return_derived_type(returnID.derived), return_id(returnID.id) {
    setCallable();
    assert(num_parameters <= Settings::MAX_FUNCTION_PARAMETERS);
    std::memcpy(this->parameter_derived_types, parameter_derived_types.data(), num_parameters);
    std::memcpy(this->parameter_ids, parameter_ids.data(), num_parameters);
  }

public:

  edenInlineNodiscardCXPR sz_t numParameters() const noexcept { return num_parameters; }
  edenInlineNodiscardCXPR bool isVariadic() const noexcept    { return is_variadic; }
  edenInlineNodiscardCXPR TypeID returnID(u16_t module_id) const noexcept { return { .derived = return_derived_type, .module_id = module_id, .id = return_id }; }
  edenNoInlineCold [[nodiscard]] std::string toString(Module const& owning_module) const noexcept;
  edenInlineNodiscardCXPR TypeID parameterID(sz_t parameter_idx, u16_t module_id) const noexcept { return { .derived = parameter_derived_types[parameter_idx], .module_id = module_id, .id = parameter_ids[parameter_idx] }; }

#define pre assert(&other != this);
  edenInlineNodiscardCXPR bool operator==(FunctionType const& other) const noexcept { pre return eden::are_bitwise_equal_restrict(this, &other); }
#undef pre
};

class SymbolTable;
class CustomType final : public Type {
  friend class SymbolTable; friend class Module;

  //byte_t _pad[2];
  u32_t name_len;
  char const* name;

  alignas(SYMBOL_TABLE_ALIGNMENT)
  std::byte symboltable_buff[SYMBOL_TABLE_SIZE];

  [[nodiscard]] SymbolTable* member_table() noexcept;

public:

  explicit CustomType(std::string_view name);
  [[nodiscard]] SymbolTable const* member_table() const noexcept;

  edenInlineNodiscardCXPR std::string_view nameof()                             const noexcept { return {name, name_len}; }
  edenInlineNodiscardCXPR std::string      toString()                           const noexcept { return std::string(nameof()); }
  edenInlineNodiscardCXPR bool             coercibleTo(CustomType const* other) const noexcept { return this == other; }
  edenInlineNodiscardCXPR bool             castableTo(CustomType const* other)  const noexcept { return this == other; }

  edenInlineNodiscardCXPR bool operator==(CustomType const& other) const noexcept { return nameof() == other.name; }

  [[nodiscard]] std::string definitionToString() const noexcept;
};

edenNodiscardCXPR sz_t
Type::bitwidth() const noexcept {
  if (isPointer()) return sizeof(void*);

  switch (derived_type) {
  case ERROR:     return sizeof(void*) * 8;
  case PRIMITIVE: return static_cast<PrimitiveType const*>(this)->bitwidth();

  case FUNCTION:
  case CUSTOM:
  default:
    edenUnreachable("Unimplemented type to assess bitwidth.");
  }
}

edenInlineNodiscardCXPR PrimitiveType const* Type::castToPrimitive() const noexcept { assert(derived_type == PRIMITIVE); return static_cast<PrimitiveType const*>(this); }
edenInlineNodiscardCXPR PointerType   const* Type::castToPointer()   const noexcept { assert(derived_type == POINTER);   return static_cast<PointerType   const*>(this); }
edenInlineNodiscardCXPR ArrayType     const* Type::castToArray()     const noexcept { assert(derived_type == ARRAY);     return static_cast<ArrayType     const*>(this); }
edenInlineNodiscardCXPR FunctionType  const* Type::castToFunction()  const noexcept { assert(derived_type == FUNCTION);  return static_cast<FunctionType  const*>(this); }
edenInlineNodiscardCXPR CustomType    const* Type::castToCustom()    const noexcept { assert(derived_type == CUSTOM);    return static_cast<CustomType    const*>(this); }

static_assert(std::to_underlying(PrimitiveType::STRING) == 16);

inline constexpr QualifiedTypeID devoid_literal{
  TypeID{
    .derived = Type::DEVOID,
    .module_id = 0,
    .id = 0
  }
};
inline constexpr QualifiedTypeID error_literal{
  TypeID{
    .derived = Type::ERROR,
    .module_id = 0,
    .id = 0
  }
};
inline constexpr QualifiedTypeID i8_literal{PrimitiveType::i8()};
inline constexpr QualifiedTypeID i16_literal{PrimitiveType::i16()};
inline constexpr QualifiedTypeID i32_literal{PrimitiveType::i32()};
inline constexpr QualifiedTypeID i64_literal{PrimitiveType::i64()};
inline constexpr QualifiedTypeID u7_literal{PrimitiveType::u7()};
inline constexpr QualifiedTypeID u8_literal{PrimitiveType::u8()};
inline constexpr QualifiedTypeID u15_literal{PrimitiveType::u15()};
inline constexpr QualifiedTypeID u16_literal{PrimitiveType::u16()};
inline constexpr QualifiedTypeID u31_literal{PrimitiveType::u31()};
inline constexpr QualifiedTypeID u32_literal{PrimitiveType::u32()};
inline constexpr QualifiedTypeID u63_literal{PrimitiveType::u63()};
inline constexpr QualifiedTypeID u64_literal{PrimitiveType::u64()};
inline constexpr QualifiedTypeID f32_literal{PrimitiveType::f32()};
inline constexpr QualifiedTypeID f64_literal{PrimitiveType::f64()};
inline constexpr QualifiedTypeID bool_literal{PrimitiveType::bool_()};
inline constexpr QualifiedTypeID char_literal{PrimitiveType::char_()};
inline constexpr QualifiedTypeID string_literal{PrimitiveType::string()};

static constexpr QualifiedTypeID signedToLiteralInstance(i64_t val) {
  val = val < 0 ? (val * -1) - 1 : val;
  if (val <= std::numeric_limits<i8_t>::max())  return i8_literal;
  if (val <= std::numeric_limits<i16_t>::max()) return i16_literal;
  if (val <= std::numeric_limits<i32_t>::max()) return i32_literal;
  return i64_literal;
}

static constexpr QualifiedTypeID unsignedToLiteralInstance(u64_t val) {
  if (val <= i8_max) return u7_literal;
  if (val <= u8_max) return u8_literal;

  if (val <= i16_max) return u15_literal;
  if (val <= u16_max) return u16_literal;

  if (val <= i32_max) return u31_literal;
  if (val <= u32_max) return u32_literal;

  if (val <= i64_max) return u63_literal;
  return u64_literal;
}

}
