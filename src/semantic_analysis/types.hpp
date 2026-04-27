#pragma once
#include "edenlib/arena.hpp"
#include "edenlib/enum_utils.hpp"
#include "edenlib/macros.hpp"
#include "edenlib/typedefs.hpp"
#include "semantic_analysis/types.hpp"
#include "settings.hpp"

#include <cassert>
#include <limits>
#include <span>
#include <utility>
#include <vector>

namespace LOM::Lexer {
class Token;
}

namespace LOM {

class PrimitiveType;
class PointerType;
class VariantType;
class FunctionType;

class Type {
  friend class TypeContext;

  static constexpr u8_t is_arithmetic_mask = 1 << 0;
  static constexpr u8_t is_callable_mask = 1 << 1;
  static constexpr u8_t is_array_mask = 1 << 2;
  u8_t flags{};
protected:
  constexpr void setArithmetic() noexcept {flags or_eq is_arithmetic_mask;}
  constexpr void setCallable()   noexcept {flags or_eq is_callable_mask;}
  constexpr void setArray()      noexcept {flags or_eq is_array_mask;}
  enum : u8_t {DEVOID, PRIMITIVE, POINTER, VARIANT, FUNCTION, CUSTOM}
  derived_type{};

  explicit constexpr Type(auto derived_type)
  : derived_type(derived_type) {}
public:
  [[nodiscard]] static consteval const Type* devoid() {static constexpr Type devoid{DEVOID}; return &devoid;}

  [[nodiscard]] constexpr bool isDevoid()           const noexcept  {return derived_type == DEVOID;}
  [[nodiscard]] constexpr bool isPrimitive()        const noexcept  {return derived_type == PRIMITIVE;}
  [[nodiscard]] constexpr bool isPointer()          const noexcept  {return derived_type == POINTER;}
  [[nodiscard]] constexpr bool isVariant()          const noexcept  {return derived_type == VARIANT;}
  [[nodiscard]] constexpr bool isFunction()         const noexcept  {return derived_type == FUNCTION;}
  [[nodiscard]] constexpr bool isCustom()           const noexcept  {return derived_type == CUSTOM;}

  [[nodiscard]] constexpr bool isArithmetic()       const noexcept  {return flags bitand is_arithmetic_mask;}
  [[nodiscard]] constexpr bool isCallable()         const noexcept  {return flags bitand is_callable_mask;}
  [[nodiscard]] constexpr bool isArray()            const noexcept  {return flags bitand is_array_mask;}

  [[nodiscard]] constexpr bool isBool()             const noexcept;
  [[nodiscard]] constexpr bool isIntegral()         const noexcept;
  [[nodiscard]] constexpr bool isUnsignedIntegral() const noexcept;
  [[nodiscard]] constexpr bool isSignedIntegral()   const noexcept;
  [[nodiscard]] constexpr bool isFloating()         const noexcept;
  [[nodiscard]] constexpr sz_t bitwidth()           const noexcept;


  [[nodiscard]] constexpr const PrimitiveType* castToPrimitive() const noexcept;
  [[nodiscard]] constexpr const PointerType* castToPointer() const noexcept;
  [[nodiscard]] constexpr const VariantType* castToVariant() const noexcept;
  [[nodiscard]] constexpr const FunctionType* castToFunction() const noexcept;

  [[nodiscard]] bool convertibleTo(const Type* other) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;

  Type(const Type&) = delete;
  Type(Type&&) noexcept = delete;
  void operator=(const Type&) = delete;
  void operator=(Type &&) noexcept = delete;
};

struct InstantiatedType {
  const Type* type;

  struct InstanceDetails {
    bool is_mutable : 1 = false;
    bool is_public : 1 = false;

    constexpr InstanceDetails() = default;
    constexpr explicit InstanceDetails(const bool is_mutable) : is_mutable(is_mutable) {}

    [[nodiscard]] constexpr bool
    operator ==(const InstanceDetails &) const = default;
  }details;

  constexpr InstantiatedType() = default;
  constexpr InstantiatedType(const Type* type, const InstanceDetails instance_details)
  : type(type), details(instance_details) {}

  constexpr InstantiatedType(const Type* type, const bool is_mutable)
  : type(type), details(is_mutable) {}

  [[nodiscard]] constexpr bool
  operator==(const InstantiatedType&) const = default;

  [[nodiscard]] constexpr bool
  isPlain() const noexcept
  {return details == InstanceDetails{};}

  [[nodiscard]] constexpr std::string
  toString() const noexcept
  {return std::string(details.is_public ? "pub " : "") + (details.is_mutable ? "mut " : "") + type->toString();}
};

class PrimitiveType final : public Type {
  friend class TypeContext;

  enum : u8_t {
    I8, I16, I32, I64,
    U_, U8, U16, U32, U64,
    F32, F64,
    BOOL, CHAR, STRING
  }primitive_type;

  constexpr explicit
  PrimitiveType(decltype(primitive_type) type) noexcept : Type(PRIMITIVE), primitive_type(type)
  { eden::enumBetween(type, I8, F64) ? setArithmetic() : void{}; }

public:

  [[nodiscard]] constexpr bool
  isIntegral() const noexcept
  {return eden::enumBetween(primitive_type, I8, U64);}

  [[nodiscard]] constexpr bool
  isSignedIntegral() const noexcept
  {return eden::enumBetween(primitive_type, I8, I64);}

  [[nodiscard]] constexpr bool
  isUnsignedIntegral() const noexcept
  {return eden::enumBetween(primitive_type, U_, U64);}

  [[nodiscard]] constexpr bool
  isFloating() const noexcept
  {return eden::enumBetween(primitive_type, F32, F64);}

  [[nodiscard]] constexpr bool
  isBool() const noexcept
  {return primitive_type == BOOL;}

  [[nodiscard]] constexpr bool
  isString() const noexcept
  {return primitive_type == STRING;}

  [[nodiscard]] constexpr bool
  isLiteral() const noexcept
  {return primitive_type == U_;}


  [[nodiscard]] constexpr sz_t
  bitwidth() const noexcept {
    switch (primitive_type) {
    case U8:
    case I8:
    case BOOL:
    case CHAR:
      return 8;
    case U16:
    case I16:
      return 16;
    case U32:
    case I32:
    case F32:
      return 32;
    case U_:
    case U64:
    case I64:
    case F64:
      return 64;
    default:
      std::unreachable();
    }
  }

  [[nodiscard]] constexpr u64_t
  maxIntegralValueRepresentable() const noexcept {
    switch (primitive_type) {
    case I8:
      return std::numeric_limits<i8_t>::max();
    case I16:
      return std::numeric_limits<i16_t>::max();
    case I32:
      return std::numeric_limits<i32_t>::max();
    case I64:
      return std::numeric_limits<i64_t>::max();
    case U8:
      return std::numeric_limits<u8_t>::max();
    case U16:
      return std::numeric_limits<u16_t>::max();
    case U32:
      return std::numeric_limits<u32_t>::max();
    case U_:
    case U64:
      return std::numeric_limits<u64_t>::max();

    default:
      std::unreachable();
    }
  }

  [[nodiscard]] constexpr i64_t
  minIntegralValueRepresentable() const noexcept {
    switch (primitive_type) {
    case I8:
      return std::numeric_limits<i8_t>::min();
    case I16:
      return std::numeric_limits<i16_t>::min();
    case I32:
      return std::numeric_limits<i32_t>::min();
    case I64:
      return std::numeric_limits<i64_t>::min();
    case U_:
    case U8:
    case U16:
    case U32:
    case U64:
      return 0;

    default:
      std::unreachable();
    }
  }

  [[nodiscard]] bool
  convertibleTo(const PrimitiveType* other) const noexcept;

  [[nodiscard]] std::string
  toString() const noexcept;

  [[nodiscard]] constexpr bool
  canValueBeRepresented(u64_t value) const noexcept
  {return isIntegral() and value <= maxIntegralValueRepresentable();}

  [[nodiscard]] constexpr bool
  canValueBeRepresented(i64_t value) const noexcept
  {return isIntegral() and value <= static_cast<i64_t>(maxIntegralValueRepresentable()) and value >= minIntegralValueRepresentable();}

#define type_singleton(type_name, type_enum) \
  static consteval const PrimitiveType* \
  type_name() noexcept \
  {static constexpr PrimitiveType type_name{type_enum}; return &(type_name);}

  type_singleton(i8, I8)
  type_singleton(i16, I16)
  type_singleton(i32, I32)
  type_singleton(i64, I64)

  type_singleton(unsigned_literal, U_)
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
};

constexpr bool Type::isBool()               const noexcept {return derived_type == PRIMITIVE and static_cast<const PrimitiveType*>(this)->isBool();    }
constexpr bool Type::isIntegral()           const noexcept {return derived_type == PRIMITIVE and static_cast<const PrimitiveType*>(this)->isIntegral();}
constexpr bool Type::isUnsignedIntegral()   const noexcept {return derived_type == PRIMITIVE and static_cast<const PrimitiveType*>(this)->isUnsignedIntegral();}
constexpr bool Type::isSignedIntegral()     const noexcept {return derived_type == PRIMITIVE and static_cast<const PrimitiveType*>(this)->isSignedIntegral();}
constexpr bool Type::isFloating()           const noexcept {return derived_type == PRIMITIVE and static_cast<const PrimitiveType*>(this)->isFloating();}

class PointerType final : public Type {
  friend class TypeContext;

  enum : u8_t {
    RAW, UNIQUE, VAGUE
  }pointer_type;
  InstantiatedType subtype;

  explicit constexpr PointerType(bool mutable_subtype)
  : Type(POINTER), pointer_type(VAGUE), subtype(nullptr, mutable_subtype) {}

  constexpr PointerType(InstantiatedType subtype, bool is_unique)
  : Type(POINTER), pointer_type(is_unique ? UNIQUE : RAW), subtype(subtype) {}

public:

  [[nodiscard]] bool convertibleTo(const PointerType* other) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;

  [[nodiscard]] constexpr bool
  sameAs(InstantiatedType other_subtype, bool is_unique) const noexcept
  {return other_subtype == subtype and ((pointer_type == UNIQUE) == is_unique);}

  [[nodiscard]] constexpr InstantiatedType
  getSubtype() const noexcept {return subtype;}

  [[nodiscard]] constexpr bool
  isRaw() const noexcept {return pointer_type == RAW;}

  [[nodiscard]] constexpr bool
  isUnique() const noexcept {return pointer_type == UNIQUE;}

  [[nodiscard]] constexpr bool
  isVague() const noexcept {return pointer_type == VAGUE;}

  static constexpr const PointerType*
  vague(bool subtype_mutable) noexcept {
    static constexpr PointerType immutable_vague{false};
    static constexpr PointerType mutable_vague{true};
    return subtype_mutable ? &mutable_vague : &immutable_vague;
  }

};

class VariantType final : public Type {
  friend class TypeContext;

  bool is_nullable;
  std::vector<const Type*> subtypes;

  constexpr VariantType(std::vector<const Type*> subtypes, bool nullable)
  : Type(VARIANT), is_nullable(nullable), subtypes(std::move(subtypes)) {
#ifndef NDEBUG
    for (const auto subtype : this->subtypes)
    {assert(subtype not_eq this); assert(not subtype->isVariant());}
#endif
  }

public:

  [[nodiscard]] constexpr const std::vector<const Type*>& getSubtypes() const noexcept {return subtypes;}
  [[nodiscard]] bool contains(const Type* type) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;
  [[nodiscard]] bool sameAs(const std::vector<const Type*>& subtypes, bool nullable) const noexcept;
};

class FunctionType final : public Type {
  friend class TypeContext;

  bool is_variadic;
  std::vector<const Type*> subtypes; //last is return type

public:
  constexpr FunctionType(std::span<const Type*> parameters, const Type* return_type, bool is_variadic)
  : Type(FUNCTION), is_variadic(is_variadic) {
    setCallable();
    for (auto p : parameters)
      subtypes.push_back(p);
    subtypes.push_back(return_type);
  }

  [[nodiscard]] constexpr sz_t
  numParameters() const noexcept {return subtypes.size() - 1;}

  [[nodiscard]] constexpr bool
  isVariadic() const noexcept {return is_variadic;}

  [[nodiscard]] constexpr std::span<const Type* const>
  parameterTypes() const noexcept {return std::span(subtypes.data(), subtypes.size() - 1);}

  [[nodiscard]] constexpr const Type*
  returnType() const noexcept {return subtypes.back();}

  [[nodiscard]] std::string
  toString() const noexcept;

  [[nodiscard]] constexpr bool
  sameAs(std::span<const Type*> parameters, const Type* ret_type, bool variadic) const noexcept {
    if (parameters.size() not_eq subtypes.size()-1 or subtypes.back() not_eq ret_type or is_variadic not_eq variadic)
      return false;

    for (auto i{0uz}; i < parameters.size(); ++i)
      if (parameters[i] not_eq subtypes[i])
        return false;

    return true;
  }

  void validateCall(std::span<InstantiatedType> parameters) const;

};

class CustomType final : public Type {
  friend class TypeContext;
public:
  CustomType() = delete;
};

constexpr sz_t Type::bitwidth() const noexcept {
  switch (derived_type) {
  case PRIMITIVE:
    return static_cast<const PrimitiveType*>(this)->bitwidth();
  case POINTER:
    return sizeof(void*) * 8;
  case VARIANT:
  case FUNCTION:
  case CUSTOM:
  default:
    std::unreachable();
  }
}

constexpr const PrimitiveType* Type::castToPrimitive() const noexcept {
  assume_assert(derived_type == PRIMITIVE);
  return static_cast<const PrimitiveType*>(this);
}
constexpr const PointerType* Type::castToPointer() const noexcept {
  assume_assert(derived_type == POINTER);
  return static_cast<const PointerType*>(this);
}
constexpr const VariantType* Type::castToVariant() const noexcept {
  assume_assert(derived_type == VARIANT);
  return static_cast<const VariantType*>(this);
}
constexpr const FunctionType* Type::castToFunction() const noexcept {
  assume_assert(derived_type == FUNCTION);
  return static_cast<const FunctionType*>(this);
}

static constexpr InstantiatedType devoid_literal{Type::devoid(), {}};
static constexpr InstantiatedType i8_literal{PrimitiveType::i8(), {}};
static constexpr InstantiatedType i16_literal{PrimitiveType::i16(), {}};
static constexpr InstantiatedType i32_literal{PrimitiveType::i32(), {}};
static constexpr InstantiatedType i64_literal{PrimitiveType::i64(), {}};
static constexpr InstantiatedType unsigned_literal{PrimitiveType::unsigned_literal(), {}};
static constexpr InstantiatedType u8_literal{PrimitiveType::u8(), {}};
static constexpr InstantiatedType u16_literal{PrimitiveType::u16(), {}};
static constexpr InstantiatedType u32_literal{PrimitiveType::u32(), {}};
static constexpr InstantiatedType u64_literal{PrimitiveType::u64(), {}};
static constexpr InstantiatedType f32_literal{PrimitiveType::f32(), {}};
static constexpr InstantiatedType f64_literal{PrimitiveType::f64(), {}};
static constexpr InstantiatedType bool_literal{PrimitiveType::bool_(), {}};
static constexpr InstantiatedType char_literal{PrimitiveType::char_(), {}};
static constexpr InstantiatedType string_literal{PrimitiveType::string(), {}};

static constexpr InstantiatedType signedToLiteralInstance(i64_t val) {
  val = val < 0 ? (val * -1) - 1 : val;
  if (val <= std::numeric_limits<i8_t>::max())
    return i8_literal;
  if (val <= std::numeric_limits<i16_t>::max())
    return i16_literal;
  if (val <= std::numeric_limits<i32_t>::max())
    return i32_literal;
  return i64_literal;
}

static constexpr InstantiatedType unsignedToLiteralInstance(u64_t val) {
  if (val <= std::numeric_limits<u8_t>::max())
    return u8_literal;
  if (val <= std::numeric_limits<u16_t>::max())
    return u16_literal;
  if (val <= std::numeric_limits<u32_t>::max())
    return u32_literal;
  return u64_literal;
}

class TypeContext {
  eden::Arena<> type_arena;
  std::vector<PointerType*> pointers;
  std::vector<VariantType*> variants;
  std::vector<FunctionType*> functions;

  template <std::derived_from<Type> T, class... Args>
  constexpr T*
  allocateAndConstruct(Args&&... args)
  {return new (type_arena.allocate<T>()) T (std::forward<Args>(args)...);}

  template <std::derived_from<Type> T, class ... Args>
  constexpr T*
  returnExistingOrNew(std::vector<T*>& types, Args&&... args) {
    auto curr = types.rbegin();
    const auto end = types.rend();
    while (curr not_eq end) {
      const auto type = *curr;
      if (type->sameAs(std::forward<Args>(args)...)) {
        std::swap(types.back(), *curr);
        return type;
      }
      ++curr;
    }

    const auto new_type =
    allocateAndConstruct<T>(std::forward<Args>(args)...);
    types.push_back(new_type);
    return new_type;
  }

public:
  TypeContext() = default;
  TypeContext(TypeContext&& other) noexcept = default;

  [[nodiscard]] const PointerType*
  addRawPointer(InstantiatedType subtype) noexcept
  {return returnExistingOrNew(pointers, subtype, false);}

  [[nodiscard]] const PointerType*
  addUniquePointer(InstantiatedType subtype) noexcept
  {return returnExistingOrNew(pointers, subtype, true);}

  [[nodiscard]] const VariantType*
  addVariant(std::vector<const Type*> subtypes, bool nullable) noexcept
  {return returnExistingOrNew(variants, std::move(subtypes), nullable);}

  [[nodiscard]] const FunctionType*
  addFunction(std::span<const Type*> parameter_types, const Type* return_type, bool is_variadic = false) noexcept
  {return returnExistingOrNew(functions, parameter_types, return_type, is_variadic);}

  ~TypeContext() { //not sure if this is necessary. Type Context will only be destroyed at the end of the program
    for (const auto variant : variants)
      std::destroy_at(variant);
    for (const auto function : functions)
      std::destroy_at(function);
  }
};

}
