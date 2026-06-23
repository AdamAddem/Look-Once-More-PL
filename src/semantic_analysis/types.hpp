#pragma once
#include "edenlib/arena.hpp"
#include "edenlib/enum_utils.hpp"
#include "edenlib/macros.hpp"
#include "edenlib/typedefs.hpp"
#include "edenlib/vectors/swap_vector.hpp"
#include "settings.hpp"
#include "table_and_module_sync.hpp"

#include <cassert>
#include <limits>
#include <span>
#include <utility>
#include <vector>

namespace LOM {

class PrimitiveType;
class PointerType;
class VariantType;
class FunctionType;
class CustomType;

class Type {
  friend class TypeContext;

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
  explicit consteval Type() : flags(u8_max), derived_type(ERROR) {}

protected:
  constexpr void setArithmetic() noexcept {flags or_eq is_arithmetic_mask;}
  constexpr void setCallable()   noexcept {flags or_eq is_callable_mask;}
  enum : u8_t {DEVOID, ERROR, PRIMITIVE, POINTER, VARIANT, FUNCTION, CUSTOM}
  derived_type{};

  explicit constexpr Type(auto derived_type)
  : derived_type(derived_type) {}

public:

  [[nodiscard]] static consteval const Type* devoid()     noexcept  {static constexpr Type devoid{DEVOID}; return &devoid;}
  [[nodiscard]] static consteval const Type* error()      noexcept  {static constexpr Type error;  return &error;}

  [[nodiscard]] constexpr bool isDevoid()           const noexcept  {return derived_type == DEVOID;}
  [[nodiscard]] constexpr bool isPrimitive()        const noexcept  {return derived_type == PRIMITIVE;}
  [[nodiscard]] constexpr bool isPointer()          const noexcept  {return derived_type == POINTER;}
  [[nodiscard]] constexpr bool isVariant()          const noexcept  {return derived_type == VARIANT;}
  [[nodiscard]] constexpr bool isFunction()         const noexcept  {return derived_type == FUNCTION;}
  [[nodiscard]] constexpr bool isCustom()           const noexcept  {return derived_type == CUSTOM;}

  [[nodiscard]] constexpr bool isArithmetic()       const noexcept  {return flags bitand is_arithmetic_mask;}
  [[nodiscard]] constexpr bool isCallable()         const noexcept  {return flags bitand is_callable_mask;}

  [[nodiscard]] constexpr bool isBool()             const noexcept;
  [[nodiscard]] constexpr bool isIntegral()         const noexcept;
  [[nodiscard]] constexpr bool isUnsignedIntegral() const noexcept;
  [[nodiscard]] constexpr bool isSignedIntegral()   const noexcept;
  [[nodiscard]] constexpr bool isFloating()         const noexcept;
  [[nodiscard]] constexpr sz_t bitwidth()           const noexcept;


  [[nodiscard]] constexpr const PrimitiveType*  castToPrimitive() const noexcept;
  [[nodiscard]] constexpr const PointerType*    castToPointer()   const noexcept;
  [[nodiscard]] constexpr const VariantType*    castToVariant()   const noexcept;
  [[nodiscard]] constexpr const FunctionType*   castToFunction()  const noexcept;
  [[nodiscard]] constexpr const CustomType*     castToCustom()    const noexcept;

  [[nodiscard]] bool coercibleTo(const Type* other) const noexcept;
  [[nodiscard]] bool castableTo(const Type* other) const noexcept;


  [[nodiscard]] std::string toString() const noexcept;

  Type(const Type&) = delete;
  Type(Type&&) noexcept = delete;
  void operator=(const Type&) = delete;
  void operator=(Type &&) noexcept = delete;
};

struct InstantiatedType {
  const Type* type;

  struct Qualifiers {
    bool is_mutable;

    constexpr Qualifiers() noexcept : is_mutable(false) {}
    explicit constexpr Qualifiers(eden::flags::DoNotInitialize) noexcept {}
    explicit constexpr Qualifiers(bool is_mutable) noexcept : is_mutable(is_mutable) {}

    [[nodiscard]] constexpr bool
    operator ==(Qualifiers const&) const noexcept = default;
  }qualifiers; static_assert(sizeof(Qualifiers) == 1, "If increased this would make SymbolTable::Variable go from 24 -> 32 bytes");

  constexpr InstantiatedType() noexcept : type(nullptr) {}
  explicit constexpr InstantiatedType(eden::flags::DoNotInitialize flag) noexcept : qualifiers(flag) {}

  explicit constexpr InstantiatedType(const Type* type) noexcept : type(type), qualifiers(false) {}
  constexpr InstantiatedType(const Type* type, Qualifiers instance_qualifiers) noexcept : type(type), qualifiers(instance_qualifiers) {}
  constexpr InstantiatedType(const Type* type, bool is_mutable) : type(type), qualifiers(is_mutable) {}

  [[nodiscard]] constexpr bool
  operator==(InstantiatedType const&) const noexcept = default;

  [[nodiscard]] constexpr bool
  isUnqualified() const noexcept
  { return qualifiers == Qualifiers{}; }

  [[nodiscard]] constexpr std::string
  toString() const noexcept
  { return (qualifiers.is_mutable ? "mut " : "") + type->toString(); }
private:
  [[no_unique_address]] struct Empty { constexpr bool operator==(const Empty&) const noexcept = default; }
  make_nonstandard_layout_for_packing_optimizations;
};

class PrimitiveType final : public Type {
  friend class TypeContext;

  enum : u8_t {
    I8, I16, I32, I64,

    U7, U15, U31, U63, //unsigned literal whos values are compatable with both signed and unsigned
    U8, U16, U32, U64,

    F32, F64,
    BOOL, CHAR, STRING
  }primitive_type;

  constexpr explicit
  PrimitiveType(decltype(primitive_type) type) noexcept
  : Type(PRIMITIVE), primitive_type(type)
  { eden::enumBetween(type, I8, F64) ? setArithmetic() : void{}; }

public:

  static constexpr sz_t num_types = 17; static_assert(STRING == 16);

  [[nodiscard]] constexpr bool isIntegral() const noexcept { return eden::enumBetween(primitive_type, I8, U64); }
  [[nodiscard]] constexpr bool isSignedIntegral() const noexcept { return eden::enumBetween(primitive_type, I8, I64); }
  [[nodiscard]] constexpr bool isUnsignedIntegral() const noexcept { return eden::enumBetween(primitive_type, U7, U64); }
  [[nodiscard]] constexpr bool isFloating() const noexcept { return eden::enumBetween(primitive_type, F32, F64); }
  [[nodiscard]] constexpr bool isBool() const noexcept { return primitive_type == BOOL; }
  [[nodiscard]] constexpr bool isChar() const noexcept { return primitive_type == CHAR; }
  [[nodiscard]] constexpr bool isString() const noexcept { return primitive_type == STRING; }

  [[nodiscard]] constexpr sz_t
  bitwidth() const noexcept {
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

  eden_nonull_args [[nodiscard]] bool
  coercibleTo(const PrimitiveType* other) const noexcept;

  eden_nonull_args [[nodiscard]] bool
  castableTo(const PrimitiveType* other) const noexcept;

  [[nodiscard]] std::string
  toString() const noexcept;

#define type_singleton(type_name, type_enum) \
  eden_return_nonnull \
  [[nodiscard]] static consteval const PrimitiveType* \
  type_name() noexcept \
  { static constexpr PrimitiveType type_name{type_enum}; return &(type_name); }

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
};

[[nodiscard]] constexpr bool Type::isBool()               const noexcept { return derived_type == PRIMITIVE and static_cast<const PrimitiveType*>(this)->isBool();    }
[[nodiscard]] constexpr bool Type::isIntegral()           const noexcept { return derived_type == PRIMITIVE and static_cast<const PrimitiveType*>(this)->isIntegral();}
[[nodiscard]] constexpr bool Type::isUnsignedIntegral()   const noexcept { return derived_type == PRIMITIVE and static_cast<const PrimitiveType*>(this)->isUnsignedIntegral();}
[[nodiscard]] constexpr bool Type::isSignedIntegral()     const noexcept { return derived_type == PRIMITIVE and static_cast<const PrimitiveType*>(this)->isSignedIntegral();}
[[nodiscard]] constexpr bool Type::isFloating()           const noexcept { return derived_type == PRIMITIVE and static_cast<const PrimitiveType*>(this)->isFloating();}

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

  eden_nonull_args [[nodiscard]] bool
  coercibleTo(const PointerType* other) const noexcept;

  eden_nonull_args [[nodiscard]] bool
  castableTo(const PointerType* other) const noexcept;

  [[nodiscard]] std::string toString() const noexcept;

  [[nodiscard]] constexpr bool
  sameAs(InstantiatedType other_subtype, bool is_unique) const noexcept
  { return other_subtype == subtype and ((pointer_type == UNIQUE) == is_unique); }

  [[nodiscard]] constexpr InstantiatedType
  getSubtype() const noexcept
  { return subtype; }

  [[nodiscard]] constexpr bool isRaw() const noexcept { return pointer_type == RAW; }
  [[nodiscard]] constexpr bool isUnique() const noexcept { return pointer_type == UNIQUE; }
  [[nodiscard]] constexpr bool isVague() const noexcept { return pointer_type == VAGUE; }

  eden_return_nonnull
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
  u32_t num_subtypes;
  const Type* subtypes[Settings::MAX_TYPELIST_MEMBERS];

  constexpr VariantType(std::span<const Type*> variant_subtypes, bool nullable)
  : Type(VARIANT), is_nullable(nullable), num_subtypes(variant_subtypes.size()) {
    assume_assert(num_subtypes <= Settings::MAX_TYPELIST_MEMBERS);

    for (auto i{0uz}; i<variant_subtypes.size(); ++i) {
      const auto subtype = variant_subtypes[i];
      assert(subtype not_eq this); assert(not subtype->isVariant());
      subtypes[i] = subtype;
    }

  }

public:

  [[nodiscard]] constexpr auto const& getSubtypes() const noexcept {return subtypes;}

  eden_nonull_args
  [[nodiscard]] bool
  coercibleTo() const noexcept = delete;

  eden_nonull_args [[nodiscard]] bool contains(const Type* type) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;
  [[nodiscard]] bool sameAs(std::span<const Type*> subtypes, bool nullable) const noexcept;
};

class FunctionType final : public Type {
  friend class TypeContext;

  bool is_variadic;
  u32_t num_parameters;
  const Type* subtypes[Settings::MAX_FUNCTION_PARAMETERS + 1]; //last is return type

public:

  eden_nonull_args
  constexpr FunctionType(std::span<const Type*> parameters, const Type* return_type, bool is_variadic)
  : Type(FUNCTION), is_variadic(is_variadic), num_parameters(parameters.size()) { setCallable();
    assume_assert(num_parameters <= Settings::MAX_FUNCTION_PARAMETERS);
    auto i{0uz};
    for (; i<num_parameters; ++i)
      subtypes[i] = parameters[i];
    subtypes[i] = return_type;
  }

  [[nodiscard]] constexpr sz_t
  numParameters() const noexcept {return num_parameters;}

  [[nodiscard]] constexpr bool
  isVariadic() const noexcept {return is_variadic;}

  [[nodiscard]] constexpr std::span<const Type* const>
  parameterTypes() const noexcept {return std::span(subtypes, num_parameters);}

  eden_return_nonnull
  [[nodiscard]] constexpr const Type*
  returnType() const noexcept {return subtypes[num_parameters];}

  eden_nonull_args
  [[nodiscard]] bool
  coercibleTo() const noexcept = delete;

  [[nodiscard]] std::string
  toString() const noexcept;

  eden_nonull_args
  [[nodiscard]] constexpr bool
  sameAs(std::span<const Type*> parameters, const Type* ret_type, bool variadic) const noexcept {
    if (
      parameters.size() not_eq num_parameters or
      returnType() not_eq ret_type or
      is_variadic not_eq variadic)
      return false;

    for (auto i{0uz}; i < parameters.size(); ++i)
      if (parameters[i] not_eq subtypes[i])
        return false;

    return true;
  }

};

class SymbolTable; class Module;
class CustomType final : public Type {
  friend class TypeContext; friend class SymbolTable; friend class Module;
  u32_t name_len; //not using string_view reduces size 72 -> 64
  const char* name;

  alignas(SYMBOL_TABLE_ALIGNMENT)
  std::byte symboltable_buff[SYMBOL_TABLE_SIZE];

  [[nodiscard]] SymbolTable*
  member_table() noexcept;

public:

  explicit CustomType(std::string_view name);

  [[nodiscard]] const SymbolTable*
  member_table() const noexcept;

  [[nodiscard]] constexpr bool
  sameAs(std::string_view other_name) const noexcept
  { return nameof() == other_name; }

  [[nodiscard]] constexpr std::string_view
  nameof() const noexcept
  { return {name, name_len}; }

  [[nodiscard]] constexpr std::string
  toString() const noexcept
  { return std::string(nameof()); }

  [[nodiscard]] std::string
  definitionToString() const noexcept;

  eden_nonull_args
  [[nodiscard]] bool
  coercibleTo(const CustomType* other) const noexcept { return this == other; }

};

[[nodiscard]] constexpr sz_t
Type::bitwidth() const noexcept {
  switch (derived_type) {
  case PRIMITIVE:
    return static_cast<const PrimitiveType*>(this)->bitwidth();
  case ERROR:
  case POINTER:
    return sizeof(void*) * 8;
  case VARIANT:
  case FUNCTION:
  case CUSTOM:
  default:
    std::unreachable();
  }
}

[[nodiscard]] constexpr const PrimitiveType* Type::castToPrimitive()  const noexcept { assume_assert(derived_type == PRIMITIVE); return static_cast<const PrimitiveType*>(this); }
[[nodiscard]] constexpr const PointerType* Type::castToPointer()      const noexcept { assume_assert(derived_type == POINTER); return static_cast<const PointerType*>(this); }
[[nodiscard]] constexpr const VariantType* Type::castToVariant()      const noexcept { assume_assert(derived_type == VARIANT); return static_cast<const VariantType*>(this); }
[[nodiscard]] constexpr const FunctionType* Type::castToFunction()    const noexcept { assume_assert(derived_type == FUNCTION); return static_cast<const FunctionType*>(this); }
[[nodiscard]] constexpr const CustomType* Type::castToCustom()        const noexcept { assume_assert(derived_type == CUSTOM); return static_cast<const CustomType*>(this); }

static constexpr InstantiatedType devoid_literal{Type::devoid(), {}};
static constexpr InstantiatedType error_literal{Type::error(), true};
static constexpr InstantiatedType i8_literal{PrimitiveType::i8(), {}};
static constexpr InstantiatedType i16_literal{PrimitiveType::i16(), {}};
static constexpr InstantiatedType i32_literal{PrimitiveType::i32(), {}};
static constexpr InstantiatedType i64_literal{PrimitiveType::i64(), {}};
static constexpr InstantiatedType u7_literal{PrimitiveType::u7(), {}};
static constexpr InstantiatedType u8_literal{PrimitiveType::u8(), {}};
static constexpr InstantiatedType u15_literal{PrimitiveType::u15(), {}};
static constexpr InstantiatedType u16_literal{PrimitiveType::u16(), {}};
static constexpr InstantiatedType u31_literal{PrimitiveType::u31(), {}};
static constexpr InstantiatedType u32_literal{PrimitiveType::u32(), {}};
static constexpr InstantiatedType u63_literal{PrimitiveType::u63(), {}};
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
  if (val <= i8_max) return u7_literal;
  if (val <= u8_max) return u8_literal;

  if (val <= i16_max) return u15_literal;
  if (val <= u16_max) return u16_literal;

  if (val <= i32_max) return u31_literal;
  if (val <= u32_max) return u32_literal;

  if (val <= i64_max) return u63_literal;
  return u64_literal;
}

class TypeContext {
  static constexpr auto search_pred = [] (auto* type, auto&&... args) { return type->sameAs(std::forward<decltype(args)>(args)...); };
  eden::Arena<> type_arena;
  eden::swap_vector<PointerType*> pointers;
  eden::swap_vector<VariantType*> variants;
  eden::swap_vector<FunctionType*> functions;
  eden::swap_vector<CustomType*> custom_types;

  template <std::derived_from<Type> T, class... Args>
  eden_return_nonnull [[nodiscard]]
  constexpr T*
  allocateAndConstruct(Args&&... args)
  { return new (type_arena.allocate<T>()) T (std::forward<Args>(args)...); }

  template <std::derived_from<Type> T, class... Args>
  eden_return_nonnull [[nodiscard]]
  constexpr T*
  returnExistingOrNew(eden::swap_vector<T*>& types, Args&&... args) {
    auto res = types.search(search_pred, std::forward<Args>(args)...);
    if (res) return *res;

    const auto new_type =
    allocateAndConstruct<T>(std::forward<Args>(args)...);
    types.push_back(new_type);
    return new_type;
  }

public:

  [[nodiscard]] sz_t
  totalNumberOfTypes() const noexcept {
    return PrimitiveType::num_types +
           2 + //vague and vague mutable
           pointers.size() + variants.size() + functions.size() + custom_types.size();
  }

  // this technically exposes the types as non const, not sure if I should care or not
  [[nodiscard]] auto const& getPointers() const noexcept { return pointers; }
  [[nodiscard]] auto const& getVariants() const noexcept { return variants; }
  [[nodiscard]] auto const& getFunctions() const noexcept { return functions; }
  [[nodiscard]] auto const& getCustomTypes() const noexcept { return custom_types; }


  eden_return_nonnull
  [[nodiscard]] PointerType*
  addRawPointer(InstantiatedType subtype) noexcept
  { return returnExistingOrNew(pointers, subtype, false); }

  eden_return_nonnull
  [[nodiscard]] PointerType*
  addUniquePointer(InstantiatedType subtype) noexcept
  { return returnExistingOrNew(pointers, subtype, true); }

  eden_return_nonnull
  [[nodiscard]] VariantType*
  addVariant(std::span<const Type*> subtypes, bool nullable) noexcept
  {return returnExistingOrNew(variants, subtypes, nullable);}

  eden_return_nonnull eden_nonull_args
  [[nodiscard]] FunctionType*
  addFunction(std::span<const Type*> parameter_types, const Type* return_type, bool is_variadic = false) noexcept
  {return returnExistingOrNew(functions, parameter_types, return_type, is_variadic);}

  eden_return_nonnull
  [[nodiscard]] CustomType*
  addCustomType(std::string_view name) noexcept
  {return returnExistingOrNew(custom_types, name);}

  //returns nullptr if not found
  [[nodiscard]] const CustomType*
  getCustomType(std::string_view name) noexcept {
    auto curr = custom_types.rbegin();
    const auto end = custom_types.rend();
    while (curr not_eq end) {
      const auto type = *curr;
      if (type->nameof() == name) {
        std::swap(custom_types.back(), *curr);
        return type;
      }
      ++curr;
    }
    return nullptr;
  }
};

}
