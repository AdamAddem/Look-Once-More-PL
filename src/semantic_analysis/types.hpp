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

#include <unordered_map>
namespace LOM {

class PrimitiveType;
class PointerType;
class ArrayType;
class FunctionType;
class CustomType;
class VariantType;

/*
    Types must:
      - have a respective enum within Type
      - have a "toString" method
      - have a "coercibleTo" method
      - have a "castableTo" method
      - have a "sameAs" method
*/

class Type {
  friend class TypeContext;
public:
  enum class DerivedType : u8_t {DEVOID, ERROR, PRIMITIVE, POINTER, ARRAY, FUNCTION, CUSTOM, VARIANT};
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
  explicit consteval Type() : derived_type(ERROR) { setArithmetic(); }

protected:
  constexpr void setArithmetic() noexcept {flags or_eq is_arithmetic_mask;}
  constexpr void setCallable()   noexcept {flags or_eq is_callable_mask;}

  explicit constexpr Type(auto derived_type) : derived_type(derived_type) {}

public:
  eden_always_inline [[nodiscard]] constexpr DerivedType getDerivedType() const noexcept { return derived_type; }

  struct Qualifiers {
    bool writable;

    constexpr Qualifiers() noexcept : writable(false) {}
    explicit constexpr Qualifiers(eden::flags::DoNotInitialize) noexcept {}
    explicit constexpr Qualifiers(bool is_mutable) noexcept : writable(is_mutable) {}

    [[nodiscard]] constexpr bool
    operator ==(Qualifiers const&) const noexcept = default;
  }; static_assert(sizeof(Qualifiers) == 1, "If increased this would make SymbolTable::Variable go from 24 -> 32 bytes");

  [[nodiscard]] static consteval Type const* devoid() noexcept  { static constexpr Type devoid{DEVOID}; return &devoid; }
  [[nodiscard]] static consteval Type const* error()  noexcept  { static constexpr Type error;          return &error; }

  eden_always_inline [[nodiscard]] constexpr bool isDevoid()     const noexcept  { return derived_type == DEVOID;}
  eden_always_inline [[nodiscard]] constexpr bool isError()      const noexcept  { return derived_type == ERROR;}
  eden_always_inline [[nodiscard]] constexpr bool isPrimitive()  const noexcept  { return derived_type == PRIMITIVE;}
  eden_always_inline [[nodiscard]] constexpr bool isPointer()    const noexcept  { return derived_type == POINTER;}
  eden_always_inline [[nodiscard]] constexpr bool isArray()      const noexcept  { return derived_type == ARRAY;}
  eden_always_inline [[nodiscard]] constexpr bool isFunction()   const noexcept  { return derived_type == FUNCTION;}
  eden_always_inline [[nodiscard]] constexpr bool isCustom()     const noexcept  { return derived_type == CUSTOM;}
  eden_always_inline [[nodiscard]] constexpr bool isVariant()    const noexcept  { return derived_type == VARIANT;}
  eden_always_inline [[nodiscard]] constexpr bool isArithmetic() const noexcept  { return flags bitand is_arithmetic_mask;}
  eden_always_inline [[nodiscard]] constexpr bool isCallable()   const noexcept  { return flags bitand is_callable_mask;}

  [[nodiscard]] constexpr bool isBool()             const noexcept;
  [[nodiscard]] constexpr bool isIntegral()         const noexcept;
  [[nodiscard]] constexpr bool isUnsignedIntegral() const noexcept;
  [[nodiscard]] constexpr bool isSignedIntegral()   const noexcept;
  [[nodiscard]] constexpr bool isFloating()         const noexcept;
  [[nodiscard]] constexpr sz_t bitwidth()           const noexcept;

  eden_always_inline [[nodiscard]] constexpr PrimitiveType const*   castToPrimitive() const noexcept;
  eden_always_inline [[nodiscard]] constexpr PointerType   const*   castToPointer()   const noexcept;
  eden_always_inline [[nodiscard]] constexpr ArrayType     const*   castToArray()     const noexcept;
  eden_always_inline [[nodiscard]] constexpr FunctionType  const*   castToFunction()  const noexcept;
  eden_always_inline [[nodiscard]] constexpr CustomType    const*   castToCustom()    const noexcept;
  eden_always_inline [[nodiscard]] constexpr VariantType   const*   castToVariant()   const noexcept;

  [[nodiscard]] std::string toString() const noexcept;
  [[nodiscard]] bool coercibleTo(Type const* other) const noexcept;
  [[nodiscard]] bool castableTo(Type const* other) const noexcept;

  Type(const Type&) = delete;
  Type(Type&&) noexcept = delete;
  void operator=(const Type&) = delete;
  void operator=(Type &&) noexcept = delete;
};

struct QualifiedType {
  Type const* type;
  Type::Qualifiers qualifiers;

  constexpr QualifiedType() noexcept : type(nullptr) {}
  explicit constexpr QualifiedType(eden::flags::DoNotInitialize flag) noexcept : qualifiers(flag) {}

  explicit constexpr QualifiedType(Type const* type) noexcept : type(type), qualifiers(false) {}
  constexpr QualifiedType(Type const* type, Type::Qualifiers instance_qualifiers) noexcept : type(type), qualifiers(instance_qualifiers) {}
  constexpr QualifiedType(Type const* type, bool is_mutable) : type(type), qualifiers(is_mutable) {}

  [[nodiscard]] constexpr bool
  operator==(QualifiedType const&) const noexcept = default;

  [[nodiscard]] constexpr bool
  isUnqualified() const noexcept
  { return qualifiers == Type::Qualifiers{}; }

  [[nodiscard]] constexpr std::string
  toString() const noexcept
  { return (qualifiers.writable ? "$" : "") + type->toString(); }
private:
  [[no_unique_address]] struct Empty { constexpr bool operator==(const Empty&) const noexcept = default; }
  make_nonstandard_layout_for_packing_optimizations;
};

class PrimitiveType final : public Type {
  friend class TypeContext;
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
  PrimitiveTypeEnum primitive_type;

  constexpr explicit
  PrimitiveType(PrimitiveTypeEnum type) noexcept
  : Type(PRIMITIVE), primitive_type(type)
  { eden::enumBetween(type, I8, F64) ? setArithmetic() : void{}; }

  static constexpr sz_t num_types = 17; static_assert(std::to_underlying(STRING) == 16);
public:

  eden_always_inline [[nodiscard]] constexpr PrimitiveTypeEnum         getUnderlyingPrimitiveType() const noexcept { return primitive_type; }
  eden_always_inline [[nodiscard]] constexpr bool isIntegral()         const noexcept { return eden::enumBetween(primitive_type, I8, U64); }
  eden_always_inline [[nodiscard]] constexpr bool isSignedIntegral()   const noexcept { return eden::enumBetween(primitive_type, I8, I64); }
  eden_always_inline [[nodiscard]] constexpr bool isUnsignedIntegral() const noexcept { return eden::enumBetween(primitive_type, U7, U64); }
  eden_always_inline [[nodiscard]] constexpr bool isFloating()         const noexcept { return eden::enumBetween(primitive_type, F32, F64); }
  eden_always_inline [[nodiscard]] constexpr bool isBool()             const noexcept { return primitive_type == BOOL; }
  eden_always_inline [[nodiscard]] constexpr bool isChar()             const noexcept { return primitive_type == CHAR; }
  eden_always_inline [[nodiscard]] constexpr bool isString()           const noexcept { return primitive_type == STRING; }

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
  coercibleTo(PrimitiveType const* other) const noexcept;

  eden_nonull_args [[nodiscard]] bool
  castableTo(PrimitiveType const* other) const noexcept;

  [[nodiscard]] std::string
  toString() const noexcept;

#define type_singleton(type_name, type_enum) \
  [[nodiscard]] static consteval PrimitiveType const* \
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
  [[nodiscard]] static consteval PrimitiveType const* iptr_t() noexcept { return sizeof(void*) == 8 ? i64() : i32(); }
  [[nodiscard]] static consteval PrimitiveType const* uptr_t() noexcept { return sizeof(void*) == 8 ? u64() : u32(); }
};

eden_always_inline [[nodiscard]] constexpr bool Type::isBool()               const noexcept { return derived_type == PRIMITIVE and static_cast<PrimitiveType const*>(this)->isBool();    }
eden_always_inline [[nodiscard]] constexpr bool Type::isIntegral()           const noexcept { return derived_type == PRIMITIVE and static_cast<PrimitiveType const*>(this)->isIntegral();}
eden_always_inline [[nodiscard]] constexpr bool Type::isUnsignedIntegral()   const noexcept { return derived_type == PRIMITIVE and static_cast<PrimitiveType const*>(this)->isUnsignedIntegral();}
eden_always_inline [[nodiscard]] constexpr bool Type::isSignedIntegral()     const noexcept { return derived_type == PRIMITIVE and static_cast<PrimitiveType const*>(this)->isSignedIntegral();}
eden_always_inline [[nodiscard]] constexpr bool Type::isFloating()           const noexcept { return derived_type == PRIMITIVE and static_cast<PrimitiveType const*>(this)->isFloating();}

class PointerType final : public Type {
  friend class TypeContext;

  bool pointed_is_readwrite{};
  Type const* pointed_type;

  constexpr PointerType(Type const* pointed_type, bool pointed_is_readwrite)
  : Type(POINTER), pointed_is_readwrite(pointed_is_readwrite), pointed_type(pointed_type) {}

public:

  [[nodiscard]] bool
  coercibleTo(PointerType const* other) const noexcept;

  [[nodiscard]] bool
  castableTo(PointerType const* other) const noexcept;

  [[nodiscard]] std::string toString() const noexcept;

  eden_always_inline [[nodiscard]] constexpr bool sameAs(Type const* other_pointed_type, bool other_pointed_is_readwrite) const noexcept { return pointed_type == other_pointed_type and pointed_is_readwrite == other_pointed_is_readwrite; }
  eden_always_inline [[nodiscard]] constexpr QualifiedType getSubtype() const noexcept { return {pointed_type, pointed_is_readwrite}; }
};

class ArrayType final : public Type {
  friend class TypeContext;

  // char _pad[6];
  u64_t size;
  Type const* subtype;

  explicit constexpr ArrayType(u64_t size, Type const* subtype)
  : Type(ARRAY), size(size), subtype(subtype) { assert(size not_eq 0); }
public:

  eden_always_inline [[nodiscard]] constexpr u64_t        getSize()                           const noexcept { return size; }
  eden_always_inline [[nodiscard]] constexpr Type const*  getSubtype()                        const noexcept { return subtype; }
  eden_always_inline [[nodiscard]] constexpr bool         coercibleTo(ArrayType const* other) const noexcept { return this == other; }
  eden_always_inline [[nodiscard]] constexpr bool         castableTo(ArrayType const* other)  const noexcept { return this == other; }

  [[nodiscard]] constexpr std::string toString() const noexcept { return std::format("[{}]{}", size, subtype->toString()); }
  eden_always_inline [[nodiscard]] constexpr bool sameAs( u64_t other_size, Type const* other_subtype ) const noexcept { return other_size == size and other_subtype == subtype; }
};

class FunctionType final : public Type {
  friend class TypeContext;

  // char _pad[4];
  bool is_variadic;
  u8_t num_parameters;
  Type const* subtypes[Settings::MAX_FUNCTION_PARAMETERS];
  Type const* return_type;

  constexpr FunctionType(std::span<Type const*> parameters, Type const* return_type, bool is_variadic)
  : Type(FUNCTION), is_variadic(is_variadic), num_parameters(parameters.size()), return_type(return_type) {
    setCallable();
    assume_assert(num_parameters <= Settings::MAX_FUNCTION_PARAMETERS);
    auto i{0uz};
    for (; i<num_parameters; ++i)
      subtypes[i] = parameters[i];
  }

public:

  eden_always_inline [[nodiscard]] constexpr sz_t         numParameters() const noexcept { return num_parameters; }
  eden_always_inline [[nodiscard]] constexpr bool         isVariadic() const noexcept    { return is_variadic; }
  eden_always_inline [[nodiscard]] constexpr Type const*  returnType() const noexcept    { return return_type; }

  eden_always_inline [[nodiscard]] constexpr std::span<Type const* const>
  parameterTypes() const noexcept { return std::span(subtypes, num_parameters);}

  [[nodiscard]] bool        coercibleTo() const noexcept = delete;
  [[nodiscard]] std::string toString() const noexcept;

  [[nodiscard]] constexpr bool
  sameAs(std::span<Type const*> parameters, Type const* ret_type, bool variadic) const noexcept {
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

  // char _pad[2];
  u32_t name_len;
  char const* name;

  alignas(SYMBOL_TABLE_ALIGNMENT)
  std::byte symboltable_buff[SYMBOL_TABLE_SIZE];

  [[nodiscard]] SymbolTable* member_table() noexcept;

public:

  explicit CustomType(std::string_view name);
  [[nodiscard]] SymbolTable const* member_table() const noexcept;

  eden_always_inline [[nodiscard]] constexpr bool              sameAs(std::string_view other_name)  const noexcept { return nameof() == other_name; }
  eden_always_inline [[nodiscard]] constexpr std::string_view  nameof()                             const noexcept { return {name, name_len}; }
  eden_always_inline [[nodiscard]] constexpr std::string       toString()                           const noexcept { return std::string(nameof()); }
  eden_always_inline [[nodiscard]] constexpr bool              coercibleTo(CustomType const* other) const noexcept { return this == other; }
  eden_always_inline [[nodiscard]] constexpr bool              castableTo(CustomType const* other)  const noexcept { return this == other; }

  [[nodiscard]] std::string
  definitionToString() const noexcept;

};

// not used atm
class VariantType final : public Type {
  friend class TypeContext;

  bool is_nullable;
  u32_t num_subtypes;
  Type const* subtypes[Settings::MAX_TYPELIST_MEMBERS];

  constexpr VariantType(std::span<Type const*> variant_subtypes, bool nullable)
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

  [[nodiscard]] bool
  coercibleTo() const noexcept = delete;

  [[nodiscard]] bool contains(Type const* type) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;
  [[nodiscard]] bool sameAs(std::span<Type const*> subtypes, bool nullable) const noexcept;
};

[[nodiscard]] constexpr sz_t
Type::bitwidth() const noexcept {
  if (isPointer()) return sizeof(void*);

  switch (derived_type) {
  case ERROR:     return sizeof(void*) * 8;
  case PRIMITIVE: return static_cast<PrimitiveType const*>(this)->bitwidth();

  case VARIANT:
  case FUNCTION:
  case CUSTOM:
  default:
    eden_unreachable("Unimplemented type to assess bitwidth.");
  }
}

eden_always_inline [[nodiscard]] constexpr PrimitiveType const* Type::castToPrimitive()  const noexcept { assume_assert(derived_type == PRIMITIVE); return static_cast<PrimitiveType const*>(this); }
eden_always_inline [[nodiscard]] constexpr PointerType   const* Type::castToPointer()    const noexcept { assume_assert(derived_type == POINTER);   return static_cast<PointerType   const*>(this); }
eden_always_inline [[nodiscard]] constexpr ArrayType     const* Type::castToArray()      const noexcept { assume_assert(derived_type == ARRAY);     return static_cast<ArrayType     const*>(this); }
eden_always_inline [[nodiscard]] constexpr FunctionType  const* Type::castToFunction()   const noexcept { assume_assert(derived_type == FUNCTION);  return static_cast<FunctionType  const*>(this); }
eden_always_inline [[nodiscard]] constexpr CustomType    const* Type::castToCustom()     const noexcept { assume_assert(derived_type == CUSTOM);    return static_cast<CustomType    const*>(this); }
eden_always_inline [[nodiscard]] constexpr VariantType   const* Type::castToVariant()    const noexcept { assume_assert(derived_type == VARIANT);   return static_cast<VariantType   const*>(this); }

static constexpr QualifiedType devoid_literal{Type::devoid(), {}};
static constexpr QualifiedType error_literal{Type::error(), true};
static constexpr QualifiedType i8_literal{PrimitiveType::i8(), {}};
static constexpr QualifiedType i16_literal{PrimitiveType::i16(), {}};
static constexpr QualifiedType i32_literal{PrimitiveType::i32(), {}};
static constexpr QualifiedType i64_literal{PrimitiveType::i64(), {}};
static constexpr QualifiedType u7_literal{PrimitiveType::u7(), {}};
static constexpr QualifiedType u8_literal{PrimitiveType::u8(), {}};
static constexpr QualifiedType u15_literal{PrimitiveType::u15(), {}};
static constexpr QualifiedType u16_literal{PrimitiveType::u16(), {}};
static constexpr QualifiedType u31_literal{PrimitiveType::u31(), {}};
static constexpr QualifiedType u32_literal{PrimitiveType::u32(), {}};
static constexpr QualifiedType u63_literal{PrimitiveType::u63(), {}};
static constexpr QualifiedType u64_literal{PrimitiveType::u64(), {}};
static constexpr QualifiedType f32_literal{PrimitiveType::f32(), {}};
static constexpr QualifiedType f64_literal{PrimitiveType::f64(), {}};
static constexpr QualifiedType bool_literal{PrimitiveType::bool_(), {}};
static constexpr QualifiedType char_literal{PrimitiveType::char_(), {}};
static constexpr QualifiedType string_literal{PrimitiveType::string(), {}};

static constexpr QualifiedType signedToLiteralInstance(i64_t val) {
  val = val < 0 ? (val * -1) - 1 : val;
  if (val <= std::numeric_limits<i8_t>::max())  return i8_literal;
  if (val <= std::numeric_limits<i16_t>::max()) return i16_literal;
  if (val <= std::numeric_limits<i32_t>::max()) return i32_literal;
  return i64_literal;
}

static constexpr QualifiedType unsignedToLiteralInstance(u64_t val) {
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
  static constexpr auto named_search_pred = [] (auto* type, std::string_view name) { return type->nameof() == name; };

  eden::ArenaPool<> type_arena;

  eden::swap_vector<PointerType*>  pointers;
  eden::swap_vector<ArrayType*>    arrays;
  eden::swap_vector<FunctionType*> functions;
  eden::swap_vector<CustomType*>   custom_types;
  eden::swap_vector<VariantType*>  variants;

  template <std::derived_from<Type> T, class... Args>
  eden_always_inline [[nodiscard]] constexpr T*
  allocateAndConstruct(Args&&... args)
  { return new (type_arena.allocate<T>(1)) T (std::forward<Args>(args)...); }

  template <std::derived_from<Type> T, class... Args>
  [[nodiscard]] constexpr T*
  returnExistingOrNew(eden::swap_vector<T*>& types, Args&&... args) {
    auto res = types.search(search_pred, std::forward<Args>(args)...);
    if (res) return *res;

    auto const new_type = allocateAndConstruct<T>(std::forward<Args>(args)...);
    types.push_back(new_type);
    return new_type;
  }

public:

  // should this be public? idk, probably not. don't make this though.
  TypeContext() noexcept {
    arrays.reserve(8);
    functions.reserve(8);
    custom_types.reserve(8);
    variants.reserve(8);
  }

  [[nodiscard]] sz_t
  totalNumberOfTypes() const noexcept {
    return PrimitiveType::num_types +
           2 + //vague and vague mutable
           arrays.size()       +
           functions.size()    +
           custom_types.size() +
           variants.size();
  }

  eden_always_inline [[nodiscard]] sz_t numArrayTypes()      const noexcept { return arrays.size(); }
  eden_always_inline [[nodiscard]] sz_t numFunctionTypes()   const noexcept { return functions.size(); }
  eden_always_inline [[nodiscard]] sz_t numCustomTypes()     const noexcept { return custom_types.size(); }
  eden_always_inline [[nodiscard]] sz_t numVariantTypes()    const noexcept { return variants.size(); }

  [[nodiscard]] PointerType*
  addRawPointer(Type const* subtype) noexcept
  { return returnExistingOrNew(pointers, subtype, true); }

  [[nodiscard]] PointerType*
  addRefPointer(Type const* subtype) noexcept
  { return returnExistingOrNew(pointers, subtype, false); }

  [[nodiscard]] ArrayType*
  addArray(u64_t size, Type const* element_type) noexcept
  { return returnExistingOrNew(arrays, size, element_type); }

  [[nodiscard]] FunctionType*
  addFunction(std::span<Type const*> parameter_types, Type const* return_type, bool is_variadic = false) noexcept
  { return returnExistingOrNew(functions, parameter_types, return_type, is_variadic);}

  [[nodiscard]] CustomType*
  addCustomType(std::string_view name) noexcept
  { return returnExistingOrNew(custom_types, name);}

  // returns nullptr if not found
  [[nodiscard]] CustomType const*
  getCustomType(std::string_view name) noexcept {
    auto const res = custom_types.search(named_search_pred, name);
    if (res) return *res;
    return nullptr;
  }

  [[nodiscard]] VariantType*
  addVariant(std::span<Type const*> subtypes, bool nullable) noexcept
  { return returnExistingOrNew(variants, subtypes, nullable);}
};

}
