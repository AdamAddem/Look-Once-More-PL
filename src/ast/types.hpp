#pragma once

#include "settings.hpp"
#include <cassert>
#include <cstdint>
#include <vector>

namespace Lexer {
struct Token;
}


namespace AST {

class Primitive;
class Pointer;
class Variant;
class Function;

class Type {
protected:
  static constexpr std::uint8_t is_arithmetic_mask = 1 << 0;
  static constexpr std::uint8_t is_callable_mask = 1 << 1;
  static constexpr std::uint8_t is_array_mask = 1 << 2;
  std::uint8_t flags{};
  constexpr void setArithmetic() noexcept {flags |= is_arithmetic_mask;}
  constexpr void setCallable()   noexcept {flags |= is_callable_mask;}
  constexpr void setArray()      noexcept {flags |= is_array_mask;}

  enum : uint8_t {DEVOID, PRIMITIVE, POINTER, VARIANT, FUNCTION, CUSTOM}
  derived_type{};

  Type() = default;
public:
  [[nodiscard]] static constexpr const Type* getDevoid() {
    static Type devoid{};
    return &devoid;
  };

  [[nodiscard]] constexpr bool isDevoid()     const noexcept  {return derived_type == DEVOID;}
  [[nodiscard]] constexpr bool isPrimitive()  const noexcept  {return derived_type == PRIMITIVE;}
  [[nodiscard]] constexpr bool isPointer()    const noexcept  {return derived_type == POINTER;}
  [[nodiscard]] constexpr bool isVariant()    const noexcept  {return derived_type == VARIANT;}
  [[nodiscard]] constexpr bool isFunction()   const noexcept  {return derived_type == FUNCTION;}
  [[nodiscard]] constexpr bool isCustom()     const noexcept  {return derived_type == CUSTOM;}

  [[nodiscard]] constexpr bool isArithmetic() const noexcept  {return flags & is_arithmetic_mask;}
  [[nodiscard]] constexpr bool isCallable()   const noexcept  {return flags & is_callable_mask;}
  [[nodiscard]] constexpr bool isArray()      const noexcept  {return flags & is_array_mask;}

  [[nodiscard]] constexpr bool isBool()       const noexcept;
  [[nodiscard]] constexpr bool isIntegral()   const noexcept;
  [[nodiscard]] constexpr bool isFloating()   const noexcept;

  [[nodiscard]] constexpr Primitive* castToPrimitive() noexcept; [[nodiscard]] constexpr const Primitive* castToPrimitive() const noexcept;
  [[nodiscard]] constexpr Pointer* castToPointer() noexcept; [[nodiscard]] constexpr const Pointer* castToPointer() const noexcept;
  [[nodiscard]] constexpr Variant* castToVariant() noexcept; [[nodiscard]] constexpr const Variant* castToVariant() const noexcept;
  [[nodiscard]] constexpr Function* castToFunction() noexcept; [[nodiscard]] constexpr const Function* castToFunction() const noexcept;


  [[nodiscard]] bool convertibleTo(const Type* other) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;

  Type(const Type&) = delete;
  Type(Type&&) noexcept = delete;
  void operator=(const Type&) = delete;
  void operator=(Type&&) noexcept = delete;
};

//An instance of anything that can have a type needs to use InstantiatedType
struct InstantiatedType {
  const Type* type;

  struct InstanceDetails {
    bool is_mutable : 1 = false;

    constexpr InstanceDetails() = default;
    constexpr explicit InstanceDetails(const bool is_mutable) : is_mutable(is_mutable) {}
  }details;

  constexpr InstantiatedType(const Type* type, const InstanceDetails instance_details)
  : type(type), details(instance_details) {assert(type != nullptr);}

  constexpr InstantiatedType(const Type* type, const bool is_mutable)
: type(type), details(is_mutable) {assert(type != nullptr);}

  [[nodiscard]] constexpr std::string toString() const noexcept { return (details.is_mutable ? "mut " : "") + type->toString(); }
};

class Primitive final : public Type {
public:
  enum class Primitives : std::uint8_t {
    I8, I16, I32, I64,
    U8, U16, U32, U64,
    F32, F64,
    BOOL, CHAR, STRING
  };

  constexpr explicit Primitive(const Primitives type) noexcept : primitive_type(type) {
    derived_type = PRIMITIVE;
    switch (primitive_type) {
    case Primitives::I8:
    case Primitives::I16:
    case Primitives::I32:
    case Primitives::I64:
    case Primitives::U8:
    case Primitives::U16:
    case Primitives::U32:
    case Primitives::U64:
    case Primitives::F32:
    case Primitives::F64:
      setArithmetic();

    default:
      break;
    }
  }
  explicit Primitive(const Lexer::Token& primitive_token);

  [[nodiscard]] constexpr bool
  isIntegral() const noexcept {
    using enum Primitives;
    switch (primitive_type) {
    case I8:
    case I16:
    case I32:
    case I64:
    case U8:
    case U16:
    case U32:
    case U64:
      return true;
    default:
      return false;
    }
  }

  [[nodiscard]] constexpr bool
  isUnsignedIntegral() const noexcept {
    using enum Primitives;
    switch (primitive_type) {
    case I8:
    case I16:
    case I32:
    case I64:
      return false;
    case U8:
    case U16:
    case U32:
    case U64:
      return true;
    default:
      return false;
    }
  }

  [[nodiscard]] constexpr bool
  isFloating() const noexcept {
    using enum Primitives;
    switch (primitive_type) {
    case F32:
    case F64:
      return true;
    default:
      return false;
    }
  }

  [[nodiscard]] constexpr bool
  isBool() const noexcept { return primitive_type == Primitives::BOOL; }

  [[nodiscard]] constexpr std::uint64_t
  maxIntegralValueRepresentable() const noexcept {
    switch (primitive_type) {
    case Primitives::I8:
      return std::numeric_limits<std::int8_t>::max();
    case Primitives::I16:
      return std::numeric_limits<std::int16_t>::max();
    case Primitives::I32:
      return std::numeric_limits<std::int32_t>::max();
    case Primitives::I64:
      return std::numeric_limits<std::int64_t>::max();
    case Primitives::U8:
      return std::numeric_limits<std::uint8_t>::max();
    case Primitives::U16:
      return std::numeric_limits<std::uint16_t>::max();
    case Primitives::U32:
      return std::numeric_limits<std::uint32_t>::max();
    case Primitives::U64:
      return std::numeric_limits<std::uint64_t>::max();

    default:
      assert(false && "called w/ non integral type");
    }
  }

  [[nodiscard]] constexpr std::int64_t
  minIntegralValueRepresentable() const noexcept {
    switch (primitive_type) {
    case Primitives::I8:
      return std::numeric_limits<std::int8_t>::min();
    case Primitives::I16:
      return std::numeric_limits<std::int16_t>::min();
    case Primitives::I32:
      return std::numeric_limits<std::int32_t>::min();
    case Primitives::I64:
      return std::numeric_limits<std::int64_t>::min();
    case Primitives::U8:
    case Primitives::U16:
    case Primitives::U32:
    case Primitives::U64:
      return 0;

    default:
      assert(false && "called w/ non integral type");
    }
  }

  [[nodiscard]] bool convertibleTo(const Primitive* other) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;

  [[nodiscard]] bool canValueBeRepresented(const std::uint64_t value) const noexcept { return isIntegral() && value < maxIntegralValueRepresentable(); }
  [[nodiscard]] bool canValueBeRepresented(const std::int64_t value) const noexcept
  { return isIntegral() && value < static_cast<std::int64_t>(maxIntegralValueRepresentable()) && value > minIntegralValueRepresentable(); }

  [[nodiscard]] bool canValueBeRepresented(float) const noexcept { return primitive_type == Primitives::F32 || primitive_type == Primitives::F64; }
  [[nodiscard]] bool canValueBeRepresented(double) const noexcept { return primitive_type == Primitives::F64; }
protected:
  Primitives primitive_type;
};
constexpr bool Type::isBool()       const noexcept {return derived_type == PRIMITIVE && static_cast<const Primitive*>(this)->isBool();}
constexpr bool Type::isIntegral()   const noexcept {return derived_type == PRIMITIVE && static_cast<const Primitive*>(this)->isIntegral();}
constexpr bool Type::isFloating()   const noexcept {return derived_type == PRIMITIVE && static_cast<const Primitive*>(this)->isFloating();}

class Pointer final : public Type {
public:
  enum class Pointers : std::uint8_t {
    RAW, UNIQUE, VAGUE
  };
  constexpr explicit Pointer() noexcept : pointer_type(Pointers::VAGUE), is_pointed_mutable(false), pointed_type(nullptr) {derived_type = POINTER;}
  constexpr explicit Pointer(const Pointers pointer_type, const Type* pointed_type, const bool is_pointed_mutable) noexcept
  : pointer_type(pointer_type), is_pointed_mutable(is_pointed_mutable), pointed_type(pointed_type)  {
    assert(pointed_type!= this);
    derived_type = POINTER;
    if (pointed_type == nullptr)
      assert(pointer_type == Pointer::Pointers::VAGUE);
  }
  explicit Pointer(const Lexer::Token& pointer_token, const Type* pointed_type, bool is_pointed_mutable);

  [[nodiscard]] bool convertibleTo(const Pointer* other) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;
  [[nodiscard]] bool sameAs(Pointers ptr_type, const Type* pointed_type, bool is_pointed_mutable) const noexcept;
protected:
  Pointers pointer_type;
  bool is_pointed_mutable;
  const Type* pointed_type;
};
inline constexpr Pointer vague_pointer{}; static constexpr InstantiatedType vague_pointer_instance(&vague_pointer, {});

class Variant final : public Type {
protected:
  const bool is_nullable;
  const std::vector<const Type*> subtypes;
public:
  constexpr explicit Variant(std::vector<const Type*> subtypes, const bool nullable)
  : is_nullable(nullable), subtypes(std::move(subtypes)) {
    derived_type = VARIANT;
    for (const auto subtype : this->subtypes) {
      assert(subtype != this);
      assert(not subtype->isVariant());
    }
  }
  [[nodiscard]] constexpr const std::vector<const Type*>& getSubtypes() const noexcept {return subtypes;}
  [[nodiscard]] bool contains(const Type* type) const noexcept;
  [[nodiscard]] std::string toString() const noexcept;
  [[nodiscard]] bool sameAs(const std::vector<const Type*>& subtypes, bool nullable) const noexcept;
};

class Function final : public Type {
protected:
  std::uint8_t num_parameters;
  const Type* parameter_types[Settings::MAX_FUNCTION_PARAMETERS]{nullptr};
  const Type* return_type;

public:
  constexpr Function(const std::vector<const Type*>& parameters, const Type* ret_type)
  : num_parameters(parameters.size()), return_type(ret_type) {
    assert(num_parameters <= Settings::MAX_FUNCTION_PARAMETERS);
    assert(num_parameters > 0 || ret_type);
    derived_type = FUNCTION;
    setCallable();
    for (auto i{0uz}; i<num_parameters; ++i)
      parameter_types[i] = parameters[i];

  }

  [[nodiscard]] constexpr std::uint8_t numParameters() const noexcept {return num_parameters;}
  [[nodiscard]] constexpr std::span<const Type* const> parameterTypes() const noexcept {return std::span(parameter_types, num_parameters);}
  [[nodiscard]] constexpr const Type* returnType() const noexcept {return return_type;}
  [[nodiscard]] bool sameAs(const std::vector<const Type*>& parameters, const Type* ret_type) const noexcept;

  [[nodiscard]] bool isValidCall(const std::vector<InstantiatedType>& parameters) const noexcept;

};

class Custom final : public Type {
public:
  Custom() = delete;
};


constexpr Primitive* Type::castToPrimitive() noexcept {
  assert(derived_type == PRIMITIVE);
  return static_cast<Primitive*>(this);
}
constexpr const Primitive* Type::castToPrimitive() const noexcept {
  assert(derived_type == PRIMITIVE);
  return static_cast<const Primitive*>(this);
}

constexpr Pointer* Type::castToPointer() noexcept {
  assert(derived_type == POINTER);
  return static_cast<Pointer*>(this);
}
constexpr const Pointer* Type::castToPointer() const noexcept {
  assert(derived_type == POINTER);
  return static_cast<const Pointer*>(this);
}

constexpr Variant* Type::castToVariant() noexcept {
  assert(derived_type == VARIANT);
  return static_cast<Variant*>(this);
}
constexpr const Variant* Type::castToVariant() const noexcept {
  assert(derived_type == VARIANT);
  return static_cast<const Variant*>(this);
}

constexpr Function* Type::castToFunction() noexcept {
  assert(derived_type == FUNCTION);
  return static_cast<Function*>(this);
}
constexpr const Function* Type::castToFunction() const noexcept {
  assert(derived_type == FUNCTION);
  return static_cast<const Function*>(this);
}


inline constexpr Primitive i8_type(Primitive::Primitives::I8);      static constexpr InstantiatedType i8_instance(&i8_type, {});
inline constexpr Primitive i16_type(Primitive::Primitives::I16);    static constexpr InstantiatedType i16_instance(&i16_type, {});
inline constexpr Primitive i32_type(Primitive::Primitives::I32);    static constexpr InstantiatedType i32_instance(&i32_type, {});
inline constexpr Primitive i64_type(Primitive::Primitives::I64);    static constexpr InstantiatedType i64_instance(&i64_type, {});
inline constexpr Primitive u8_type(Primitive::Primitives::U8);      static constexpr InstantiatedType u8_instance(&u8_type, {});
inline constexpr Primitive u16_type(Primitive::Primitives::U16);    static constexpr InstantiatedType u16_instance(&u16_type, {});
inline constexpr Primitive u32_type(Primitive::Primitives::U32);    static constexpr InstantiatedType u32_instance(&u32_type, {});
inline constexpr Primitive u64_type(Primitive::Primitives::U64);    static constexpr InstantiatedType u64_instance(&u64_type, {});
inline constexpr Primitive f32_type(Primitive::Primitives::F32);    static constexpr InstantiatedType f32_instance(&f32_type, {});
inline constexpr Primitive f64_type(Primitive::Primitives::F64);    static constexpr InstantiatedType f64_instance(&f64_type, {});
inline constexpr Primitive bool_type(Primitive::Primitives::BOOL);  static constexpr InstantiatedType bool_instance(&bool_type, {});
inline constexpr Primitive char_type(Primitive::Primitives::CHAR);  static constexpr InstantiatedType char_instance(&char_type, {});
inline constexpr Primitive string_type(Primitive::Primitives::STRING); static constexpr InstantiatedType string_instance(&string_type, {});
static const InstantiatedType devoid_instance(Type::getDevoid(), {}); //this can't be constexpr for some reason :(
inline const Type* devoid_type = devoid_instance.type; //this neither :(
}
