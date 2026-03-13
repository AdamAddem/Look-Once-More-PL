#include "types.hpp"
#include "lexing/lex.hpp"
using namespace AST;





[[nodiscard]] static constexpr bool
isUnsignedIntegral(const Primitive::Primitives type) noexcept {
  using enum Primitive::Primitives;
  switch (type) {
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

bool Type::convertibleTo(const Type* other) const noexcept {
  assert(derived_type != CUSTOM  && other->derived_type != CUSTOM );
  if (other == this)
    return true;


  const auto other_type = other->derived_type;
  if (other_type == VARIANT)
    return other->castToVariant()->contains(this);

  if (derived_type != other_type || flags != other->flags)
    return false;


  switch (derived_type) {
  case DEVOID:
    assert(false); //only one devoid instance allowed, so this should've returned earlier
  case PRIMITIVE:
    return castToPrimitive()->convertibleTo(other->castToPrimitive());
  case POINTER:
    return castToPointer()->convertibleTo(other->castToPointer());
  case VARIANT:
  case FUNCTION:
  case CUSTOM:
    return false;
   default:
    assert(false);
  }

}

std::string Type::toString() const noexcept {
  switch (derived_type) {
  case DEVOID:
    return "devoid";
  case PRIMITIVE:
    return static_cast<const Primitive*>(this)->toString();
  case POINTER:
    return static_cast<const Primitive*>(this)->toString();
  case VARIANT:
    return static_cast<const Primitive*>(this)->toString();

  case FUNCTION:
  case CUSTOM:
  default:
    assert(false && "unimplemented");
  }
}

bool Primitive::convertibleTo(const Primitive* other) const noexcept {
  const auto other_type = other->primitive_type;
  if (other_type == primitive_type) return true;

  using enum Primitives;
  switch (primitive_type) {
  case I8:
  case I16:
  case I32:
  case I64:
    static_assert(std::to_underlying(I8) == 0);
    static_assert(std::to_underlying(I64) == 3);
    return std::to_underlying(other_type) > std::to_underlying(primitive_type) &&
           std::to_underlying(other_type) <= std::to_underlying(I64);
  case U8:
  case U16:
  case U32:
  case U64:
    static_assert(std::to_underlying(U8) == 4);
    static_assert(std::to_underlying(U64) == 7);
    static_assert(std::to_underlying(U8) - 4 == std::to_underlying(I8));
    if (isUnsignedIntegral(other_type) && std::to_underlying(primitive_type) > std::to_underlying(other_type))
      return true;

    //this is so dumb
    return std::to_underlying(other_type) > (std::to_underlying(primitive_type) - 4) &&
       std::to_underlying(other_type) <= std::to_underlying(I64);

  case F32:
    if (other_type == F32) return true;
    [[fallthrough]];
  case F64:
    return other_type == F64;

  case BOOL:
  case CHAR:
  case STRING:
    return false; //only converts to the same type which has been checked already
  default:
    assert(false);
  }
}

std::string Primitive::toString() const noexcept {
  using enum Primitives;
  switch (primitive_type) {
  case I8:
    return "i8";
  case I16:
    return "i16";
  case I32:
    return "i32";
  case I64:
    return "i64";
  case U8:
    return "u8";
  case U16:
    return "u16";
  case U32:
    return "u32";
  case U64:
    return "u64";
  case F32:
    return "f32";
  case F64:
    return "f64";
  case BOOL:
    return "bool";
  case CHAR:
    return "char";
  case STRING:
    return "string";
  default:
    assert(false);
  }
}

//each pointer type can only convert to its own
//immutable to mutable subtype not allowed
bool Pointer::convertibleTo(const Pointer* other) const noexcept {
  using enum Pointers;
  const auto other_type = other->pointer_type;
  if (pointer_type == VAGUE)
    return other_type == VAGUE;

  return (pointer_type == other_type) &&
         (pointed_type == other->pointed_type) &&
           (is_pointed_mutable || !other->is_pointed_mutable);
}

std::string Pointer::toString() const noexcept {
  switch (pointer_type) {
  case Pointers::RAW:
    return "raw -> " + pointed_type->toString();
  case Pointers::UNIQUE:
    return "unique -> " + pointed_type->toString();
  case Pointers::VAGUE:
    return "vague -> ";
  default:
    assert(false);
  }
}


bool Pointer::sameAs(const Pointers ptr_type, const Type* pointed_type_,
                     const bool is_pointed_mutable_) const noexcept {
  return (ptr_type == pointer_type) &&
  (pointed_type_ == pointed_type) &&
    (is_pointed_mutable_ == is_pointed_mutable);
}

bool Variant::contains(const Type* type) const noexcept {
  for (const auto t : subtypes)
    if (type == t)
      return true;

  return false;
}

std::string Variant::toString() const noexcept {
  std::string retval = is_nullable ? "<devoid, " : "<";
  for (const auto subtype : subtypes) {
    retval.append(subtype->toString());
    retval.push_back(',');
    retval.push_back(' ');
  }

  retval.pop_back();
  retval.pop_back();
  retval.push_back('>');
  retval.push_back(' ');
  return retval;
}

bool Variant::sameAs(const std::vector<const Type*>& subtypes_, const bool nullable) const noexcept {
  const auto sz = subtypes.size();
  if (nullable != is_nullable || subtypes_.size() != sz)
    return false;

  for (auto i{0uz}; i<sz; ++i) {
    if (subtypes_[i] != subtypes[i])
      return false;
  }

  return true;
}

Primitive::Primitive(const Lexer::Token &primitive_token)
: Primitive([&] { //I hate this language so much
    using enum Lexer::TokenType;
    using enum Primitives;
    assert(Lexer::isCategoryPRIMITIVES(primitive_token.type));

    switch (primitive_token.type) {
    case KEYWORD_i8:
      return I8;
    case KEYWORD_i16:
      return I16;
    case KEYWORD_i32:
      return I32;
    case KEYWORD_i64:
      return I64;
    case KEYWORD_u8:
      return U8;
    case KEYWORD_u16:
      return U16;
    case KEYWORD_u32:
      return U32;
    case KEYWORD_u64:
      return U64;
    case KEYWORD_f32:
      return F32;
    case KEYWORD_f64:
      return F64;

    case KEYWORD_BOOL:
      return BOOL;
    case KEYWORD_CHAR:
      return CHAR;
    case KEYWORD_STRING:
      return STRING;

    default:
      assert(false);
    }
    }()) {}

Pointer::Pointer(const Lexer::Token& pointer_token, const Type* pointed_type, const bool is_pointed_mutable)
  : Pointer([&] {
    using enum Lexer::TokenType;
    using enum Pointers;
    assert(Lexer::isCategoryPOINTERS(pointer_token.type));

    switch (pointer_token.type) {
    case KEYWORD_RAW:
      return RAW;
    case KEYWORD_UNIQUE:
      return UNIQUE;
    case KEYWORD_VAGUE:
      return VAGUE;
    default:
      assert(false);
    }

  }(), pointed_type, is_pointed_mutable){}

bool Function::isValidCall(const std::vector<InstantiatedType>& parameters) const noexcept {
  assert(parameters.size() <= Settings::MAX_FUNCTION_PARAMETERS);
  const auto num_params = parameters.size();
  for (auto i{0uz}; i<num_params; ++i) {
    if (parameter_types[i] == nullptr)
      return false;
    if (not parameters[i].type->convertibleTo(parameter_types[i]))
      return false;
  }
  return true;
}

bool Function::sameAs(const std::vector<const Type *> &parameters, const Type *ret_type) const noexcept {
  if (ret_type != return_type || parameters.size() != num_parameters)
    return false;

  for (auto i{0uz}; i<num_parameters; ++i) {
    if (parameters[i] != parameter_types[i])
      return false;
  }

  return true;
}
