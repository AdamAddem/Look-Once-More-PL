#include "types.hpp"
#include "edenlib/assume_assert.hpp"
#include "lexing/lex.hpp"

using namespace LOM;

bool Type::convertibleTo(const Type* other) const noexcept {
  assume_assert(derived_type not_eq CUSTOM);
  assume_assert(other->derived_type not_eq CUSTOM);

  if (other == this)
    return true;

  const auto other_type = other->derived_type;
  if (other_type == VARIANT)
    return other->castToVariant()->contains(this);

  if (derived_type not_eq other_type or flags not_eq other->flags)
    return false;


  switch (derived_type) {
  case DEVOID:
    std::unreachable(); //only one devoid instance allowed, so this should've returned earlier
  case PRIMITIVE:
    return castToPrimitive()->convertibleTo(other->castToPrimitive());
  case POINTER:
    return castToPointer()->convertibleTo(other->castToPointer());
  case VARIANT:
  case FUNCTION:
  case CUSTOM:
    return false;
  default:
    std::unreachable();
  }

}

std::string Type::toString() const noexcept {
  switch (derived_type) {
  case DEVOID:
    return "devoid";
  case PRIMITIVE:
    return static_cast<const PrimitiveType*>(this)->toString();
  case POINTER:
    return static_cast<const PointerType*>(this)->toString();
  case VARIANT:
    return static_cast<const VariantType*>(this)->toString();
  case FUNCTION:
    return static_cast<const FunctionType*>(this)->toString();
  case CUSTOM:
  default:
    std::unreachable();
  }
}

bool PrimitiveType::convertibleTo(const PrimitiveType* other) const noexcept {
  const auto other_type = other->primitive_type;
  if (other_type == primitive_type) return true;

  switch (primitive_type) {
  case I8:
  case I16:
  case I32:
  case I64: //convert if other type is a greater size signed integer
    static_assert(std::to_underlying(I8) < std::to_underlying(I16));
    static_assert(std::to_underlying(I16) < std::to_underlying(I32));
    static_assert(std::to_underlying(I32) < std::to_underlying(I64));
    return std::to_underlying(other_type) > std::to_underlying(primitive_type) and
           std::to_underlying(other_type) <= std::to_underlying(I64);
  case U8:
  case U16:
  case U32:
  case U64: //convert if other type is a greater size signed/unsigned integer
    static_assert(std::to_underlying(U8) < std::to_underlying(U16));
    static_assert(std::to_underlying(U16) < std::to_underlying(U32));
    static_assert(std::to_underlying(U32) < std::to_underlying(U64));
    static_assert(std::to_underlying(U8) - 4 == std::to_underlying(I8));
    static_assert(std::to_underlying(U64) - 4 == std::to_underlying(I64));
    if (other->isUnsignedIntegral() and std::to_underlying(other_type) > std::to_underlying(primitive_type))
      return true;

    return std::to_underlying(other_type) > (std::to_underlying(primitive_type) - 4) and
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
    std::unreachable();
  }
}

std::string PrimitiveType::toString() const noexcept {
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
    std::unreachable();
  }
}

//each pointer type can only convert to its own
//immutable to mutable subtype not allowed
bool PointerType::convertibleTo(const PointerType* other) const noexcept {
  const auto other_type = other->pointer_type;
  if (pointer_type == VAGUE)
    return other_type == VAGUE;

  return (pointer_type == other_type) and
         (subtype.type == other->subtype.type) and
         (subtype.details.is_mutable or not other->subtype.details.is_mutable);
}

std::string PointerType::toString() const noexcept {
  switch (pointer_type) {
  case RAW:
    return "raw -> " + subtype.toString();
  case UNIQUE:
    return "unique -> " + subtype.toString();
  case VAGUE:
    return "vague -> ";
  default:
    std::unreachable();
  }
}

bool VariantType::contains(const Type* type) const noexcept {
  for (const auto t : subtypes)
    if (type == t)
      return true;

  return false;
}

std::string VariantType::toString() const noexcept {
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

bool VariantType::sameAs(const std::vector<const Type*>& subtypes_, bool nullable) const noexcept {
  const auto sz = subtypes.size();
  if (nullable not_eq is_nullable or subtypes_.size() not_eq sz)
    return false;

  for (auto i{0uz}; i < sz; ++i)
    if (subtypes_[i] not_eq subtypes[i])
      return false;

  return true;
}

std::string FunctionType::toString() const noexcept {
  std::string string_rep("(");
  auto parameter_typesx = parameterTypes();
  for (auto param_type : parameter_typesx) {
    string_rep.append(param_type->toString());
    string_rep.append(", ");
  }
  if (num_parameters not_eq 0) {
    string_rep.pop_back();
    string_rep.pop_back();
  }
  string_rep.push_back(')');
  if (not return_type->isDevoid()) {
    string_rep.append(" -> ");
    string_rep.append(return_type->toString());
  }

  return string_rep;
}


bool FunctionType::isValidCall(std::span<InstantiatedType> parameters) const noexcept {
  assert(parameters.size() <= Settings::MAX_FUNCTION_PARAMETERS);
  const auto num_params = parameters.size();
  for (auto i{0uz}; i < num_params; ++i) {
    if (parameter_types[i] == nullptr)
      return false;
    if (not parameters[i].type->convertibleTo(parameter_types[i]))
      return false;
  }
  return true;
}
