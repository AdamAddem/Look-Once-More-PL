#include "types.hpp"

#include "edenlib/macros.hpp"
#include "error.hpp"
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

  if (derived_type == PRIMITIVE and castToPrimitive()->isString()) {
    if (other->isPointer()) {
      auto other_subtype = other->castToPointer()->getSubtype();
      if (other_subtype.details.is_mutable == false and other_subtype.type == PrimitiveType::char_())
        return true;
    }
  }

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
  if (other_type == U_ and isIntegral())
    return true;

  switch (primitive_type) {
  case I8:
  case I16:
  case I32:
  case I64:
    return other->isSignedIntegral() and (other->bitwidth() > bitwidth());
  case U_:
    return other->isIntegral();
  case U8:
  case U16:
  case U32:
  case U64: //convert if other type is a greater size signed/unsigned integer
    return other->isIntegral() and (other->bitwidth() > bitwidth());
  case F32:
    return other_type == F64;
  case F64:
  case BOOL:
  case CHAR:
    return false; //only converts to the same type which has been checked already
  case STRING: //only converts to char pointer
    if (other->isPointer()) {
      auto other_subtype = other->castToPointer()->getSubtype();
      if (other_subtype.details.is_mutable == false and other_subtype.type == char_())
        return true;
    }
    return false;
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
  case U_:
    return "u_";
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
  for (auto i{0uz}; i<subtypes.size() - 1; ++i) {
    string_rep.append(subtypes[i]->toString());
    string_rep.append(", ");
  }

  if (is_variadic) {
    string_rep.append("...");
  }
  else if (subtypes.size() not_eq 1) {
    string_rep.pop_back();
    string_rep.pop_back();
  }

  string_rep.push_back(')');
  if (not subtypes.back()->isDevoid()) {
    string_rep.append(" -> ");
    string_rep.append(subtypes.back()->toString());
  }

  return string_rep;
}

void FunctionType::validateCall(std::span<InstantiatedType> parameters) const {
  const auto num_params = subtypes.size() - 1;
  if (parameters.size() < num_params)
    throw ValidationError("Too few parameters for function call.", "PLACEHOLDER", 0);
  if (parameters.size() > num_params and (not is_variadic))
    throw ValidationError("Too many parameters for function call.", "PLACEHOLDER", 0);

  for (auto i{0uz}; i < num_params; ++i) {
    if (not parameters[i].type->convertibleTo(subtypes[i]))
      throw ValidationError("Cannot convert parameter to parameter type.",
      std::format("Parameter of type '{}' cannot convert to type '{}'", parameters[i].toString(), subtypes[i]->toString()),
      0);
  }

}