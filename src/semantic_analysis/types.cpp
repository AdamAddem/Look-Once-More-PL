#include "types.hpp"

#include "edenlib/macros.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include <utility>

using namespace LOM;

eden_nonull_args
bool Type::coercibleTo(const Type* other) const noexcept {
  const auto other_type = other->derived_type;
  assume_assert(derived_type not_eq VARIANT);
  assume_assert(other_type not_eq VARIANT);
  if (other == this)
    return true;

  // temporary to allow string literal to raw<char> conversion
  if (derived_type == PRIMITIVE and castToPrimitive()->isString()) {
    if (other->isPointer() and other->castToPointer()->getSubtype().type == PrimitiveType::char_())
      return true;
  }

  if (derived_type not_eq other_type or flags not_eq other->flags)
    return false;

  switch (derived_type) {
  case DEVOID:
    eden_unreachable("Only one devoid instance allowed, this should've returned earlier.");
  case PRIMITIVE:
    return castToPrimitive()->coercibleTo(other->castToPrimitive());
  case POINTER:
    return castToPointer()->coercibleTo(other->castToPointer());
  case VARIANT:
  case FUNCTION:
  case CUSTOM:
    return castToCustom()->coercibleTo(other->castToCustom());
  default:
    eden_unreachable("Invalid derived type.");
  }

}

eden_nonull_args
bool Type::castableTo(const Type* other) const noexcept {
  const auto other_type = other->derived_type;
  assume_assert(derived_type not_eq CUSTOM); assume_assert(derived_type not_eq VARIANT);
  assume_assert(other_type not_eq CUSTOM); assume_assert(other_type not_eq VARIANT);
  if (other == this)
    return true;

  if (derived_type == PRIMITIVE and castToPrimitive()->isString()) {
    if (other->isPointer() and other->castToPointer()->getSubtype().type == PrimitiveType::char_())
      return true;
  }

  if (derived_type not_eq other_type)
    return false;

  switch (derived_type) {
  case DEVOID:
    eden_unreachable("Only one devoid instance allowed, this should've returned earlier.");
  case PRIMITIVE:
    return castToPrimitive()->castableTo(other->castToPrimitive());
  case POINTER:
    return castToPointer()->castableTo(other->castToPointer());
  case VARIANT:
  case FUNCTION:
  case CUSTOM:
  default:
    eden_unreachable("Invalid derived type.");
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
    return static_cast<const CustomType*>(this)->toString();
  default:
    eden_unreachable("Invalid derived type.");
  }
}


/* Primitive Type */
eden_nonull_args
bool PrimitiveType::coercibleTo(const PrimitiveType* other) const noexcept {
  const auto other_type = other->primitive_type;
  switch (primitive_type) {
  case I8:
  case I16:
  case I32:
  case I64:
    return other->isSignedIntegral() and (other->bitwidth() > bitwidth());
  case U8:
  case U16:
  case U32:
  case U64: //convert if other type is a greater size signed/unsigned integer
    return other->isIntegral() and (other->bitwidth() > bitwidth());

  case U7:
  case U15:
  case U31:
  case U63:
    return other->isIntegral() and (other->bitwidth() >= bitwidth());


  case F32:
    return other_type == F64;
  case F64:
  case BOOL:
  case CHAR:
    return false; //only converts to the same type which has been checked already
  case STRING: //only converts to char pointer
    if (other->isPointer()) {
      auto other_subtype = other->castToPointer()->getSubtype();
      if (other_subtype.qualifiers.is_mutable == false and other_subtype.type == char_())
        return true;
    }
    return false;
  default:
    eden_unreachable("Invalid primitive type.");
  }
}

eden_nonull_args
bool PrimitiveType::castableTo(const PrimitiveType* other) const noexcept {
  const auto other_type = other->primitive_type;
  switch (primitive_type) {
  case I8:
  case I16:
  case I32:
  case I64:
  case U7:
  case U15:
  case U31:
  case U63:
  case U8:
  case U16:
  case U32:
  case U64:
  case F32:
  case F64:
  case BOOL:
  case CHAR:
    return eden::enumBetween(other_type, I8, CHAR);

  case STRING: //Right now string types can only be literals, so it is only convertible to raw -> char
    if (other->isPointer()) {
      const auto other_subtype = other->castToPointer()->getSubtype();
      if (not other_subtype.qualifiers.is_mutable and other_subtype.type == char_())
        return true;
    }
    return false;
  default:
    eden_unreachable("Invalid primitive type.");
  }
}

std::string PrimitiveType::toString() const noexcept {
  switch (primitive_type) {
  case I8: return "i8";
  case I16: return "i16";
  case I32: return "i32";
  case I64: return "i64";
  case U7: return  "u7";
  case U15: return "u15";
  case U31: return "u31";
  case U63: return "u63";
  case U8: return "u8";
  case U16: return "u16";
  case U32: return "u32";
  case U64: return "u64";


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
    eden_unreachable("Invalid primitive type.");
  }
}
/* Primitive Type */

/* Pointer Type */
eden_nonull_args
bool PointerType::coercibleTo(const PointerType* other) const noexcept {
  const auto other_type = other->pointer_type;
  const auto other_subtype = other->subtype;
  assume_assert(pointer_type not_eq UNIQUE); assume_assert(other_type not_eq UNIQUE);

  if (pointer_type == VAGUE)
    return other_type == VAGUE;
  if (other_type == VAGUE)
    return true;

  //reject if different pointer types
  if (pointer_type not_eq other_type)
    return false;

  //reject const to mutable conversion
  if (not subtype.qualifiers.is_mutable and other_subtype.qualifiers.is_mutable)
    return false;

  //if our subtype is a pointer, return whether other subtype is a pointer and our sub-pointer is convertible to theirs
  if (subtype.type->isPointer())
    return other_subtype.type->isPointer() and
           subtype.type->castToPointer()->coercibleTo(other_subtype.type->castToPointer());

  //otherwise, return whether the subtypes are identical
  return subtype.type == other->subtype.type;
}

eden_nonull_args //TODO: Revisit pointer casting rules. At the moment its unconditional.
bool PointerType::castableTo(const PointerType* other) const noexcept {
  assume_assert(pointer_type not_eq UNIQUE); assume_assert(other->pointer_type not_eq UNIQUE);
  return true;
}

std::string PointerType::toString() const noexcept {
  switch (pointer_type) {
  case RAW:
    return "raw<" + subtype.toString() + ">";
  case UNIQUE:
    return "unique<" + subtype.toString() + ">";
  case VAGUE:
    return "vague<" + std::string(subtype.qualifiers.is_mutable ? "mut " : "") + ">";
  default:
    eden_unreachable("Invalid pointer type.");
  }
}
/* Pointer Type */

/* Variant Type (Incomplete) */
eden_nonull_args
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

bool VariantType::sameAs(std::span<const Type*> subtypes_, bool nullable) const noexcept {
  if (nullable not_eq is_nullable or subtypes_.size() not_eq num_subtypes)
    return false;

  for (auto i{0uz}; i < num_subtypes; ++i)
    if (subtypes_[i] not_eq subtypes[i])
      return false;

  return true;
}
/* Variant Type (Incomplete) */

/* Function Type */
std::string FunctionType::toString() const noexcept {
  std::string string_rep("(");
  for (auto i{0uz}; i<num_parameters; ++i) {
    string_rep.append(subtypes[i]->toString());
    string_rep.append(", ");
  }

  if (is_variadic) {
    string_rep.append("...");
  }
  else if (num_parameters not_eq 0) {
    string_rep.pop_back();
    string_rep.pop_back();
  }

  string_rep.append(") ");
  if (not returnType()->isDevoid())
    string_rep.append(returnType()->toString());

  return string_rep;
}
/* Function Type */

#include "symbol_table.hpp"
/* Custom Type */
CustomType::CustomType(std::string_view name)
: Type(CUSTOM),  name_len(name.length()), name(name.data())
{ std::construct_at<SymbolTable>(reinterpret_cast<SymbolTable*>(symboltable_buff)); }

[[nodiscard]] SymbolTable*
CustomType::member_table() noexcept
{ return std::launder(reinterpret_cast<SymbolTable*>(symboltable_buff)); }

[[nodiscard]] const SymbolTable*
CustomType::member_table() const noexcept
{ return std::launder(reinterpret_cast<const SymbolTable*>(symboltable_buff)); }

std::string CustomType::definitionToString() const noexcept {
  std::string string_rep("struct ");
  string_rep.append(nameof());
  string_rep.append(" {");

  auto const& member_variables = member_table()->getVariableList();
  for (auto const& member : member_variables) {
    string_rep.append("\n\t");
    string_rep.append(member.type.toString());
    string_rep.push_back(' ');
    string_rep.append(member.name);
    string_rep.push_back(',');
  }

  if (not member_variables.empty())
    string_rep.pop_back();
  string_rep.append("\n}");

  return string_rep;
}
/* Custom Type */
