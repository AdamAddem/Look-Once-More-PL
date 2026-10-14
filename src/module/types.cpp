#include "types.hpp"

#include "edenlib/macros.hpp"

#include "error.hpp"
#include "lexing/lex.hpp"
#include "table_and_module.hpp"

#include <format>
#include <utility>

using namespace LOM;

/*
[[nodiscard]] Type const&
TypeID::getType() const noexcept {
  if (isPointer())
    return PointerType{*this};

  switch (derived) { using enum Type::DerivedType;
  case DEVOID:      return Type::devoid();
  case ERROR:       return Type::error();
  case PRIMITIVE:   return PrimitiveType::getTypeFromID(*this);
  case ARRAY:       return getModule(module_id).getArrayType(*this);
  case FUNCTION:    return getModule(module_id).getFunctionType(*this);
  case CUSTOM:      return getModule(module_id).getCustomType(*this);

  default: edenUnreachable("Invalid derived type.");
  }
} */

NoInlineCold [[nodiscard]] std::string
TypeID::toString() const noexcept {
  if (isPointer()) {
    auto x = *this;
    x.removePointer();
    if (ptr_specs.isTopLevelRaw())
      return "raw " + x.toString();
    return "ref " + x.toString();
  }

  switch (derived) { using enum Type::DerivedType;
  case DEVOID:      return "devoid";
  case ERROR:       return "!ERROR!";

  case PRIMITIVE:   return getPrimitiveType().toString();
  case ARRAY:       return getArrayType().toString();
  case FUNCTION:    return getFunctionType().toString();
  case CUSTOM:      return getCustomType().toString();
  default:
    edenUnreachable("Invalid derived type.");
  }
}

#define pre Assume(isArray());
[[nodiscard]] ArrayType const& TypeID::getArrayType() const noexcept { pre return getModule(module_id).getArrayType(*this); }
#undef pre

#define pre Assume(isFunction());
[[nodiscard]] FunctionType const& TypeID::getFunctionType() const noexcept { pre return getModule(module_id).getFunctionType(*this); }
#undef pre

#define pre Assume(isCustom());
[[nodiscard]] CustomType const& TypeID::getCustomType() const noexcept { pre return getModule(module_id).getCustomType(*this); }
#undef pre

NodiscardCXPR static bool
primitiveCoercibleFromTo(PrimitiveType const& from, PrimitiveType const& to) noexcept {
  switch (from.getUnderlyingPrimitiveType()) { using enum PrimitiveType::PrimitiveTypeEnum;
  case I8:
  case I16:
  case I32:
  case I64: return to.isSignedIntegral() and (to.bitwidth() > from.bitwidth());

  case U8:
  case U16:
  case U32: //convert if other type is a greater size signed/unsigned integer
  case U64: return to.isIntegral() and (to.bitwidth() > from.bitwidth());

  case U7:
  case U15:
  case U31:
  case U63: return to.isIntegral() and (to.bitwidth() >= from.bitwidth());


  case F32: return to.getUnderlyingPrimitiveType() == F64;
  case F64:
  case BOOL:
  case CHAR: return false; //only converts to the same type which has been checked already

  case STRING: return false;
  default:
    Unreachable("Invalid primitive type.");
  }
}

NodiscardCXPR static bool
primitiveCastableFromTo(PrimitiveType const& from, PrimitiveType const& to) noexcept {
  switch (from.getUnderlyingPrimitiveType()) { using enum PrimitiveType::PrimitiveTypeEnum;
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
  case CHAR: return eden::enumBetween(to.getUnderlyingPrimitiveType(), U7, CHAR);

  case STRING: return false;
  default:
    Unreachable("Invalid primitive type.");
  }
}

[[nodiscard]] bool
TypeID::coercibleTo(TypeID other) const noexcept {
  if (sameAs(other)) return true;
  if (isError() or other.isError()) return true;

  if (isPointer()) {
    if (not other.isPointer()) return false;

    if (not getPointedTypeID().sameAs(other.getPointedTypeID())) return false;
    if (other.isTopLevelPtrRaw()) return isTopLevelPtrRaw();
    return true;
  }

  if (other.isPointer()) {
    if (isPointer()) return true; // pointer casts are unchecked currently

    // bodge to accept string -> ref u8 conversion. TODO: Change
    if (not isPrimitive()) return false;
    if (not getPrimitiveType().isString()) return false;
    if (other.isTopLevelPtrRaw()) return false;
    return other.getPointedTypeID() == PrimitiveType::u8ID();
  }

  if (derived != other.derived) return false;
  switch (derived) { using enum Type::DerivedType;
  case DEVOID:
  case ERROR:      edenUnreachable("This should've returned earlier.");
  case FUNCTION:   edenUnreachable("I'd be very confused if this code ran");

  // can only coerce to same type which has been checked
  case ARRAY:      return false;
  case CUSTOM:     return false;

  case PRIMITIVE:  return primitiveCoercibleFromTo( this->getPrimitiveType() , other.getPrimitiveType() );
  default: edenUnreachable("Invalid derived type.");
  }
}

[[nodiscard]] bool
TypeID::castableTo(TypeID other) const noexcept {
  if (sameAs(other)) return true;
  if (isError() or other.isError()) return true;

  if (other.isPointer()) {
    if (isPointer()) return true; // pointer casts are unchecked currently

    // bodge to accept string -> ref u8 conversion. TODO: Change
    if (not isPrimitive()) return false;
    if (not getPrimitiveType().isString()) return false;
    if (other.isTopLevelPtrRaw()) return false;
    return other.getPointedTypeID() == PrimitiveType::u8ID();
  }

  if (derived != other.derived) return false;

  switch (derived) { using enum Type::DerivedType;
  case PRIMITIVE:   return primitiveCastableFromTo( this->getPrimitiveType(), other.getPrimitiveType() );

  // can only cast to self which has been checked earlier
  case ARRAY:
  case CUSTOM: return false;

  case DEVOID:
  case ERROR:      Unreachable("Only one devoid/error instance allowed, this should've returned earlier.");
  case FUNCTION:   Unreachable("Why in the world would this run?");
  default:         Unreachable("Invalid derived type.");
  }
}

[[nodiscard]] bool
TypeID::sameAs(TypeID other) const noexcept {
  if (derived != other.derived) return false;
  if (ptr_specs != other.ptr_specs) return false;

  if (isPointer()) {
    return getMostPointedTypeID().sameAs(other.getMostPointedTypeID());
  }

  switch (derived) { using enum Type::DerivedType;
  case DEVOID:
  case ERROR:
  case CUSTOM:
  case PRIMITIVE: return *this == other;

  case ARRAY:    return getArrayType().sameAs(other.getArrayType());
  case FUNCTION: return getFunctionType().sameAs(other.getFunctionType());

  default: Unreachable("Invalid derived type.");
  }

}


CustomType::CustomType(std::string_view name) noexcept : Type(CUSTOM),  name_len(name.length()), name(name.data()) { std::construct_at<SymbolTable>( (SymbolTable*) symboltable_buff); }
[[nodiscard]] SymbolTable&       CustomType::member_table()       noexcept { return *std::launder( (SymbolTable*) symboltable_buff ); }
[[nodiscard]] SymbolTable const& CustomType::member_table() const noexcept { return *std::launder( (SymbolTable const*) symboltable_buff ); }

NoInlineCold [[nodiscard]] std::string
PrimitiveType::toString() const noexcept {
  switch (primitive_type) {
  case I8:      return "i8";
  case I16:     return "i16";
  case I32:     return "i32";
  case I64:     return "i64";
  case U7:      return  "u7";
  case U15:     return "u15";
  case U31:     return "u31";
  case U63:     return "u63";
  case U8:      return "u8";
  case U16:     return "u16";
  case U32:     return "u32";
  case U64:     return "u64";
  case F32:     return "f32";
  case F64:     return "f64";
  case BOOL:    return "bool";
  case CHAR:    return "char";
  case STRING:  return "string";
  default: Unreachable("Invalid primitive type.");
  }
}

NoInlineCold [[nodiscard]] std::string
ArrayType::toString() const noexcept {
  return std::format("[{}]{}", getSize(), getSubtypeID().toString());
}

NoInlineCold [[nodiscard]] std::string
FunctionType::toString() const noexcept {
  auto const parameter_typeIDs = getParameterTypeIDs();
  auto const return_typeID = getReturnTypeID();
  std::string string_rep("(");
  for (auto parameter : parameter_typeIDs) {
    string_rep.append( parameter.toString() );
    string_rep.append(", ");
  }

  if (isVariadic()) {
    string_rep.append("...");
  }
  else if (numParameters() not_eq 0) {
    string_rep.pop_back();
    string_rep.pop_back();
  }

  string_rep.append(") ");
  if (return_typeID.derived not_eq DEVOID)
    string_rep.append(return_typeID.toString());

  return string_rep;
}

NoInlineCold [[nodiscard]] std::string
CustomType::definitionToString() const noexcept {
  std::string string_rep("struct ");
  string_rep.append(nameof());
  string_rep.append(" {");

  auto const& table = member_table();
  auto const num_members = table.num_variables();
  for (sz_t i{}; i<num_members; ++i) {
    auto const& member = table.getVariable(i);
    string_rep.append("\n\t");
    string_rep.append(member.typeID.toString());
    string_rep.push_back(' ');
    string_rep.append(member.nameof());
    string_rep.push_back(',');
  }

  if (num_members not_eq 0)
    string_rep.pop_back();
  string_rep.append("\n}");

  return string_rep;
}
