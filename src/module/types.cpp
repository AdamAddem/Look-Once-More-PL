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

edenNoInlineCold [[nodiscard]] std::string
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

  case PRIMITIVE:   return primitiveToString();
  case ARRAY:       return arrayToString();
  case FUNCTION:    return functionToString();
  case CUSTOM:      return customToString();
  default:
    edenUnreachable("Invalid derived type.");
  }
}

#define pre edenAssume(isArray());
[[nodiscard]] ArrayType const& TypeID::getArrayType() const noexcept { pre return getModule(module_id).getArrayType(*this); }
#undef pre

#define pre edenAssume(isFunction());
[[nodiscard]] FunctionType const& TypeID::getFunctionType() const noexcept { pre return getModule(module_id).getFunctionType(*this); }
#undef pre

#define pre edenAssume(isCustom());
[[nodiscard]] CustomType const& TypeID::getCustomType() const noexcept { pre return getModule(module_id).getCustomType(*this); }
#undef pre

edenNodiscardCXPR static bool
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
    edenUnreachable("Invalid primitive type.");
  }
}

edenNodiscardCXPR static bool
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
  case CHAR: return eden::enumBetween(to.getUnderlyingPrimitiveType(), I8, CHAR);

  case STRING: return false;
  default:
    edenUnreachable("Invalid primitive type.");
  }
}

edenNodiscardCXPR static bool
pointerCoercibleFromTo(PointerType const& from, PointerType const& to) noexcept {
  if (to.isRaw() and from.isRef()) return false;
  return from.getPointedTypeID().sameAs( to.getPointedTypeID() );
}

[[nodiscard]] bool
TypeID::coercibleTo(TypeID other) const noexcept {
  if (sameAs(other)) return true;
  if (isError() or other.isError()) return true;

  if (isPointer()) {
    if (not other.isPointer()) return false;

    auto const pointed = getPointerType().getPointedTypeID();
    auto const other_pointed = other.getPointerType().getPointedTypeID();
    if (not pointed.sameAs(other_pointed)) return false;
    if (other.isTopLevelPtrRaw()) return this->isTopLevelPtrRaw();
    return true;
  }

  // bodge to accept ref u8 -> string conversion. TODO: Change
  if (other.isPointer() and this->isPrimitive()) {
    if (not getPrimitiveType().isString()) return false;
    if (other.isTopLevelPtrRaw()) return false;

    auto const other_pointedID = other.getPointerType().getPointedTypeID();
    return other_pointedID == PrimitiveType::u8ID();
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
  case POINTER:    return pointerCoercibleFromTo( this->getPointerType(), other.getPointerType() );


  default: edenUnreachable("Invalid derived type.");
  }

}

[[nodiscard]] bool
TypeID::castableTo(TypeID other) const noexcept {
  if (sameAs(other)) return true;

  // bodge to accept ref u8 -> string conversion. TODO: Change
  if (other.isPointer() and this->isPrimitive()) {
    if (not getPrimitiveType().isString()) return false;
    if (other.isTopLevelPtrRaw()) return false;

    auto const other_pointedID = other.getPointerType().getPointedTypeID();
    return other_pointedID == PrimitiveType::u8ID();
  }

  if (derived != other.derived) return false;

  switch (derived) { using enum Type::DerivedType;
  case DEVOID:
  case ERROR:      edenUnreachable("Only one devoid instance allowed, this should've returned earlier.");
  case FUNCTION:   edenUnreachable("Why in the world would this run?");

  // can only cast to self which has been checked earlier
  case ARRAY:       return false;
  case CUSTOM:      return false;

  case POINTER:     return true; // pointer are currently unchecked

  case PRIMITIVE:   return primitiveCastableFromTo( this->getPrimitiveType(), other.getPrimitiveType() );
  default:          edenUnreachable("Invalid derived type.");
  }
}


[[nodiscard]] bool
TypeID::sameAs(TypeID other) const noexcept {
  if (*this == other) return true;

  if (derived != other.derived) return false;
  if (ptr_specs != other.ptr_specs) return false;

  if (isPointer()) {
    // other should be pointer if this is
    return getPointerType().getPointedTypeID().sameAs(other.getPointerType().getPointedTypeID());
  }

  switch (derived) { using enum Type::DerivedType;
  case DEVOID:
  case ERROR:
  case PRIMITIVE: return true;

  case POINTER: edenUnreachable("Should have succeeded earlier.");

  case ARRAY: return getArrayType().sameAs( other.getArrayType() );

  case FUNCTION: {
    auto const& this_fn_type = getFunctionType();
    auto const& other_fn_type = other.getFunctionType();

    if (this_fn_type.numParameters() != other_fn_type.numParameters()) return false;
    if (this_fn_type.isVariadic() != other_fn_type.isVariadic()) return false;
    if (not this_fn_type.getReturnTypeID().sameAs( other_fn_type.getReturnTypeID() )) return false;

    auto const this_parameterTypeIDs = this_fn_type.getParameterTypeIDs();
    auto const other_parameterTypeIDs = other_fn_type.getParameterTypeIDs();
    for (auto i{0uz}; i<this_fn_type.numParameters(); ++i) {
      auto const this_param_typeID = this_parameterTypeIDs[i];
      auto const other_param_typeID = other_parameterTypeIDs[i];

      if (not this_param_typeID.sameAs(other_param_typeID)) return false;
    }
    return true;
  }

  case CUSTOM: return false; // custom types can only be the same bitwise

  default: edenUnreachable("Invalid derived type.");
  }

}


CustomType::CustomType(std::string_view name) noexcept : Type(CUSTOM),  name_len(name.length()), name(name.data()) { std::construct_at<SymbolTable>( (SymbolTable*) symboltable_buff); }
[[nodiscard]] SymbolTable&       CustomType::member_table()       noexcept { return *std::launder( (SymbolTable*) symboltable_buff ); }
[[nodiscard]] SymbolTable const& CustomType::member_table() const noexcept { return *std::launder( (SymbolTable const*) symboltable_buff ); }

edenNoInlineCold [[nodiscard]] std::string
TypeID::primitiveToString() const noexcept {
  auto const primitive_type = PrimitiveType::getTypeFromID(*this).getUnderlyingPrimitiveType();
  switch (primitive_type) { using enum PrimitiveType::PrimitiveTypeEnum;
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
  default: edenUnreachable("Invalid primitive type.");
  }
}

edenNoInlineCold [[nodiscard]] std::string
TypeID::arrayToString() const noexcept {
  auto const& array_type = getModule(module_id).getArrayType(*this);
  return std::format("[{}]{}", array_type.getSize(), array_type.getSubtypeID().toString());
}

edenNoInlineCold [[nodiscard]] std::string
TypeID::functionToString() const noexcept {
  auto const& fn_type = getModule(module_id).getFunctionType(*this);
  auto const parameter_typeIDs = fn_type.getParameterTypeIDs();
  auto const return_typeID = fn_type.getReturnTypeID();
  std::string string_rep("(");
  for (auto parameter : parameter_typeIDs) {
    string_rep.append( parameter.toString() );
    string_rep.append(", ");
  }

  if (fn_type.isVariadic()) {
    string_rep.append("...");
  }
  else if (fn_type.numParameters() not_eq 0) {
    string_rep.pop_back();
    string_rep.pop_back();
  }

  string_rep.append(") ");
  if (return_typeID.derived not_eq Type::DEVOID)
    string_rep.append(return_typeID.toString());

  return string_rep;
}

edenNoInlineCold [[nodiscard]] std::string
TypeID::customToString() const noexcept {
  return getModule(module_id).getCustomType(*this).toString();
}

edenNoInlineCold [[nodiscard]] std::string
CustomType::definitionToString() const noexcept {
  std::string string_rep("struct ");
  string_rep.append(nameof());
  string_rep.append(" {");

  auto const& table = member_table();
  auto const num_members = table.num_variables();
  for (auto i{0uz}; i<num_members; ++i) {
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
