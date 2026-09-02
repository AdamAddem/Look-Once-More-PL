#include "types.hpp"

#include "edenlib/macros.hpp"

#include "error.hpp"
#include "lexing/lex.hpp"
#include "table_and_module.hpp"

#include <utility>
#include <format>

using namespace LOM;

[[nodiscard]] Type const& TypeID::getType(Module const& contained_module) const noexcept { assert(contained_module.getID() == module_id); return contained_module.getTypeFromID(*this); }
[[nodiscard]] Type const& TypeID::getType() const noexcept { return getType(getModule(module_id)); }

bool Type::coercibleTo(Type const* other) const noexcept {
  if (this  == other)    return true;
  if (this  == error())  return true;
  if (other == error())  return true;

  auto const other_type = other->derived_type;

  // temporary! TODO: Change
  if (other_type == POINTER and derived_type == PRIMITIVE) {
    auto const as_primitive = castToPrimitive();
    if (not as_primitive->isString()) return false;

    auto const other_as_pointer = other->castToPointer();
    auto const other_subtype = other_as_pointer->getSubtype();
    if (other_subtype.qualifiers.writable) return false;
    return other_subtype.type == PrimitiveType::u8();
  }

  if (derived_type not_eq other_type) return false;
  if (flags not_eq other->flags)      return false;

  switch (derived_type) {
  case DEVOID:
  case ERROR:      edenUnreachable("Only one devoid / error instance allowed, this should've returned earlier.");
  case FUNCTION:   edenUnreachable("Why in the world would this run?");

  case ARRAY:      return false;
  case PRIMITIVE:  return castToPrimitive()->coercibleTo(other->castToPrimitive());
  case POINTER:    return castToPointer()->coercibleTo(other->castToPointer());
  case CUSTOM:     return castToCustom()->coercibleTo(other->castToCustom());
  default:
    edenUnreachable("Invalid derived type.");
  }

}
bool PrimitiveType::coercibleTo(PrimitiveType const* other) const noexcept {
  auto const other_type = other->primitive_type;
  switch (primitive_type) {
  case I8:
  case I16:
  case I32:
  case I64: return other->isSignedIntegral() and (other->bitwidth() > bitwidth());

  case U8:
  case U16:
  case U32: //convert if other type is a greater size signed/unsigned integer
  case U64: return other->isIntegral() and (other->bitwidth() > bitwidth());

  case U7:
  case U15:
  case U31:
  case U63: return other->isIntegral() and (other->bitwidth() >= bitwidth());


  case F32: return other_type == F64;
  case F64:
  case BOOL:
  case CHAR: return false; //only converts to the same type which has been checked already

  case STRING: return false;
  default:
    edenUnreachable("Invalid primitive type.");
  }
}
bool PointerType::coercibleTo(PointerType const* other) const noexcept {
  if (pointed_type == error()) return true;

  auto const other_subtype = other->getSubtype();
  if (other_subtype.type == error()) return true;
  if (other_subtype.type == PrimitiveType::u8()) return true;

  if (other_subtype.qualifiers.writable and not pointed_is_readwrite) return false;

  if (pointed_type->isPointer())
    return other_subtype.type->isPointer() and pointed_type->castToPointer()->coercibleTo(other_subtype.type->castToPointer());

  if (pointed_type not_eq other_subtype.type) return false;
  return true;
}

bool Type::castableTo(Type const* other) const noexcept {
  if (this == other)    return true;
  if (this == error())  return true;
  if (other == error())  return true;

  auto const other_type = other->derived_type;

  if (other_type == POINTER and derived_type == PRIMITIVE) {
    auto const as_primitive = castToPrimitive();
    if (not as_primitive->isString()) return false;

    auto const other_as_pointer = other->castToPointer();
    auto const other_subtype = other_as_pointer->getSubtype();
    if (other_subtype.qualifiers.writable) return false;
    return other_subtype.type == PrimitiveType::u8();
  }

  if (derived_type not_eq other_type) return false;

  switch (derived_type) {
  case DEVOID:
  case ERROR:      edenUnreachable("Only one devoid instance allowed, this should've returned earlier.");
  case FUNCTION:   edenUnreachable("Why in the world would this run?");

  case ARRAY:       return castToArray()->    castableTo(other->castToArray());
  case CUSTOM:      return castToCustom()->   castableTo(other->castToCustom());
  case PRIMITIVE:   return castToPrimitive()->castableTo(other->castToPrimitive());
  case POINTER:     return castToPointer()->  castableTo(other->castToPointer());

  default:          edenUnreachable("Invalid derived type.");
  }
}
bool PrimitiveType::castableTo(PrimitiveType const* other) const noexcept {
  auto const other_type = other->primitive_type;
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

  case STRING: return false;
  default:
    edenUnreachable("Invalid primitive type.");
  }
}
bool PointerType::castableTo(PointerType const*) const noexcept {
  return true;
}

CustomType::CustomType(std::string_view name) noexcept : Type(CUSTOM),  name_len(name.length()), name(name.data()) { std::construct_at<SymbolTable>( (SymbolTable*) symboltable_buff); }
[[nodiscard]] SymbolTable*       CustomType::member_table()       noexcept { return std::launder( (SymbolTable*) symboltable_buff ); }
[[nodiscard]] SymbolTable const* CustomType::member_table() const noexcept { return std::launder( (SymbolTable const*) symboltable_buff ); }

edenNoInlineCold std::string TypeID::toString(Module const& owning_module) const noexcept { assert(owning_module.getID() == module_id); return getType(owning_module).toString(owning_module); }
edenNoInlineCold std::string TypeID::toString() const noexcept { return toString(getModule(module_id)); }
edenNoInlineCold std::string Type::toString(Module const& owning_module) const noexcept {
  switch (derived_type) {
  case DEVOID:      return "devoid";
  case ERROR:       return "!ERROR!";

  case PRIMITIVE:   return static_cast<PrimitiveType const&>(*this).toString();
  case CUSTOM:      return static_cast<CustomType const&>(*this).toString();
  case POINTER:     return static_cast<PointerType const&>(*this).toString(owning_module);
  case ARRAY:       return static_cast<ArrayType const&>(*this).toString(owning_module);
  case FUNCTION:    return static_cast<FunctionType const&>(*this).toString(owning_module);

  default:
    edenUnreachable("Invalid derived type.");
  }
}
edenNoInlineCold std::string PrimitiveType::toString() const noexcept {
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
  default: edenUnreachable("Invalid primitive type.");
  }
}
edenNoInlineCold std::string PointerType::toString(Module const& owning_module) const noexcept {
  auto const module_id = owning_module.getID();
  if (is_raw) return "raw " + pointedID(module_id).toString(owning_module);
  return "ref " + pointedID(module_id).toString(owning_module);
}
edenNoInlineCold std::string ArrayType::toString(Module const& owning_module) const noexcept { return std::format("[{}]{}", array_size, subtypeID(owning_module.getID()).toString(owning_module)); }
edenNoInlineCold std::string FunctionType::toString(Module const& owning_module) const noexcept {
  std::string string_rep("(");
  auto const module_id = owning_module.getID();
  for (auto i{0uz}; i<num_parameters; ++i) {
    auto const parameter = parameterID(i, module_id);
    string_rep.append( parameter.toString(owning_module) );
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
  if (return_derived_type not_eq DEVOID)
    string_rep.append(returnTypeID(module_id).toString());

  return string_rep;
}
edenNoInlineCold std::string CustomType::definitionToString() const noexcept {
  std::string string_rep("struct ");
  string_rep.append(nameof());
  string_rep.append(" {");

  auto const table = member_table();
  auto const num_members = table->num_variables();
  for (auto i{0uz}; i<num_members; ++i) {
    auto const member = table->getVariable(i); assert(member);
    string_rep.append("\n\t");
    string_rep.append(member->type.toString());
    string_rep.push_back(' ');
    string_rep.append(member->nameof());
    string_rep.push_back(',');
  }

  if (num_members not_eq 0)
    string_rep.pop_back();
  string_rep.append("\n}");

  return string_rep;
}
