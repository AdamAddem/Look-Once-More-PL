#include "peepir.hpp"

#include "build_system/build.hpp"
#include "error.hpp"
#include "parsing/ast.hpp"
#include "parsing/parse.hpp"
#include "module/table_and_module.hpp"

#include "edenlib/casts.hpp"
#include "edenlib/typedefs.hpp"
#include <cassert>
#include <format>
#include <numeric>

using namespace LOM;
using namespace LOM::PeepIR;

using ASTNode = Parser::ASTNode;
using Operator = Parser::Operator;

namespace {

NodiscardCXPR Instruction::Enum
castForType(TypeID typeID) noexcept {
  if (typeID.isPointer()) return Instruction::PCAST;
  switch (typeID.derived) { using enum Type::DerivedType;
  case PRIMITIVE: {
    auto const primitive = PrimitiveType::getTypeFromID(typeID).getUnderlyingPrimitiveType();
    switch (primitive) { using enum PrimitiveType::PrimitiveTypeEnum;
    case STRING:
      return Instruction::PCAST;

    case F32: case F64:
      return Instruction::FCAST;

    case I8: case I16: case I32: case I64:
      return Instruction::SCAST;

    case U8: case U16: case U32: case U64:
    case CHAR: case BOOL:
      return Instruction::UCAST;

    default:
      Unreachable("Invalid primitive type.");
    }
  }

  case ERROR: return Instruction::UCAST;
  default: Unreachable("This shouldn't happen!");
  }
}

struct TreeView {
  eden::vector<Parser::ASTNode>::const_iterator begin;
  eden::vector<Parser::ASTNode>::const_iterator end;

  InlineNodiscardCXPR ASTNode peek(i64_t i = 0) const noexcept { return *(begin + i); }
  InlineNodiscardCXPR ASTNode previous() const noexcept { return *(begin-1); }
  InlineNodiscardCXPR ASTNode take() noexcept { return *(begin++); }
  InlineCXPR void undo() noexcept { --begin; }

  InlineNodiscardCXPR bool empty() const noexcept { return begin == end; }
};

struct QualifiedTypeID : TypeID {
  Type::Qualifiers qualifiers{};
  bool is_function{false}; // if is function, id and module_id should be interpreted as the function id and the module_id for the containing module

  edenInlineCXPR QualifiedTypeID& operator=(TypeID other) noexcept { TypeID::operator=(other); return *this; }
};

// i gently unfold the class...
class PeepingContext {
  Parser::TU& parsed_tu;
  Symbol::Function const* current_function{};
  TreeView nodes;

  // eden::vector<TypeID> locals;
  eden::vector16<Instruction> instructions;
  eden::vector16<Block> blocks;

  Block current_block{};
  bool has_error{};
  u32_t previous_blocks_total_instructions{};
  sz_t current_local_idx{};

  constexpr explicit PeepingContext(Parser::TU& parsed_tu) : parsed_tu(parsed_tu) {

  }

  InlineNodiscardCXPR ssz_t current_block_index() const noexcept { return blocks.size(); }
  InlineNodiscardCXPR bool is_current_block_empty() const noexcept { return previous_blocks_total_instructions == instructions.size(); }

  constexpr void forceNewBlock() noexcept {
    current_block.num_instructions = eden::narrow<u16_t>(instructions.size() - previous_blocks_total_instructions);
    blocks.emplace_back(current_block);
    current_block = {};
    previous_blocks_total_instructions = instructions.size();
  }

  // call before any instructions are made
  // does nothing if current block is empty
  constexpr void makeNewBlock() noexcept {
    if (not is_current_block_empty())
      forceNewBlock();
  }

  // if current block is empty does nothing
  // otherwise, ends current block with br that goes to the next block, then makes new block
  constexpr void fallthroughToNewBlock() noexcept {
    current_block.end_with_br(1);
    makeNewBlock();
  }

  constexpr void addInstruction(Instruction new_instruction = {}) noexcept {
    instructions.emplace_back(new_instruction);
  }

  // returns whether coersion was successful
  // TODO: VERY POOPY
  constexpr bool
  coerceIfArithmeticLiteral(Instruction& possible_literal, TypeID expected_type) const noexcept {
    if (not possible_literal.is_literal() or not expected_type.isArithmetic()) return false;
    auto const to_signed = expected_type.isSignedIntegral();
    auto const to_floating = expected_type.isFloating();
    auto const to_bitwidth = expected_type.bitwidth();

    auto const is_signed = eden::enumBetween(possible_literal.type, Instruction::I8_LITERAL, Instruction::I64_LITERAL);

    // might be the worst switch statement of all time
    switch (possible_literal.type) { using enum Instruction::Enum;
    case I8_LITERAL:  case U8_LITERAL:
    case I16_LITERAL: case U16_LITERAL:
    case I32_LITERAL: case U32_LITERAL:
    case I64_LITERAL: case U64_LITERAL:
      switch (to_bitwidth) {
      case 8:  possible_literal.type = to_signed ? I8_LITERAL : U8_LITERAL;   return true;
      case 16: possible_literal.type = to_signed ? I16_LITERAL : U16_LITERAL; return true;

      case 32:
        if (to_floating)
          possible_literal.type = FLOAT_LITERAL,
          possible_literal.float_literal_data.value =
            (f32_t) (is_signed ? possible_literal.signed_literal_data.value : possible_literal.unsigned_literal_data.value); // TODO: this branch is unnecessary
        else
          possible_literal.type = to_signed ? I32_LITERAL : U32_LITERAL;
        return true;

      case 64:
        if (to_floating)
          possible_literal.type = DOUBLE_LITERAL,
          possible_literal.double_literal_data.value =
            (f64_t) (is_signed ? possible_literal.signed_literal_data.value : possible_literal.unsigned_literal_data.value); // TODO: this branch is unnecessary
        else
          possible_literal.type = to_signed ? I32_LITERAL : U32_LITERAL;
        return true;

      default: Unreachable("Invalid destination bitwidth.");
      }

    case FLOAT_LITERAL:
      if (to_bitwidth == 32) return true;
      assert(to_bitwidth == 64);
      possible_literal.type = DOUBLE_LITERAL;
      possible_literal.double_literal_data.value = (f64_t) possible_literal.float_literal_data.value;
      return true;

    case DOUBLE_LITERAL: assert(to_bitwidth == 64); assert(to_floating); return true;
    default: Unreachable("Invalid literal type.");
    }

    return true;
  }

  NoInlineCold void
  error(auto err, std::string msg) noexcept { reportError(parsed_tu.source_files[err.file_idx], err.length, err.position, std::move(msg)); has_error = true; }

  InlineNodiscardCXPR File
  fileOf(ASTNode node) const noexcept { return parsed_tu.source_files[node.file_idx]; }

  InlineNodiscardCXPR static Instruction
  makeInstruction(ASTNode node, Instruction::Enum type = Instruction::NOOP) noexcept {
    return Instruction { type, node.file_idx, node.length, node.position };
  }

  NodiscardCXPR QualifiedTypeID
  peepLiteral(ASTNode node) noexcept {
    QualifiedTypeID res;
    auto new_instruction = makeInstruction(node);
    switch (node.type) { using enum ASTNode::NodeType;
    case SIGNED_LITERAL:          new_instruction.type = Instruction::I8_LITERAL;
                                  new_instruction.signed_literal_data.value = node.signed_val();
                                  res = signedToLiteralTypeID(node.signed_val()); break;

    case UNSIGNED_LITERAL:        new_instruction.type = Instruction::U8_LITERAL,
                                  new_instruction.unsigned_literal_data.value = node.unsigned_val();
                                  res = unsignedToLiteralTypeID(node.unsigned_val()); break;

    case FLOAT_LITERAL:           new_instruction.type = Instruction::FLOAT_LITERAL;
                                  new_instruction.float_literal_data.value = node.float_val();
                                  res = PrimitiveType::f32ID(); break;

    case DOUBLE_LITERAL:          new_instruction.type = Instruction::DOUBLE_LITERAL;
                                  new_instruction.double_literal_data.value = node.double_val();
                                  res = PrimitiveType::f64ID(); break;

    case BOOL_LITERAL:            new_instruction.type = Instruction::BOOL_LITERAL;
                                  new_instruction.bool_literal_data.value = node.bool_val();
                                  res = PrimitiveType::bool_ID(); break;

    case CHAR_LITERAL:            new_instruction.type = Instruction::CHAR_LITERAL;
                                  new_instruction.char_literal_data.value = node.char_val();
                                  res = PrimitiveType::char_ID(); break;

    case STRING_LITERAL:          new_instruction.type = Instruction::STRING_LITERAL;          res = PrimitiveType::stringID(); break;
    case ESCAPED_STRING_LITERAL:  new_instruction.type = Instruction::ESCAPED_STRING_LITERAL;  res = PrimitiveType::stringID(); break;
    default: edenUnreachable("Invalid literal type.");
    }

    addInstruction(new_instruction);
    return res;
  }

#define pre Assume(module_access_node.type == ASTNode::MODULE_ACCESS);
  NodiscardCXPR QualifiedTypeID
  peepModuleAccess(ASTNode module_access_node) noexcept { pre
    Instruction access_instr = makeInstruction(module_access_node);

    auto const import_id = module_access_node.module_access_data.module_id;
    access_instr.module_member_data.module_id = import_id;

    auto const instruction_idx = instructions.size();
    addInstruction(Instruction{Instruction::NOOP});
    QualifiedTypeID res;

    auto const& imported_module = module_access_node.accessed_module();
    access_instr.module_member_data.module_id = imported_module.getID();

    auto const member_name = module_access_node.module_member_name( fileOf(module_access_node) );

    if (auto const member_function = imported_module.getFunction(member_name)) {
      assert(member_function->id not_eq Symbol::INVALID_ID);
      if (not member_function->is_public) error(module_access_node, "Identifier not a public member of module.");

      access_instr.type = Instruction::MODULE_FUNCTION;
      access_instr.module_member_data.member_id = member_function->id;
      res.module_id = import_id;
      res.id = member_function->id;
      res.is_function = true;
    }
    else if (auto const member_variable = imported_module.getVariable(member_name)) {
      edenUnreachable("Globals unimplemented.");
      assert(member_variable->id not_eq Symbol::INVALID_ID);
      if (not member_variable->is_public) error(module_access_node, "Identifier not a public member of module.");

      access_instr.type = Instruction::MODULE_GLOBAL;
      access_instr.module_member_data.member_id = member_variable->id;
      res = member_variable->typeID;
      res.qualifiers = member_variable->qualifiers;
    }
    else {
      error(module_access_node, "Identifier not a public member of module.");
      access_instr.type = Instruction::NOOP;
      res = errorID;
      res.qualifiers.writable = true;
    }

    instructions[instruction_idx] = access_instr;
    return res;
  }
#undef pre

#define pre Assume(member_access_node.type == ASTNode::MEMBER_ACCESS);
  NodiscardCXPR QualifiedTypeID
  peepMemberAccess(ASTNode member_access_node) noexcept { pre
    Instruction type_member = makeInstruction(member_access_node, Instruction::TYPE_VARIABLE);
    auto const instruction_idx = instructions.size();
    addInstruction();
    QualifiedTypeID res;

    auto const member_expression = peepExpression();
    res = member_expression;

    type_member.type_member_data.custom_type_module_id = member_expression.module_id;
    type_member.type_member_data.custom_type_id = member_expression.id;

    if (not member_expression.isCustom()) {
      if (not member_expression.isError())
        error(member_access_node, "Attempt to access member of non-custom type.");
      return res = errorID;
    }

    auto const& custom_type = member_expression.getCustomType();
    auto const& member_table = custom_type.member_table();
    auto const identifier_node = nodes.take();
    auto const identifier = identifier_node.identifier_val( fileOf(identifier_node) );

    if (auto const member_variable = member_table.getPublicVariable(identifier)) {
      assert(member_variable->id not_eq Symbol::INVALID_ID);
      type_member.type_member_data.member_id = member_variable->id;
      auto const var_typeID = member_variable->typeID;
      res = var_typeID;
      if (member_expression.qualifiers.writable) res.qualifiers = member_variable->qualifiers;
    }
    else if (auto const member_function = member_table.getPublicFunction(identifier)) {
      Unreachable("Member functions unimplemented."); assert(member_function->id not_eq Symbol::INVALID_ID);
      (void)member_function;
    }
    else {
      error(identifier_node, "Identifier is not a public member of type.");
      res = errorID;
    }

    instructions[instruction_idx] = type_member;
    return res;
  }
#undef pre

#define pre Assume(identifier_node.type == ASTNode::IDENTIFIER);
  NodiscardCXPR QualifiedTypeID
  peepIdentifier(ASTNode identifier_node) noexcept { pre
    Instruction identifier_instructon = makeInstruction(identifier_node);
    auto const identifier = identifier_node.identifier_val( fileOf(identifier_node) );
    QualifiedTypeID res;

    if (auto const variable = current_function->getLocal(identifier)) {
      identifier_instructon.type = Instruction::LOCAL;
      res = variable->typeID;
      identifier_instructon.local_data.idx = variable->id;
    }
    else if (auto const function = parsed_tu.module.getFunction(identifier)) {
      identifier_instructon.type = Instruction::FUNCTION;
      res.module_id = parsed_tu.module.getID();
      res.id = function->id;
      res.is_function = true;
    }
    else if (auto const global = parsed_tu.module.getVariable(identifier)) {
      identifier_instructon.type = Instruction::GLOBAL;
      res = global->typeID;
    }
    else {
      auto const possible_module = getModule(identifier);
      if (possible_module) {
        if (nodes.peek(-2).type == ASTNode::MEMBER_ACCESS)
          error(identifier_node, std::format("Module access uses '::', not '.'", identifier));
        else
          error(identifier_node, std::format("Module name used in invalid context.", identifier));
      }
      else {
        error(identifier_node, std::format("Unrecognized identifier {}", identifier));
      }
      res = errorID;
    }

    addInstruction(identifier_instructon);
    return res;
  }
#undef pre

#define pre Assume(subscript_node.type == ASTNode::SUBSCRIPT);
  NodiscardCXPR QualifiedTypeID
  peepSubscriptExpression(ASTNode subscript_node) noexcept { pre
    Instruction subscript_instruction = makeInstruction(subscript_node, Instruction::SUBSCRIPT);
    auto const subscript_idx = instructions.size();
    addInstruction();

    auto const array_expr = peepExpression(); bool const is_array = array_expr.isArray();
    subscript_instruction.subscript_data.array_typeID = (TypeID) array_expr;

    auto const index_idx /*lol*/ = instructions.size();
    auto const index_expr = peepExpression(); bool const is_unsigned = index_expr.isUnsignedIntegral();

    QualifiedTypeID res;

    bool const valid = is_array && is_unsigned;
    if (not valid) {
      res = errorID;
      if (not is_array)
        error(subscript_node, std::format("Subscript operator used on non-array type {}.", array_expr.toString()) );
      if (not is_unsigned)
        error(subscript_node, std::format("Expression used as array index is of signed type {}.", index_expr.toString()) );
      return res;
    }

    auto const& array_type = array_expr.getArrayType();
    auto const array_subtypeID = array_type.getSubtypeID();
    res = array_subtypeID;
    res.qualifiers = array_expr.qualifiers;

    instructions[subscript_idx] = subscript_instruction;

    coerceIfArithmeticLiteral(instructions[index_idx], PrimitiveType::u64ID());
    return res;
  }
#undef pre

#define pre Assume(cast_node.type == ASTNode::CAST);
  NodiscardCXPR QualifiedTypeID
  peepCastExpression(ASTNode cast_node) noexcept { pre
    QualifiedTypeID res;
    Instruction cast_instruction = makeInstruction(cast_node);
    auto const cast_idx = instructions.size();
    addInstruction();

    auto const cast_typeID = cast_node.cast_data.cast_type;
    cast_instruction.cast_data.destination_typeID = cast_typeID;
    res = cast_typeID;

    auto const casted_expr = peepExpression();
    if (not casted_expr.castableTo(cast_typeID)) {
      error(cast_node,
        std::format("Invalid cast from {} to {}", casted_expr.toString(), cast_typeID.toString())
        );
      res = errorID;
      return res;
    }

    cast_instruction.type = castForType( (TypeID) casted_expr );
    instructions[cast_idx] = cast_instruction;

    return res;
  }
#undef pre

#define pre Assume(calling_node.type == ASTNode::CALLING);
  NodiscardCXPR QualifiedTypeID
  peepCallingExpression(ASTNode calling_node) noexcept { pre
    auto const args_passed_count = calling_node.call_data.num_parameters;
    {
      auto call_instruction = makeInstruction(calling_node, Instruction::CALL);
      call_instruction.call_data.num_parameters = args_passed_count;
      addInstruction(call_instruction);
    }

    QualifiedTypeID res;
    auto const called = peepExpression();
    if (not called.is_function) {
      if (not called.isError()) error(calling_node, "Call operator used on non-callable.");
      for (sz_t i{}; i<args_passed_count; ++i) (void)peepExpression();
      res = errorID;
      return res;
    }

    auto const& fn = getModule(called.module_id).getFunction(called.id);
    auto const parameter_count = (sz_t) fn.num_parameters;
    auto const is_variadic = fn.is_variadic;

    if (args_passed_count < parameter_count) {
      error(calling_node, "Too few parameters for function call.");
      res = errorID;
      return res;
    }

    if (args_passed_count > parameter_count and not is_variadic) {
      error(calling_node, "Too many parameters for function call.");
      res = errorID;
      return res;
    }

    for (sz_t i{}; i<parameter_count; ++i) {
      auto const& parameter_typeID = fn.getLocal(eden::narrow<i16_t>(i)).typeID;

      auto const argument_instr_index = instructions.size();
      auto const argument_typeID = peepExpression();
      auto& argument_instr = instructions[argument_instr_index];

      if (argument_typeID.coercibleTo(parameter_typeID))
        coerceIfArithmeticLiteral(argument_instr, parameter_typeID);
      else {
        error(argument_instr, std::format("Cannot coerce parameter of type '{}' to type '{}'.", argument_typeID.toString(), parameter_typeID.toString()));
        res = errorID;
      }
    }

    if (is_variadic)
      for (sz_t i{parameter_count}; i<args_passed_count; ++i) (void)peepExpression();

    res = fn.return_typeID;
    return res;
  }
#undef pre

  //TODO: Add Short Circuiting
#define pre Assume(binary_node.type == ASTNode::BINARY);
  NodiscardCXPR QualifiedTypeID
  peepBinaryExpression(ASTNode binary_node) noexcept { pre
    Instruction binary_instruction = makeInstruction(binary_node);
    auto const binary_idx = instructions.size();
    addInstruction();

    auto const left_idx = instructions.size();
    auto left = peepExpression();

    bool const left_signed = left.isSignedIntegral();
    bool const left_float = left.isFloating();
    bool const arithmetic = left.isArithmetic();

    auto const right_idx = instructions.size();
    auto right = peepExpression();

    // TODO: THIS IS BAD AND SUCKS FIX IT NOW
    if ( not left.sameAs( (TypeID) right) ) { // if left or right is an integer literal, coerce its type to the other
      if (coerceIfArithmeticLiteral(instructions[left_idx], (TypeID) right))
        left = right;
      else if (coerceIfArithmeticLiteral(instructions[right_idx], (TypeID) left))
        right = left;
      else {
        error(binary_node,
          std::format("Right type {} in binary expression cannot coerce to left type {}.",
          right.toString(), left.toString()));

        left = errorID;
      }
    }

    // first switch validates
    switch (binary_node.binary_data.opr) {
    case Operator::ADD:
    case Operator::SUBTRACT:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::MODULUS:
      if (not arithmetic) {
        error(binary_node, "Non-arithmetic expressions in arithmetic binary operation.");
        left = errorID;
        break;
      }

      left.qualifiers.writable = false;
      break;

    case Operator::LESS:
    case Operator::GREATER:
    case Operator::LESS_EQUAL:
    case Operator::GREATER_EQUAL:
      if (not arithmetic) {
        error(binary_node, "Non-arithmetic expressions in arithmetic binary operation.");
        left = errorID;
        break;
      }

      left = PrimitiveType::bool_ID();
      break;

    case Operator::AND:
    case Operator::OR:
    case Operator::XOR:
      if (not left.isBool()) {
        error(binary_node, "Non-boolean expressions in boolean binary operation.");
        left = errorID;
        break;
      }

      left = PrimitiveType::bool_ID();
      break;

    case Operator::BITAND:
    case Operator::BITOR:
    case Operator::BITXOR:
      if (not arithmetic) {
        error(binary_node, "Non-arithmetic expressions in bitwise operation.");
        left = errorID;
        break;
      }

      left.qualifiers.writable = false;
      break;

    case Operator::ASSIGN:
      if (not left.qualifiers.writable) {
        error(binary_node, "Left expression in assignment non-mutable.");
        left = errorID;
      }
      break;

    case Operator::EQUAL:
    case Operator::NOT_EQUAL:
      left = PrimitiveType::bool_ID();
      break;

    default:
      edenUnreachable("Invalid binary operator.");
    }

    // second sets the right instruction
    switch (binary_node.binary_data.opr) {
    case Operator::ADD:
      binary_instruction.type = left_float ? Instruction::FADD : Instruction::ADD;
      break;
    case Operator::SUBTRACT:
      binary_instruction.type = left_float ? Instruction::FSUB : Instruction::SUB;
      break;
    case Operator::MULTIPLY:
      binary_instruction.type = left_float ? Instruction::FMULT : Instruction::MULT;
      break;
    case Operator::DIVIDE:
      binary_instruction.type =
      left_float ? Instruction::FDIV :
       left_signed ? Instruction::SDIV : Instruction::UDIV;
      break;
    case Operator::MODULUS:
      binary_instruction.type =
      left_float ? Instruction::FMOD :
       left_signed ? Instruction::SMOD : Instruction::UMOD;
      break;
    case Operator::LESS:
      binary_instruction.type =
      left_float ? Instruction::FLESS :
       left_signed ? Instruction::SLESS : Instruction::ULESS;
      break;
    case Operator::GREATER:
      binary_instruction.type =
      left_float ? Instruction::FGTR :
       left_signed ? Instruction::SGTR : Instruction::UGTR;
      break;
    case Operator::LESS_EQUAL:
      binary_instruction.type =
      left_float ? Instruction::FLEQ :
       left_signed ? Instruction::SLEQ : Instruction::ULEQ;
      break;
    case Operator::GREATER_EQUAL:
      binary_instruction.type =
      left_float ? Instruction::FGEQ :
       left_signed ? Instruction::SGEQ : Instruction::UGEQ;
      break;

    case Operator::AND: binary_instruction.type = Instruction::AND; break;
    case Operator::OR: binary_instruction.type = Instruction::OR; break;
    case Operator::XOR: binary_instruction.type = Instruction::NEQ; break;
    case Operator::BITAND: binary_instruction.type = Instruction::BITAND; break;
    case Operator::BITOR: binary_instruction.type = Instruction::BITOR; break;
    case Operator::BITXOR: binary_instruction.type = Instruction::BITXOR; break;
    case Operator::EQUAL: binary_instruction.type = Instruction::EQ; break;
    case Operator::NOT_EQUAL: binary_instruction.type = Instruction::NEQ; break;

    case Operator::ASSIGN: {
      binary_instruction.type = Instruction::ASSIGN;
      adjustAssignExpression(binary_instruction, left, right);
      break;
    }

    default:
      edenUnreachable("Invalid binary operator.");
    }

    instructions[binary_idx] = binary_instruction;
    return left;
  }
#undef pre

#define pre Assume(unary_node.type == ASTNode::UNARY);
  NodiscardCXPR QualifiedTypeID
  peepUnaryExpression(ASTNode unary_node) noexcept { pre
    Instruction unary_instruction = makeInstruction(unary_node);
    auto const unary_idx = instructions.size();
    addInstruction();

    auto expression = peepExpression();
    const bool float_opr = expression.isFloating();
    const bool arithmetic = expression.isArithmetic();
    switch (unary_node.unary_data.opr) {
    case Operator::ADDRESS_OF:
      if (not expression.qualifiers.writable)
        error(unary_node, "Address-of (@) operator used on readonly expression.");
      expression.addPointer(true);
      expression.qualifiers.writable = false;
      break;

    case Operator::REF_TO:
      expression.addPointer(false);
      expression.qualifiers.writable = false;
      break;

    case Operator::PRE_INCREMENT:
    case Operator::PRE_DECREMENT:
      if (not expression.qualifiers.writable) {
        error(unary_node, "Prefix operator used on readonly expression.");
        expression = errorID;
      }
      break;
    case Operator::UNARY_MINUS:
      if (not arithmetic) {
        error(unary_node, "Unary minus used on non-arithmetic expression.");
        expression = errorID;
        break;
      }
      expression.qualifiers.writable = false;
      break;
    case Operator::BITNOT:
      if (not arithmetic) {
        error(unary_node, "bitnot operator used non-arithmetic expression.");
        expression = errorID;
        break;
      }
      expression.qualifiers.writable = false;
      break;
    case Operator::NOT:
      if (not expression.isBool()) {
        error(unary_node, "not operator used on non-boolean expression.");
        expression = errorID;
        break;
      }
      expression.qualifiers.writable = false;
      break;
    case Operator::POST_INCREMENT:
    case Operator::POST_DECREMENT:
      if (not expression.qualifiers.writable) {
        error(unary_node, "Postfix decrement operator used on non-mutable expression.");
        expression = errorID;
        break;
      }
      expression.qualifiers.writable = false;
      break;
    case Operator::ARROW:
      if (not expression.isPointer()) {
        error(unary_node, "Arrow operator used on non-pointer type.");
        expression = errorID;
        break;
      }

      expression = expression.getPointedTypeID();
      break;
    default:
      edenUnreachable("Invalid unary operator.");
    }

    switch (unary_node.unary_data.opr) {
    case Operator::ADDRESS_OF:
    case Operator::REF_TO:          unary_instruction.type = Instruction::ADDRESS_OF; break;

    case Operator::PRE_INCREMENT:   unary_instruction.type = float_opr ? Instruction::FPRE_INC : Instruction::PRE_INC;  break;
    case Operator::PRE_DECREMENT:   unary_instruction.type = float_opr ? Instruction::FPRE_DEC : Instruction::PRE_DEC;  break;
    case Operator::UNARY_MINUS:     unary_instruction.type = float_opr ? Instruction::FNEGATE : Instruction::NEGATE;  break;

    case Operator::BITNOT:
    case Operator::NOT:             unary_instruction.type = Instruction::BITNOT; break;

    case Operator::POST_INCREMENT:  unary_instruction.type = float_opr ? Instruction::FPOST_INC : Instruction::POST_INC; break;
    case Operator::POST_DECREMENT:  unary_instruction.type = float_opr ? Instruction::FPOST_DEC : Instruction::POST_DEC; break;

    case Operator::ARROW:
      unary_instruction.type = Instruction::DEREFERENCE;
      unary_instruction.dereference_data.dereference_typeID = (TypeID) expression;
      break;
    default:
      edenUnreachable("Invalid unary operator.");
    }

    instructions[unary_idx] = unary_instruction;
    return expression;
  }
#undef pre

  NodiscardCXPR QualifiedTypeID
  peepExpression() noexcept {
    auto const node = nodes.take();
    switch (node.type) { using enum ASTNode::NodeType;
    case MEMBER_ACCESS:    return peepMemberAccess(node);
    case MODULE_ACCESS:    return peepModuleAccess(node);
    case UNARY:            return peepUnaryExpression(node);
    case BINARY:           return peepBinaryExpression(node);
    case CALLING:          return peepCallingExpression(node);
    case IDENTIFIER:       return peepIdentifier(node);
    case CAST:             return peepCastExpression(node);
    case SUBSCRIPT:        return peepSubscriptExpression(node);

    case SIGNED_LITERAL:
    case UNSIGNED_LITERAL:
    case FLOAT_LITERAL:
    case DOUBLE_LITERAL:
    case BOOL_LITERAL:
    case CHAR_LITERAL:
    case STRING_LITERAL:
    case ESCAPED_STRING_LITERAL: return peepLiteral(node);

    default: Unreachable("Invalid ASTNode while peeping expression.");
    }
  }

  constexpr void adjustAssignExpression(Instruction& assign, QualifiedTypeID left, QualifiedTypeID right) const noexcept {
    if (left.sameAs( (TypeID) right) and not left.isPointer()) {
      assign.type = right.isSignedIntegral() ? Instruction::SCAST_ASSIGN : Instruction::UCAST_ASSIGN;
      assign.cast_assign_data.bitwidth = left.bitwidth();
    }
  }

#define pre Assume(return_node.type == ASTNode::RETURN);
  constexpr void peepReturnStatement(ASTNode return_node) noexcept { pre
    auto const return_typeID = current_function->return_typeID;
    if (not return_node.return_data.has_value) {
      if (not return_typeID.isDevoid())
        return error(nodes.take(),"Non-devoid function expects return value.");

      current_block.end_with_ret();
      forceNewBlock();
      return;
    }

    auto const return_idx = instructions.size();
    auto const return_expression = peepExpression();
    if (return_typeID.isDevoid()) {
      error(return_node,  "Cannot return value from devoid function.");
      return;
    }
    if (not return_expression.coercibleTo(return_typeID)) {
      error(return_node, "Return statement's type is not compatible with function return type.");
      return;
    }

    coerceIfArithmeticLiteral(instructions[return_idx], return_typeID);
    current_block.end_with_ret();
  }
#undef pre

  constexpr void peepScopedStatement(u32_t num_children) noexcept {
    while (num_children-- != 0)
      peepStatement();
  }

#define pre Assume(while_node.type == ASTNode::WHILE);
  constexpr void peepWhileLoop(ASTNode while_node) noexcept { pre
    fallthroughToNewBlock();
    auto const condition_idx = current_block_index();
    auto const condition = peepExpression();
    if (not condition.isBool())
      error(while_node, "While Loop condition not boolean.");

    forceNewBlock();
    auto const loop_body_offset = current_block_index() - condition_idx;

    peepScopedStatement(while_node.while_data.num_substatements);
    if (is_current_block_empty())
      addInstruction(Instruction{Instruction::NOOP});
    current_block.end_with_br( eden::narrow<i16_t>( condition_idx - current_block_index() ) );

    forceNewBlock();
    auto const after_loop_offset = current_block_index() - condition_idx;

    blocks[condition_idx].end_with_brc(
      eden::narrow<i16_t>(loop_body_offset),
      eden::narrow<i16_t>(after_loop_offset));
  }
#undef pre

#define pre Assume(if_node.type == ASTNode::IF);
  constexpr void peepIfStatement(ASTNode if_node) noexcept { pre
    auto const condition = peepExpression();
    if (not condition.isBool()) {
      error(if_node, "If statement condition not boolean.");
      return;
    }

    auto const condition_block_idx = current_block_index();

    forceNewBlock();
    auto const true_block_idx = current_block_index();
    auto const true_block_offset = true_block_idx - condition_block_idx;
    peepScopedStatement(if_node.if_data.num_substatements); // true statement(s)

    forceNewBlock();
    auto const false_block_offset = current_block_index() - condition_block_idx;

    if (if_node.has_else()) {
      peepStatement();
      fallthroughToNewBlock();
    }

    auto const after_block_offset = current_block_index() - condition_block_idx; // equal to false_block_offset if no 'else' statements

    blocks[true_block_idx].end_with_br( eden::narrow<i16_t>(after_block_offset) );
    blocks[condition_block_idx].end_with_brc(
      eden::narrow<i16_t>(true_block_offset),
      eden::narrow<i16_t>(false_block_offset));
  }
#undef pre

#define pre Assume(decl_node.isDeclaration());
  constexpr void peepVarDeclaration(ASTNode decl_node) noexcept { pre
    QualifiedTypeID qualified_typeID;
    qualified_typeID = decl_node.declaration_type();
    qualified_typeID.qualifiers.writable = decl_node.isRWDeclaration();

    if (decl_node.isJunkDeclaration()) return;
    auto const assign_idx = instructions.size();
    addInstruction(makeInstruction(decl_node, Instruction::ASSIGN));

    {
      Instruction local_instruction = makeInstruction(decl_node, Instruction::LOCAL);
      local_instruction.local_data.idx = current_local_idx; ++current_local_idx;
      addInstruction(local_instruction);
    }

    auto const init_expr_idx = instructions.size();
    auto const init_expr = peepExpression();
    if (not init_expr.coercibleTo( (TypeID) qualified_typeID) ) {
      error(decl_node, std::format("Type {} is not coercible to type {}.", init_expr.toString(), qualified_typeID.toString()));
      return;
    }

    coerceIfArithmeticLiteral(instructions[init_expr_idx], (TypeID) qualified_typeID);
    adjustAssignExpression(instructions[assign_idx], qualified_typeID, init_expr);
  }
#undef pre

  constexpr void peepStatement() noexcept {
    auto const node = nodes.take();
    switch (node.type) { using enum Parser::ASTNode::NodeType;
    case DECLARATION_JUNK_RO: case DECLARATION_JUNK_RW:
    case DECLARATION_RO:      case DECLARATION_RW:
      return peepVarDeclaration(node);

    case IF:           return peepIfStatement(node);
    case WHILE:        return peepWhileLoop(node);
    case RETURN:       return peepReturnStatement(node);

    case UNARY:
    case BINARY:
    case CALLING:
    case IDENTIFIER:
      nodes.undo();
      return (void)peepExpression();

    case SIGNED_LITERAL:
    case UNSIGNED_LITERAL:
    case FLOAT_LITERAL:
    case DOUBLE_LITERAL:
    case BOOL_LITERAL:
    case CHAR_LITERAL:
    case STRING_LITERAL:
      return (void)peepLiteral(node);

    default: Unreachable("Invalid ASTNode while peeping statement.");
    }
  }

  constexpr void peepUntilEmpty() noexcept {
    assert(instructions.empty());
    assert(blocks.empty());
    Assume(previous_blocks_total_instructions == 0);
    while (not nodes.empty())
      peepStatement();

    makeNewBlock(); // flush the last block
    previous_blocks_total_instructions = 0;

    if (blocks.back().terminator_type == Block::Terminator::NONE)
      error(nodes.previous(), "Expected return at end of function body.");
  }

  static constexpr void transmutate_tu(Parser::TU& parsed_tu) {
    auto module_copy = std::move(parsed_tu.module);
    auto source_files = std::move(parsed_tu.source_files);
    auto const name = parsed_tu.name;

    auto const begin = parsed_tu.functions.data();
    auto const size = parsed_tu.functions.size();
    auto const cap = parsed_tu.functions.capacity();
    parsed_tu.functions.unsafe_zero_members();
    parsed_tu.~TU();

    auto const new_begin = std::start_lifetime_as_array<Function>(begin, size);
    auto& peeped_tu = * new(&parsed_tu) TU{
      .module = std::move(module_copy),
      .source_files = std::move(source_files),
      .name = name,
      .functions = {}
    };
    peeped_tu.functions.unsafe_set_members(new_begin, new_begin + size, new_begin + cap, {});
  }

public:

  // returns whether an error occured
  NodiscardCXPR static bool
  peepTU(Parser::TU& parsed_tu) {
    PeepingContext peeper(parsed_tu);

    for (sz_t i{}; i<parsed_tu.functions.size(); ++i){
      auto& parsed_fn = parsed_tu.functions[i];

      peeper.nodes.begin = parsed_fn.body.cbegin();
      peeper.nodes.end = parsed_fn.body.cend();

      peeper.current_function = &parsed_tu.module.getFunction(i);
      peeper.current_function->locals.sort_by_unique_idx(Symbol::get_id_of); // this is dumb
      peeper.peepUntilEmpty();

      // transmutate the Parser::Function into a PeepIR::Function inplace
      parsed_fn.~Function();
      auto& peeped_fn = *new (&parsed_fn) Function;

      peeped_fn.instructions = std::move(peeper.instructions);
      peeped_fn.blocks = std::move(peeper.blocks);
      peeper.current_local_idx = 0;
    }

    transmutate_tu(parsed_tu);
    return peeper.has_error;
  }

};

}

bool PeepIR::lowerToPeep(Parser::TU& parsed_tu) {
  return PeepingContext::peepTU(parsed_tu);
}