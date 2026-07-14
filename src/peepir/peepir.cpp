#include "peepir.hpp"

#include "build_system/build.hpp"
#include "edenlib/typedefs.hpp"
#include "error.hpp"
#include "parsing/ast.hpp"
#include "parsing/parse.hpp"
#include "semantic_analysis/symbol_table.hpp"

#include <cassert>
#include <format>
#include <iostream>
#include <print>
#include <chrono>

using namespace LOM;
using namespace LOM::PeepIR;
using namespace LOM::AST;

namespace {

[[nodiscard]] constexpr Instruction::InstructionType
castForType(Type const* type) noexcept {
  switch (type->getDerivedType()) { using enum Type::DerivedType;
  case ERROR: return Instruction::UCAST;
  case POINTER: return Instruction::PCAST;

  case PRIMITIVE: {
    auto const primitive = type->castToPrimitive()->getUnderlyingPrimitiveType();
    switch (primitive) { using enum PrimitiveType::PrimitiveTypeEnum;
    case STRING:        return Instruction::PCAST;
    case F32: case F64: return Instruction::FCAST;
    case CHAR:          return Instruction::UCAST;

    case I8:
    case I16:
    case I32:
    case I64:           return Instruction::SCAST;

    case U7: case U8:
    case U15: case U16:
    case U31: case U32:
    case U63: case U64: return Instruction::UCAST;

    default:
      eden_unreachable("Invalid primitive type.");
    }
  }

  default: eden_unreachable("This shouldn't happen!");
  }
}

struct TreeView {
  std::vector<ASTNode>::const_iterator begin;
  std::vector<ASTNode>::const_iterator end;

  eden_always_inline [[nodiscard]] constexpr ASTNode peek() const noexcept { return *begin; }
  eden_always_inline [[nodiscard]] constexpr bool peek_is_empty() const noexcept { return begin->type == ASTNode::EMPTY; }
  eden_always_inline constexpr ASTNode take() noexcept { return *(begin++); }
  eden_always_inline constexpr void pop() noexcept { ++begin; }

  [[nodiscard]] constexpr bool
  pop_if_empty() noexcept {
    if (peek_is_empty()) {
      pop();
      return true;
    }
    return false;
  }

  eden_always_inline constexpr void  undo() noexcept { --begin; }
  eden_always_inline [[nodiscard]] constexpr bool empty() const noexcept { return begin == end; }
};

class Peeper {
  Module* module;
  File const* current_file;
  TreeView nodes;
  FunctionType const* current_function_type;

  std::vector<Type const*> locals;
  std::vector<Instruction> instructions;
  std::vector<Block> blocks;
  bool has_error{};

  eden_always_inline [[nodiscard]] constexpr Block& current_block() noexcept { return blocks.back(); }
  eden_always_inline [[nodiscard]] constexpr u32_t current_block_index() const noexcept { return blocks.size() - 1; }
  eden_always_inline [[nodiscard]] constexpr bool is_current_block_empty() const noexcept { return blocks.back().first_instruction_idx == instructions.size(); }

  // creates a br that goes to the next block, as if it had fallen through (does not create next block)
  // does nothing if current block is empty
  constexpr void
  br_fallthrough() noexcept {
    if (not is_current_block_empty())
      blocks.back().set_br(blocks.size());
  }

  // call before any instructions are made
  // does nothing if current block is empty
  constexpr void
  new_block() noexcept {
    if (not is_current_block_empty())
      blocks.emplace_back(instructions.size(), Block::Terminator::NONE);
  }

  constexpr void force_new_block() noexcept { blocks.emplace_back(instructions.size(), Block::Terminator::NONE); }

  constexpr bool // returns whether coersion was successful
  coerce_if_integerliteral(Instruction& possible_literal, Type const* expected_type) const noexcept {
    if (not possible_literal.is_literal() or not expected_type->isIntegral()) return false;
    auto const expected_type_primitive = expected_type->castToPrimitive();
    auto const is_signed = expected_type_primitive->isSignedIntegral();
    possible_literal.adjust_literal(expected_type_primitive->bitwidth(), is_signed);
    return true;
  }

  eden_noinline_cold void error(auto err, std::string msg) noexcept
  requires requires {
    err.length_in_file;
    err.position_in_file;
  } {
    report_error(*current_file, err.length_in_file, err.position_in_file, std::move(msg)); has_error = true;
  }

  [[nodiscard]] static Instruction
  newInstruction(ASTNode node, Instruction::InstructionType type = Instruction::NOOP) noexcept {
    return Instruction {
      Instruction::CommonData {
        .type = type,
        .file_idx = node.file_idx,
        .length_in_file = node.length_in_file,
        .position_in_file = node.position_in_file
      }
    };
  }

  QualifiedType
  peepLiteral(ASTNode node) {
    QualifiedType res;
    auto new_instruction = newInstruction(node);
    switch (node.type) { using enum ASTNode::NodeType;
    case SIGNED_LITERAL:          new_instruction.m.type = Instruction::I8_LITERAL;
                                  new_instruction.signed_literal_data.value = node.signed_val();
                                  res = signedToLiteralInstance(node.signed_val()); break;

    case UNSIGNED_LITERAL:        new_instruction.m.type = Instruction::U8_LITERAL,
                                  new_instruction.unsigned_literal_data.value = node.unsigned_val();
                                  res = unsignedToLiteralInstance(node.unsigned_val()); break;

    case FLOAT_LITERAL:           new_instruction.m.type = Instruction::FLOAT_LITERAL;
                                  new_instruction.float_literal_data.value = node.float_val();
                                  res = f32_literal; break;

    case DOUBLE_LITERAL:          new_instruction.m.type = Instruction::DOUBLE_LITERAL;
                                  new_instruction.double_literal_data.value = node.double_val();
                                  res = f64_literal; break;

    case BOOL_LITERAL:            new_instruction.m.type = Instruction::BOOL_LITERAL;
                                  new_instruction.bool_literal_data.value = node.bool_val();
                                  res = bool_literal; break;

    case CHAR_LITERAL:            new_instruction.m.type = Instruction::CHAR_LITERAL;
                                  new_instruction.char_literal_data.value = node.char_val();
                                  res = char_literal; break;

    case STRING_LITERAL:          new_instruction.m.type = Instruction::STRING_LITERAL;          res = string_literal; break;
    case ESCAPED_STRING_LITERAL:  new_instruction.m.type = Instruction::ESCAPED_STRING_LITERAL;  res = string_literal; break;
    default: eden_unreachable("Invalid literal type.");
    }

    instructions.emplace_back(new_instruction);
    return res;
  }


#define pre assume_assert(module_access_node.type == ASTNode::MODULE_ACCESS);
  QualifiedType
  peepModuleAccess(ASTNode module_access_node) { pre
    Instruction module_symbol = newInstruction(module_access_node);
    module_symbol.module_member_data.module_position = module_access_node.module_position();

    auto const instruction_idx = instructions.size();
    instructions.emplace_back(Instruction::NOOP);
    QualifiedType res;

    auto const module = getModule(module_access_node.module_name(*current_file)); assume_assert(module);
    module_symbol.module_member_data.import = module;

    auto const member_name = module_access_node.module_member_name(*current_file);
    assume_assert(module_access_node.length_in_file == 1);

    if (auto const member_variable = module->getPublicVariable(member_name)) {
      eden_unreachable("Globals unimplemented.");
      assert(member_variable->id not_eq SymbolTable::INVALID_ID);
      module_symbol.m.type = Instruction::MODULE_GLOBAL;
      module_symbol.module_member_data.member_idx = member_variable->id;
      res = member_variable->type;
    }
    else if (auto const member_function = module->getPublicFunction(member_name)) {
      assert(member_function->id not_eq SymbolTable::INVALID_ID);
      module_symbol.m.type = Instruction::MODULE_FUNCTION;
      module_symbol.module_member_data.member_idx = member_function->id;
      res.type = member_function->type;
    }
    else {
      ASTNode member_node_fake{
        .type = ASTNode::EMPTY,
        .file_idx = 0,
        .length_in_file = module_access_node.module_access_data.member_length,
        .position_in_file = module_access_node.position_in_file + 1,
        .base = 0
      };
      error(member_node_fake, "Identifier is not a public member of module.");
      module_symbol.m.type = Instruction::NOOP;
      res = error_literal;
    }

    instructions[instruction_idx] = module_symbol;
    return res;
  }
#undef pre

#define pre assume_assert(member_access_node.type == ASTNode::MEMBER_ACCESS);
  QualifiedType
  peepMemberAccess(ASTNode member_access_node) { pre
    Instruction type_member = newInstruction(member_access_node, Instruction::TYPE_VARIABLE);
    auto const instruction_idx = instructions.size();
    instructions.emplace_back();
    QualifiedType res;

    auto const member_expression = peepExpression();
    res = member_expression;
    if (not member_expression.type->isCustom()) {
      error(member_access_node, "Attempt to access member of non-custom type.");
      res = error_literal;
      return res;
    }

    auto const custom_type = member_expression.type->castToCustom();
    auto const member_table = custom_type->member_table();
    auto const identifier_node = nodes.take();
    auto const identifier = std::string_view(identifier_node.identifier_val(*current_file));

    type_member.type_member_data.custom_type = custom_type;

    if (auto const member_variable = member_table->getPublicVariable(identifier)) {
      assert(member_variable->id not_eq SymbolTable::INVALID_ID);
      type_member.type_member_data.member_idx = member_variable->id;
      auto const member_variable_decltype = member_variable->type;
      res.type = member_variable_decltype.type;
      if (member_expression.qualifiers.writable) res.qualifiers = member_variable_decltype.qualifiers;
    }
    else if (auto const member_function = member_table->getPublicFunction(identifier)) {
      eden_unreachable("Member functions unimplemented."); assert(member_function->id not_eq SymbolTable::INVALID_ID);
      type_member.type_member_data.member_idx = member_function->id;
      res.type = member_function->type;
    }
    else {
      error(identifier_node, "Identifier is not a public member of type.");
      res = error_literal;
    }

    instructions[instruction_idx] = type_member;
    return res;
  }
#undef pre

#define pre assume_assert(identifier_node.type == ASTNode::IDENTIFIER);
  QualifiedType
  peepIdentifier(ASTNode identifier_node) { pre
    Instruction identifier_instructon = newInstruction(identifier_node);
    auto const identifier = current_file->view_at(identifier_node.length_in_file, identifier_node.position_in_file);
    QualifiedType res;

    if (auto const variable = module->getLocal(identifier)) {
      identifier_instructon.m.type = Instruction::LOCAL;
      res = variable->type;
      identifier_instructon.local_data.idx = variable->id + 1; // + 1 to offset for return type
    }
    else if (auto const function = module->getFunction(identifier)) {
      identifier_instructon.m.type = Instruction::FUNCTION;
      res.type = function->type;
    }
    else if (auto const global = module->getVariable(identifier)) {
      identifier_instructon.m.type = Instruction::GLOBAL;
      res = global->type;
    }
    else {
      error(identifier_node, "Unrecognized identifier.");
      res = error_literal;
    }

    instructions.emplace_back( identifier_instructon );
    return res;
  }
#undef pre

#define pre assume_assert(subscript_node.type == ASTNode::SUBSCRIPT);
  QualifiedType
  peepSubscriptExpression(ASTNode subscript_node) { pre
    Instruction subscript_instruction = newInstruction(subscript_node, Instruction::SUBSCRIPT);
    auto const subscript_idx = instructions.size();
    instructions.emplace_back();

    auto const array_expr = peepExpression(); bool const is_array = array_expr.type->isArray();

    auto const index_idx /*lol*/ = instructions.size();
    auto const index_expr = peepExpression(); bool const is_unsigned = index_expr.type->isUnsignedIntegral();

    QualifiedType res;

    bool const valid = is_array && is_unsigned;
    if (not valid) {
      res = error_literal;
      if (not is_array)
        error(subscript_node, std::format("Subscript operator used on non-array type {}.", array_expr.type->toString()) );
      if (not is_unsigned)
        error(subscript_node, std::format("Expression used as array index is of signed type {}.", index_expr.type->toString()) );
      return res;
    }

    auto const array_type = array_expr.type->castToArray();
    auto const array_subtype = array_type->getSubtype();
    res.type = array_subtype;
    res.qualifiers = array_expr.qualifiers;

    subscript_instruction.subscript_data.array_type = array_type;
    instructions[subscript_idx] = subscript_instruction;

    coerce_if_integerliteral(instructions[index_idx], PrimitiveType::uptr_t());
    return res;
  }
#undef pre

#define pre assume_assert(cast_node.type == ASTNode::CAST);
  QualifiedType
  peepCastExpression(ASTNode cast_node) { pre
    Instruction cast_instruction = newInstruction(cast_node);
    auto const cast_idx = instructions.size();
    instructions.emplace_back();

    QualifiedType res;
    auto const cast_type = cast_node.cast_data.cast_type;
    res.type = cast_type;

    auto const casted_expr = peepExpression();
    if (not casted_expr.type->castableTo(cast_type)) {
      error(cast_node,
        std::format("Invalid cast from {} to {}", casted_expr.type->toString(), cast_type->toString())
        );
      res = error_literal;
      return res;
    }

    cast_instruction.m.type = castForType(casted_expr.type);
    cast_instruction.cast_data.destination_type = cast_type;
    instructions[cast_idx] = cast_instruction;

    return res;
  }
#undef pre

#define pre assume_assert(calling_node.type == ASTNode::CALLING);
  QualifiedType
  peepCallingExpression(ASTNode calling_node) { pre
    auto const num_parameters = calling_node.call_data.num_parameters;

    // add call instruction
    {
      Instruction call_instruction = newInstruction(calling_node, Instruction::CALL);
      call_instruction.call_data.num_parameters = num_parameters;
      instructions.emplace_back(call_instruction);
    }

    QualifiedType res;
    auto const called = peepExpression();
    if (not called.type->isCallable()) {
      if (not called.type->isError()) error(calling_node, "Call operator used on non-callable.");

      for (auto i{0uz}; i<num_parameters; ++i)
        (void)peepExpression();

      res = error_literal;
      return res;
    }

    assert(called.type->isFunction() or called.type->isError());

    auto const function_type = called.type->castToFunction();
    auto const function_parameter_types = function_type->parameterTypes();
    auto const function_parameter_count = function_parameter_types.size();
    bool const variadic = function_type->isVariadic();

    if (num_parameters < function_parameter_count) {
      error(calling_node, "Too few parameters for function call.");
      res = error_literal;
      return res;
    }

    if (num_parameters > function_parameter_count and not variadic) {
      error(calling_node, "Too many parameters for function call.");
      res = error_literal;
      return res;
    }

    auto i{0uz};
    for (; i<function_parameter_count; ++i) {
      auto const given_parameter_idx = instructions.size();
      auto given_parameter = peepExpression();
      auto const function_parameter_type = function_parameter_types[i];

      if (given_parameter.type not_eq function_parameter_type) {
        if (not given_parameter.type->coercibleTo(function_parameter_type)) {
          error(calling_node, std::format("Cannot coerce parameter of type '{}' to type '{}'.", given_parameter.type->toString(), function_parameter_type->toString()));
          res = error_literal;
          return res;
        }

        coerce_if_integerliteral(instructions[given_parameter_idx], function_parameter_type);
      }
    }

    if (variadic) {
      for (; i<num_parameters; ++i)
        (void)peepExpression();
    }

    res.type = function_type->returnType();
    return res;
  }
#undef pre

  //TODO: Add Short Circuiting
#define pre assume_assert(binary_node.type == ASTNode::BINARY);
  QualifiedType
  peepBinaryExpression(ASTNode binary_node) { pre
    Instruction binary_instruction = newInstruction(binary_node);
    auto const binary_idx = instructions.size();
    instructions.emplace_back();

    auto const left_idx = instructions.size();
    auto left = peepExpression();

    bool const left_signed = left.type->isSignedIntegral();
    bool const left_float = left.type->isFloating();
    bool const arithmetic = left.type->isArithmetic();

    auto const right_idx = instructions.size();
    auto right = peepExpression();

    // TODO: THIS IS BAD AND SUCKS FIX IT NOW
    if (left.type not_eq right.type) { // if left or right is an integer literal, coerce its type to the other
      if (coerce_if_integerliteral(instructions[left_idx], right.type))
        left.type = right.type;
      else if (coerce_if_integerliteral(instructions[right_idx], left.type))
        right.type = left.type;
      else {
        error(binary_node,
          std::format("Right type {} in binary expression cannot coerce to left type {}.",
          right.type->toString(), left.type->toString()));

        left = error_literal;
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
        left = error_literal;
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
        left = error_literal;
        break;
      }

      left = bool_literal;
      break;

    case Operator::AND:
    case Operator::OR:
    case Operator::XOR:
      if (not left.type->isBool()) {
        error(binary_node, "Non-boolean expressions in boolean binary operation.");
        left = error_literal;
        break;
      }

      left = bool_literal;
      break;

    case Operator::BITAND:
    case Operator::BITOR:
    case Operator::BITXOR:
      if (not arithmetic) {
        error(binary_node, "Non-arithmetic expressions in bitwise operation.");
        left = error_literal;
        break;
      }

      left.qualifiers.writable = false;
      break;

    case Operator::ASSIGN:
      if (not left.qualifiers.writable) {
        error(binary_node, "Left expression in assignment non-mutable.");
        left = error_literal;
      }
      break;

    case Operator::EQUAL:
    case Operator::NOT_EQUAL:
      left = bool_literal;
      break;

    default:
      eden_unreachable("Invalid binary operator.");
    }

    switch (binary_node.binary_data.opr) { // second sets the right instruction
    case Operator::ADD:
      binary_instruction.m.type = left_float ? Instruction::FADD : Instruction::ADD;
      break;
    case Operator::SUBTRACT:
      binary_instruction.m.type = left_float ? Instruction::FSUB : Instruction::SUB;
      break;
    case Operator::MULTIPLY:
      binary_instruction.m.type = left_float ? Instruction::FMULT : Instruction::MULT;
      break;
    case Operator::DIVIDE:
      binary_instruction.m.type =
      left_float ? Instruction::FDIV :
       left_signed ? Instruction::SDIV : Instruction::UDIV;
      break;
    case Operator::MODULUS:
      binary_instruction.m.type =
      left_float ? Instruction::FMOD :
       left_signed ? Instruction::SMOD : Instruction::UMOD;
      break;
    case Operator::LESS:
      binary_instruction.m.type =
      left_float ? Instruction::FLESS :
       left_signed ? Instruction::SLESS : Instruction::ULESS;
      break;
    case Operator::GREATER:
      binary_instruction.m.type =
      left_float ? Instruction::FGTR :
       left_signed ? Instruction::SGTR : Instruction::UGTR;
      break;
    case Operator::LESS_EQUAL:
      binary_instruction.m.type =
      left_float ? Instruction::FLEQ :
       left_signed ? Instruction::SLEQ : Instruction::ULEQ;
      break;
    case Operator::GREATER_EQUAL:
      binary_instruction.m.type =
      left_float ? Instruction::FGEQ :
       left_signed ? Instruction::SGEQ : Instruction::UGEQ;
      break;
    case Operator::AND:
      binary_instruction.m.type = Instruction::AND;
      break;
    case Operator::OR:
      binary_instruction.m.type = Instruction::OR;
      break;
    case Operator::XOR:
      binary_instruction.m.type = Instruction::NEQ;
      break;
    case Operator::BITAND:
      binary_instruction.m.type = Instruction::BITAND;
      break;
    case Operator::BITOR:
      binary_instruction.m.type = Instruction::BITOR;
      break;
    case Operator::BITXOR:
      binary_instruction.m.type = Instruction::BITXOR;
      break;
    case Operator::ASSIGN: {
      binary_instruction.m.type = Instruction::ASSIGN;
      adjustAssignExpression(binary_instruction, left, right);
      break;
    }
    case Operator::EQUAL:
      binary_instruction.m.type = Instruction::EQ;
      break;
    case Operator::NOT_EQUAL:
      binary_instruction.m.type = Instruction::NEQ;
      break;
    default:
      eden_unreachable("Invalid binary operator.");
    }

    instructions[binary_idx] = binary_instruction;
    return left;
  }
#undef pre

#define pre assume_assert(unary_node.type == ASTNode::UNARY);
  QualifiedType
  peepUnaryExpression(ASTNode unary_node) { pre
    Instruction unary_instruction = newInstruction(unary_node);
    auto const unary_idx = instructions.size();
    instructions.emplace_back();

    auto expression = peepExpression();
    const bool float_opr = expression.type->isFloating();
    const bool arithmetic = expression.type->isArithmetic();
    switch (unary_node.unary_data.opr) {
    case Operator::ADDRESS_OF:
      if (not expression.qualifiers.writable)
        error(unary_node, "Address-of (@) operator used on readonly expression.");
      expression.type = module->getRawPointerType(expression.type);
      expression.qualifiers.writable = false;
      break;

    case Operator::REF_TO:
      expression.type = module->getRefPointerType(expression.type);
      expression.qualifiers.writable = false;
      break;

    case Operator::PRE_INCREMENT:
    case Operator::PRE_DECREMENT:
      if (not expression.qualifiers.writable) {
        error(unary_node, "Prefix operator used on readonly expression.");
        expression = error_literal;
      }
      break;
    case Operator::UNARY_MINUS:
      if (not arithmetic) {
        error(unary_node, "Unary minus used on non-arithmetic expression.");
        expression = error_literal;
        break;
      }
      expression.qualifiers.writable = false;
      break;
    case Operator::BITNOT:
      if (not arithmetic) {
        error(unary_node, "bitnot operator used non-arithmetic expression.");
        expression = error_literal;
        break;
      }
      expression.qualifiers.writable = false;
      break;
    case Operator::NOT:
      if (not expression.type->isBool()) {
        error(unary_node, "not operator used on non-boolean expression.");
        expression = error_literal;
        break;
      }
      expression.qualifiers.writable = false;
      break;
    case Operator::POST_INCREMENT:
    case Operator::POST_DECREMENT:
      if (not expression.qualifiers.writable) {
        error(unary_node, "Postfix decrement operator used on non-mutable expression.");
        expression = error_literal;
        break;
      }
      expression.qualifiers.writable = false;
      break;
    case Operator::ARROW:
      if (not expression.type->isPointer()) {
        error(unary_node, "Arrow operator used on non-pointer type.");
        expression = error_literal;
        break;
      }

      expression = expression.type->castToPointer()->getSubtype();
      break;
    default:
      eden_unreachable("Invalid unary operator.");
    }

    switch (unary_node.unary_data.opr) {
    case Operator::ADDRESS_OF:
    case Operator::REF_TO:          unary_instruction.m.type = Instruction::ADDRESS_OF; break;

    case Operator::PRE_INCREMENT:   unary_instruction.m.type = float_opr ? Instruction::FPRE_INC : Instruction::PRE_INC;  break;
    case Operator::PRE_DECREMENT:   unary_instruction.m.type = float_opr ? Instruction::FPRE_DEC : Instruction::PRE_DEC;  break;
    case Operator::UNARY_MINUS:     unary_instruction.m.type = float_opr ? Instruction::FNEGATE : Instruction::NEGATE;  break;

    case Operator::BITNOT:
    case Operator::NOT:             unary_instruction.m.type = Instruction::BITNOT; break;

    case Operator::POST_INCREMENT:  unary_instruction.m.type = float_opr ? Instruction::FPOST_INC : Instruction::POST_INC; break;
    case Operator::POST_DECREMENT:  unary_instruction.m.type = float_opr ? Instruction::FPOST_DEC : Instruction::POST_DEC; break;

    case Operator::ARROW:
      unary_instruction.m.type = Instruction::DEREFERENCE;
      unary_instruction.dereference_data.dereference_type = expression.type;
      break;
    default:
      eden_unreachable("Invalid unary operator.");
    }

    instructions[unary_idx] = unary_instruction;
    return expression;
  }
#undef pre

  [[nodiscard]] QualifiedType
  peepExpression() {
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

    default: eden_unreachable("Invalid ASTNode while peeping expression.");
    }
  }

  void adjustAssignExpression(Instruction& assign, QualifiedType left, QualifiedType right) const noexcept {
    if (left.type not_eq right.type and not left.type->isPointer()) {
      assign.m.type = right.type->isSignedIntegral() ? Instruction::SCAST_ASSIGN : Instruction::UCAST_ASSIGN;
      assign.cast_assign_data.bitwidth = left.type->bitwidth();
    }
  }

#define pre assume_assert(return_node.type == ASTNode::RETURN);
  void peepReturnStatement(ASTNode return_node) { pre
    auto const return_type = current_function_type->returnType();
    if (not return_node.return_data.has_value) {
      if (not return_type->isDevoid())
        return error(nodes.take(),"Non-devoid function expects return value.");

      current_block().set_ret();
      new_block();
      return;
    }

    auto const assign_idx = instructions.size();
    {
      instructions.emplace_back(newInstruction(return_node, Instruction::ASSIGN));

      Instruction local_instruction = newInstruction(return_node, Instruction::LOCAL);
      local_instruction.local_data.idx = 0;
      instructions.emplace_back(local_instruction);
    }

    auto const return_idx = instructions.size();
    auto const return_expression = peepExpression();
    if (return_type->isDevoid()) {
      error(return_node,  "Cannot return value from devoid function.");
      return;
    }

    if (not return_expression.type->coercibleTo(return_type)) {
      error(return_node, "Return statement's type is not compatible with function return type.");
      return;
    }

    coerce_if_integerliteral(instructions[return_idx], return_type);
    adjustAssignExpression(instructions[assign_idx], {return_type, {}}, return_expression);
    current_block().set_ret();
  }
#undef pre

  void peepScopedStatement(u32_t num_children) {
    if (num_children == 0) return;
    while (num_children-- not_eq 0)
      peepStatement();
  }

#define pre assume_assert(while_node.type == ASTNode::WHILE);
  void peepWhileLoop(ASTNode while_node) { pre
    br_fallthrough();
    new_block();
    auto const condition_idx = current_block_index();
    auto const condition = peepExpression();
    if (not condition.type->isBool()) {
      error(while_node, "While Loop condition not boolean.");
      return;
    }

    auto const loop_body_idx = current_block_index() + 1;
    force_new_block();
    peepScopedStatement(while_node.while_data.num_substatements);
    if (is_current_block_empty())
      instructions.emplace_back(Instruction::NOOP);
    current_block().set_br(condition_idx);
    force_new_block();
    auto const after_loop_idx = current_block_index();

    blocks[condition_idx].set_brc(loop_body_idx, after_loop_idx);
  }
#undef pre

#define pre assume_assert(if_node.type == ASTNode::IF);
  void peepIfStatement(ASTNode if_node) { pre
    auto const condition = peepExpression();
    if (not condition.type->isBool()) {
      error(if_node, "If statement condition not boolean.");
      return;
    }

    auto const condition_block_idx = current_block_index();
    auto const true_block_idx = condition_block_idx + 1;

    new_block(); //true block
    peepScopedStatement(if_node.if_data.num_substatements); //true statement(s)

    new_block(); //false block
    auto const false_block_idx = current_block_index();
    auto after_block_idx = false_block_idx;
    if (not nodes.pop_if_empty()) {
      peepStatement();
      blocks.back().set_br(blocks.size());
      new_block();
      after_block_idx = current_block_index();
    }
    blocks[true_block_idx].set_br(after_block_idx);
    blocks[condition_block_idx].set_brc(true_block_idx, false_block_idx);
  }
#undef pre

#define pre assume_assert(decl_node.type == ASTNode::DECLARATION);
  void peepVarDeclaration(ASTNode decl_node) { pre
    auto const declared = nodes.take();
    auto const declared_name = declared.identifier_val(*current_file);
    auto const type = declared.identifier_data.decl_type;
    auto const qualified_type = QualifiedType{type, decl_node.declaration_data.qualifiers};
    locals.emplace_back(type);

    if (module->containsLocal(declared_name)) {
      error(declared, "Redefinition of symbol name in variable declaration.");
      if (decl_node.declaration_data.has_init) (void)peepExpression();
      return;
    }

    if (not decl_node.declaration_data.has_init) {
      module->addLocal(declared_name, qualified_type);
      return;
    }

    auto const assign_idx = instructions.size();
    {
      instructions.emplace_back(newInstruction(decl_node, Instruction::ASSIGN));

      Instruction local_instruction = newInstruction(decl_node, Instruction::LOCAL);
      local_instruction.local_data.idx = locals.size() - 1;
      instructions.emplace_back(local_instruction);
    }
    module->addLocal(declared_name, qualified_type);

    auto const init_expr_idx = instructions.size();
    auto const init_expr = peepExpression();
    if (not init_expr.type->coercibleTo(qualified_type.type)) {
      error(decl_node, std::format("Type {} is not coercible to type {}.", init_expr.type->toString(), qualified_type.type->toString()));
      return;
    }

    coerce_if_integerliteral(instructions[init_expr_idx], type);
    adjustAssignExpression(instructions[assign_idx], qualified_type, init_expr);
  }
#undef pre

  void peepStatement() {
    auto const node = nodes.take();
    switch (node.type) { using enum ASTNode::NodeType;
    case EMPTY:        eden_unreachable("Empty node peeped in peepStatement.");
    case DECLARATION:  return peepVarDeclaration(node);
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

    default: eden_unreachable("Invalid ASTNode while peeping statement.");
    }
  }

  void peepUntilEmpty() {
    assert(instructions.empty());
    assert(blocks.empty());
    auto const return_type = current_function_type->returnType();
    blocks.emplace_back(0);
    while (not nodes.empty())
      peepStatement();

    if (blocks.back().terminator_type == Block::Terminator::NONE)
      br_fallthrough();

    //set up return block
    new_block();
    if (return_type->isDevoid())
      instructions.emplace_back(Instruction::NOOP);
    else {
      Instruction tmp;
        tmp.m.type = Instruction::LOCAL,
        tmp.m.file_idx = 0,
        tmp.m.length_in_file = 0,
        tmp.m.position_in_file = 0,
        tmp.local_data.idx = 0;
      instructions.emplace_back(tmp);
    }

    current_block().set_ret();

    //turn ret into a br to the return block
    //turn brc with identical branches into a br
    for (auto i{0uz}; i<blocks.size()-1; ++i) {
      auto& block = blocks[i];
      switch (block.terminator_type) {
      case Block::Terminator::BR: break;

      case Block::Terminator::BRC: //turn brc with identical branches into a br
        if (block.brc.true_block_idx == block.brc.false_block_idx) {
          block.terminator_type = Block::Terminator::BR;
          block.br.next_block_idx = block.brc.true_block_idx;
        }
        break;
      case Block::Terminator::RET:
        block.terminator_type = Block::Terminator::BR;
        block.br.next_block_idx = current_block_index();
        break;
      default:
        eden_unreachable("Invalid ASTNode while peeping statement.");
      }

    }
  }

public:

  // peeps parsed_functions and fills peeped_tu.functions
  // returns whether an error occurted
  [[nodiscard]] static bool
  peepFunctions(TU& peep_tu, std::vector<Parser::Function> const& parsed_functions) {
    Peeper peeper;
    peeper.module = peep_tu.module;
    for (auto const& func : parsed_functions) {
      auto const function_type = peep_tu.module->enterFunctionScope(func.nameof());
      peeper.current_file = &peep_tu.source_files[func.file_idx];
      peeper.nodes.begin = func.body.cbegin();
      peeper.nodes.end = func.body.cend();
      peeper.current_function_type = function_type;

      auto const parameter_types = function_type->parameterTypes();
      peeper.locals.reserve(parameter_types.size() + 1); // + return type
      peeper.locals.emplace_back(function_type->returnType());
      for (auto const parameter_type : parameter_types)
        peeper.locals.emplace_back(parameter_type);

      peeper.peepUntilEmpty();
      peep_tu.functions.emplace_back(
        Function {
        .is_public = func.is_public, .file_idx = func.file_idx,
        .name_len = func.name_len, .name_ptr = func.name_ptr,

        .type = function_type,
        .locals = std::move(peeper.locals),
        .instructions = std::move(peeper.instructions),
        .blocks = std::move(peeper.blocks)
        });
    }

    return peeper.has_error;
  }

};

}

// printing functions
namespace {

void printPeepInstruction(Instruction instruction, File file) {
  switch (instruction.m.type) { using enum Instruction::InstructionType;
  case NOOP: return std::println("NOOP");
  case GLOBAL: return std::println("GLOBAL {}", instruction.original_string(file));
  case FUNCTION: return std::println("FUNCTION {}", instruction.original_string(file));

  case MODULE_GLOBAL: return std::println("GLOBAL {} FROM MODULE {}", instruction.module_variable_name(), instruction.module_name(file));
  case MODULE_FUNCTION: return std::println("FUNCTION {} FROM MODULE {}", instruction.module_function_name(), instruction.module_name(file));
  case TYPE_VARIABLE: return std::println("TYPE_VARIABLE {}", instruction.type_member_data.custom_type->member_table()->getVariable(instruction.type_member_data.member_idx)->nameof());

  case LOCAL: return std::println("LOCAL {}", instruction.local_data.idx);

  case I8_LITERAL: return std::println("I8_LITERAL {}", instruction.signed_literal_data.value);
  case I16_LITERAL: return std::println("I16_LITERAL {}", instruction.signed_literal_data.value);
  case I32_LITERAL: return std::println("I32_LITERAL {}", instruction.signed_literal_data.value);
  case I64_LITERAL: return std::println("I64_LITERAL {}", instruction.signed_literal_data.value);
  case U8_LITERAL: return std::println("U8_LITERAL {}", instruction.unsigned_literal_data.value);
  case U16_LITERAL: return std::println("U16_LITERAL {}", instruction.unsigned_literal_data.value);
  case U32_LITERAL: return std::println("U32_LITERAL {}", instruction.unsigned_literal_data.value);
  case U64_LITERAL: return std::println("U64_LITERAL {}", instruction.unsigned_literal_data.value);
  case FLOAT_LITERAL: return std::println("FLOAT_LITERAL {}", instruction.float_literal_data.value);
  case DOUBLE_LITERAL: return std::println("DOUBLE_LITERAL {}", instruction.double_literal_data.value);
  case BOOL_LITERAL: return std::println("BOOL_LITERAL {}", instruction.bool_literal_data.value);
  case CHAR_LITERAL: return std::println("CHAR_LITERAL {}", instruction.char_literal_data.value);
  case STRING_LITERAL: return std::println("STRING_LITERAL {}", instruction.original_string(file));
  case ESCAPED_STRING_LITERAL: return std::println("STRING_LITERAL W/ ESCAPE_SEQUENCE {}", instruction.original_string(file));

  case ADD: return std::println("ADD");
  case FADD: return std::println("FADD");
  case SUB: return std::println("SUB");
  case FSUB: return std::println("FSUB");
  case MULT: return std::println("MULT");
  case FMULT: return std::println("FMULT");
  case UDIV: return std::println("UDIV");
  case SDIV: return std::println("SDIV");
  case FDIV: return std::println("FDIV");
  case UMOD: return std::println("UMOD");
  case SMOD: return std::println("SMOD");
  case FMOD: return std::println("FMOD");
  case ASSIGN: return std::println("ASSIGN");
  case UCAST_ASSIGN: return std::println("UCAST_ASSIGN {}b", instruction.cast_assign_data.bitwidth);
  case SCAST_ASSIGN: return std::println("SCAST_ASSIGN {}b", instruction.cast_assign_data.bitwidth);
  case ULESS: return std::println("ULESS");
  case SLESS: return std::println("SLESS");
  case FLESS: return std::println("FLESS");
  case UGTR: return std::println("UGTR");
  case SGTR: return std::println("SGTR");
  case FGTR: return std::println("FGTR");
  case ULEQ: return std::println("ULEQ");
  case SLEQ: return std::println("SLEQ");
  case FLEQ: return std::println("FLEQ");
  case UGEQ: return std::println("UGEQ");
  case SGEQ: return std::println("SGEQ");
  case FGEQ: return std::println("FGEQ");
  case EQ: return std::println("EQ");
  case NEQ: return std::println("NEQ");
  case AND: return std::println("AND");
  case OR: return std::println("OR");
  case BITAND: return std::println("BITAND");
  case BITOR: return std::println("BITOR");
  case BITXOR: return std::println("BITXOR");
  case SUBSCRIPT: return std::println("SUBSCRIPT WITH ARRAY TYPE {}", instruction.subscript_data.array_type->toString());

  case PRE_INC: return std::println("PRE_INC");
  case FPRE_INC: return std::println("FPRE_INC");
  case PRE_DEC: return std::println("PRE_DEC");
  case FPRE_DEC: return std::println("FPRE_DEC");
  case ADDRESS_OF: return std::println("ADDRESS_OF");
  case NEGATE: return std::println("NEGATE");
  case FNEGATE: return std::println("FNEGATE");
  case BITNOT: return std::println("BITNOT");
  case POST_INC: return std::println("POST_INC");
  case FPOST_INC: return std::println("FPOST_INC");
  case POST_DEC: return std::println("POST_DEC");
  case FPOST_DEC: return std::println("FPOST_DEC");
  case DEREFERENCE: return std::println("DEREFERENCE TO {}", instruction.dereference_data.dereference_type->toString());

  case UCAST: return std::println("UCAST TO {}", instruction.cast_data.destination_type->toString());
  case SCAST: return std::println("SCAST TO {}", instruction.cast_data.destination_type->toString());
  case FCAST: return std::println("FCAST TO {}", instruction.cast_data.destination_type->toString());
  case PCAST: return std::println("PCAST TO {}", instruction.cast_data.destination_type->toString());

  case CALL: return std::println("CALL WITH {} PARAMETER(S)", instruction.call_data.num_parameters);
  default:
    eden_unreachable("Invalid peep instruction.");
  }
}

void printPeepBlockTerminator(Block block) {
  switch (block.terminator_type) { using enum Block::Terminator;
  case BR:  return std::println("\tBR {}", block.br.next_block_idx);
  case BRC: return std::println("\tBRC TRUE {}, FALSE {}", block.brc.true_block_idx, block.brc.false_block_idx);
  case RET: return std::println("\tRET");
  default:
    eden_unreachable("Invalid block terminator type.");
  }
}

void printPeepBlocks(std::vector<Block> const& blocks, std::vector<Instruction> const& instructions, File file) {
  auto const num_blocks = blocks.size();
  sz_t current_block{};
  sz_t current_instruction{};

  while (true) {
    if (blocks[current_block].first_instruction_idx == current_instruction) {
      if (current_block not_eq 0)
        printPeepBlockTerminator(blocks[current_block - 1]);
      if (current_block == num_blocks - 1)
        break;
      std::println("Block {}: ", current_block);
      ++current_block;
    }
    std::print("\t");
    printPeepInstruction(instructions[current_instruction], file);
    ++current_instruction;
  }

  std::print("Return Block {}:\n\t", current_block);
  printPeepInstruction(instructions[current_instruction++], file);
  printPeepBlockTerminator(blocks[current_block]);
}

}

void PeepIR::printPeep(TU const& tu) {
  for (auto const& func : tu.functions) {
    std::println("{}fn {}{} {{",
    func.is_public ? "pub " : "",
    func.nameof(),
    func.type->toString());

    std::print("Locals: | ");
    auto const num_locals = func.locals.size() - 1;
    for (auto i{0uz}; i<num_locals; ++i)
      std::print("{}: {} | ", i + 1, func.locals[i + 1]->toString());
    std::println();

    printPeepBlocks(func.blocks, func.instructions, tu.source_files[func.file_idx]);
    std::println("}}\n");
  }
}

bool PeepIR::lowerToPeep(TU& tu, Parser::TU&& parsed_tu) {

#ifdef STAGE_BENCHMARKS
  auto begin_time = std::chrono::high_resolution_clock::now();
#endif

  tu.source_files = std::move(parsed_tu.source_files);
  tu.module = parsed_tu.module;
  tu.functions.reserve(parsed_tu.functions.size());
  tu.name = parsed_tu.name;

  bool const has_error = Peeper::peepFunctions(tu, parsed_tu.functions);
#ifdef STAGE_BENCHMARKS
  auto end_time = std::chrono::high_resolution_clock::now();
  std::println("{:>10}, {:>10} | Peeping {}",
    end_time - begin_time,
    std::chrono::duration_cast<std::chrono::microseconds>(end_time - begin_time),
    parsed_tu.module->nameof()
  );
#endif
  return has_error;
}