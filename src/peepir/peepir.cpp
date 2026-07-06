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
using namespace LOM::PeepMIR;
using namespace LOM::AST;

namespace {

[[nodiscard]] constexpr Instruction::Type
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
  eden_always_inline [[nodiscard]] constexpr bool peek_is_empty() const noexcept { return begin->m.type == ASTNode::EMPTY; }
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

struct ExpressionResult {
  Type const* type;
  Type::Qualifiers qualifiers{eden::flags::do_not_initialize};
  // char _pad[1];
  u16_t length_in_file;
  u32_t position_in_file;

  ExpressionResult() = default;
  ExpressionResult(QualifiedType t, ASTNode node)
  : type(t.type), qualifiers(t.qualifiers), length_in_file(node.m.length_in_file), position_in_file(node.m.position_in_file) {}

  ExpressionResult(QualifiedType t, u16_t length_in_file, u32_t position_in_file)
 : type(t.type), qualifiers(t.qualifiers), length_in_file(length_in_file), position_in_file(position_in_file) {}

  QualifiedType instantiated_type() const noexcept { return {type, qualifiers}; }
};

class Peeper {
  Module* module;
  File const* current_file;
  bool has_error{false};

  eden::swap_vector<Module*> imports;
  std::vector<Type const*> locals;
  std::vector<Instruction> instructions;
  std::vector<Block> blocks;

  TreeView nodes;
  FunctionType const* current_function_type;

  eden_always_inline [[nodiscard]] constexpr Block&
  current_block() noexcept
  { return blocks.back(); }

  eden_always_inline [[nodiscard]] constexpr u32_t
  current_block_index() const noexcept
  { return blocks.size() - 1; }

  eden_always_inline [[nodiscard]] constexpr bool
  is_current_block_empty() const noexcept
  { return blocks.back().first_instruction_idx == instructions.size(); }

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

  constexpr void
  force_new_block() noexcept
  { blocks.emplace_back(instructions.size(), Block::Terminator::NONE); }

  constexpr bool // returns whether coersion was successful
  coerce_if_integerliteral(Instruction& possible_literal, Type const* expected_type) const noexcept {
    if (not possible_literal.is_literal() or not expected_type->isIntegral()) return false;
    auto const expected_type_primitive = expected_type->castToPrimitive();
    auto const is_signed = expected_type_primitive->isSignedIntegral();
    possible_literal.adjust_literal(expected_type_primitive->bitwidth(), is_signed);
    return true;
  }

  eden_noinline_cold
  void error(auto err, std::string msg) noexcept
  { report_error(*current_file, err.length_in_file, err.position_in_file, std::move(msg)); has_error = true; }

  eden_noinline_cold
  void error(ASTNode err, std::string msg) noexcept
  { report_error(*current_file, err.m.length_in_file, err.m.position_in_file, std::move(msg)); has_error = true; }

  ExpressionResult
  peepLiteral(ASTNode node) {
    switch (node.m.type) { using enum ASTNode::NodeType;
    case SIGNED_LITERAL:
      instructions.emplace_back(Instruction::I8_LITERAL, std::bit_cast<u64_t>(node.signed_val())); // signed and unsigned sizes will be adjusted later
      return {signedToLiteralInstance(node.signed_val()), node};
    case UNSIGNED_LITERAL:
      instructions.emplace_back(Instruction::U8_LITERAL, node.unsigned_val());
      return {unsignedToLiteralInstance(node.unsigned_val()), node};
    case FLOAT_LITERAL:
      instructions.emplace_back(Instruction::FLOAT_LITERAL, static_cast<u64_t>(std::bit_cast<u32_t>(node.float_val())));
      return {f32_literal, node};
    case DOUBLE_LITERAL:
      instructions.emplace_back(Instruction::DOUBLE_LITERAL, std::bit_cast<u64_t>(node.double_val()));
      return {f64_literal, node};
    case BOOL_LITERAL:
      instructions.emplace_back(Instruction::BOOL_LITERAL, static_cast<u64_t>(node.bool_val()));
      return {bool_literal, node};
    case CHAR_LITERAL:
      instructions.emplace_back(Instruction::CHAR_LITERAL, static_cast<u64_t>(node.char_val()));
      return {char_literal, node};
    case STRING_LITERAL:
      instructions.emplace_back(Instruction::STRING_LITERAL, node.string_val(*current_file));
      return {string_literal, node};
    case ESCAPED_STRING_LITERAL:
      instructions.emplace_back(Instruction::ESCAPED_STRING_LITERAL, node.string_val(*current_file));
      return {string_literal, node};
    default: eden_unreachable("Invalid literal type.");
    }
  }

  ExpressionResult
  peepMemberAccess(ASTNode::MemberAccessData data) {
    auto const member_access_idx = instructions.size();
    instructions.emplace_back(Instruction::NOOP, 0).~Instruction(); // doing this to reuse the constructor

    if (nodes.peek().m.type == ASTNode::IDENTIFIER) {
      static constexpr auto module_search_predicate = [](const Module* element, std::string_view name) { return element->nameof() == name; };

      auto const possible_module = imports.search(module_search_predicate, nodes.peek().identifier_val(*current_file));
      if (possible_module == nullptr) goto not_module;
      nodes.pop();

      auto const member_access_instruction = &instructions[member_access_idx];
      auto const other_module = *possible_module;

      auto const member_node = nodes.take();
      auto const member_name = std::string_view(member_node.identifier_val(*current_file));
      if (auto const member_variable = other_module->getPublicVariable(member_name)) {
        eden_unreachable("Globals unimplemented.");
        assert(member_variable->get_id() not_eq SymbolTable::INVALID_ID);
        new (member_access_instruction) Instruction(Instruction::MODULE_GLOBAL, other_module, member_variable->get_id());
        return {member_variable->type, data.length_in_file, data.position_in_file};
      }

      if (auto const member_function = other_module->getPublicFunction(member_name)) {
        assert(member_function->get_id() not_eq SymbolTable::INVALID_ID);
        new (member_access_instruction) Instruction(Instruction::MODULE_FUNCTION, other_module, member_function->get_id());
        return {{member_function->type, {}}, data.length_in_file, data.position_in_file};
      }

      error(member_node, "Identifier is not a public member of module.");
      return {error_literal, member_node};
    }

    not_module:
    auto const object_expression = peepExpression();
    if (not object_expression.type->isCustom()) {
      error(object_expression, "Attempt to access member of non-custom type.");
      return {error_literal, object_expression.length_in_file, object_expression.position_in_file};
    }

    auto const member_access_instruction = &instructions[member_access_idx];
    auto const custom_type = object_expression.type->castToCustom();
    auto const member_table = custom_type->member_table();
    auto const identifier_node = nodes.take();
    auto const identifier = std::string_view(identifier_node.identifier_val(*current_file));

    if (auto const member_variable = member_table->getPublicVariable(identifier)) {
      assert(member_variable->get_id() not_eq SymbolTable::INVALID_ID);

      new (member_access_instruction) Instruction(Instruction::TYPE_VARIABLE, custom_type, member_variable->get_id());
      return{
        {member_variable->type.type, object_expression.qualifiers}, // use the member's type and the object's qualifiers, member shouldn't have qualifiers regardless
        data.length_in_file, data.position_in_file};
    }

    if (auto const member_function = member_table->getPublicFunction(identifier)) {
      eden_unreachable("Member functions unimplemented."); assert(member_function->get_id() not_eq SymbolTable::INVALID_ID);
      return { {member_function->type, {}}, data.length_in_file, data.position_in_file};
    }

    new (member_access_instruction) Instruction(Instruction::UCAST, 0); // this is only here to avoid the UB from double destruction once this returns

    error(identifier_node,"Identifier is not a public member of type.");
    return {error_literal, identifier_node};
  }

#define pre assert(data.type == ASTNode::IDENTIFIER);
  ExpressionResult
  peepIdentifier(ASTNode::IdentifierData data) { pre
    // data.decl_type should NOT be used here
    auto const identifier = current_file->view_at(data.length_in_file, data.position_in_file);
    auto const len = data.length_in_file;
    auto const pos = data.position_in_file;

    if (auto const variable = module->getLocal(identifier)) {
      instructions.emplace_back(Instruction::LOCAL, variable->second + 1); // adjust by 1 accounting for return type
      return {variable->first, len, pos};
    }

    if (auto const function = module->getFunction(identifier)) {
      instructions.emplace_back(Instruction::FUNCTION, identifier);
      return {{function->type, {}}, len, pos};
    }

    error(data, std::format("Undeclared identifier: {}.", identifier));
    instructions.emplace_back(Instruction::NOOP, 0);
    return {error_literal, len, pos};
  }
#undef pre

  ExpressionResult
  peepSubscriptExpression(ASTNode::SubscriptData data) {
    auto const subscript_idx = instructions.size();
    instructions.emplace_back(Instruction::SUBSCRIPT, 0);

    auto const array_expr = peepExpression(); bool const is_array = array_expr.type->isArray();

    auto const index_idx /*lol*/ = instructions.size();
    auto const index_expr = peepExpression(); bool const is_unsigned = index_expr.type->isUnsignedIntegral();

    ExpressionResult res;
    res.length_in_file = data.length_in_file;
    res.position_in_file = data.position_in_file;

    bool const valid = is_array & is_unsigned;
    if (not valid) {
      res.type = Type::devoid();
      res.qualifiers = Type::Qualifiers{};
      if (not is_array)
        error(array_expr, std::format("Subscript operator used on non-array type {}.", array_expr.type->toString()) );
      if (not is_unsigned)
        error(index_expr, std::format("Expression used as array index is of signed type {}.", index_expr.type->toString()) );
      return res;
    }

    auto const array_type = array_expr.type->castToArray();
    auto const array_subtype = array_type->getSubtype();
    res.type = array_subtype;

    instructions[subscript_idx].value = std::bit_cast<u64_t>(array_type);
    res.qualifiers = array_expr.qualifiers;

    coerce_if_integerliteral(instructions[index_idx], PrimitiveType::uptr_t());
    return res;
  }

  ExpressionResult
  peepCastExpression(ASTNode::CastData data) {
    auto const cast_idx = instructions.size();
    instructions.emplace_back(Instruction::NOOP, std::bit_cast<u64_t>(data.cast_type));

    auto const casted_expr = peepExpression();
    if (not casted_expr.type->castableTo(data.cast_type)) {
      error(casted_expr,
        std::format("Invalid cast from {} to {}", casted_expr.type->toString(), data.cast_type->toString())
        );
      return {error_literal, data.length_in_file, data.position_in_file};
    }

    auto& cast_instruction = instructions[cast_idx];
    cast_instruction.type = castForType(casted_expr.type);
    return {{data.cast_type, {}}, data.length_in_file, data.position_in_file};
  }

  ExpressionResult
  peepCallingExpression(ASTNode::CallingData data) {
    instructions.emplace_back(Instruction::CALL, data.num_parameters);
    auto const called = peepExpression();
    if (not called.type->isCallable()) {
      if (not called.type->isError()) error(called, "Call operator used on non-callable.");

      for (auto i{0uz}; i<data.num_parameters; ++i)
        (void)peepExpression();

      return {error_literal, called.length_in_file, called.position_in_file};
    }

    assert(called.type->isFunction() or called.type->isError());

    FunctionType const* function_type = called.type->castToFunction();
    auto const function_parameter_types = function_type->parameterTypes();
    auto const function_parameter_count = function_parameter_types.size();
    bool const variadic = function_type->isVariadic();

    if (data.num_parameters < function_parameter_count) {
      error(called, "Too few parameters for function call.");
      return {error_literal, called.length_in_file, called.position_in_file};
    }

    if (data.num_parameters > function_parameter_count and not variadic) {
      error(called, "Too many parameters for function call.");
      return {error_literal, called.length_in_file, called.position_in_file};
    }

    auto i{0uz};
    for (; i<function_parameter_count; ++i) {
      auto const given_parameter_idx = instructions.size();
      auto given_parameter = peepExpression();
      auto const function_parameter_type = function_parameter_types[i];

      if (given_parameter.type not_eq function_parameter_type) {
        if (not given_parameter.type->coercibleTo(function_parameter_type)) {
          error(given_parameter, std::format("Cannot convert parameter of type {} to type {}.", given_parameter.type->toString(), function_parameter_type->toString()));
          return {error_literal, given_parameter.length_in_file, given_parameter.position_in_file};
        }

        if (not coerce_if_integerliteral(instructions[given_parameter_idx], function_parameter_type)) {
          Instruction cast{castForType(given_parameter.type), std::bit_cast<u64_t>(function_parameter_type)};
          instructions.insert(instructions.begin() + given_parameter_idx, cast);
        }
      }
    }

    if (variadic) {
      for (; i<data.num_parameters; ++i)
        (void)peepExpression();
    }

    return {{function_type->returnType(), {}}, data.length_in_file, data.position_in_file};
  }

  //TODO: Add Short Circuiting
  ExpressionResult
  peepBinaryExpression(ASTNode::BinaryData data) {
    auto const opr_idx = instructions.size(); instructions.emplace_back(Instruction::NOOP, 0);

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
        error(data,
          std::format("Right type {} in binary expression cannot coerce to left type {}.",
          right.type->toString(), left.type->toString()));

        left = {error_literal, data.length_in_file, data.position_in_file};
      }
    }

    switch (data.opr) { // first switch validates
    case Operator::ADD:
    case Operator::SUBTRACT:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::MODULUS:
      if (not arithmetic) {
        error(data, "Non-arithmetic expressions in arithmetic binary operation.");
        left = {error_literal, data.length_in_file, data.position_in_file};
        break;
      }

      left.qualifiers.is_mutable = false;
      break;

    case Operator::LESS:
    case Operator::GREATER:
    case Operator::LESS_EQUAL:
    case Operator::GREATER_EQUAL:
      if (not arithmetic) {
        error(data, "Non-arithmetic expressions in arithmetic binary operation.");
        left = {error_literal, data.length_in_file, data.position_in_file};
        break;
      }

      left = {bool_literal, data.length_in_file, data.position_in_file};
      break;

    case Operator::AND:
    case Operator::OR:
    case Operator::XOR:
      if (not left.type->isBool()) {
        error(data, "Non-boolean expressions in boolean binary operation.");
        left = {error_literal, data.length_in_file, data.position_in_file};
        break;
      }

      left = {bool_literal, data.length_in_file, data.position_in_file};
      break;

    case Operator::BITAND:
    case Operator::BITOR:
    case Operator::BITXOR:
      if (not arithmetic) {
        error(data, "Non-arithmetic expressions in bitwise operation.");
        left = {error_literal, data.length_in_file, data.position_in_file};
        break;
      }

      left.qualifiers.is_mutable = false;
      break;

    case Operator::ASSIGN:
      if (not left.qualifiers.is_mutable) {
        error(data, "Left expression in assignment non-mutable.");
        left = {error_literal, data.length_in_file, data.position_in_file};
      }
      break;

    case Operator::EQUAL:
    case Operator::NOT_EQUAL:
      left = {bool_literal, data.length_in_file, data.position_in_file};
      break;

    default:
      eden_unreachable("Invalid binary operator.");
    }

    auto& opr_instruction = instructions[opr_idx];
    switch (data.opr) { // second sets the right instruction
    case Operator::ADD:
      opr_instruction.type = left_float ? Instruction::FADD : Instruction::ADD;
      return left;
    case Operator::SUBTRACT:
      opr_instruction.type = left_float ? Instruction::FSUB : Instruction::SUB;
      return left;
    case Operator::MULTIPLY:
      opr_instruction.type = left_float ? Instruction::FMULT : Instruction::MULT;
      return left;
    case Operator::DIVIDE:
      opr_instruction.type =
      left_float ? Instruction::FDIV :
       left_signed ? Instruction::SDIV : Instruction::UDIV;
      return left;
    case Operator::MODULUS:
      opr_instruction.type =
      left_float ? Instruction::FMOD :
       left_signed ? Instruction::SMOD : Instruction::UMOD;
      return left;
    case Operator::LESS:
      opr_instruction.type =
      left_float ? Instruction::FLESS :
       left_signed ? Instruction::SLESS : Instruction::ULESS;
      return left;
    case Operator::GREATER:
      opr_instruction.type =
      left_float ? Instruction::FGTR :
       left_signed ? Instruction::SGTR : Instruction::UGTR;
      return left;
    case Operator::LESS_EQUAL:
      opr_instruction.type =
      left_float ? Instruction::FLEQ :
       left_signed ? Instruction::SLEQ : Instruction::ULEQ;
      return left;
    case Operator::GREATER_EQUAL:
      opr_instruction.type =
      left_float ? Instruction::FGEQ :
       left_signed ? Instruction::SGEQ : Instruction::UGEQ;
      return left;
    case Operator::AND:
      opr_instruction.type = Instruction::AND;
      return left;
    case Operator::OR:
      opr_instruction.type = Instruction::OR;
      return left;
    case Operator::XOR:
      opr_instruction.type = Instruction::NEQ;
      return left;
    case Operator::BITAND:
      opr_instruction.type = Instruction::BITAND;
      return left;
    case Operator::BITOR:
      opr_instruction.type = Instruction::BITOR;
      return left;
    case Operator::BITXOR:
      opr_instruction.type = Instruction::BITXOR;
      return left;
    case Operator::ASSIGN: {
      opr_instruction.type = Instruction::ASSIGN;
      adjustAssignExpression(opr_instruction, left.instantiated_type(), right.instantiated_type());
      return left;
    }
    case Operator::EQUAL:
      opr_instruction.type = Instruction::EQ;
      return left;
    case Operator::NOT_EQUAL:
      opr_instruction.type = Instruction::NEQ;
      return left;
    default:
      eden_unreachable("Invalid binary operator.");
    }

  }

  ExpressionResult
  peepUnaryExpression(ASTNode::UnaryData data) {
    if (data.opr == Operator::ADDRESS_OF) {
      instructions.emplace_back(Instruction::ADDRESS_OF, 0);
      auto const addressed = peepExpression();
      return {{module->getRawPointerType(addressed.instantiated_type()), {}}, data.length_in_file, data.position_in_file };
    }

    auto const instruction_idx = instructions.size();
    instructions.emplace_back(Instruction::NOOP, 0);

    auto expression = peepExpression();
    const bool float_opr = expression.type->isFloating();
    const bool arithmetic = expression.type->isArithmetic();
    switch (data.opr) {
    case Operator::PRE_INCREMENT:
    case Operator::PRE_DECREMENT:
      if (not expression.qualifiers.is_mutable) {
        error(expression, "Prefix operator used on non-mutable expression.");
        expression = {error_literal, expression.length_in_file, expression.position_in_file};
      }
      break;
    case Operator::UNARY_MINUS:
      if (not arithmetic) {
        error(expression, "Unary minus used on non-arithmetic expression.");
        expression = {error_literal, expression.length_in_file, expression.position_in_file};
        break;
      }
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::BITNOT:
      if (not arithmetic) {
        error(expression, "bitnot operator used non-arithmetic expression.");
        expression = {error_literal, expression.length_in_file, expression.position_in_file};
        break;
      }
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::NOT:
      if (not expression.type->isBool()) {
        error(expression, "not operator used on non-boolean expression.");
        expression = {error_literal, expression.length_in_file, expression.position_in_file};
        break;
      }
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::POST_INCREMENT:
    case Operator::POST_DECREMENT:
      if (not expression.qualifiers.is_mutable) {
        error(expression, "Postfix decrement operator used on non-mutable expression.");
        expression = {error_literal, expression.length_in_file, expression.position_in_file};
        break;
      }
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::ARROW:
      if (not expression.type->isPointer()) {
        error(expression, "Arrow operator used on non-pointer type.");
        expression = {error_literal, expression.length_in_file, expression.position_in_file};
        break;
      }

      expression = {expression.type->castToPointer()->getSubtype(), expression.length_in_file, expression.position_in_file};
      break;
    default:
      eden_unreachable("Invalid unary operator.");
    }

    auto& instruction = instructions[instruction_idx];
    switch (data.opr) {
    case Operator::PRE_INCREMENT:
      instruction.type = float_opr ? Instruction::FPRE_INC : Instruction::PRE_INC;
      return expression;
    case Operator::PRE_DECREMENT:
      instruction.type = float_opr ? Instruction::FPRE_DEC : Instruction::PRE_DEC;
      return expression;
    case Operator::UNARY_MINUS:
      instruction.type = float_opr ? Instruction::FNEGATE : Instruction::NEGATE;
      return expression;
    case Operator::BITNOT:
    case Operator::NOT:
      instruction.type = Instruction::BITNOT;
      return expression;
    case Operator::POST_INCREMENT:
      instruction.type = float_opr ? Instruction::FPOST_INC : Instruction::POST_INC;
      return expression;
    case Operator::POST_DECREMENT:
      instruction.type = float_opr ? Instruction::FPOST_DEC : Instruction::POST_DEC;
      return expression;
    case Operator::ARROW:
      instruction.type = Instruction::DEREFERENCE;
      instruction.value = std::bit_cast<u64_t>(expression.type);
      return expression;
    default:
      eden_unreachable("Invalid unary operator.");
    }

  }

  [[nodiscard]] ExpressionResult
  peepExpression() {
    auto const node = nodes.take();
    switch (node.m.type) { using enum ASTNode::NodeType;
    case MEMBER_ACCESS:    return peepMemberAccess(node.member_access_data);
    case UNARY:            return peepUnaryExpression(node.unary_data);
    case BINARY:           return peepBinaryExpression(node.binary_data);
    case CALLING:          return peepCallingExpression(node.calling_data);
    case IDENTIFIER:       return peepIdentifier(node.identifier_data);
    case CAST:             return peepCastExpression(node.cast_data);
    case SUBSCRIPT:        return peepSubscriptExpression(node.subscript_data);

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
      assign.type = right.type->isSignedIntegral() ? Instruction::SCAST_ASSIGN : Instruction::UCAST_ASSIGN;
      assign.value = left.type->bitwidth();
    }
  }

  void peepReturnStatement(bool return_has_value) {
    auto const return_type = current_function_type->returnType();
    if (not return_has_value) {
      if (not return_type->isDevoid()) {
        auto const empty_return = nodes.take();
        return error(empty_return,"Non-devoid function expects return value.");
      }

      current_block().set_ret();
      new_block();
      return;
    }

    auto const assign_idx = instructions.size();
    instructions.emplace_back(Instruction::ASSIGN, 0);
    instructions.emplace_back(Instruction::LOCAL, 0);

    auto const return_idx = instructions.size();
    auto const return_expression = peepExpression();
    if (return_type->isDevoid()) {
      error(return_expression,  "Cannot return value from devoid function.");
      return;
    }

    if (not return_expression.type->coercibleTo(return_type)) {
      error(return_expression, "Return statement's type is not compatible with function return type.");
      return;
    }

    coerce_if_integerliteral(instructions[return_idx], return_type);
    adjustAssignExpression(instructions[assign_idx], {return_type, {}}, return_expression.instantiated_type());
    current_block().set_ret();
  }

  void peepScopedStatement(u32_t num_children) {
    if (num_children == 0)
      return ;//(void)instructions.emplace_back(Instruction::NOOP, 0);

    while (num_children-- not_eq 0)
      peepStatement();
  }

  void peepWhileLoop(u32_t num_substatements) {
    br_fallthrough();
    new_block();
    auto const condition_idx = current_block_index();
    auto const condition = peepExpression();
    if (not condition.type->isBool()) {
      error(condition, "While Loop condition not boolean.");
      return;
    }

    auto const loop_body_idx = current_block_index() + 1;
    force_new_block();
    peepScopedStatement(num_substatements);
    if (is_current_block_empty())
      instructions.emplace_back(Instruction::NOOP, 0);
    current_block().set_br(condition_idx);
    force_new_block();
    auto const after_loop_idx = current_block_index();

    blocks[condition_idx].set_brc(loop_body_idx, after_loop_idx);
  }

  void peepIfStatement(u32_t num_substatements) {
    auto const condition = peepExpression();
    if (not condition.type->isBool()) {
      error(condition, "If statement condition not boolean.");
      return;
    }

    auto const condition_block_idx = current_block_index();
    auto const true_block_idx = condition_block_idx + 1;

    new_block(); //true block
    peepScopedStatement(num_substatements); //true statement(s)

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

  void peepVarDeclaration(ASTNode::DeclarationData data) {
    auto const declared = nodes.take();
    auto const declared_name = declared.identifier_val(*current_file);
    auto const type = declared.identifier_data.decl_type;
    auto const qualified_type = QualifiedType{type, data.qualifiers};
    locals.emplace_back(type);

    if (module->containsLocal(declared_name)) {
      error(declared, "Redefinition of symbol name in variable declaration.");
      if (data.has_init) (void)peepExpression();
      return;
    }

    if (not data.has_init) {
      module->addLocal(declared_name, qualified_type);
      return;
    }

    auto const assign_idx = instructions.size();
    instructions.emplace_back(Instruction::ASSIGN, 0);
    instructions.emplace_back(Instruction::LOCAL, locals.size() - 1);
    module->addLocal(declared_name, qualified_type);

    auto const init_expr_idx = instructions.size();
    auto const init_expr = peepExpression();
    if (not init_expr.type->coercibleTo(qualified_type.type)) {
      error(init_expr, std::format("Type {} is not coercible to type {}.", init_expr.type->toString(), qualified_type.type->toString()));
      return;
    }

    coerce_if_integerliteral(instructions[init_expr_idx], type);
    adjustAssignExpression(instructions[assign_idx], qualified_type, init_expr.instantiated_type());
  }

  void peepStatement() {
    auto const node = nodes.take();
    switch (node.m.type) { using enum ASTNode::NodeType;
    case EMPTY:
      assert(false);
    case DECLARATION:  return peepVarDeclaration(node.declaration_data);
    case IF:           return peepIfStatement(node.if_numstatements());
    case WHILE:        return peepWhileLoop(node.while_numstatements());
    case RETURN:       return peepReturnStatement(node.return_has_value());

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
    if (not return_type->isDevoid())
      instructions.emplace_back(Instruction::LOCAL, 0);
    else
      instructions.emplace_back(Instruction::NOOP, 0);

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
  static void peepFunctions(TU& peep_tu, std::vector<Parser::Function> const& parsed_functions) {
    Peeper peeper;
    peeper.module = peep_tu.module;
    peeper.imports = std::move(peep_tu.imports);

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

    peep_tu.imports = std::move(peeper.imports);
    peep_tu.has_errors = peeper.has_error;
  }

};

}

// printing functions
namespace {

void printPeepInstruction(Instruction instruction) {
  switch (instruction.type) { using enum Instruction::Type;
  case NOOP: return std::println("NOOP");
  case GLOBAL: return std::println("GLOBAL {}", instruction.global_name());
  case FUNCTION: return std::println("FUNCTION {}", instruction.function_name());

  case MODULE_GLOBAL: return std::println("MODULE_GLOBAL {}", instruction.module_global().nameof());
  case MODULE_FUNCTION: return std::println("MODULE_FUNCTION {}", instruction.module_function().nameof());
  case TYPE_VARIABLE: return std::println("TYPE_VARIABLE {}", instruction.type_variable().nameof());

  case LOCAL: return std::println("LOCAL {}", instruction.local_idx());

  case I8_LITERAL: return std::println("I8_LITERAL {}", instruction.int_value());
  case I16_LITERAL: return std::println("I16_LITERAL {}", instruction.int_value());
  case I32_LITERAL: return std::println("I32_LITERAL {}", instruction.int_value());
  case I64_LITERAL: return std::println("I64_LITERAL {}", instruction.int_value());
  case U8_LITERAL: return std::println("U8_LITERAL {}", instruction.uint_value());
  case U16_LITERAL: return std::println("U16_LITERAL {}", instruction.uint_value());
  case U32_LITERAL: return std::println("U32_LITERAL {}", instruction.uint_value());
  case U64_LITERAL: return std::println("U64_LITERAL {}", instruction.uint_value());
  case FLOAT_LITERAL: return std::println("FLOAT_LITERAL {}", instruction.float_value());
  case DOUBLE_LITERAL: return std::println("DOUBLE_LITERAL {}", instruction.double_value());
  case BOOL_LITERAL: return std::println("BOOL_LITERAL {}", instruction.bool_value());
  case CHAR_LITERAL: return std::println("CHAR_LITERAL {}", instruction.char_value());
  case STRING_LITERAL: return std::println("STRING_LITERAL {}", instruction.string_value());
  case ESCAPED_STRING_LITERAL: return std::println("STRING_LITERAL W/ ESCAPE_SEQUENCE {}", instruction.original_string());

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
  case UCAST_ASSIGN: return std::println("UCAST_ASSIGN {}b", instruction.cast_assign_bitwidth());
  case SCAST_ASSIGN: return std::println("SCAST_ASSIGN {}b", instruction.cast_assign_bitwidth());
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
  case SUBSCRIPT: return std::println("SUBSCRIPT WITH ARRAY TYPE {}", instruction.array_type()->toString());

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
  case DEREFERENCE: return std::println("DEREFERENCE TO {}", instruction.dereference_type()->toString());

  case UCAST: return std::println("UCAST TO {}", instruction.cast_type()->toString());
  case SCAST: return std::println("SCAST TO {}", instruction.cast_type()->toString());
  case FCAST: return std::println("FCAST TO {}", instruction.cast_type()->toString());
  case PCAST: return std::println("PCAST TO {}", instruction.cast_type()->toString());

  case CALL: return std::println("CALL WITH {} PARAMETER(S)", instruction.num_params());
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

void printPeepBlocks(std::vector<Block> const& blocks, std::vector<Instruction> const& instructions) {
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
    std::cout << "\t";
    printPeepInstruction(instructions[current_instruction]);
    ++current_instruction;
  }

  std::println("Return Block {}:", current_block);
  std::cout << "\t";
  printPeepInstruction(instructions[current_instruction++]);
  printPeepBlockTerminator(blocks[current_block]);
}

}

void PeepMIR::printPeep(TU const& tu) {
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

    printPeepBlocks(func.blocks, func.instructions);
    std::println("}}\n");
  }
}

TU PeepMIR::lowerToPeep(Parser::TU&& parsed_tu) {

#ifdef STAGE_BENCHMARKS
  auto begin_time = std::chrono::high_resolution_clock::now();
#endif

  TU tu;
  tu.source_files = std::move(parsed_tu.source_files);
  tu.module = parsed_tu.module;
  tu.functions.reserve(parsed_tu.functions.size());

  tu.imports.reserve(parsed_tu.imports.size() + 1);
  tu.imports.emplace_back(getModule("__C"));
  for (auto const import_name : parsed_tu.imports)
    tu.imports.emplace_back(getModule(import_name));

  Peeper::peepFunctions(tu, parsed_tu.functions);


#ifdef STAGE_BENCHMARKS
  auto end_time = std::chrono::high_resolution_clock::now();
  std::println("Peeping {}: {} | {} | {}",
    parsed_tu.module->nameof(),
    end_time - begin_time,
    std::chrono::duration_cast<std::chrono::microseconds>(end_time - begin_time),
    std::chrono::duration_cast<std::chrono::milliseconds>(end_time - begin_time)
  );
#endif
  return tu;
}