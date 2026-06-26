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

using namespace LOM;
using namespace LOM::PeepMIR;
using namespace LOM::AST;

/*
    For now, globals will be parsed but not added to the symbol table or validated, and can't be used in anyway.
*/

namespace {

eden_nonull_args
[[nodiscard]] constexpr Instruction::Type
castForType(const Type* type) noexcept {
  if (type->isPointer())
    return Instruction::PCAST;

  if (type->isPrimitive()) {
    auto const primitive = type->castToPrimitive();
    if (primitive->isString())
      return Instruction::PCAST;
    if (primitive->isFloating())
      return Instruction::FCAST;
    if (primitive->isIntegral()) {
     return
        primitive->isSignedIntegral()
        ? Instruction::SCAST : Instruction::UCAST;
    }
    if (primitive->isChar())
      return Instruction::UCAST;
  }
  eden_unreachable("This shouldn't happen!");
}

struct TreeView {
  std::vector<ASTNode>::const_iterator begin;
  std::vector<ASTNode>::const_iterator end;

  [[nodiscard]] constexpr ASTNode const&
  peek() const noexcept
  { return *begin; }

  [[nodiscard]] constexpr bool
  peek_is_empty() const noexcept
  { return begin->m.type == ASTNode::EMPTY; }

  constexpr ASTNode const&
  pop() noexcept
  { return *(begin++); }

  [[nodiscard]] constexpr bool
  pop_if_empty() noexcept {
    if (peek_is_empty()) {
      pop();
      return true;
    }
    return false;
  }

  [[nodiscard]] constexpr u64_t
  pop_scoped() noexcept
  {assert(begin->m.type == ASTNode::SCOPED); return (begin++)->sub_statements();}

  constexpr void
  put_back() noexcept
  {--begin;}

  [[nodiscard]] constexpr bool
  empty() const noexcept
  {return begin == end;}
};

class Peeper {
  Module* module;
  const File* current_file;

  eden::swap_vector<Module*> imports;

  std::vector<const Type*> locals;
  std::vector<Instruction> instructions;
  std::vector<Block> blocks;

  TreeView nodes;
  const FunctionType* current_function_type;

  [[nodiscard]] constexpr Block&
  current_block() noexcept
  { return blocks.back(); }

  [[nodiscard]] constexpr u32_t
  current_block_index() const noexcept
  { return blocks.size() - 1; }

  [[nodiscard]] constexpr bool
  is_current_block_empty() const noexcept
  { return blocks.back().first_instruction_idx == instructions.size(); }

  //creates a br that goes to the next block, as if it had fallen through (does not create next block)
  //does nothing if current block is empty
  constexpr void
  br_fallthrough() noexcept {
    if (not is_current_block_empty())
      blocks.back().set_br(blocks.size());
  }

  //call before any instructions are made
  //does nothing if current block is empty
  constexpr void
  new_block() noexcept {
    if (not is_current_block_empty())
      blocks.emplace_back(instructions.size(), Block::Terminator::NONE);
  }

  constexpr void
  force_new_block() noexcept
  { blocks.emplace_back(instructions.size(), Block::Terminator::NONE); }

  eden_nonull_args constexpr bool //returns whether coersion was successful
  coerce_if_integerliteral(Instruction& possible_literal, const Type* expected_type) const noexcept {
    if (not possible_literal.is_literal() or not expected_type->isIntegral()) return false;
    auto const expected_type_primitive = expected_type->castToPrimitive();
    auto const is_signed = expected_type_primitive->isSignedIntegral();
    possible_literal.adjust_literal(expected_type_primitive->bitwidth(), is_signed);
    return true;
  }

  InstantiatedType peepLiteral() {
    auto& node = nodes.pop();
    switch (node.m.type) { using enum ASTNode::Type;
    case SIGNED_LITERAL:
      instructions.emplace_back(Instruction::I8_LITERAL, std::bit_cast<u64_t>(node.signed_val())); // signed and unsigned sizes will be adjusted later
      return signedToLiteralInstance(node.signed_val());
    case UNSIGNED_LITERAL:
      instructions.emplace_back(Instruction::U8_LITERAL, node.unsigned_val());
      return unsignedToLiteralInstance(node.unsigned_val());
    case FLOAT_LITERAL:
      instructions.emplace_back(Instruction::FLOAT_LITERAL, static_cast<u64_t>(std::bit_cast<u32_t>(node.float_val())));
      return f32_literal;
    case DOUBLE_LITERAL:
      instructions.emplace_back(Instruction::DOUBLE_LITERAL, std::bit_cast<u64_t>(node.double_val()));
      return f64_literal;
    case BOOL_LITERAL:
      instructions.emplace_back(Instruction::BOOL_LITERAL, static_cast<u64_t>(node.bool_val()));
      return bool_literal;
    case CHAR_LITERAL:
      instructions.emplace_back(Instruction::CHAR_LITERAL, static_cast<u64_t>(node.char_val()));
      return char_literal;
    case STRING_LITERAL:
      instructions.emplace_back(Instruction::STRING_LITERAL, node.string_val(*current_file));
      return string_literal;
    default:
      eden_unreachable("Invalid literal type.");
    }
  }

  InstantiatedType peepMemberAccess() {
    auto const member_access_idx = instructions.size();
    instructions.emplace_back(Instruction::NOOP, 0).~Instruction(); // doing this to reuse the constructor

    if (nodes.peek().m.type == ASTNode::IDENTIFIER) {
      static constexpr auto module_search_predicate = [](const Module* element, std::string_view name) { return element->nameof() == name; };

      auto const possible_module = imports.search(module_search_predicate, nodes.peek().identifier(*current_file));
      if (possible_module == nullptr) goto not_module;

      auto const member_access_instruction = &instructions[member_access_idx];
      nodes.pop();
      auto const other_module = *possible_module;

      auto const identifier = std::string_view(nodes.pop().identifier(*current_file));
      if (auto const member_variable = other_module->getPublicVariable(identifier)) {
        assert(member_variable->get_id() not_eq u16_max);
        new (member_access_instruction) Instruction(Instruction::MODULE_GLOBAL, other_module, member_variable->get_id());
        return member_variable->type;
      }

      if (auto const member_function = other_module->getPublicFunction(identifier)) {
        assert(member_function->get_id() not_eq u16_max);
        new (member_access_instruction) Instruction(Instruction::MODULE_FUNCTION, other_module, member_function->get_id());
        return {member_function->type, {}};
      }

      report_error(*current_file, "Identifier is not a public member of module.");
      return error_literal;
    }

    not_module:
    auto const object_expression = peepExpression();
    if (not object_expression.type->isCustom()) {
      report_error(*current_file, "Attempt to access member of non-custom type.");
      return error_literal;
    }

    auto const member_access_instruction = &instructions[member_access_idx];
    auto const custom_type = object_expression.type->castToCustom();
    auto const member_table = custom_type->member_table();
    auto const identifier = std::string_view(nodes.pop().identifier(*current_file));

    if (auto const member_variable = member_table->getPublicVariable(identifier)) {
      assert(member_variable->get_id() not_eq u16_max);

      new (member_access_instruction) Instruction(Instruction::TYPE_VARIABLE, custom_type, member_variable->get_id());
      return {member_variable->type.type, object_expression.qualifiers};
    }

    if (auto const member_function = member_table->getPublicFunction(identifier)) {
      assert(false and "Unimplemented"); assert(member_function->get_id() not_eq u16_max); return {member_function->type, {}};
    }

    new (member_access_instruction) Instruction(Instruction::UCAST, 0); // this is only here to avoid the UB from double destruction once this throws

    report_error(*current_file, "Identifier is not a public member of type.");
    return error_literal;
  }

  InstantiatedType
  peepIdentifier(std::string_view identifier) {
    if (auto const variable = module->getLocal(identifier)) {
      instructions.emplace_back(Instruction::LOCAL, variable->second + 1); // adjust by 1 accounting for return type
      return variable->first;
    }

    if (auto const function = module->getFunction(identifier)) {
      instructions.emplace_back(Instruction::FUNCTION, identifier);
      return {function->type, {}};
    }

    report_error(*current_file, identifier, std::format("Undeclared identifier: {}.", identifier));
    instructions.emplace_back(Instruction::NOOP, 0);
    return error_literal;
  }

  [[maybe_unused]] InstantiatedType peepSubscriptExpression() const { assert(false); return {};}

  eden_nonull_args InstantiatedType
  peepCastExpression(const Type* cast_type) {
    auto const cast_idx = instructions.size();
    instructions.emplace_back(Instruction::NOOP, std::bit_cast<u64_t>(cast_type));

    auto const casted_expr = peepExpression();
    if (not casted_expr.type->castableTo(cast_type)) {
      report_error(*current_file, std::format("Invalid cast from {} to {}", casted_expr.type->toString(), cast_type->toString()));
      return error_literal;
    }

    auto& cast_instruction = instructions[cast_idx];
    cast_instruction.type = castForType(casted_expr.type);
    return {cast_type, {}};
  }

  InstantiatedType peepCallingExpression(u64_t call_parameter_count) {
    instructions.emplace_back(Instruction::CALL, call_parameter_count);
    auto const called = peepExpression();
    if (not called.type->isCallable()) {
      report_error(*current_file, "Call operator used on non-callable.");
      return error_literal;
    }

    if (not called.type->isFunction()) {
      if (called.type not_eq Type::error())
        report_error(*current_file, "Internal error: Callable non-functions unimplemented.");
      return error_literal;
    }

    const FunctionType* function_type = called.type->castToFunction();
    auto const function_parameter_types = function_type->parameterTypes();
    auto const function_parameter_count = function_parameter_types.size();
    bool const is_variadic = function_type->isVariadic();

    if (call_parameter_count < function_parameter_count) {
      report_error(*current_file, "Too few parameters for function call.");
      return error_literal;
    }

    if (call_parameter_count > function_parameter_count and not is_variadic) {
      report_error(*current_file, "Too many parameters for function call.");
      return error_literal;
    }

    auto i{0uz};
    for (; i<function_parameter_count; ++i) {
      auto const given_parameter_idx = instructions.size();
      auto const given_parameter = peepExpression();
      auto const function_parameter_type = function_parameter_types[i];

      if (given_parameter.type not_eq function_parameter_type) {
        if (not given_parameter.type->coercibleTo(function_parameter_type)) {
          report_error(*current_file, std::format("Cannot convert parameter of type {} to type {}.", given_parameter.toString(), function_parameter_type->toString()));
          return error_literal;
        }

        if (not coerce_if_integerliteral(instructions[given_parameter_idx], function_parameter_type)) {
          Instruction cast{castForType(given_parameter.type), std::bit_cast<u64_t>(function_parameter_type)};
          instructions.insert(instructions.begin() + given_parameter_idx, cast);
        }
      }
    }

    if (is_variadic) {
      for (; i<call_parameter_count; ++i)
        (void)peepExpression();
    }

    return {function_type->returnType(), {}};
  }

  //TODO: Add Short Circuiting
  InstantiatedType peepBinaryExpression(Operator opr) {
    auto const opr_idx = instructions.size(); instructions.emplace_back(Instruction::NOOP, 0);

    auto const left_idx = instructions.size();
    auto left = peepExpression();

    const bool left_signed = left.type->isSignedIntegral();
    const bool left_float = left.type->isFloating();
    const bool arithmetic = left.type->isArithmetic();

    auto const right_idx = instructions.size();
    auto right = peepExpression();

    if (left.type not_eq right.type) { // if left or right is an integer literal, coerce its type to the other
      if (coerce_if_integerliteral(instructions[left_idx], right.type))
        left.type = right.type;
      else if (coerce_if_integerliteral(instructions[right_idx], left.type))
        right.type = left.type;
      else {
        report_error(*current_file,
          std::format("Right type {} in binary expression incompatable with left type {}.", left.type->toString(), operatorToString(opr), right.type->toString()));
        left = error_literal;
      }
    }

    switch (opr) { // first switch validates
    case Operator::ADD:
    case Operator::SUBTRACT:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::MODULUS:
      if (not arithmetic) {
        report_error(*current_file, "Non-arithmetic expression(s) in arithmetic binary operation.");
        left = error_literal;
        break;
      }

      left.qualifiers.is_mutable = false;
      break;

    case Operator::LESS:
    case Operator::GREATER:
    case Operator::LESS_EQUAL:
    case Operator::GREATER_EQUAL:
      if (not arithmetic) {
        report_error(*current_file, "Non-arithmetic expression(s) in arithmetic binary operation.");
        left = error_literal;
        break;
      }

      left = bool_literal;
      break;

    case Operator::AND:
    case Operator::OR:
    case Operator::XOR:
      if (not left.type->isBool()) {
        report_error(*current_file, "Non-boolean expression(s) in boolean binary operation.");
        left = error_literal;
        break;
      }

      left = bool_literal;
      break;

    case Operator::BITAND:
    case Operator::BITOR:
    case Operator::BITXOR:
      if (not arithmetic) {
        report_error(*current_file, "Non-arithmetic expression(s) in bitwise operation.");
        left = error_literal;
        break;
      }

      left.qualifiers.is_mutable = false;
      break;

    case Operator::ASSIGN:
      if (not left.qualifiers.is_mutable) {
        report_error(*current_file, "Left expression in assignment non-mutable.");
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

    auto& opr_instruction = instructions[opr_idx];
    switch (opr) { //second sets the right instruction
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
      adjustAssignExpression(opr_instruction, left, right);
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

  InstantiatedType peepUnaryExpression(const Operator opr) {
    if (opr == Operator::ADDRESS_OF) {
      instructions.emplace_back(Instruction::ADDRESS_OF, 0);
      return {module->getRawPointerType(peepExpression()), {}};
    }

    auto const instruction_idx = instructions.size();
    instructions.emplace_back(Instruction::NOOP, 0);

    auto expression = peepExpression();
    const bool float_opr = expression.type->isFloating();
    const bool arithmetic = expression.type->isArithmetic();
    switch (opr) {
    case Operator::PRE_INCREMENT:
    case Operator::PRE_DECREMENT:
      if (not expression.qualifiers.is_mutable) {
        report_error(*current_file, "Prefix operator used on non-mutable expression.");
        expression = error_literal;
      }
      break;
    case Operator::UNARY_MINUS:
      if (not arithmetic) {
        report_error(*current_file, "Unary minus used on non-arithmetic expression.");
        expression = error_literal;
        break;
      }
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::BITNOT:
      if (not arithmetic) {
        report_error(*current_file, "bitnot operator used non-arithmetic expression.");
        expression = error_literal;
        break;
      }
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::NOT:
      if (not expression.type->isBool()) {
        report_error(*current_file, "not operator used on non-boolean expression.");
        expression = error_literal;
        break;
      }
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::POST_INCREMENT:
    case Operator::POST_DECREMENT:
      if (not expression.qualifiers.is_mutable) {
        report_error(*current_file, "Postfix decrement operator used on non-mutable expression.");
        expression = error_literal;
        break;
      }
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::ARROW:
      if (not expression.type->isPointer()) {
        report_error(*current_file, "Arrow operator used on non-pointer type.");
        expression = error_literal;
        break;
      }
      expression = expression.type->castToPointer()->getSubtype();
      break;
    default:
      eden_unreachable("Invalid unary operator.");
    }

    auto& instruction = instructions[instruction_idx];
    switch (opr) {
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


  [[nodiscard]] InstantiatedType
  peepExpression() {
    auto const& node = nodes.pop();

    switch (node.m.type) {
    using enum ASTNode::Type;
    case MEMBER_ACCESS:    return peepMemberAccess();
    case UNARY:            return peepUnaryExpression(node.operator_val());
    case BINARY:           return peepBinaryExpression(node.operator_val());
    case CALLING:          return peepCallingExpression(node.parameter_count());
    case IDENTIFIER:       return peepIdentifier(node.identifier(*current_file));
    case CAST:             return peepCastExpression(node.cast_type());

    case SIGNED_LITERAL:
    case UNSIGNED_LITERAL: nodes.put_back(); return peepLiteral();

    case FLOAT_LITERAL:
    case DOUBLE_LITERAL:
    case BOOL_LITERAL:
    case CHAR_LITERAL:
    case STRING_LITERAL:   nodes.put_back(); return peepLiteral();

    default: eden_unreachable("Invalid ASTNode while peeping expression.");
    }
  }

  void adjustAssignExpression(Instruction& assign, InstantiatedType left, InstantiatedType right) const noexcept {
    if (left.type not_eq right.type and not left.type->isPointer()) {
      assign.type = right.type->isSignedIntegral() ? Instruction::SCAST_ASSIGN : Instruction::UCAST_ASSIGN;
      assign.value = left.type->bitwidth();
    }
  }

  void peepReturnStatement() {
    auto const return_type = current_function_type->returnType();
    if (nodes.pop_if_empty()) {
      if (not return_type->isDevoid()) {
        report_error(*current_file, "Non-devoid function expects return value.");
        return;
      }

      current_block().set_ret();
      new_block();
      return;
    }

    auto const assign_idx = instructions.size();
    instructions.emplace_back(Instruction::ASSIGN, 0);
    instructions.emplace_back(Instruction::LOCAL, 0);

    auto const return_expression = peepExpression();
    if (return_type->isDevoid()) {
      report_error(*current_file, "Cannot return value from devoid function");
      return;
    }


    if (not return_expression.type->coercibleTo(return_type)) {
      report_error(*current_file, "Return statement's type is not compatible with function return type.");
      return;
    }

    coerce_if_integerliteral(instructions.back(), return_type);
    adjustAssignExpression(instructions[assign_idx], {return_type, {}}, return_expression);
    current_block().set_ret();
  }

  void peepScopedStatement(u64_t num_children) {
    if (num_children == 0)
      return (void)instructions.emplace_back(Instruction::NOOP, 0);

    while (num_children-- not_eq 0)
      peepStatement();
  }

  void peepWhileLoop() {
    br_fallthrough();
    new_block();
    const u32_t condition_idx = current_block_index();
    auto const condition = peepExpression();
    if (not condition.type->isBool()) {
      report_error(*current_file, "While Loop condition not boolean.");
      return;
    }

    auto const loop_body_idx = current_block_index() + 1;
    force_new_block();
    peepScopedStatement(nodes.pop_scoped());
    if (is_current_block_empty())
      instructions.emplace_back(Instruction::NOOP, 0);
    current_block().set_br(condition_idx);
    force_new_block();
    auto const after_loop_idx = current_block_index();

    blocks[condition_idx].set_brc(loop_body_idx, after_loop_idx);
  }

  void peepIfStatement() {
    auto const condition = peepExpression();
    if (not condition.type->isBool()) {
      report_error(*current_file, "If statement condition not boolean.");
      return;
    }

    const u32_t condition_block_idx = current_block_index();
    const u32_t true_block_idx = condition_block_idx + 1;

    new_block(); //true block
    auto const num_true_statements = nodes.pop_scoped();
    peepScopedStatement(num_true_statements); //true statement(s)

    new_block(); //false block
    const u32_t false_block_idx = current_block_index();
    u32_t after_block_idx = false_block_idx;
    if (not nodes.pop_if_empty()) {
      peepStatement();
      blocks.back().set_br(blocks.size());
      new_block();
      after_block_idx = current_block_index();
    }
    blocks[true_block_idx].set_br(after_block_idx);
    blocks[condition_block_idx].set_brc(true_block_idx, false_block_idx);
  }

  void peepVarDeclaration() {
    auto const declaration_type = nodes.pop().instance_type();
    locals.emplace_back(declaration_type.type);
    auto const name = nodes.pop().identifier(*current_file);

    if (module->containsLocal(name)) {
      report_error(*current_file, "Redefinition of symbol name in variable declaration.");
      return;
    }

    if (nodes.pop_if_empty()) {
      module->addLocal(name, declaration_type);
      return;
    }

    auto const assign_idx = instructions.size();
    instructions.emplace_back(Instruction::ASSIGN, 0);
    instructions.emplace_back(Instruction::LOCAL, locals.size() - 1);
    module->addLocal(name, declaration_type);

    auto const init_expr_idx = instructions.size();
    auto const init_expr = peepExpression();
    if (not init_expr.type->coercibleTo(declaration_type.type)) {
      report_error(*current_file, "Variable initialization's type is not compatible with variable type.");
      return;
    }

    coerce_if_integerliteral(instructions[init_expr_idx], declaration_type.type);
    adjustAssignExpression(instructions[assign_idx], declaration_type, init_expr);
  }

  void peepStatement() {
    auto const& node = nodes.pop();
    switch (node.m.type) {
    case ASTNode::EMPTY:
      assert(false);
    case ASTNode::DECLARATION:  return peepVarDeclaration();
    case ASTNode::IF:           return peepIfStatement();
    case ASTNode::WHILE:        return peepWhileLoop();
    case ASTNode::SCOPED:       return peepScopedStatement(node.sub_statements());
    case ASTNode::RETURN:       return peepReturnStatement();
    case ASTNode::EXPR_STMT:
      if (nodes.pop_if_empty())
        instructions.emplace_back(Instruction::NOOP, 0);
      else
        (void)peepExpression();
      return;

    case ASTNode::UNARY:
    case ASTNode::BINARY:
    case ASTNode::CALLING:
    case ASTNode::IDENTIFIER:
      nodes.put_back();
      return (void)peepExpression();

    case ASTNode::SIGNED_LITERAL:
    case ASTNode::UNSIGNED_LITERAL:
    case ASTNode::FLOAT_LITERAL:
    case ASTNode::DOUBLE_LITERAL:
    case ASTNode::BOOL_LITERAL:
    case ASTNode::CHAR_LITERAL:
    case ASTNode::STRING_LITERAL:
      nodes.put_back();
      return (void)peepLiteral();

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

  // parses functions and fills tu.functions
  static void peepFunctions(TU& tu, std::vector<Parser::Function> const& functions) {
    Peeper peeper;
    peeper.module = tu.module;
    peeper.imports = std::move(tu.imports);

    for (auto const& func : functions) {
      auto const function_name = func.nameof();
      auto const function_type = tu.module->enterFunctionScope(function_name);
      peeper.current_file = &(tu.source_files[func.file_idx]);
      peeper.nodes.begin = func.body.cbegin();
      peeper.nodes.end = func.body.cend();
      peeper.current_function_type = function_type;
      peeper.locals.emplace_back(function_type->returnType());
      for (auto const parameter_type : function_type->parameterTypes())
        peeper.locals.emplace_back(parameter_type);

      peeper.peepUntilEmpty();
      tu.functions.emplace_back(
        function_name, function_type,
        std::move(peeper.locals),
        std::move(peeper.instructions),
        std::move(peeper.blocks));
    }

    tu.imports = std::move(peeper.imports);
  }

};

}



//printing functions
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

void printPeepFunction(Function& func) {
  std::println("{}fn {}{} {{",
    func.is_public ? "pub " : "",
    func.name,
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

void PeepMIR::printPeep(TU& tu) {
  for (auto& func : tu.functions) {
    printPeepFunction(func);
  }
}



TU PeepMIR::lowerToPeep(Parser::TU&& parsed_tu) {
  TU tu;
  tu.source_files = std::move(parsed_tu.source_files);
  tu.module = parsed_tu.module;
  tu.imports.reserve(parsed_tu.imports.size() + 1);
  for (auto const import_name : parsed_tu.imports)
    tu.imports.emplace_back(getModule(import_name));
  tu.imports.emplace_back(getModule("__C"));
  tu.functions.reserve(parsed_tu.functions.size());

  Peeper::peepFunctions(tu, parsed_tu.functions);
  return tu;
}