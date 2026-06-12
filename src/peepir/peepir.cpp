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
 *  For now, globals will be parsed but not added to the symbol table or validated, and can't be used in anyway.
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

class TreeView {
  std::vector<ASTNode>::iterator begin;
  std::vector<ASTNode>::iterator end;
public:
  TreeView(std::vector<ASTNode>::iterator begin, std::vector<ASTNode>::iterator end) noexcept
  : begin(begin), end(end) {}

  [[nodiscard]] constexpr ASTNode&
  peek() const noexcept
  {return *begin;}

  [[nodiscard]] constexpr bool
  peek_is_empty() const noexcept
  {return begin->type() == ASTNode::EMPTY;}

  constexpr ASTNode&
  pop() noexcept
  {return *(begin++);}

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
  {assert(begin->type() == ASTNode::SCOPED); return (begin++)->sub_statements();}

  constexpr void
  put_back() noexcept
  {--begin;}

  [[nodiscard]] constexpr bool
  empty() const noexcept
  {return begin == end;}
};

class Peeper {
  Module* module;

  const SymbolTable* subscope;
  bool subscope_is_module{true};
  InstantiatedType::Qualifiers subscope_qualifiers;

  std::unordered_map<std::string_view, Module*>& imports;

  std::vector<const Type*> locals;
  std::vector<Instruction> instructions;
  std::vector<Block> blocks;

  TreeView nodes;
  const FunctionType* current_function_type;
  u64_t current_line_number{};

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
  force_new_block() noexcept {
    blocks.emplace_back(instructions.size(), Block::Terminator::NONE);
  }

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
    switch (node.type()) {
      using enum ASTNode::Type;
    case SIGNED_LITERAL:
      instructions.emplace_back(Instruction::I8_LITERAL, node.value());
      return signedToLiteralInstance(node.signed_val());
    case UNSIGNED_LITERAL:
      instructions.emplace_back(Instruction::U8_LITERAL, node.value());
      return unsignedToLiteralInstance(node.unsigned_val());
    case FLOAT_LITERAL:
      instructions.emplace_back(Instruction::FLOAT_LITERAL, node.value());
      return f32_literal;
    case DOUBLE_LITERAL:
      instructions.emplace_back(Instruction::DOUBLE_LITERAL, node.value());
      return f64_literal;
    case BOOL_LITERAL:
      instructions.emplace_back(Instruction::BOOL_LITERAL, node.value());
      return bool_literal;
    case CHAR_LITERAL:
      instructions.emplace_back(Instruction::CHAR_LITERAL, node.value());
      return char_literal;
    case STRING_LITERAL:
      instructions.emplace_back(Instruction::STRING_LITERAL, node.value());
      return string_literal;
    default:
      eden_unreachable("Invalid literal type.");
    }
  }

  InstantiatedType peepMemberAccess() {
    if (nodes.peek().type() == ASTNode::IDENTIFIER) {
      auto const possible_module = imports.find(nodes.peek().identifier());
      if (possible_module not_eq imports.end()) {
        nodes.pop();
        subscope = possible_module->second; subscope_is_module = true;
        auto const res = peepExpression();
        subscope = nullptr;
        return res;
      }
    }

    auto const object_expression = peepExpression();
    if (not object_expression.type->isCustom())
      throw ValidationError("Attempt to access member of non-custom type.", object_expression.toString(), current_line_number);

    subscope_is_module = false; subscope_qualifiers = object_expression.qualifiers;
    subscope = object_expression.type->castToCustom()->member_table();
    auto const res = peepExpression();
    subscope = nullptr;
    return res;
  }

  eden_nonull_args InstantiatedType
  peepIdentifier(char* identifier) {
    if (subscope) {
      if (auto const member_variable = subscope->getPublicVariable(identifier)) {
        assert(member_variable->get_id() not_eq u16_max);
        InstantiatedType res = member_variable->type;
        if (subscope_is_module)
          instructions.emplace_back(Instruction::MODULE_GLOBAL, static_cast<const Module*>(subscope), member_variable->get_id());
        else {
          instructions.emplace_back(Instruction::TYPE_VARIABLE, subscope, member_variable->get_id());
          res.qualifiers = subscope_qualifiers;
        }
        return res;
      }

      if (auto const member_function = subscope->getPublicFunction(identifier)) {
        assert(member_function->get_id() not_eq u16_max);
        if (subscope_is_module)
          instructions.emplace_back(Instruction::MODULE_FUNCTION, static_cast<const Module*>(subscope), member_function->get_id());
        else
          assert(false and "Unimplemented");
        return {member_function->type, {}};
      }

      const char* err_msg = subscope_is_module ? "Identifier is not a public member of module." : "Identifier is not a public member of type.";
      throw ValidationError(err_msg, identifier, current_line_number);
    }

    if (auto const variable = module->getLocal(identifier)) {
      instructions.emplace_back(Instruction::LOCAL, variable->second + 1); // adjust by 1 accounting for return type
      return variable->first;
    }

    if (auto const function = module->getFunction(identifier)) {
      instructions.emplace_back(Instruction::FUNCTION, std::bit_cast<u64_t>(identifier));
      return {function->type, {}};
    }

    throw ValidationError("Undeclared Identifier.", identifier, current_line_number);
  }

  InstantiatedType peepSubscriptExpression() const {assert(false); return {};}

  eden_nonull_args InstantiatedType
  peepCastExpression(const Type* cast_type) {
    auto const cast_idx = instructions.size();
    instructions.emplace_back(Instruction::NOOP, std::bit_cast<u64_t>(cast_type));
    const InstantiatedType res{cast_type, {}};

    auto const casted_expr = peepExpression();
    if (not casted_expr.type->castableTo(cast_type))
      throw ValidationError("Invalid cast.",
        std::format("Cast from {} to {}", casted_expr.type->toString(), cast_type->toString()),
        current_line_number);


    auto& cast_instruction = instructions[cast_idx];
    cast_instruction.type = castForType(casted_expr.type);
    return res;
  }

  InstantiatedType peepCallingExpression(u64_t call_parameter_count) {
    instructions.emplace_back(Instruction::CALL, call_parameter_count);
    auto const called = peepExpression();
    if (not called.type->isCallable())
      throw ValidationError("Call operator used on non-callable.", std::format("Expression has type '{}'", called.toString()), current_line_number);
    if (not called.type->isFunction())
      throw ValidationError("Callable non-functions not implemented.", "Woopsie", current_line_number);

    const FunctionType* function_type = called.type->castToFunction();
    auto const function_parameter_types = function_type->parameterTypes();
    auto const function_parameter_count = function_parameter_types.size();

    if (call_parameter_count < function_parameter_count)
      throw ValidationError("Too few parameters for function call.", "PLACEHOLDER", current_line_number);
    if (call_parameter_count > function_parameter_count and not function_type->isVariadic())
      throw ValidationError("Too many parameters for function call.", "PLACEHOLDER", current_line_number);

    auto i{0uz};
    for (; i<call_parameter_count; ++i) {
      auto const given_parameter_idx = instructions.size();
      auto const given_parameter = peepExpression();
      auto const function_parameter_type = function_parameter_types[i];

      if (given_parameter.type not_eq function_parameter_type) {
        if (not given_parameter.type->coercibleTo(function_parameter_type)) {
          throw ValidationError("Cannot convert parameter to parameter type.",
          std::format("Parameter of type '{}' cannot convert to type '{}'", given_parameter.toString(), function_parameter_type->toString()),
          current_line_number);
        }

        if (not coerce_if_integerliteral(instructions[given_parameter_idx], function_parameter_type)) {
          Instruction cast{castForType(given_parameter.type), std::bit_cast<u64_t>(function_parameter_type)};
          instructions.insert(instructions.begin() + given_parameter_idx, cast);
        }
      }
    }

    return {function_type->returnType(), {}};
  }

  //TODO: Add Short Circuiting
  InstantiatedType peepBinaryExpression(const Operator opr) {
    auto const opr_idx = instructions.size(); instructions.emplace_back(Instruction::NOOP, 0);

    auto const left_idx = instructions.size();
    auto left = peepExpression();

    const bool left_signed = left.type->isSignedIntegral();
    const bool left_float = left.type->isFloating();
    const bool arithmetic = left.type->isArithmetic();

    auto const right_idx = instructions.size();
    auto right = peepExpression();

    if (left.type not_eq right.type) {
      // if left or right is an integer literal, coerce its type to the other
      if (coerce_if_integerliteral(instructions[left_idx], right.type))
        left.type = right.type;
      else if (coerce_if_integerliteral(instructions[right_idx], left.type))
        right.type = left.type;
      else {
          throw ValidationError("Right type in binary expression incompatable with left type.",
            std::format("'{}' {} '{}'", left.type->toString(), operatorToString(opr), right.type->toString()), current_line_number);
      }
    }

    switch (opr) { //first switch validates
    case Operator::ADD:
    case Operator::SUBTRACT:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::MODULUS:
      if (not arithmetic)
        throw ValidationError("Non-arithmetic expression in arithmetic binary operation.",
          std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);
      left.qualifiers.is_mutable = false;
      break;

    case Operator::LESS:
    case Operator::GREATER:
    case Operator::LESS_EQUAL:
    case Operator::GREATER_EQUAL:
      if (not arithmetic)
        throw ValidationError("Non-arithmetic expression in arithmetic binary operation.",
          std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);
      left = bool_literal;
      break;

    case Operator::AND:
    case Operator::OR:
    case Operator::XOR:
      if (not left.type->isBool())
        throw ValidationError("Non-boolean expressions in boolean binary operation.",
          std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);
      left = bool_literal;
      break;

    case Operator::BITAND:
    case Operator::BITOR:
    case Operator::BITXOR:
      if (not arithmetic)
        throw ValidationError("Non-arithmetic expression(s) in bitwise operation.",
          std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);
      left.qualifiers.is_mutable = false;
      break;

    case Operator::ASSIGN:
      if (not left.qualifiers.is_mutable)
        throw ValidationError("Left expression in assignment non-mutable.",
          std::format("Type of expression is '{}'", left.type->toString()), current_line_number);
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
      if (not expression.qualifiers.is_mutable)
        throw ValidationError("Prefix operator used on non-mutable expression.", expression.toString(), current_line_number);
      break;
    case Operator::UNARY_MINUS:
      if (not arithmetic)
        throw ValidationError("Unary minus used on non-arithmetic expression.", expression.toString(), current_line_number);
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::BITNOT:
      if (not arithmetic)
        throw ValidationError("bitnot operator used non-arithmetic expression", expression.toString(), current_line_number);
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::NOT:
      if (not expression.type->isBool())
        throw ValidationError("not operator used non-boolean expression", expression.toString(), current_line_number);
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::POST_INCREMENT:
    case Operator::POST_DECREMENT:
      if (not expression.qualifiers.is_mutable)
        throw ValidationError("Postfix decrement operator used on non-mutable expression.", expression.toString(), current_line_number);
      expression.qualifiers.is_mutable = false;
      break;
    case Operator::ARROW:
      if (not expression.type->isPointer())
        throw ValidationError("Arrow operator used on non-pointer type.", expression.toString(), current_line_number);
      expression = expression.type->castToPointer()->getSubtype();
      break;
    default:
      eden_unreachable("Invalid unary operator.");
    }

    auto& instruction = instructions[instruction_idx];
    switch (opr) {
    case Operator::PRE_INCREMENT:
      instruction.type = float_opr ? Instruction::FPRE_INC : Instruction::PRE_INC;
      break;
    case Operator::PRE_DECREMENT:
      instruction.type = float_opr ? Instruction::FPRE_DEC : Instruction::PRE_DEC;
      break;
    case Operator::UNARY_MINUS:
      instruction.type = float_opr ? Instruction::FNEGATE : Instruction::NEGATE;
      break;
    case Operator::BITNOT:
    case Operator::NOT:
      instruction.type = Instruction::BITNOT;
      break;
    case Operator::POST_INCREMENT:
      instruction.type = float_opr ? Instruction::FPOST_INC : Instruction::POST_INC;
      break;
    case Operator::POST_DECREMENT:
      instruction.type = float_opr ? Instruction::FPOST_DEC : Instruction::POST_DEC;
      break;
    case Operator::ARROW:
      instruction.type = Instruction::DEREFERENCE;
      instruction.value = std::bit_cast<u64_t>(expression.type);
      break;
    default:
      eden_unreachable("Invalid unary operator.");
    }

    return expression;
  }


  [[nodiscard]] InstantiatedType
  peepExpression() {
    auto const& node = nodes.pop();

    switch (node.type()) {
    using enum ASTNode::Type;
    case MEMBER_ACCESS:
      return peepMemberAccess();
    case UNARY:
      return peepUnaryExpression(node.operator_val());
    case BINARY:
      return peepBinaryExpression(node.operator_val());
    case CALLING:
      return peepCallingExpression(node.parameter_count());
    case SUBSCRIPT:
      eden_unreachable("Subscript unimplemented.");
    case IDENTIFIER:
      return peepIdentifier(node.identifier());
    case CAST:
      return peepCastExpression(node.cast_type());

    case SIGNED_LITERAL:
    case UNSIGNED_LITERAL:
      nodes.put_back();
      return peepLiteral();

    case FLOAT_LITERAL:
    case DOUBLE_LITERAL:
    case BOOL_LITERAL:
    case CHAR_LITERAL:
    case STRING_LITERAL:
      nodes.put_back();
      return peepLiteral();

    default:
      eden_unreachable("Invalid ASTNode while peeping expression.");
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
      if (not return_type->isDevoid())
        throw ValidationError("Non-devoid function expects return value",
        std::format("Scope return type is '{}'", return_type->toString()), current_line_number);

      current_block().set_ret();
      new_block();
      return;
    }

    auto const assign_idx = instructions.size();
    instructions.emplace_back(Instruction::ASSIGN, 0);
    instructions.emplace_back(Instruction::LOCAL, 0);

    auto const return_expression = peepExpression();
    if (return_type->isDevoid())
      throw ValidationError("Cannot return value from devoid function",
      std::format("Return value type is '{}'", return_expression.toString()), current_line_number);

    if (not return_expression.type->coercibleTo(return_type))
      throw ValidationError("Return statement's type is not compatible with function return type.",
       std::format("Function return type is '{}' and expression is of type '{}'", return_type->toString(), return_expression.type->toString()),
       current_line_number);

    coerce_if_integerliteral(instructions.back(), return_type);
    adjustAssignExpression(instructions[assign_idx], {return_type, {}}, return_expression);
    current_block().set_ret();
  }

  void peepScopedStatement(u64_t num_children) {
    if (num_children == 0)
      return (void)instructions.emplace_back(Instruction::NOOP, current_line_number);

    while (num_children-- not_eq 0)
      peepStatement();
  }

  void peepWhileLoop() {
    br_fallthrough();
    new_block();
    const u32_t condition_idx = current_block_index();
    auto const condition = peepExpression();
    if (not condition.type->isBool())
      throw ValidationError("While Loop condition non-boolean.", std::format("Condition is of type '{}'", condition.toString()), current_line_number);

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

  void peepForLoop() const {assert(false and "Not sure about for loop form yet");}

  void peepIfStatement() {
    auto const condition = peepExpression();
    if (not condition.type->isBool())
      throw ValidationError("If statement condition non-boolean.", std::format("Condition is of type '{}'", condition.toString()), current_line_number);

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
    auto const name = nodes.pop().identifier();

    if (module->containsLocal(name))
      throw ValidationError("Redefinition of symbol name in variable declaration.", std::format("Symbol name: '{}'", name), current_line_number);

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
    if (not init_expr.type->coercibleTo(declaration_type.type))
      throw ValidationError("Variable initialization's type is not compatible with variable type.",
        std::format("Variable '{}' is of type '{}' and expression '{}' is of type '{}'.",
        name,  declaration_type.toString(), "PLACEHOLDER EXPRESSION STRING", init_expr.toString()), current_line_number);

    coerce_if_integerliteral(instructions[init_expr_idx], declaration_type.type);
    adjustAssignExpression(instructions[assign_idx], declaration_type, init_expr);
  }

  void peepStatement() {
    auto const& node = nodes.pop();
    switch (node.type()) {
    case ASTNode::EMPTY:
      assert(false);
    case ASTNode::DECLARATION:
      current_line_number = node.line_number();
      return peepVarDeclaration();
    case ASTNode::IF:
      current_line_number = node.line_number();
      return peepIfStatement();
    case ASTNode::FOR:
      current_line_number = node.line_number();
      return peepForLoop();
    case ASTNode::WHILE:
      current_line_number = node.line_number();
      return peepWhileLoop();
    case ASTNode::SCOPED:
      return peepScopedStatement(node.sub_statements());
    case ASTNode::RETURN:
      current_line_number = node.line_number();
      return peepReturnStatement();
    case ASTNode::EXPR_STMT:
      current_line_number = node.line_number();
      if (nodes.pop_if_empty())
        instructions.emplace_back(Instruction::NOOP, current_line_number);
      else
        (void)peepExpression();
      return;

    case ASTNode::UNARY:
    case ASTNode::BINARY:
    case ASTNode::CALLING:
    case ASTNode::SUBSCRIPT:
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

    default:
      eden_unreachable("Invalid ASTNode while peeping statement.");
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
      case Block::Terminator::BR:
        break;
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

  Peeper(
    TU& tu,
    SyntaxTree& tree,
    const FunctionType* function_type)
  : module(tu.table), imports(tu.imports),
    nodes{tree.nodes.begin(), tree.nodes.end()}, current_function_type(function_type) {
    locals.emplace_back(function_type->returnType());
    for (auto parameter_type : function_type->parameterTypes())
      locals.emplace_back(parameter_type);
  }

  Peeper(TU& tu, SyntaxTree& tree)
  : module(tu.table), imports(tu.imports),
    nodes{tree.nodes.begin(), tree.nodes.end()}, current_function_type(nullptr) {}
public:

  static void peepFunction(TU& tu, Parser::Function &func) {
    auto const function_type = tu.table->enterFunctionScope(func.name);
    Peeper function_peeper(tu, func.body, function_type);
    function_peeper.peepUntilEmpty();
    tu.functions.emplace_back(
      func.name, function_type,
      std::move(function_peeper.locals),
      std::move(function_peeper.instructions),
      std::move(function_peeper.blocks),
      func.is_public);

    tu.table->leaveFunctionScope();
  }

};

}

TU::TU(const Parser::TU& ptu) noexcept
: name(ptu.name), table(ptu.module) {
  for (auto import : ptu.imports)
    imports.emplace(std::pair(import, getModule(import)));
}

//printing functions
namespace {

void printPeepInstruction(Instruction instruction) {
  switch (instruction.type) {
    using enum Instruction::Type;
  case NOOP: return std::println("NOOP");
  case GLOBAL: return std::println("GLOBAL {}", instruction.global_name());
  case FUNCTION: return std::println("FUNCTION {}", instruction.function_name());

  case MODULE_GLOBAL: return std::println("MODULE_GLOBAL {}", instruction.module_global().nameof());
  case MODULE_FUNCTION: return std::println("MODULE_FUNCTION {}", instruction.module_function().nameof());
  case TYPE_VARIABLE: return std::println("TYPE_VARIABLE {}", instruction.type_variable().nameof());
  //case TYPE_FUNCTION: return std::println("TYPE_FUNCTION {}", instruction.type_function().nameof());

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
  switch (block.terminator_type) {
    using enum Block::Terminator;
  case BR:
    return std::println("\tBR {}", block.br.next_block_idx);
  case BRC:
    return std::println("\tBRC TRUE {}, FALSE {}", block.brc.true_block_idx, block.brc.false_block_idx);
  case RET:
    return std::println("\tRET");
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
  std::println("{}", func.is_public ? "pub " : "");
  std::println("fn {}{} {{", func.name, func.type->toString());

  std::println("Return Type: {}", func.locals[0]->toString());
  std::print("| Locals: | ");
  auto const num_locals = func.locals.size() - 1;
  for (auto i{0uz}; i<num_locals; ++i)
    std::print("{}: {} | ", i + 1, func.locals[i + 1]->toString());

  std::println();

  printPeepBlocks(func.blocks, func.instructions);
  std::println("}}");
}

}

void PeepMIR::printPeep(TU& tu) {
  /* std::println("--- Global ---");
  for (auto const global : tu.globals) {
    std::println("{}", global.name);
  }
  std::println("--- Globals ---\n"); */

  for (auto& func : tu.functions) {
    printPeepFunction(func);
  }
}

TU PeepMIR::lowerToPeep(Parser::TU&& parsed_tu) {
  TU tu(parsed_tu);
  tu.imports.emplace(std::pair(std::string_view("__C"), getModule("__C")));
  tu.functions.reserve(parsed_tu.functions.size());

  for (auto &func : parsed_tu.functions)
    Peeper::peepFunction(tu, func);

  return tu;
}