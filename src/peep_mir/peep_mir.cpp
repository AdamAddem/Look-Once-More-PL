#include "peep_mir.hpp"

#include "ast/ast.hpp"
#include "edenlib/typedefs.hpp"
#include "error.hpp"
#include "parsing/parse.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include "settings.hpp"

#include <cassert>
#include <format>
#include <iostream>

using namespace LOM;
using namespace LOM::PeepMIR;
using namespace LOM::AST;

namespace {


class TreeView {
  std::vector<ASTNode>::iterator begin;
  std::vector<ASTNode>::iterator end;
public:
  TreeView(std::vector<ASTNode>::iterator begin, std::vector<ASTNode>::iterator end) noexcept
  :begin(begin), end(end) {}

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

template <bool global_peeping>
class Peeper {
  SymbolTable& table;

  eden::releasing_vector<const Type*> locals;
  eden::releasing_vector<Instruction> instructions;
  eden::releasing_vector<Block> blocks;

  TreeView nodes;
  const FunctionType* current_function_type;
  u64_t current_line_number{};

  [[nodiscard]] constexpr Block&
  current_block() noexcept {return blocks.back();}

  [[nodiscard]] constexpr u32_t
  current_block_index() const noexcept {return blocks.size() - 1;}

  [[nodiscard]] constexpr bool
  is_current_block_empty() noexcept
  {return blocks.back().first_instruction_idx == instructions.size();}

  //creates a brc
  //true index will be the first block created by body
  //false index will be the block after body executes
  constexpr void
  brc_to_after(auto&& body) {
    const u32_t current_block_idx = current_block_index();
    const u32_t true_block_idx = current_block_idx + 1;
    body();
    const u32_t false_block_idx = blocks.size() - 1;
    blocks[current_block_idx].set_brc(true_block_idx, false_block_idx);
  }

  //creates a br that goes to the next block, as if it had fallen through
  //does nothing if current block is empty
  constexpr void
  br_fallthrough() {
    if (not is_current_block_empty())
      blocks.back().set_br(blocks.size());
  }

  //call before any instructions are made
  //does nothing if current block is empty
  constexpr void
  new_block() {
    if (not is_current_block_empty())
      blocks.emplace_back(instructions.size(), Block::Terminator::NONE);
  }

  constexpr void
  force_new_block() {
    blocks.emplace_back(instructions.size(), Block::Terminator::NONE);
  }

  InstantiatedType peepLiteral() {
    auto& node = nodes.pop();
    switch (node.type()) {
      using enum ASTNode::Type;
    case INTEGER_LITERAL:
      instructions.emplace_back(Instruction::UINT_LITERAL, node.value());
      return unsigned_literal;
    case SIGNED_LITERAL:
      instructions.emplace_back(Instruction::INT_LITERAL, node.value());
      return signedToLiteralInstance(node.signed_val());
    case UNSIGNED_LITERAL:
      instructions.emplace_back(Instruction::UINT_LITERAL, node.value());
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
      std::unreachable();
    }
  }

  InstantiatedType peepIdentifier(eden::releasing_string::released_ptr identifier) {
    if constexpr(not global_peeping) {
      if (table.containsLocalVariable(identifier.get())) {
        auto variable = table.localVariable(identifier.get());
        instructions.emplace_back(Instruction::LOCAL, variable.second + 1);
        identifier.destroy_and_deallocate();
        return variable.first;
      }

      if (table.containsFunction(identifier.get())) {
        instructions.emplace_back(Instruction::FUNCTION, std::bit_cast<u64_t>(identifier.get()));
        auto function_instance_type = table.instanceTypeOfFunction(identifier.get());
        return function_instance_type;
      }
    }

    if (table.containsGlobalVariable(identifier.get())) {
      auto variable = table.globalVariable(identifier.get());
      instructions.emplace_back(Instruction::GLOBAL, std::bit_cast<u64_t>(identifier.release()));
      return variable;
    }

    throw ValidationError("Undeclared Identifier.", identifier.get(), current_line_number);
  }

  InstantiatedType peepSubscriptExpression() const {assert(false); return {};}

  InstantiatedType peepCallingExpression(u64_t parameter_count) {
    assert(parameter_count <= Settings::MAX_FUNCTION_PARAMETERS);
    if constexpr (global_peeping)
      throw ValidationError("Function calls not allowed when initializing globals.", "Sorry!", current_line_number);

    instructions.emplace_back(Instruction::CALL, parameter_count);
    const auto called = peepExpression();
    if (not called.type->isCallable())
      throw ValidationError("Call operator used on non-callable.", std::format("Expression has type '{}'", called.toString()), current_line_number);
    if (not called.type->isFunction())
      throw ValidationError("Callable non-functions not implemented.", "Woopsie", current_line_number);

    const FunctionType* function_type = called.type->castToFunction();
    auto parameter_types = function_type->parameterTypes();
    for (auto i{0uz}; i<parameter_count; ++i) {
      const auto cast_idx = instructions.size(); instructions.emplace_back(Instruction::NOOP, 0);
      const auto parameter = peepExpression();
      if (not parameter.type->convertibleTo(parameter_types[i]))
        throw ValidationError("Cannot convert parameter to parameter type.",
          std::format("Parameter of type '{}' cannot convert to type '{}'", parameter.toString(), parameter_types[i]->toString()),
          current_line_number);

      const bool unsigned_cast = parameter.type->isUnsignedIntegral();
      const bool float_cast = parameter.type->isFloating();
      instructions[cast_idx].type =
      float_cast ? Instruction::FCAST :
      unsigned_cast ? Instruction::UCAST : Instruction::SCAST;
      instructions[cast_idx].value = parameter_types[i]->bitwidth();
    }

    const auto result = function_type->returnType();
    return {result, {}};
  }

  //TODO: Add Short Circuiting
  InstantiatedType peepBinaryExpression(Operator opr) {
    const auto instruction_idx = instructions.size(); instructions.emplace_back(Instruction::NOOP, 0);

    auto left = peepExpression();
    const auto cast_idx = instructions.size(); instructions.emplace_back(Instruction::NOOP, 0);
    const bool signed_opr = left.type->isSignedIntegral();
    const bool float_opr = left.type->isFloating();
    const auto right = peepExpression();
    const bool signed_cast = right.type->isSignedIntegral();
    const bool float_cast = right.type->isFloating();

    if (not right.type->convertibleTo(left.type))
      throw ValidationError("Binary operator used on differing types.",
        std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);

    if (left.type->isVariant()) //types should be same, so checking just one is sufficient
      throw ValidationError("Binary operator used on variant types.",
        std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);

    instructions[cast_idx].type =
    float_cast ? Instruction::FCAST :
    (signed_cast ? Instruction::SCAST : Instruction::UCAST);
    instructions[cast_idx].value = left.type->bitwidth();

    const bool arithmetic = left.type->isArithmetic();
    //first switch validates
    switch (opr) {
    case Operator::ADD:
    case Operator::SUBTRACT:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::MODULUS:
      if (not arithmetic)
        throw ValidationError("Non-arithmetic expression in arithmetic binary operation.",
          std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);
      left.details.is_mutable = false;
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
      left.details.is_mutable = false;
      break;

    case Operator::ASSIGN:
      if (not left.details.is_mutable)
        throw ValidationError("Left expression in assignment non-mutable.",
          std::format("Type of expression is '{}'", left.type->toString()), current_line_number);
      break;

    case Operator::EQUAL:
    case Operator::NOT_EQUAL:
      left = bool_literal;
      break;

    default:
      std::unreachable();
    }

    //second puts the right instruction(s)
    switch (opr) {
    case Operator::ADD:
      instructions[instruction_idx].type = float_opr ? Instruction::FADD : Instruction::ADD;
      return left;
    case Operator::SUBTRACT:
      instructions[instruction_idx].type = float_opr ? Instruction::FSUB : Instruction::SUB;
      return left;
    case Operator::MULTIPLY:
      instructions[instruction_idx].type = float_opr ? Instruction::FMULT : Instruction::MULT;
      return left;
    case Operator::DIVIDE:
      instructions[instruction_idx].type =
      float_opr ? Instruction::FDIV :
      signed_opr ? Instruction::SDIV : Instruction::UDIV;
      return left;
    case Operator::MODULUS:
      instructions[instruction_idx].type =
      float_opr ? Instruction::FMOD :
      signed_opr ? Instruction::SMOD : Instruction::UMOD;
      return left;
    case Operator::LESS:
      instructions[instruction_idx].type =
      float_opr ? Instruction::FLESS :
      signed_opr ? Instruction::SLESS : Instruction::ULESS;
      return left;
    case Operator::GREATER:
      instructions[instruction_idx].type =
      float_opr ? Instruction::FGTR :
      signed_opr ? Instruction::SGTR : Instruction::UGTR;
      return left;
    case Operator::LESS_EQUAL:
      instructions[instruction_idx].type =
      float_opr ? Instruction::FLEQ :
      signed_opr ? Instruction::SLEQ : Instruction::ULEQ;
      return left;
    case Operator::GREATER_EQUAL:
      instructions[instruction_idx].type =
      float_opr ? Instruction::FGEQ :
      signed_opr ? Instruction::SGEQ : Instruction::UGEQ;
      return left;
    case Operator::AND:
      instructions[instruction_idx].type = Instruction::AND;
      return left;
    case Operator::OR:
      instructions[instruction_idx].type = Instruction::OR;
      return left;
    case Operator::XOR:
      instructions[instruction_idx].type = Instruction::NEQ;
      return left;
    case Operator::BITAND:
      instructions[instruction_idx].type = Instruction::BITAND;
      return left;
    case Operator::BITOR:
      instructions[instruction_idx].type = Instruction::BITOR;
      return left;
    case Operator::BITXOR:
      instructions[instruction_idx].type = Instruction::BITXOR;
      return left;
    case Operator::ASSIGN:
      instructions[instruction_idx].type = Instruction::ASSIGN;
      return left;
    case Operator::EQUAL:
      instructions[instruction_idx].type = Instruction::EQ;
      return left;
    case Operator::NOT_EQUAL:
      instructions[instruction_idx].type = Instruction::NEQ;
      return left;
    default:
      std::unreachable();
    }

  }

  InstantiatedType peepUnaryExpression(const Operator opr) {
    const auto instruction_idx = instructions.size(); instructions.emplace_back(Instruction::NOOP, 0);

    auto expression = peepExpression();
    const bool float_opr = expression.type->isFloating();
    if (expression.type->isVariant())
      throw ValidationError("Unary operator used on variant type.", expression.toString(), current_line_number);

    const bool arithmetic = expression.type->isArithmetic();
    switch (opr) {
    case Operator::PRE_INCREMENT:
    case Operator::PRE_DECREMENT:
      if (not expression.details.is_mutable)
        throw ValidationError("Prefix operator used on non-mutable expression.", expression.toString(), current_line_number);
      break;
    case Operator::UNARY_MINUS:
      if (not arithmetic)
        throw ValidationError("Unary minus used on non-arithmetic expression.", expression.toString(), current_line_number);
      expression.details.is_mutable = false;
      break;
    case Operator::ADDRESS_OF:
      expression = {table.addRawPointer(expression), {}};
      break;
    case Operator::BITNOT:
      if (not arithmetic)
        throw ValidationError("bitnot operator used non-arithmetic expression", expression.toString(), current_line_number);
      expression.details.is_mutable = false;
      break;
    case Operator::NOT:
      if (not expression.type->isBool())
        throw ValidationError("not operator used non-boolean expression", expression.toString(), current_line_number);
      expression.details.is_mutable = false;
      break;
    case Operator::POST_INCREMENT:
    case Operator::POST_DECREMENT:
      if (not expression.details.is_mutable)
        throw ValidationError("Postfix decrement operator used on non-mutable expression.", expression.toString(), current_line_number);
      expression.details.is_mutable = false;
      break;

    default:
      std::unreachable();
    }

    switch (opr) {
    case Operator::PRE_INCREMENT:
      instructions[instruction_idx].type = float_opr ? Instruction::FPRE_INC : Instruction::PRE_INC;
      return expression;
    case Operator::PRE_DECREMENT:
      instructions[instruction_idx].type = float_opr ? Instruction::FPRE_DEC : Instruction::PRE_DEC;
      return expression;
    case Operator::UNARY_MINUS:
      instructions[instruction_idx].type = float_opr ? Instruction::FNEGATE : Instruction::NEGATE;
      return expression;
    case Operator::ADDRESS_OF:
      instructions[instruction_idx].type = Instruction::ADDRESS_OF;
      return expression;
    case Operator::BITNOT:
    case Operator::NOT:
      instructions[instruction_idx].type = Instruction::BITNOT;
      return expression;
    case Operator::POST_INCREMENT:
      instructions[instruction_idx].type = float_opr ? Instruction::FPOST_INC : Instruction::POST_INC;
      return expression;
    case Operator::POST_DECREMENT:
      instructions[instruction_idx].type = float_opr ? Instruction::FPOST_DEC : Instruction::POST_DEC;
      return expression;
    default:
      std::unreachable();
    }
  }

  [[nodiscard]] InstantiatedType
  peepExpression() {
    auto& node = nodes.pop();
    switch (node.type()) {
    using enum ASTNode::Type;
    case UNARY:
      return peepUnaryExpression(node.operator_val());
    case BINARY:
      return peepBinaryExpression(node.operator_val());
    case CALLING:
      return peepCallingExpression(node.parameter_count());
    case SUBSCRIPT:
      assert(false);
    case IDENTIFIER:
      return peepIdentifier(node.take_identifier());
    case INTEGER_LITERAL:
    case SIGNED_LITERAL:
    case UNSIGNED_LITERAL:
    case FLOAT_LITERAL:
    case DOUBLE_LITERAL:
    case BOOL_LITERAL:
    case CHAR_LITERAL:
    case STRING_LITERAL:
      nodes.put_back();
      return peepLiteral();

    default:
      std::unreachable();
    }
  }

  [[nodiscard]] InstantiatedType
  peepCondition() {
    instructions.emplace_back(Instruction::UCAST, 1);
    return peepExpression();
  }

  void peepReturnStatement() {
    const auto return_type = current_function_type->returnType();
    if (nodes.pop_if_empty()) {
      if (not return_type->isDevoid())
        throw ValidationError("Non-devoid function expects return value",
        std::format("Scope return type is '{}'", return_type->toString()), current_line_number);

      current_block().set_ret();
      new_block();
      return;
    }

    instructions.emplace_back(Instruction::ASSIGN, 0);
    instructions.emplace_back(Instruction::LOCAL, 0);

    const auto cast_idx = instructions.size(); instructions.emplace_back(Instruction::NOOP, 0);
    const auto return_expression = peepExpression();
    if (return_type->isDevoid())
      throw ValidationError("Cannot return value from devoid function",
      std::format("Return value type is '{}'", return_expression.toString()), current_line_number);

    if (not return_expression.type->convertibleTo(return_type))
      throw ValidationError("Return statement's type is not compatible with return type of scope.",
       std::format("Scope return type is '{}' and expression type is '{}'", return_type->toString(), return_expression.toString()),
       current_line_number);


    const auto bitwidth = return_type->bitwidth();
    if (return_expression.type->isFloating()) [[unlikely]]
      instructions[cast_idx].type = Instruction::FCAST;
    else if (return_expression.type->isSignedIntegral())
      instructions[cast_idx].type = Instruction::SCAST;
    else [[likely]]
      instructions[cast_idx].type = Instruction::UCAST;

    instructions[cast_idx].value = bitwidth;
    current_block().set_ret();
  }

  void peepScopedStatement(u64_t num_children) {
    if (num_children == 0) {
      return (void)instructions.emplace_back(Instruction::NOOP, current_line_number);
    }

    while (num_children-- not_eq 0)
      peepStatement();
  }

  void peepWhileLoop() {
    br_fallthrough();
    new_block();
    const u32_t condition_idx = current_block_index();
    const auto condition = peepCondition();
    if (not condition.type->isBool())
      throw ValidationError("While Loop condition non-boolean.", std::format("Condition is of type '{}'", condition.toString()), current_line_number);

    brc_to_after( [=, this] mutable {
      new_block();
      peepScopedStatement(nodes.pop_scoped());
      current_block().set_br(condition_idx);
      new_block();
    });
  }

  void peepForLoop() const {assert(false and "Not sure about for loop form yet");}

  void peepIfStatement() {
    const auto condition = peepCondition();
    if (not condition.type->isBool())
      throw ValidationError("If statement condition non-boolean.", std::format("Condition is of type '{}'", condition.toString()), current_line_number);

    const u32_t condition_block_idx = current_block_index();
    const u32_t true_block_idx = condition_block_idx + 1;

    new_block(); //true block
    const auto num_true_statements = nodes.pop_scoped();
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
    const auto declaration_type = nodes.pop().instance_type();
    auto name = nodes.pop().take_identifier();
    char* name_cstr = name.get();

    if constexpr (global_peeping) {
      if (table.containsGlobalVariable(name_cstr))
        throw ValidationError("Redefinition of global variable.", std::format("Symbol name: '{}'", name_cstr), current_line_number);
    }
    else {
      if (table.containsLocalVariable(name_cstr))
        throw ValidationError("Redefinition of symbol name in variable declaration.", std::format("Symbol name: '{}'", name_cstr), current_line_number);
    }

    if (nodes.pop_if_empty()) {
      if constexpr(global_peeping)
        throw ValidationError("Global variable may not be junk initialized.", name_cstr, current_line_number);
      else
        table.addLocalVariable(std::move(name), declaration_type);
      return;
    }

    instructions.emplace_back(Instruction::ASSIGN, 0);
    if constexpr(global_peeping) {
      instructions.emplace_back(Instruction::GLOBAL, std::bit_cast<u64_t>(name_cstr));
      table.addGlobalVariable(name, declaration_type);
    }
    else {
      instructions.emplace_back(Instruction::LOCAL, locals.size());
      table.addLocalVariable(std::move(name), declaration_type);
    }

    const auto cast_idx = instructions.size(); instructions.emplace_back(Instruction::NOOP, 0);
    const auto init_expr = peepExpression();
    if (not init_expr.type->convertibleTo(declaration_type.type))
      throw ValidationError("Variable initialization's type is not compatible with variable type.",
        std::format("Variable '{}' is of type '{}' and expression '{}' is of type '{}'.",
        name_cstr,  declaration_type.toString(), "PLACEHOLDER EXPRESSION STRING", init_expr.toString()), current_line_number);

    const auto bitwidth = declaration_type.type->bitwidth();
    if (init_expr.type->isFloating())
      instructions[cast_idx].type = Instruction::FCAST;
    else if (init_expr.type->isSignedIntegral())
      instructions[cast_idx].type = Instruction::SCAST;
    else
      instructions[cast_idx].type = Instruction::UCAST;

    instructions[cast_idx].value = bitwidth;
    locals.emplace_back(declaration_type.type);
  }

  void peepStatement() {
    auto& node = nodes.pop();
    if constexpr (global_peeping) {
      assert(node.type() == ASTNode::DECLARATION);
      return peepVarDeclaration();
    }

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

    case ASTNode::INTEGER_LITERAL:
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
      std::unreachable();
    }
  }

  void peepUntilEmpty() {
    assert(instructions.empty());
    assert(blocks.empty());
    const auto return_type = current_function_type->returnType();
    blocks.emplace_back(0);
    while (not nodes.empty())
      peepStatement();

    if (blocks.back().terminator_type == Block::Terminator::NONE) {
      //if (not return_type->isDevoid())
        //throw ValidationError("Function with non-devoid return type does not end in a return statement.", "PLACEHOLDER", current_line_number);

      br_fallthrough();
    }

    //set up return block
    new_block();
    if (not return_type->isDevoid()) {
      instructions.emplace_back(Instruction::LOCAL, 0);
    }
    else {
      instructions.emplace_back(Instruction::NOOP, 0);
    }
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
        std::unreachable();
      }

    }
  }

  Peeper(
    SyntaxTree& tree,
    SymbolTable& table,
    const FunctionType* function_type)
  : table(table), nodes{tree.nodes.begin(), tree.nodes.end()}, current_function_type(function_type) {
    locals.emplace_back(function_type->returnType());
    for (auto parameter_type : function_type->parameterTypes())
      locals.emplace_back(parameter_type);
  }

  Peeper(SyntaxTree& tree, SymbolTable& table)
  : table(table), nodes{tree.nodes.begin(), tree.nodes.end()}, current_function_type(nullptr) {}
public:

  static void peepFunction(TU& tu, Parser::Function &func, SymbolTable& table)
  requires (not global_peeping) {
    const auto function_type = table.enterFunctionScope(func.name.get());
    Peeper function_peeper(func.body, table, function_type);
    function_peeper.peepUntilEmpty();
    tu.functions.emplace_back(
      std::move(func.name), function_type,
      function_peeper.locals.release_span(),
      function_peeper.instructions.release(),
      function_peeper.blocks.release_span());

    table.leaveFunctionScope();
  }

  static void peepGlobals(TU& tu, SyntaxTree& global_tree, SymbolTable& table)
  requires global_peeping {
    assume_assert(tu.globals == nullptr);
    assume_assert(tu.global_instructions == nullptr);

    Peeper<true> global_peeper(global_tree, table);
    while (not global_peeper.nodes.empty())
      global_peeper.peepStatement();

    tu.globals = global_peeper.locals.release();
    tu.global_instructions = global_peeper.instructions.release_span();
  }
};

}

#include <print>
//printing functions
namespace {

void printPeepInstruction(Instruction instruction) {
  switch (instruction.type) {
    using enum Instruction::Type;
  case NOOP: return std::println("NOOP");
  case GLOBAL: return std::println("GLOBAL {}", instruction.global_name());
  case LOCAL: return std::println("LOCAL {}", instruction.local_idx());
  case FUNCTION: return std::println("FUNCTION {}", instruction.function_name());
  case INT_LITERAL: return std::println("INT_LITERAL {}", instruction.int_value());
  case UINT_LITERAL: return std::println("UINT_LITERAL {}", instruction.uint_value());
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
  case BITNOT: return std::println("BITNOT");
  case PRE_INC: return std::println("PRE_INC");
  case FPRE_INC: return std::println("FPRE_INC");
  case PRE_DEC: return std::println("PRE_DEC");
  case FPRE_DEC: return std::println("FPRE_DEC");
  case ADDRESS_OF: return std::println("ADDRESS_OF");
  case NEGATE: return std::println("NEGATE");
  case POST_INC: return std::println("POST_INC");
  case FPOST_INC: return std::println("FPOST_INC");
  case POST_DEC: return std::println("POST_DEC");
  case FPOST_DEC: return std::println("FPOST_DEC");

  case UCAST: return std::println("UCAST TO {}b", instruction.value);
  case SCAST: return std::println("SCAST TO {}b", instruction.value);
  case FCAST: return std::println("FCAST TO {}b", instruction.value);

  case CALL: return std::println("CALL");
  default:
    std::unreachable();
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
    std::unreachable();
  }
}

void printPeepBlocks(released_span<Block>& blocks, released_ptr<Instruction>& instructions) {
  const auto num_blocks = blocks.size();
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
  std::println("{} Block(s).", func.blocks.size() - 1);
  std::println("fn {}{} {{", func.name.get(), func.type->toString());

  std::cout << "Return Type: " << func.locals[0]->toString() << std::endl;
  std::cout << "| Locals: | ";
  auto num_locals = func.locals.size() - 1;
  for (auto i{0uz}; i<num_locals; ++i) {
    std::print("{}: {} | ", i + 1, func.locals[i + 1]->toString());
  }
  std::cout << std::endl;

  printPeepBlocks(func.blocks, func.instructions);
  std::cout << "\n}" << std::endl;
}

void printPeep(TU& tu) {
  for (auto& func : tu.functions) {
    printPeepFunction(func);
  }
}
}

using GlobalPeeper = Peeper<true>;
using FunctionPeeper = Peeper<false>;

TU PeepMIR::lowerToPeep(Parser::TU&& parsed_tu) {
  SymbolTable& table = parsed_tu.table;
  TU tu(table.takeTypeContext());
  tu.functions.reserve(parsed_tu.functions.size());

  GlobalPeeper::peepGlobals(tu, parsed_tu.global_tree, table);

  for (auto &func : parsed_tu.functions)
    FunctionPeeper::peepFunction(tu, func, table);

  if (Settings::doOutputValidation()) {
    std::cout << "Validation Passed!\n\n";
    std::quick_exit(0);
  }

  if (Settings::doOutputPeep()) {
    std::cout << "--- Global Body ---\n";
    for (const auto instruction : tu.global_instructions) {
      printPeepInstruction(instruction);
    }
    std::cout << "--- Global Body ---\n\n";

    printPeep(tu);
    std::quick_exit(0);
  }

  return tu;
}