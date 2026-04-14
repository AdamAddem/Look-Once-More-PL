#include "peep_mir.hpp"
#include "peep_mir/peep_mir.hpp"

#include "ast/ast.hpp"
#include "edenlib/typedefs.hpp"
#include "error.hpp"
#include "parsing/parse.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include "settings.hpp"

#include <cassert>
#include <format>
#include <iostream>
#include <variant>

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
  peek_is(ASTNode::Type type) const noexcept
  {return begin->type() == type;}

  constexpr ASTNode&
  pop() noexcept
  {return *(begin++);}

  [[nodiscard]] constexpr bool
  pop_if(ASTNode::Type type) noexcept {
    if (peek_is(type)) {
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

struct Peeper {
  std::vector<TU::Global>& globals;
  std::vector<Function>& functions;
  eden::releasing_vector<const Type*> locals;
  eden::releasing_vector<Instruction> instructions;
  eden::releasing_vector<Block> blocks;

  TreeView nodes;
  SymbolTable& table;
  u64_t current_line_number{};

  Peeper(
    std::vector<TU::Global>& globals,
    std::vector<Function>& functions,
    SyntaxTree& tree,
    SymbolTable& table)
  : globals(globals), functions(functions),
    nodes{tree.nodes.begin(), tree.nodes.end()}, table(table) {}

  [[nodiscard]] constexpr Block&
  current_block() noexcept {return blocks.back();}

  [[nodiscard]] constexpr u32_t
  current_block_index() const noexcept {return blocks.size();}

  [[nodiscard]] constexpr bool
  is_current_block_empty() noexcept
  {return blocks.back().first_instruction_idx == instructions.size();}

  //creates a brc
  //true index will be the block represented by body
  //false index will be the block after body executes
  constexpr void
  brc_to_after(auto&& body) noexcept {
    const u32_t current_block_idx = current_block_index();
    const u32_t true_block_idx = current_block_idx + 1;
    body();
    const u32_t false_block_idx = blocks.size();
    blocks[current_block_idx].set_brc(true_block_idx, false_block_idx);
  }

  //creates a br to the block after body
  constexpr void
  br_to_after(auto&& body) noexcept {
    const u32_t current_block_idx = current_block_index();
    body();
    blocks[current_block_idx].set_br(blocks.size());
  }

  //creates a br that goes to the next block, as if it had fallen through
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
      blocks.emplace_back(instructions.size());
  }

  InstantiatedType peepLiteral() {
    auto& node = nodes.pop();
    switch (node.type()) {
    case ASTNode::INT_LITERAL:
      return signedToLiteralInstance(node.int_val());
    case ASTNode::UINT_LITERAL:
      return unsignedToLiteralInstance(node.uint_val());
    case ASTNode::FLOAT_LITERAL:
      return f32_literal;
    case ASTNode::DOUBLE_LITERAL:
      return f64_literal;
    case ASTNode::BOOL_LITERAL:
      return bool_literal;
    case ASTNode::CHAR_LITERAL:
      return char_literal;
    case ASTNode::STRING_LITERAL:
      return string_literal;

    default:
      std::unreachable();
    }
  }

  InstantiatedType peepIdentifier(const char* identifier) {
    if (table.containsFunction(identifier)) {
      instructions.emplace_back(Instruction::FUNCTION, 0);
      return table.instanceTypeOfFunction(identifier);
    }

    if (table.containsLocalVariable(identifier)) {
      auto variable = table.localVariable(identifier);
      locals.emplace_back(variable.type);
      instructions.emplace_back(Instruction::LOCAL, 0);
      return variable;
    }

    if (table.containsGlobalVariable(identifier)) {
      auto variable = table.globalVariable(identifier);
      globals.emplace_back(variable.type);
      instructions.emplace_back(Instruction::GLOBAL, 0);
      return variable;
    }

    throw ValidationError("Undeclared Identifier.", identifier, current_line_number);
  }

  InstantiatedType peepSubscriptExpression() {assert(false);}

  InstantiatedType peepCallingExpression(u64_t parameter_count) {
    assert(parameter_count <= Settings::MAX_FUNCTION_PARAMETERS);
    const auto called = peepExpression();
    if (not called.type->isCallable())
      throw ValidationError("Call operator used on non-callable.", std::format("Expression has type '{}'", called.toString()), current_line_number);
    if (not called.type->isFunction())
      throw ValidationError("Callable non-functions not implemented.", "Woopsie", current_line_number);

    const FunctionType* function_type = called.type->castToFunction();
    auto parameter_types = function_type->parameterTypes();
    for (auto i{0uz}; i<parameter_count; ++i) {
      const auto parameter = peepExpression();
      if (not parameter.type->convertibleTo(parameter_types[i]))
        throw ValidationError("Cannot convert parameter to parameter type.",
          std::format("Parameter of type '{}' cannot convert to type '{}'", parameter.toString(), parameter_types[i]->toString()),
          current_line_number);
    }

    instructions.emplace_back(Instruction::CALL, parameter_count);
    const auto result = function_type->returnType();
    return {result, {}};
  }

  InstantiatedType peepBinaryExpression(Operator opr) {
    auto left = peepExpression();
    const bool left_mutable = left.details.is_mutable;
    auto right = peepExpression();

    if (not left.type->convertibleTo(right.type) and not right.type->convertibleTo(left.type))
      throw ValidationError("Binary operator used on differing types.", std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);

    if (left.type->isVariant()) //types should be same, so checking just one is sufficient
      throw ValidationError("Binary operator used on variant types.", std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);

    const bool arithmetic = left.type->isArithmetic();
    switch (opr) {
    case Operator::ADD:
    case Operator::SUBTRACT:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::MODULUS:
    case Operator::LESS:
    case Operator::GREATER:
    case Operator::LESS_EQUAL:
    case Operator::GREATER_EQUAL:
      if (not arithmetic)
        throw ValidationError("Non-arithmetic expression in arithmetic binary operation.",
          std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);
      left.details.is_mutable = false;
      break;

    case Operator::AND:
    case Operator::OR:
    case Operator::XOR:
      if (not left.type->isBool())
        throw ValidationError("Non-boolean expressions in boolean binary operation.",
          std::format("'{}' and '{}'", left.type->toString(), right.type->toString()), current_line_number);
      left.details.is_mutable = false;
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
      if (not left_mutable)
        throw ValidationError("Left expression in assignment non-mutable.",
          std::format("Type of expression is '{}'", left.type->toString()), current_line_number);
      break;

    case Operator::EQUAL:
    case Operator::NOT_EQUAL:
      left.details.is_mutable = false;
      break;

    default:
      assert(false);
    }

    switch (opr) {
    case Operator::ADD:
      return instructions.emplace_back(Instruction::ADD, 0), left;
    case Operator::SUBTRACT:
      return instructions.emplace_back(Instruction::SUB, 0), left;
    case Operator::MULTIPLY:
      return instructions.emplace_back(Instruction::MULT, 0), left;
    case Operator::DIVIDE:
      return instructions.emplace_back(Instruction::DIV, 0), left;
    case Operator::MODULUS:
      return instructions.emplace_back(Instruction::ADD, 0), left;

    case Operator::LESS:
      return instructions.emplace_back(Instruction::LESS, 0), bool_literal;
    case Operator::GREATER:
      return instructions.emplace_back(Instruction::GTR, 0), bool_literal;
    case Operator::LESS_EQUAL:
      return instructions.emplace_back(Instruction::LEQ, 0), bool_literal;
    case Operator::GREATER_EQUAL:
      return instructions.emplace_back(Instruction::GEQ, 0), bool_literal;
    case Operator::AND:
      return instructions.emplace_back(Instruction::AND, 0), bool_literal;
    case Operator::OR:
      return instructions.emplace_back(Instruction::OR, 0), bool_literal;
    case Operator::XOR:
      return instructions.emplace_back(Instruction::NEQ, 0), bool_literal;

    case Operator::BITAND:
      return instructions.emplace_back(Instruction::BITAND, 0), bool_literal;
    case Operator::BITOR:
      return instructions.emplace_back(Instruction::BITOR, 0), bool_literal;
    case Operator::BITXOR:
      return instructions.emplace_back(Instruction::BITXOR, 0), bool_literal;

    case Operator::ASSIGN:
      return instructions.emplace_back(Instruction::ASSIGN, 0), left;

    case Operator::EQUAL:
      return instructions.emplace_back(Instruction::EQ, 0), bool_literal;
    case Operator::NOT_EQUAL:
      return instructions.emplace_back(Instruction::NEQ, 0), bool_literal;

    default:
      std::unreachable();
    }
  }

  InstantiatedType peepUnaryExpression(Operator opr) {
    auto expression = peepExpression();
    if (expression.type->isVariant())
      throw ValidationError("Unary operator used on variant type.", expression.toString(), current_line_number);

    const bool arithmetic = expression.type->isArithmetic();
    switch (opr) {
    case Operator::PRE_INCREMENT:
      if (not expression.details.is_mutable)
        throw ValidationError("Prefix operator used on non-mutable expression.", expression.toString(), current_line_number);
      return instructions.emplace_back(Instruction::PRE_INC, 0), expression;
    case Operator::PRE_DECREMENT:
      if (not expression.details.is_mutable)
        throw ValidationError("Prefix operator used on non-mutable expression.", expression.toString(), current_line_number);
      return instructions.emplace_back(Instruction::PRE_DEC, 0), expression;

    case Operator::UNARY_MINUS:
      if (not arithmetic)
        throw ValidationError("Unary minus used on non-arithmetic expression.", expression.toString(), current_line_number);
      instructions.emplace_back(Instruction::NEGATE, 0);
      return expression.details.is_mutable = false, expression;

    case Operator::ADDRESS_OF:
      instructions.emplace_back(Instruction::ADDRESS_OF, 0);
      return {table.addRawPointer(expression), {}};

    case Operator::BITNOT:
      if (not arithmetic)
        throw ValidationError("bitnot operator used non-arithmetic expression", expression.toString(), current_line_number);
      instructions.emplace_back(Instruction::BITNOT, 0);
      return expression.details.is_mutable = false, expression;

    case Operator::NOT:
      if (not expression.type->isBool())
        throw ValidationError("not operator used non-boolean expression", expression.toString(), current_line_number);
      instructions.emplace_back(Instruction::BITNOT, 0);
      return expression.details.is_mutable = false, expression;

    case Operator::POST_INCREMENT:
      if (not expression.details.is_mutable)
        throw ValidationError("Postfix increment operator used on non-mutable expression.", expression.toString(), current_line_number);
      instructions.emplace_back(Instruction::POST_INC, 0);
      return expression.details.is_mutable = false, expression;
    case Operator::POST_DECREMENT:
      if (not expression.details.is_mutable)
        throw ValidationError("Postfix decrement operator used on non-mutable expression.", expression.toString(), current_line_number);
      instructions.emplace_back(Instruction::POST_DEC, 0);
      return expression.details.is_mutable = false, expression;

    default:
      std::unreachable();
    }


  }

  [[nodiscard]] InstantiatedType
  peepExpression() {
    auto& node = nodes.pop();
    switch (node.type()) {
    case ASTNode::UNARY:
      return peepUnaryExpression(node.operator_val());
    case ASTNode::BINARY:
      return peepBinaryExpression(node.operator_val());
    case ASTNode::CALLING:
      return peepCallingExpression(node.parameter_count());
    case ASTNode::SUBSCRIPT:
      assert(false);
    case ASTNode::IDENTIFIER:
      return peepIdentifier(node.identifier());

    default:
      std::unreachable();
    }
  }

  void peepReturnStatement() {
    const auto function_return_type = table.returnTypeOfCurrentFunction();
    if (nodes.pop_if(ASTNode::EMPTY)) {
      if (not function_return_type->isDevoid())
        throw ValidationError("Non-devoid function expects return value",
        std::format("Scope return type is '{}'", table.returnTypeOfCurrentFunction()->toString()), current_line_number);

      current_block().set_ret();
      new_block();
      return;
    }
    const auto return_expression = peepExpression();
    if (function_return_type->isDevoid())
      throw ValidationError("Cannot return value from devoid function",
      std::format("Return value type is '{}'", return_expression.toString()), current_line_number);

    if (not return_expression.type->convertibleTo(table.returnTypeOfCurrentFunction()))
      throw ValidationError("Return statement's type is not compatible with return type of scope.",
       std::format("Scope return type is '{}' and expression type is '{}'", function_return_type->toString(), return_expression.toString()),
       current_line_number);

    current_block().set_ret();
    new_block();
  }

  void peepScopedStatement(u64_t num_children) {
    while (num_children-- not_eq 0) {
      peepStatement();
    }
  }

  void peepWhileLoop() {
    br_fallthrough();
    new_block();
    const u32_t condition_idx = current_block_index();
    const auto condition = peepExpression();
    if (not condition.type->isBool())
      throw ValidationError("While Loop condition non-boolean.", std::format("Condition is of type '{}'", condition.toString()), current_line_number);

    brc_to_after( [=, this] mutable {
      new_block();
      peepScopedStatement(nodes.pop_scoped());
      current_block().set_br(condition_idx);
    });
  }

  void peepForLoop() {assert(false and "Not sure about for loop form yet");}

  void peepIfStatement() {
    const auto condition = peepExpression();
    if (not condition.type->isBool())
      throw ValidationError("If statement condition non-boolean.", std::format("Condition is of type '{}'", condition.toString()), current_line_number);

    //this is less complicated than it looks
    //only so layered because of the optional else
    brc_to_after( [=, this] mutable {
      new_block(); //true block
      peepScopedStatement(nodes.pop_scoped());
      br_to_after( [=, this] mutable {
        new_block(); //else block if there is one, otherwise after block
        if (not nodes.pop_if(ASTNode::EMPTY)) {
          peepStatement();
          blocks.back().set_br(blocks.size());
          new_block(); //after block
        }
      });
    });
  }

  void peepVarDeclaration() {
    const auto declaration_type = nodes.pop().instance_type();
    char* name = nodes.pop().identifier();
    if (table.containsLocalVariable(name))
      throw ValidationError("Redefinition of symbol name in variable declaration.", std::format("Symbol name: '{}'", name), current_line_number);

    table.addLocalVariable(eden::releasing_string::released_ptr(name), declaration_type);

    if (not nodes.peek_is(ASTNode::EMPTY)) {
      const auto initialization_type = peepExpression();
      if (not initialization_type.type->convertibleTo(declaration_type.type))
        throw ValidationError("Variable initialization's type is not compatible with variable type.",
          std::format("Variable '{}' is of type '{}' and expression '{}' is of type '{}'.",
          name,  declaration_type.toString(), "PLACEHOLDER EXPRESSION STRING", initialization_type.toString()), current_line_number);
      instructions.emplace_back(Instruction::LOCAL, locals.size());
      instructions.emplace_back(Instruction::ASSIGN, 0);
      locals.emplace_back(declaration_type.type);
    }
  }

  void peepStatement() {
    auto& node = nodes.pop();
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
      return (void)peepExpression();

    case ASTNode::UNARY:
    case ASTNode::BINARY:
    case ASTNode::CALLING:
    case ASTNode::SUBSCRIPT:
    case ASTNode::IDENTIFIER:
      nodes.put_back();
      return (void)peepExpression();

    case ASTNode::INT_LITERAL:
    case ASTNode::UINT_LITERAL:
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
    assert(locals.empty());
    assert(instructions.empty());
    assert(blocks.empty());
    blocks.emplace_back(0);

    while (not nodes.empty())
      peepStatement();
  }
};

void peepFunction(TU& tu, Parser::Function &func, SymbolTable& table) {
  table.enterFunctionScope(func.name.get());
  Peeper test(tu.globals, tu.functions, func.body, table);
  test.peepUntilEmpty();
  table.leaveFunctionScope();
}

}

TU PeepMIR::lowerToPeep(Parser::TU&& parsed_tu) {
  SymbolTable& table = parsed_tu.table;
  TU tu(table.takeTypeContext());
  std::vector<const Type*> globals;
  std::vector<Function> functions;
  for (auto &func : parsed_tu.functions)
    peepFunction(tu, func, table);

  if (Settings::doOutputValidation()) {
    std::cout << "--- Validation Passed ---\n\n";
    std::quick_exit(0);
  }


  assert(false);
}