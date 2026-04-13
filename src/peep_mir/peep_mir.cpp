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
  {assume_assert(begin->type() == ASTNode::SCOPED); return (begin++)->sub_statements();}

  constexpr void
  put_back() noexcept
  {--begin;}

  [[nodiscard]] constexpr bool
  empty() const noexcept
  {return begin == end;}
};

struct Peeper {
  eden::releasing_vector<const Type*> locals;
  eden::releasing_vector<Instruction> instructions;
  eden::releasing_vector<Function::Block> blocks;
  TreeView nodes;
  SymbolTable& table;
  u64_t current_line_number{};

  Peeper(SyntaxTree& tree, SymbolTable& table)
  : nodes{tree.nodes.begin(), tree.nodes.end()}, table(table) {}

  [[nodiscard]] constexpr Function::Block&
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

  InstantiatedType peepVariableExpression(const char* identifier) const {
    if (not table.containsVariable(identifier)) {
      if (table.containsFunction(identifier))
        throw ValidationError("Function name where variable name expected.", identifier, current_line_number);

      throw ValidationError("Undeclared Identifier.", identifier, current_line_number);
    }

    return table.closestVariable(identifier);
  }

  InstantiatedType peepSubscriptExpression() {assert(false);}

  InstantiatedType peepCallingExpression(u64_t parameter_count) {
    if (not std::holds_alternative<IdentifierExpression>(*calling.called))
      throw ValidationError("Callable non-functions unfortunately not supported :(", std::visit(ExpressionToStringVisitor{}, *calling.called), calling.line_number);

    const auto& identifier = std::get<IdentifierExpression>(*calling.called);
    if (not table.containsFunction(identifier.ident))
      throw ValidationError("Callable non-functions unfortunately not supported :(", identifier.ident, calling.line_number);

    std::vector<InstantiatedType> provided_params;
    for (const auto& param : calling.parameters)
      provided_params.emplace_back(peepExpression(*param));

    const auto result = table.returnTypeOfCall(identifier.ident, provided_params);
    if (result)
      return {result.value(), {}};

    const auto error = result.error();
    std::string context{"Types of parameters used in call: "};
    for (const auto& t : provided_params) {
      context.append(t.toString());
      context.append(", ");
    }
    if (provided_params.empty())
      context = "Called with no parameters";
    else {
      context.pop_back();
      context.pop_back();
    }

    if (error == SymbolTable::CallError::NO_SUITABLE_FUNCTION)
      throw ValidationError("No suitable function found for call.", context, calling.line_number);

    throw ValidationError("Ambiguous function call.", context, calling.line_number);
  }

  InstantiatedType peepBinaryExpression(Operator opr) {
    InstantiatedType left_instance = peepExpression(*binary.expr_left);
    const bool left_is_mutable = left_instance.details.is_mutable;
    const InstantiatedType right_instance = peepExpression(*binary.expr_right);

    const auto left_type = left_instance.type;
    const auto right_type = right_instance.type;


    if (not left_type->convertibleTo(right_type) && not right_type->convertibleTo(left_type))
      throw ValidationError("Binary operator used on differing types.", std::format("'{}' and '{}'", left_type->toString(), right_type->toString()), binary.line_number);


    if (left_type->isVariant()) //types should be same, so checking just one is sufficient
      throw ValidationError("Binary operator used on variant types.", std::format("'{}' and '{}'", left_type->toString(), right_type->toString()), binary.line_number);


    const bool arithmetic = left_type->isArithmetic();
    switch (binary.opr) {
    case Operator::ADD:
    case Operator::SUBTRACT:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::POWER:
    case Operator::MODULUS:
    case Operator::LESS:
    case Operator::GREATER:
    case Operator::LESS_EQUAL:
    case Operator::GREATER_EQUAL:
      if (not arithmetic)
        throw ValidationError("Non-arithmetic expression in arithmetic binary operation.", std::format("'{}' and '{}'", left_type->toString(), right_type->toString()), binary.line_number);
      break;

    case Operator::AND:
    case Operator::OR:
    case Operator::XOR:
      if (not left_type->isBool())
        throw ValidationError("Non-boolean expressions in boolean binary operation.", std::format("'{}' and '{}'", left_type->toString(), right_type->toString()), binary.line_number);
      break;

    case Operator::BITAND:
    case Operator::BITOR:
    case Operator::BITXOR:
      if (not arithmetic)
        throw ValidationError("Non-arithmetic expression(s) in bitwise operation.", std::format("'{}' and '{}'", left_type->toString(), right_type->toString()), binary.line_number);
      break;

    case Operator::ASSIGN:
      if (not left_is_mutable)
        throw ValidationError("Left expression in assignment non-mutable.", std::format("Type of expression is '{}'", left_type->toString()), binary.line_number);
      break;

    case Operator::EQUAL:
    case Operator::NOT_EQUAL:
      break;

    default:
      assert(false);
    }

    switch (binary.opr) {
    case Operator::ADD:
    case Operator::SUBTRACT:
    case Operator::MULTIPLY:
    case Operator::DIVIDE:
    case Operator::POWER:
    case Operator::MODULUS:
      return left_instance;

    case Operator::LESS:
    case Operator::GREATER:
    case Operator::LESS_EQUAL:
    case Operator::GREATER_EQUAL:
    case Operator::AND:
    case Operator::OR:
      return bool_instance;
    case Operator::XOR:
      binary.opr = Operator::NOT_EQUAL;
      return bool_instance;

    case Operator::BITAND:
    case Operator::BITOR:
    case Operator::BITXOR:
    case Operator::BITNOT:
    case Operator::ASSIGN:
      left_instance.details.is_mutable = false;
      return left_instance;

    case Operator::EQUAL:
    case Operator::NOT_EQUAL:
      return bool_instance;

    default:
      assert(false);
    }
  }

  InstantiatedType peepUnaryExpression(Operator opr) {
    InstantiatedType instance = peepExpression(*unary.expr);
    if (instance.type->isVariant())
      throw ValidationError("Unary operator used on variant type.", instance.toString(), unary.line_number);


    const bool arithmetic = instance.type->isArithmetic();
    if (unary.opr == Operator::UNARY_MINUS) {
      if (not arithmetic)
        throw ValidationError("Unary minus used on non-arithmetic expression.", instance.toString(), unary.line_number);
      instance.details.is_mutable = false;
      return instance;
    }

    if (unary.opr == Operator::BITNOT) {
      if (not arithmetic)
        throw ValidationError("bitnot operator used non-arithmetic expression", instance.toString(), unary.line_number);
      instance.details.is_mutable = false;
      return instance;
    }

    if (unary.opr == Operator::NOT) {
      if (not instance.type->isBool())
        throw ValidationError("not operator used non-boolean expression", instance.toString(), unary.line_number);
      unary.opr = Operator::BITNOT;
      instance.details.is_mutable = false;
      return instance;
    }

    if (unary.opr == Operator::ADDRESS_OF)
      return {table.addRawPointer(instance.type, instance.details.is_mutable), {}};

    if (not instance.details.is_mutable)
      throw ValidationError("Pre/Postfix operator used on non-mutable expression.", instance.toString(), unary.line_number);



    if (unary.opr == Operator::POST_INCREMENT ||
        unary.opr == Operator::POST_DECREMENT) {
      instance.details.is_mutable = false;
      return instance;
        }


    return instance;
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

  void peepExpressionStatement() {
    if (expression_statement.expr)
      peepExpression(*expression_statement.expr);
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
       std::format("Scope return type is '{}' and expression '{}' returns type '{}'", function_return_type->toString(), "PLACEHOLDER"),
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
      return peepExpressionStatement();

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
    assume_assert(locals.empty());
    assume_assert(instructions.empty());
    assume_assert(blocks.empty());
    blocks.emplace_back(0);

    while (not nodes.empty())
      peepStatement();
  }
};

void peepFunction(TU& tu, Parser::Function &func, SymbolTable& table) {
  auto function_type = table.enterFunctionScope(func.name.get());
  Peeper test{func.body, table};
  test.peepUntilEmpty();
  table.leaveFunctionScope();
}

}

TU PeepMIR::lowerToPeep(Parser::TU&& parsed_tu) {
  SymbolTable& table = parsed_tu.table;
  TU tu(table.takeTypeContext());
  for (auto &func : parsed_tu.functions)
    peepFunction(tu, func, table);

  if (Settings::doOutputValidation()) {
    std::cout << "--- Validation Passed ---\n\n";
    std::quick_exit(0);
  }


  assert(false);
}