#include "parse.hpp"

#include "ast.hpp"
#include "build_system/build.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "semantic_analysis/symbol_table.hpp"
#include "settings.hpp"

#include <iostream>
#include <unordered_set>
#include <utility>

using namespace LOM;
using namespace LOM::Lexer;
using namespace LOM::Parser;
using namespace LOM::AST;
using eden::releasing_string;

namespace {

InstantiatedType::InstanceDetails parseTypeDetails(TokenView& tokens) {
  InstantiatedType::InstanceDetails details;
  if (tokens.pop_if(TokenType::KEYWORD_PUB))
    details.is_public = true;

  if (tokens.peek().isTypeModifier()) {
    if (tokens.peek_is(TokenType::KEYWORD_MUT))
      details.is_mutable = true;
    else
      std::unreachable();

    tokens.pop();
  }

  return details;
}

InstantiatedType parseType(TokenView& tokens, Module& table) {
  const auto details = parseTypeDetails(tokens);
  Token token = tokens.take();

  if (token.isPrimitive()) {
    if (token.isPointer()) {
      tokens.expect_then_pop(TokenType::ARROW, "Expected arrow in pointer declaration.");
      if (token.is(TokenType::KEYWORD_VAGUE)) {
        const auto subtype_details = parseTypeDetails(tokens);
        return InstantiatedType(PointerType::vague(subtype_details.is_mutable), details);
      }

      const InstantiatedType subtype_instance = parseType(tokens, table);
      switch (token.getType()) {
      case TokenType::KEYWORD_RAW:
        return InstantiatedType(table.addRawPointer(subtype_instance), details);
      case TokenType::KEYWORD_UNIQUE:
        return InstantiatedType(table.addUniquePointer(subtype_instance), details);
      default:
        std::unreachable();
      }

    }

    switch (token.getType()) {
    case TokenType::KEYWORD_i8:
      return InstantiatedType(PrimitiveType::i8(), details);
    case TokenType::KEYWORD_i16:
      return InstantiatedType(PrimitiveType::i16(), details);
    case TokenType::KEYWORD_i32:
      return InstantiatedType(PrimitiveType::i32(), details);
    case TokenType::KEYWORD_i64:
      return InstantiatedType(PrimitiveType::i64(), details);
    case TokenType::KEYWORD_u8:
      return InstantiatedType(PrimitiveType::u8(), details);
    case TokenType::KEYWORD_u16:
      return InstantiatedType(PrimitiveType::u16(), details);
    case TokenType::KEYWORD_u32:
      return InstantiatedType(PrimitiveType::u32(), details);
    case TokenType::KEYWORD_u64:
      return InstantiatedType(PrimitiveType::u64(), details);
    case TokenType::KEYWORD_f32:
      return InstantiatedType(PrimitiveType::f32(), details);
    case TokenType::KEYWORD_f64:
      return InstantiatedType(PrimitiveType::f64(), details);

    case TokenType::KEYWORD_CHAR:
      return InstantiatedType(PrimitiveType::char_(), details);
    case TokenType::KEYWORD_BOOL:
      return InstantiatedType(PrimitiveType::bool_(), details);
    case TokenType::KEYWORD_STRING:
      return InstantiatedType(PrimitiveType::string(), details);
    case TokenType::KEYWORD_DEVOID:
      if (details not_eq InstantiatedType::InstanceDetails{})
        throw ParsingError("Devoid may not have type qualifiers.",
          InstantiatedType(PrimitiveType::devoid(), details).toString(), token.getLN());

      return devoid_literal;
    default:
      std::unreachable();
    }
  }

  if (token.is(TokenType::LESS)) {
    TokenView variant_types_tokens = tokens.getTokensBetweenAngleBrackets();

    bool nullable{false};
    std::vector<const Type*> subtypes;
    std::unordered_set<const Type* > typenames; //prevent duplicate types

    const unsigned variant_ln = variant_types_tokens.peek().getLN();
    do {
      const unsigned subtype_ln = variant_types_tokens.peek().getLN();
      const InstantiatedType subtype = parseType(variant_types_tokens, table);
      if (subtype.type->isVariant())
        throw ParsingError("Nested variant types not allowed.", subtype.toString(), subtype_ln);

      if (subtype.details.is_mutable)
        throw ParsingError("Mutability cannot be specified within variant type list, must be specified prior to type list.", subtype.toString(), subtype_ln);

      if (typenames.contains(subtype.type))
        throw ParsingError("Duplicate types specified in variant declaration.", subtype.toString(), subtype_ln);

      if (subtype.type->isDevoid()) {
        if (nullable) throw ParsingError("Devoid may not be specified more than once in variant declaration", "", subtype_ln);

        nullable = true;
      }
      else
        typenames.emplace(subtype.type);

      subtypes.push_back(subtype.type);
    } while (variant_types_tokens.pop_if(TokenType::COMMA));

    if (subtypes.size() < 2)
      throw ParsingError("Two or more types must be specified in variant type list.", "", variant_ln);

    return InstantiatedType(table.addVariant(subtypes, nullable), details);
  }

  throw ParsingError("Expected typename.", token);
}

releasing_string::released_ptr parseIdentifier(TokenView& tokens) {
  Token token = tokens.take();
  token.throw_if_not(TokenType::IDENTIFIER, "Expected identifier.");
  return token.takeString();
}

struct Body {
  struct Expression {
    ASTNode::Type type;
    u16_t left_idx;
    u16_t right_idx; //unary expressions do not contain right_idx
    u64_t value;
  };
  struct ExpressionTree {
    static constexpr u64_t max_expressiontree_size{200};
    static_assert(max_expressiontree_size < std::numeric_limits<u16_t>::max());
    std::array<Expression, max_expressiontree_size> data;
    u64_t begin{1};

    u16_t create(ASTNode::Type type, u64_t value, u16_t left_idx = 0, u16_t right_idx = 0) {
      auto& expr = data[begin];
      expr.type = type;
      expr.value = value;
      expr.left_idx = left_idx;
      expr.right_idx = right_idx;
      return begin++;
    }

    void reset()
    {begin = 1;}
  };

  std::vector<ASTNode> tree;
  TokenView tokens;
  Module& table;
  ExpressionTree& expression_tree;
  inline static releasing_string dunder_cextern_name_vec{"__C"};
  inline static releasing_string::released_ptr dunder_cextern_name{dunder_cextern_name_vec.release()};

  Body(TokenView tokens, Module& table, ExpressionTree& expression_tree)
  : tokens(tokens), table(table), expression_tree(expression_tree) {}

  u16_t generateParameters() {
    const auto parameter = generateAssignmentExpression();
    if (tokens.pop_if(TokenType::RPAREN))
      return expression_tree.create(ASTNode::EMPTY, 0, parameter, 0);
    tokens.expect_then_pop(TokenType::COMMA, "Expected comma in call expression.");
    return expression_tree.create(ASTNode::EMPTY, 0, parameter, generateParameters());
  }

  u16_t generateSubscript() {
    const auto res = generateAssignmentExpression();
    tokens.expect_then_pop(TokenType::RBRACKET, "Expected closing bracket in subscript expression.");
    return res;
  }

  u16_t generateIdentifier() {
    u64_t identifier_value;
    if (tokens.pop_if(TokenType::DUNDER_CEXTERN))
      identifier_value = std::bit_cast<u64_t>(dunder_cextern_name.get());
    else {
      tokens.expect(TokenType::IDENTIFIER, "Expected identifier.");
      identifier_value = std::bit_cast<u64_t>(tokens.take().takeString().get());
    }

    const auto res = expression_tree.create(ASTNode::IDENTIFIER, identifier_value);
    if (tokens.pop_if(TokenType::DOT)) {
      expression_tree.data[res].type = ASTNode::DOT_IDENTIFIER;
      generateIdentifier();
    }
    return res;
  }

  u16_t generatePrimaryExpression() {
    if (tokens.empty())
      return 0;

    if (tokens.peek().isLiteral()) {
      auto token = tokens.take();
      ASTNode::Type literal_type;
      u64_t literal_value;

      switch (token.getType()) {
      case TokenType::INTEGER_LITERAL:
        literal_type = ASTNode::INTEGER_LITERAL;
        literal_value = token.getRawValue();
        break;
      case TokenType::SIGNED_LITERAL:
        literal_type = ASTNode::SIGNED_LITERAL;
        literal_value = token.getRawValue();
        break;
      case TokenType::UNSIGNED_LITERAL:
        literal_type = ASTNode::UNSIGNED_LITERAL;
        literal_value = token.getRawValue();
        break;
      case TokenType::FLOAT_LITERAL:
        literal_type = ASTNode::FLOAT_LITERAL;
        literal_value = token.getRawValue();
        break;
      case TokenType::DOUBLE_LITERAL:
        literal_type = ASTNode::DOUBLE_LITERAL;
        literal_value = token.getRawValue();
        break;
      case TokenType::BOOL_LITERAL:
        literal_type = ASTNode::BOOL_LITERAL;
        literal_value = token.getRawValue();
        break;
      case TokenType::CHAR_LITERAL:
        literal_type = ASTNode::CHAR_LITERAL;
        literal_value = token.getRawValue();
        break;
      case TokenType::STRING_LITERAL:
        literal_type = ASTNode::STRING_LITERAL;
        literal_value = std::bit_cast<u64_t>(token.takeString().get());
        break;
      default:
        std::unreachable();
      }

      return expression_tree.create(literal_type, literal_value);
    }

    if (tokens.pop_if(TokenType::LPAREN)) {
      const auto res = generateExpressionBetweenParenthesis();
      return res;
    }

    return generateIdentifier();
  }

  u16_t generatePostfixExpression() {
    auto left = generatePrimaryExpression();
    while (true) {
      u64_t value;
      switch (tokens.peek().getType()) {
      case TokenType::PLUSPLUS:
        value = static_cast<u64_t>(Operator::POST_INCREMENT);
        break;
      case TokenType::MINUSMINUS:
        value = static_cast<u64_t>(Operator::POST_DECREMENT);
        break;
      case TokenType::ARROW:
        value = static_cast<u64_t>(Operator::ARROW);
        break;
      case TokenType::LPAREN: {
        tokens.pop();
        const u16_t parameters = tokens.pop_if(TokenType::RPAREN) ? 0 : generateParameters();
        left = expression_tree.create(ASTNode::CALLING, 0, left, parameters); //pretty sure the 0 is supposed to be a placeholder but its been working so wtv
        continue;
      }
      case TokenType::LBRACKET:
        tokens.pop();
        tokens.reject(TokenType::RBRACKET, "Expected expression within brackets.");
        left = expression_tree.create(ASTNode::SUBSCRIPT, 0, left, generateSubscript());
        continue;
      default:
        return left;
      }

      tokens.pop();
      left = expression_tree.create(
        ASTNode::UNARY, value,
        left, 0);
    }
  }

  u16_t generatePrefixExpression() {
    u64_t value;
    switch (tokens.peek().getType()) {
    case TokenType::PLUSPLUS:
      value = static_cast<u64_t>(Operator::PRE_INCREMENT);
      break;
    case TokenType::MINUSMINUS:
      value = static_cast<u64_t>(Operator::PRE_DECREMENT);
      break;
    case TokenType::MINUS:
      value = static_cast<u64_t>(Operator::UNARY_MINUS);
      break;
    case TokenType::ADDR:
      value = static_cast<u64_t>(Operator::ADDRESS_OF);
      break;
    case TokenType::KEYWORD_NOT:
      value = static_cast<u64_t>(Operator::NOT);
      break;
    case TokenType::KEYWORD_BITNOT:
      value = static_cast<u64_t>(Operator::BITNOT);
      break;
    default:
      return generatePostfixExpression();
    }
    tokens.pop();
    return expression_tree.create(
      ASTNode::UNARY, value,
      generatePrefixExpression(), 0);
  }

  u16_t generateFactorExpression() {
    auto left = generatePrefixExpression();
    while (true) {
      u64_t value;
      switch (tokens.peek().getType()) {
      case TokenType::STAR:
        value = static_cast<u64_t>(Operator::MULTIPLY);
        break;
      case TokenType::SLASH:
        value = static_cast<u64_t>(Operator::DIVIDE);
        break;
      case TokenType::MOD:
        value = static_cast<u64_t>(Operator::MODULUS);
        break;
      default:
        return left;
      }
      tokens.pop();
      left = expression_tree.create(
        ASTNode::BINARY, value,
        left, generatePrefixExpression());
    }
  }

  u16_t generateTermExpression() {
    auto left = generateFactorExpression();
    while (true) {
      u64_t value;
      switch (tokens.peek().getType()) {
      case TokenType::PLUS:
        value = static_cast<u64_t>(Operator::ADD);
        break;
      case TokenType::MINUS:
        value = static_cast<u64_t>(Operator::SUBTRACT);
        break;
      default:
        return left;
      }
      tokens.pop();
      left = expression_tree.create(
    ASTNode::BINARY, value,
         left, generateFactorExpression());
    }
  }

  u16_t generateRelationalExpression() {
    auto left= generateTermExpression();
    while (true) {
      u64_t value;
      switch (tokens.peek().getType()) {
      case TokenType::KEYWORD_EQUALS:
        value = static_cast<u64_t>(Operator::EQUAL);
        break;
      case TokenType::KEYWORD_NOT_EQUAL:
        value = static_cast<u64_t>(Operator::NOT_EQUAL);
        break;
      case TokenType::LESS:
        value = static_cast<u64_t>(Operator::LESS);
        break;
      case TokenType::GTR:
        value = static_cast<u64_t>(Operator::GREATER);
        break;
      case TokenType::LESSEQ:
        value = static_cast<u64_t>(Operator::LESS_EQUAL);
        break;
      case TokenType::GTREQ:
        value = static_cast<u64_t>(Operator::GREATER_EQUAL);
        break;
      default:
        return left;
      }
      tokens.pop();
      left = expression_tree.create(
        ASTNode::BINARY, value,
        left, generateTermExpression());
    }

  }

  u16_t generateBitwiseExpression() {
    auto left = generateRelationalExpression();
    while (true) {
      u64_t value;
      switch (tokens.peek().getType()) {
      case TokenType::KEYWORD_BITAND:
        value = static_cast<u64_t>(Operator::BITAND);
        break;
      case TokenType::KEYWORD_BITOR:
        value = static_cast<u64_t>(Operator::BITOR);
        break;
      case TokenType::KEYWORD_BITXOR:
        value = static_cast<u64_t>(Operator::BITXOR);
        break;
      default:
        return left;
      }
      tokens.pop();
      left = expression_tree.create(
        ASTNode::BINARY, value,
        left, generateRelationalExpression());
    }
  }

  u16_t generateLogicalExpression() {
    auto left = generateBitwiseExpression();
    while (true) {
      u64_t value;
      switch (tokens.peek().getType()) {
      case TokenType::KEYWORD_AND:
        value = static_cast<u64_t>(Operator::AND);
        break;
      case TokenType::KEYWORD_OR:
        value = static_cast<u64_t>(Operator::OR);
        break;
      case TokenType::KEYWORD_XOR:
        value = static_cast<u64_t>(Operator::XOR);
        break;
      default:
        return left;
      }

      tokens.pop();
      left = expression_tree.create(
        ASTNode::BINARY, value,
        left, generateBitwiseExpression());
    }
  }

  u16_t generateAssignmentExpression() {
    const auto left = generateLogicalExpression();
    if (tokens.pop_if(TokenType::ASSIGN)) {
      return expression_tree.create(
        ASTNode::BINARY, static_cast<u64_t>(Operator::ASSIGN),
        left, generateAssignmentExpression());
    }

    return left;
  }

  u16_t generateExpressionBetweenParenthesis() {
    const TokenView until_ending = tokens.getTokensBetweenParenthesis();
    const TokenView after_ending = tokens;
    tokens = until_ending;
    const auto res = generateAssignmentExpression();
    if (not tokens.empty())
      throw ParsingError(std::format("Expected {}", ")"), tokens.peek());
    tokens = after_ending;
    return res;
  }

  u16_t generateExpressionUntil(TokenType ending_token) {
    const TokenView until_ending = tokens.getAllTokensUntilFirstOf(ending_token); tokens.pop();
    const TokenView after_ending = tokens;
    tokens = until_ending;
    const auto res = generateAssignmentExpression();
    if (not tokens.empty())
     throw ParsingError(std::format("Expected {}", tokenTypeToString(ending_token)), tokens.peek());
    tokens = after_ending;
    return res;
  }

  void translateExpression(u16_t idx) {
    if (idx == 0)
      return;

    using enum ASTNode::Type;
    auto expression = expression_tree.data[idx];
    switch (expression.type) {
    case EMPTY:
    case DECLARATION:
    case IF:
    case FOR:
    case WHILE:
    case SCOPED:
    case RETURN:
    case EXPR_STMT:
      std::unreachable();

    case UNARY:
      tree.emplace_back(UNARY, expression.value);
      return translateExpression(expression.left_idx);
    case BINARY:
      tree.emplace_back(BINARY, expression.value);
      translateExpression(expression.left_idx);
      translateExpression(expression.right_idx);
      return;
    case CALLING: {
      tree.emplace_back(CALLING, 0);
      const auto calling_idx = tree.size() - 1;
      translateExpression(expression.left_idx);
      if (expression.right_idx == 0)
        return;

      u16_t left_idx = expression_tree.data[expression.right_idx].left_idx;
      u16_t right_idx = expression_tree.data[expression.right_idx].right_idx;
      u64_t num_parameters{1};
      while (true) {
        translateExpression(left_idx);
        if (right_idx == 0)
          break;

        left_idx = expression_tree.data[right_idx].left_idx;
        right_idx = expression_tree.data[right_idx].right_idx;
        ++num_parameters;
      }

      tree[calling_idx].value() = num_parameters;
      return;
    }
    case SUBSCRIPT:
      std::unreachable();
      tree.emplace_back(SUBSCRIPT, expression.value);
      translateExpression(expression.left_idx);
      translateExpression(expression.right_idx);
      return;

    case DOT_IDENTIFIER:
      tree.emplace_back(DOT_IDENTIFIER, expression.value);
      translateExpression(idx + 1);
      return;
      /*
      loop: //heheehehehehehe
      expression = expression_tree.data[++idx];
      if (expression.type == DOT_IDENTIFIER) {
        tree.emplace_back(DOT_IDENTIFIER, expression.value);
        goto loop;
      }
      assert(expression.type == IDENTIFIER); [[fallthrough]]; */
    case IDENTIFIER:
      tree.emplace_back(IDENTIFIER, expression.value);
      return;
    case INTEGER_LITERAL:
    case SIGNED_LITERAL:
    case UNSIGNED_LITERAL:
    case FLOAT_LITERAL:
    case DOUBLE_LITERAL:
    case BOOL_LITERAL:
    case CHAR_LITERAL:
    case STRING_LITERAL:
      tree.emplace_back(expression.type, expression.value);
      return;

    default:
      std::unreachable();
    }
  }

  void parseExpressionBetweenParenthesis() {
    expression_tree.reset();
    if (tokens.pop_if(TokenType::RPAREN)) {
      tree.emplace_back(ASTNode::Type::EMPTY);
      return;
    }
    const auto idx = generateExpressionBetweenParenthesis();
    translateExpression(idx);
  }
  void parseExpressionUntil(TokenType ending_token) {
    expression_tree.reset();
    if (tokens.pop_if(ending_token)) {
      tree.emplace_back(ASTNode::Type::EMPTY);
      return;
    }
    const auto idx = generateExpressionUntil(ending_token);
    translateExpression(idx);
  }

  void parseExpressionStatement() {
    tree.emplace_back(ASTNode::EXPR_STMT, tokens.current_line_number());
    if (tokens.pop_if(TokenType::SEMI_COLON)) [[unlikely]]
      tree.emplace_back(ASTNode::EMPTY, tokens.current_line_number());
    else
      parseExpressionUntil(TokenType::SEMI_COLON);
  }

  void parseVarDecl() {
    tree.emplace_back(ASTNode::DECLARATION, tokens.current_line_number());
    tree.emplace_back(parseType(tokens, table));

    tree.emplace_back(ASTNode::IDENTIFIER,std::bit_cast<u64_t>(parseIdentifier(tokens).get()));
    tokens.expect_then_pop(TokenType::ASSIGN, "Expected assignment in variable declaration.");

    if (tokens.pop_if(TokenType::KEYWORD_JUNK)) {
      tree.emplace_back(ASTNode::EMPTY);
      tokens.expect_then_pop(TokenType::SEMI_COLON, "Expected semicolon ending junk variable declaration.");
    }
    else {
      tokens.reject(TokenType::SEMI_COLON, "Expected initializing expression in variable declaration.");
      parseExpressionUntil(TokenType::SEMI_COLON);
    }
  }

  void parseIf() {
    tree.emplace_back(ASTNode::IF, tokens.current_line_number());
    tokens.expect_then_pop(TokenType::LPAREN, "Expected opening parenthesis in if statement condition.");
    tokens.reject(TokenType::RPAREN, "Expected condition in if statement.");
    parseExpressionBetweenParenthesis();
    parseScoped();
    if (tokens.pop_if(TokenType::KEYWORD_ELSE))
      parseStatement();
    else
      tree.emplace_back(ASTNode::EMPTY);
  }

  void parseFor() {
    assert(false and "Sorry, unimplemented.");
    /*
    tree.emplace_back(ASTNode::FOR, tokens.current_line_number());
    tokens.expect_then_pop(TokenType::LPAREN, "Expected opening patenthesis in for loop statement.");
    tokens.reject(TokenType::RPAREN, "Expected for loop header.");

    parseVarDecl();
    parseExpressionUntil(TokenType::SEMI_COLON);
    parseExpressionUntil(TokenType::RPAREN);
    parseScoped(); */
  }

  void parseWhile() {
    tree.emplace_back(ASTNode::WHILE, tokens.current_line_number());
    tokens.expect_then_pop(TokenType::LPAREN,"Expected open parenthesis in while loop condition.");
    tokens.reject(TokenType::RPAREN, "Expected while loop condition.");

    parseExpressionBetweenParenthesis();
    parseScoped();
  }

  void parseScoped() {
    if (tokens.pop_if(TokenType::SEMI_COLON))
      return (void)tree.emplace_back(ASTNode::SCOPED, 0);

    tree.emplace_back(ASTNode::SCOPED, 1);
    if (tokens.pop_if(TokenType::LBRACE)) {
      u64_t num_statements = 0;
      const auto scoped_idx = tree.size() - 1;
      while (not tokens.pop_if(TokenType::RBRACE)) {
        parseStatement();
        ++num_statements;
        if (tokens.empty())
          throw ParsingError("Expected ending rbrace to scoped statement.", tokens.previousToken());
      }
      tree[scoped_idx].value() = num_statements;
    }
    else
      parseStatement();
  }

  void parseReturn() {
    tree.emplace_back(ASTNode::RETURN, tokens.current_line_number());
    parseExpressionUntil(TokenType::SEMI_COLON);
  }

  void parseStatement() {
    const Token &first = tokens.peek();
    if (first.isPrimitive() or first.isTypeModifier())
      return parseVarDecl();

    switch (first.getType()) {
    case TokenType::KEYWORD_IF:
      tokens.pop();
      return parseIf();
    case TokenType::KEYWORD_FOR:
      tokens.pop();
      return parseFor();
    case TokenType::KEYWORD_WHILE:
      tokens.pop();
      return parseWhile();
    case TokenType::KEYWORD_DO:
      std::unreachable();
    case TokenType::KEYWORD_RETURN:
      tokens.pop();
      return parseReturn();
    case TokenType::KEYWORD_SWITCH:
      std::unreachable();
    case TokenType::LBRACE:
      return parseScoped();
    case TokenType::LESS:
      return parseVarDecl();

    case TokenType::IDENTIFIER: //this is problematic as FUCK
      if (tokens.peek_ahead(1).is(TokenType::IDENTIFIER))
        return parseVarDecl();
      [[fallthrough]];
    default:
      return parseExpressionStatement();
    }
  }

  void parseStatementsUntilEmpty() {
    while (not tokens.empty())
      parseStatement();
  }
};

void parseCExtern(TU& tu, Body& global_body) {
  // __fd name(...) -> ...;
  auto& tokens = global_body.tokens;
  auto& table = *tu.table;
  auto name = parseIdentifier(tokens);
  tokens.expect_then_pop(TokenType::LPAREN, "Expected parameter list.");

  std::vector<Module::Variable> parameters; parameters.reserve(4);
  bool is_variadic = false;
  if (not tokens.pop_if(TokenType::RPAREN))
    while (true) {
      if (tokens.pop_if(TokenType::DUNDER_VA)) {
        is_variadic = true;
        tokens.expect_then_pop(TokenType::RPAREN, "Expected closing parenthesis in parameter list.");
        break;
      }

      auto type = parseType(tokens, table);
      parameters.emplace_back(type, parseIdentifier(tokens));

      if (not tokens.pop_if(TokenType::COMMA)) {
        tokens.expect_then_pop(TokenType::RPAREN, "Expected closing parenthesis in parameter list.");
        break;
      }
    }

  const Type* return_type = Type::devoid();
  if (tokens.pop_if(TokenType::ARROW)) {
    InstantiatedType type = parseType(tokens, table);
    if (not type.isPlain())
      throw ParsingError("Return type may not have type qualifiers.", type.toString(), tokens.current_line_number());
    return_type = type.type;
  }

  tokens.expect_then_pop(TokenType::SEMI_COLON, "Expected semi-colon.");

  getModule("__C")->addFunction(
    eden::owned_stringview(name.get(), name.size() - 1),
    std::span(parameters), return_type,
    true, is_variadic);
}

bool parseImports(TU& tu, Body& global_body) {
  TokenView& tokens = global_body.tokens;
  if (tokens.pop_if(TokenType::DUNDER_CEXTERN)) {
    parseCExtern(tu, global_body);
    return true;
  }

  if (not tokens.pop_if(TokenType::KEYWORD_IMPORT))
    return false;

  tokens.expect(TokenType::IDENTIFIER, "Expected module name.");
  tu.imports.emplace_back(tokens.take().takeString());
  tokens.expect_then_pop(TokenType::SEMI_COLON, "Expected semicolon.");
  return true;
}

bool parseGlobals(Body& global_body) {
  TokenView& tokens = global_body.tokens;
  if (tokens.peek_is(TokenType::KEYWORD_FN) or tokens.peek_is(TokenType::KEYWORD_PUB) or tokens.empty())
    return false;

  tokens.expect_then_pop(TokenType::KEYWORD_GLOBAL, "Expected global keyword before declaration.");
  global_body.parseVarDecl();
  return true;
}

void parseFunctions(Body& global_body, std::vector<Function>& functions) {
  TokenView& tokens = global_body.tokens;
  Module& table = global_body.table;
  std::vector<TokenView> function_tokens;

  //just parses the declarations to load into symbol table
  while (not tokens.empty()) {
    Function& function = functions.emplace_back();
    const bool is_public = tokens.pop_if(TokenType::KEYWORD_PUB);
    function.is_public = is_public;

    tokens.expect_then_pop(TokenType::KEYWORD_FN, "Expected function declaration.");
    function.name = parseIdentifier(tokens);
    tokens.expect_then_pop(TokenType::LPAREN, "Expected parameter list.");

    std::vector<Module::Variable> parameters; parameters.reserve(4);
    if (not tokens.pop_if(TokenType::RPAREN))
      while (true) {
        auto type = parseType(tokens, table);
        auto name = parseIdentifier(tokens);
        parameters.emplace_back(type, std::move(name));

        if (not tokens.pop_if(TokenType::COMMA)) {
          tokens.expect_then_pop(TokenType::RPAREN, "Expected closing parenthesis in parameter list.");
          break;
        }
      }

    const Type* return_type = Type::devoid();
    if (tokens.pop_if(TokenType::ARROW)) {
      InstantiatedType type = parseType(tokens, table);
      if (not type.isPlain())
        throw ParsingError("Return type may not have type qualifiers.", type.toString(), tokens.current_line_number());
      return_type = type.type;
    }

    tokens.expect_then_pop(TokenType::LBRACE, "Expected function definition.");
    function_tokens.emplace_back(tokens.getTokensBetweenBraces());
    table.addFunction(function.name, std::span(parameters), return_type, is_public);
  }

  //actually parse the function bodies
  const auto sz = functions.size();
  assert(sz == function_tokens.size());
  for (auto i{0uz}; i < sz; ++i) {
    Body function_body
    (function_tokens[i], table, global_body.expression_tree);

    table.enterFunctionScope(functions[i].name.get());
    function_body.parseStatementsUntilEmpty();
    table.leaveFunctionScope();
    functions[i].body.nodes = std::move(function_body.tree);
  }
}

void printFunction(const Function& func, Module& table) {
  std::cout << "fn " << func.name.get() << "(";
  auto [parameters, return_type] = table.getFunction(func.name.get());

  for (auto &parameter : parameters) {
    std::cout << parameter.type.toString();
    std::cout << " " << parameter.name.get() << ", ";
  }

  if (not parameters.empty())
    std::cout << "\b\b";

  std::cout << ")";
  if (not return_type->isDevoid()) {
    std::cout << " -> ";
    std::cout << return_type->toString();
  }
  std::cout << " { ";
  func.body.print(0);
  std::cout << "\n}";
}

}

#include <print>
void Parser::printTU(TU& ptu) {
  for (auto& import : ptu.imports) {
    std::println("import {};", std::string_view(import));
  }
  if (not ptu.global_tree.nodes.empty()) {
    ptu.global_tree.print(1);
    std::println();
  }
  for (const auto &f : ptu.functions) {
    printFunction(f, *ptu.table);
    std::println();
  }
}

TU Parser::parseTokens(std::vector<Token>& token_list, Module* table) {
  Body::ExpressionTree expression_tree;
  TU ptu; ptu.table = table;
  Body global_body(TokenView{token_list}, *ptu.table, expression_tree);

  while (parseImports(ptu, global_body)) {}
  while (parseGlobals(global_body)) {}

  ptu.global_tree.nodes = std::move(global_body.tree);
  parseFunctions(global_body, ptu.functions);

  if (Settings::doOutputParser()) [[unlikely]] {
    std::cout << "\n--- Parser Output ---\n";
    printTU(ptu);
    std::cout << "--- Parser Output ---\n";
    std::quick_exit(0);
  }

  return ptu;
}