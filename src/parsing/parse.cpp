#include "parse.hpp"

#include "ast.hpp"
#include "build_system/build.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "semantic_analysis/symbol_table.hpp"

#include <iostream>
#include <unordered_set>
#include <utility>
#include <print>

using namespace LOM;
using namespace LOM::Lexer;
using namespace LOM::Parser;
using namespace LOM::AST;
using eden::releasing_string;

namespace {


[[nodiscard]] InstantiatedType
parseType(TokenView& tokens, TU& tu);

[[nodiscard]] InstantiatedType::Qualifiers
parseTypeQualifiers(TokenView& tokens) {
  InstantiatedType::Qualifiers qualifiers;
  if (tokens.peek().isTypeModifier()) {
    if (tokens.peek_is(TokenType::KEYWORD_MUT))
      qualifiers.is_mutable = true;
    else
      eden_unreachable("Type modifier not supported.");

    tokens.pop();
  }

  return qualifiers;
}

eden_return_nonnull
[[nodiscard]] const Type*
parseUnqualifiedType(TokenView& tokens, TU& tu) {
  auto const& src_file = tu.source_files.back();
  auto const token = tokens.take();
  auto& module = *tu.module;

  if (token.isPrimitive()) {
    if (token.isPointer()) {
      tokens.expect_then_pop(TokenType::LESS, "Expected opening < in pointer declaration.");
      if (token.is(TokenType::KEYWORD_VAGUE)) {
        const auto subtype_qualifiers = parseTypeQualifiers(tokens);
        tokens.expect_then_pop(TokenType::GTR, "Expected closing < in pointer declaration.");
        return PointerType::vague(subtype_qualifiers.is_mutable);
      }

      const InstantiatedType subtype_instance = parseType(tokens, tu);
      tokens.expect_then_pop(TokenType::GTR, "Expected closing < in pointer declaration.");
      switch (token.getType()) {
      case TokenType::KEYWORD_RAW:
        return module.getRawPointerType(subtype_instance);
      case TokenType::KEYWORD_UNIQUE:
        return module.getUniquePointerType(subtype_instance);
      default:
        eden_unreachable("Pointer type unsupported.");
      }
    }

    switch (token.getType()) {
    case TokenType::KEYWORD_i8:
      return PrimitiveType::i8();
    case TokenType::KEYWORD_i16:
      return PrimitiveType::i16();
    case TokenType::KEYWORD_i32:
      return PrimitiveType::i32();
    case TokenType::KEYWORD_i64:
      return PrimitiveType::i64();
    case TokenType::KEYWORD_u8:
      return PrimitiveType::u8();
    case TokenType::KEYWORD_u16:
      return PrimitiveType::u16();
    case TokenType::KEYWORD_u32:
      return PrimitiveType::u32();
    case TokenType::KEYWORD_u64:
      return PrimitiveType::u64();
    case TokenType::KEYWORD_f32:
      return PrimitiveType::f32();
    case TokenType::KEYWORD_f64:
      return PrimitiveType::f64();

    case TokenType::KEYWORD_CHAR:
      return PrimitiveType::char_();
    case TokenType::KEYWORD_BOOL:
      return PrimitiveType::bool_();
    case TokenType::KEYWORD_STRING:
      return PrimitiveType::string();
    case TokenType::KEYWORD_DEVOID:
      return Type::devoid();
    default:
      eden_unreachable("Pointer type not supported.");
    }
  }

  if (token.is(TokenType::IDENTIFIER)) {
    auto const type = module.getCustomType(token.getString(src_file));
    if (not type) report_parsing_error(src_file, token, "Expected typename.");
    return type;
  }

  if (token.is(TokenType::LESS)) {
    TokenView variant_types_tokens = tokens.getTokensBetweenAngleBrackets();
    auto const variant_types_tokens_cpy = variant_types_tokens;

    bool nullable{false};
    std::vector<const Type*> subtypes;
    std::unordered_set<const Type*> typenames; //prevent duplicate types

    do {
      auto const subtype_token = variant_types_tokens.peek();
      auto subtype = parseUnqualifiedType(variant_types_tokens, tu);
      if (subtype->isVariant()) report_parsing_error(src_file, subtype_token, "Nested variant types not allowed.");

      if (subtype->isDevoid()) {
        if (nullable) report_parsing_error(src_file, subtype_token, "Devoid may not be specified more than once in variant declaration.");
        nullable = true;
      }
      else if (typenames.contains(subtype))
        report_parsing_error(src_file, subtype_token, "Duplicate types specified in variant declaration.");
      else typenames.emplace(subtype);

      subtypes.push_back(subtype);
    } while (variant_types_tokens.pop_if(TokenType::COMMA));

    if (subtypes.size() < 2) report_parsing_error(src_file, variant_types_tokens_cpy.viewAsStringToken(), "Two or more types must be specified in variant type list.");

    return module.getVariantType(subtypes, nullable);
  }

  report_parsing_error(src_file, token, "Expected typename.");
  return Type::devoid();
}

template<bool allow_qualifiers>
[[maybe_unused]] [[nodiscard]] auto
parseTypeList(TokenView& tokens, TU& tu) {
  using type = std::conditional_t<allow_qualifiers, InstantiatedType, const Type*>;
  std::vector<type> list;
  list.reserve(2);

  do {
    if constexpr(allow_qualifiers)
      list.emplace_back(parseType(tokens, tu));
    else
      list.emplace_back(parseUnqualifiedType(tokens, tu));
  }
  while (tokens.pop_if(TokenType::COMMA));

  tokens.expect_then_pop(TokenType::GTR, "Expected closing > in type list.");
  return list;
}

[[nodiscard]] InstantiatedType
parseType(TokenView& tokens, TU& tu) {
  const auto qualifiers = parseTypeQualifiers(tokens);
  return {parseUnqualifiedType(tokens, tu), qualifiers};
}

std::string_view
parseIdentifier(TokenView& tokens, TU& tu) {
  Token token = tokens.take();
  token.throw_if_not(TokenType::IDENTIFIER, "Expected identifier.");
  return token.getString(tu.source_files.back());
}

struct Body {
  struct Expression {
    u32_t left_idx;
    u32_t right_idx; // unary expressions do not contain right_idx
    ASTNode::NodeData data;
  };

  // Exists to allow expression parsing w/o allocation
  struct ExpressionTree {
    static constexpr u64_t max_expressiontree_size{256};
    std::array<Expression, max_expressiontree_size> data;
    u64_t begin{};

    u16_t create(ASTNode::NodeData node_data, u32_t left_idx = 0, u32_t right_idx = 0) noexcept {
      assume_assert(left_idx < max_expressiontree_size); assume_assert(right_idx < max_expressiontree_size);
      ++begin; assume_assert(begin < max_expressiontree_size);
      auto& expr = data[begin];
      expr.left_idx = left_idx;
      expr.right_idx = right_idx;
      expr.data = node_data;
      return begin;
    }

    void reset() noexcept {begin = 0;}
  };

  std::vector<ASTNode> tree;
  TokenView tokens;
  TU& tu;
  ExpressionTree& expression_tree;

  Body(TokenView tokens, TU& tu, ExpressionTree& expression_tree)
  : tokens(tokens), tu(tu), expression_tree(expression_tree) {}

  u32_t generateParameters() {
    const auto parameter = generateAssignmentExpression();
    if (tokens.pop_if(TokenType::RPAREN))
      return expression_tree.create(EMPTY_NODE_DATA, 0, parameter, 0);
    tokens.expect_then_pop(TokenType::COMMA, "Expected comma in call expression.");
    return expression_tree.create(ASTNode::EMPTY, 0, parameter, generateParameters());
  }

  u32_t generateSubscript() {
    const auto res = generateAssignmentExpression();
    tokens.expect_then_pop(TokenType::RBRACKET, "Expected closing bracket in subscript expression.");
    return res;
  }

  u32_t generateIdentifier() {
    if (tokens.pop_if(TokenType::DUNDER_CEXTERN))
      return expression_tree.create(ASTNode::IDENTIFIER, std::string_view("__C"));
    tokens.expect(TokenType::IDENTIFIER, "Expected identifier.");
    return expression_tree.create(ASTNode::IDENTIFIER, tokens.take().getString(tu.source_files.back()));
  }

  u32_t generatePrimaryExpression() {
    if (tokens.empty())
      return 0;

    if (tokens.peek().isLiteral()) {
      auto const token = tokens.take();
      ASTNode::Type literal_type;
      switch (token.getType()) {
      case TokenType::SIGNED_LITERAL: literal_type = ASTNode::SIGNED_LITERAL; break;
      case TokenType::UNSIGNED_LITERAL: literal_type = ASTNode::UNSIGNED_LITERAL; break;
      case TokenType::FLOAT_LITERAL: literal_type = ASTNode::FLOAT_LITERAL; break;
      case TokenType::DOUBLE_LITERAL:literal_type = ASTNode::DOUBLE_LITERAL; break;
      case TokenType::BOOL_LITERAL: literal_type = ASTNode::BOOL_LITERAL; break;
      case TokenType::CHAR_LITERAL: literal_type = ASTNode::CHAR_LITERAL; break;
      case TokenType::STRING_LITERAL: literal_type = ASTNode::STRING_LITERAL; break;
      default:
        eden_unreachable("Invalid literal token type.");
      }

      const u64_t literal_value = token.getRawValue();
      return expression_tree.create(literal_type, literal_value);
    }

    if (tokens.pop_if(TokenType::LPAREN)) {
      const auto res = generateExpressionBetweenParenthesis();
      return res;
    }

    return generateIdentifier();
  }

  u32_t generatePostfixExpression() {
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
        if (tokens.peek_ahead(1).is(TokenType::IDENTIFIER)) { // should only occur when accessing a member.
          tokens.pop();
          left = expression_tree.create(
            ASTNode::UNARY, static_cast<u64_t>(Operator::ARROW),
            left, 0);
          left = expression_tree.create(
            ASTNode::MEMBER_ACCESS, 0,
            left, generatePostfixExpression());

          continue;
        }
        value = static_cast<u64_t>(Operator::ARROW);
        break;
      case TokenType::LPAREN: {
        tokens.pop();
        const u16_t parameters = tokens.pop_if(TokenType::RPAREN) ? 0 : generateParameters();
        left = expression_tree.create(ASTNode::CALLING, 0, left, parameters); // pretty sure the 0 is supposed to be a placeholder but I don't remember
        continue;
      }
      case TokenType::LBRACKET:
        tokens.pop();
        tokens.reject(TokenType::RBRACKET, "Expected expression within brackets.");
        left = expression_tree.create(ASTNode::SUBSCRIPT, 0, left, generateSubscript());
        continue;
      case TokenType::DOT:
        tokens.pop();
        left = expression_tree.create(ASTNode::MEMBER_ACCESS, 0, left, generatePostfixExpression());
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

  u32_t generatePrefixExpression() {
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
    case TokenType::KEYWORD_CAST: {
      tokens.pop();
      tokens.expect_then_pop(TokenType::LESS, "Expected opening < in cast.");
      const auto type = std::bit_cast<u64_t>(parseUnqualifiedType(tokens, table));
      tokens.expect_then_pop(TokenType::GTR, "Expected closing > in cast.");
      return expression_tree.create(ASTNode::CAST, type, generatePrefixExpression(), 0);
    }
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

  u32_t generateFactorExpression() {
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

  u32_t generateTermExpression() {
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

  u32_t generateRelationalExpression() {
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

  u32_t generateBitwiseExpression() {
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

  u32_t generateLogicalExpression() {
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

  u32_t generateAssignmentExpression() {
    const auto left = generateLogicalExpression();
    if (tokens.pop_if(TokenType::ASSIGN)) {
      return expression_tree.create(
        ASTNode::BINARY, static_cast<u64_t>(Operator::ASSIGN),
        left, generateAssignmentExpression());
    }

    return left;
  }

  u32_t generateExpressionBetweenParenthesis() {
    const TokenView until_ending = tokens.getTokensBetweenParenthesis();
    const TokenView after_ending = tokens;
    tokens = until_ending;
    const auto res = generateAssignmentExpression();
    if (not tokens.empty())
      throw ParsingError(std::format("Expected {}", ")"), tokens.peek());
    tokens = after_ending;
    return res;
  }

  u32_t generateExpressionUntil(TokenType ending_token) {
    const TokenView until_ending = tokens.getAllTokensUntilFirstOf(ending_token); tokens.pop();
    const TokenView after_ending = tokens;
    tokens = until_ending;
    const auto res = generateAssignmentExpression();
    if (not tokens.empty())
     throw ParsingError(std::format("Expected {}", tokenTypeToString(ending_token)), tokens.peek());
    tokens = after_ending;
    return res;
  }

  void translateExpression(u32_t idx) {
    if (idx == 0) return;

    auto expression = expression_tree.data[idx];
    switch (expression.data.type) { using enum ASTNode::Type;
    case EMPTY:
    case DECLARATION:
    case IF:
    case FOR:
    case WHILE:
    case SCOPED:
    case RETURN:
    case EXPR_STMT:
      eden_unreachable("Statements should not be contained in an expression.");

    case UNARY:
    case CAST:
      tree.emplace_back(expression.data);
      translateExpression(expression.left_idx);
      return;
    case BINARY:
    case SUBSCRIPT:
    case MEMBER_ACCESS:
      tree.emplace_back(expression.data);
      translateExpression(expression.left_idx);
      translateExpression(expression.right_idx);
      return;
    case CALLING: { // i think this might be unnecessarily complicated
      tree.emplace_back(expression.data);
      const auto calling_idx = tree.size() - 1;
      translateExpression(expression.left_idx);
      if (expression.right_idx == 0) return;

      u32_t left_idx = expression_tree.data[expression.right_idx].left_idx;
      u32_t right_idx = expression_tree.data[expression.right_idx].right_idx;
      u64_t num_parameters{1};
      while (true) {
        translateExpression(left_idx);
        if (right_idx == 0) break;
        left_idx = expression_tree.data[right_idx].left_idx;
        right_idx = expression_tree.data[right_idx].right_idx;
        ++num_parameters;
      }

      tree[calling_idx].m.num_parameters = num_parameters;
      return;
    }

    case IDENTIFIER:
    case STRING_LITERAL:
    case SIGNED_LITERAL:
    case UNSIGNED_LITERAL:
    case FLOAT_LITERAL:
    case DOUBLE_LITERAL:
    case BOOL_LITERAL:
    case CHAR_LITERAL:
      tree.emplace_back(expression.data);
      return;

    default:
      eden_unreachable("Invalid ast node type in expression translation.");
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
      tree.emplace_back(ASTNode::EMPTY, 0);
      return;
    }
    const auto idx = generateExpressionUntil(ending_token);
    translateExpression(idx);
  }

  void parseExpressionStatement() {
    tree.emplace_back(ASTNode::EXPR_STMT, 0);
    parseExpressionUntil(TokenType::SEMI_COLON);
  }

  void parseVarDecl() {
    tree.emplace_back(ASTNode::DECLARATION, 0);
    tree.emplace_back(parseType(tokens, table));

    tree.emplace_back(ASTNode::IDENTIFIER,std::bit_cast<u64_t>(parseIdentifier(tokens)));
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
    tree.emplace_back(ASTNode::IF, 0);
    tokens.expect_then_pop(TokenType::LPAREN, "Expected opening parenthesis in if statement condition.");
    tokens.reject(TokenType::RPAREN, "Expected condition in if statement.");
    parseExpressionBetweenParenthesis();
    parseScoped();
    if (tokens.pop_if(TokenType::KEYWORD_ELSE))
      parseStatement();
    else
      tree.emplace_back(ASTNode::EMPTY);
  }

  void parseFor() { assert(false and "Sorry, unimplemented."); }

  void parseWhile() {
    tree.emplace_back(ASTNode::WHILE, 0);
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
          throw ParsingError("Expected ending rbrace to scoped statement.", {});
      }
      tree[scoped_idx].value() = num_statements;
    }
    else
      parseStatement();
  }

  void parseReturn() {
    tree.emplace_back(ASTNode::RETURN, 0);
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
    case TokenType::KEYWORD_RETURN:
      tokens.pop();
      return parseReturn();
    case TokenType::KEYWORD_SWITCH:
      eden_unreachable("Switch keyword not supported.");
    case TokenType::LBRACE:
      return parseScoped();
    case TokenType::LESS:
      return parseVarDecl();

    case TokenType::IDENTIFIER:
      // There should exist no other scenario where there exist two identifiers in a row (hopefully)
      // If that ever changes, could be disambiguated by the fact that there should exist an '=' in a declaration
      // Also, this needs to change if array types can be declared via typename[] varname;
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
  auto& tokens = global_body.tokens;
  auto& table = *tu.module;
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
      parameters.emplace_back(type, parseIdentifier(tokens), false);

      if (not tokens.pop_if(TokenType::COMMA)) {
        tokens.expect_then_pop(TokenType::RPAREN, "Expected closing parenthesis in parameter list.");
        break;
      }
    }

  const Type* return_type = Type::devoid();
  if (tokens.pop_if(TokenType::ARROW)) {
    const auto instance = parseType(tokens, table);
    if (not instance.isUnqualified())
      throw ParsingError("Return type may not have type qualifiers.", instance.toString(), 0);
    return_type = instance.type;
  }

  tokens.expect_then_pop(TokenType::SEMI_COLON, "Expected semi-colon.");

  auto const c_module = getModule("__C");
  auto const parameter_span = std::span(parameters);
  c_module->addFunction(name, parameter_span, return_type, true, is_variadic);
}

void parseStructDecl(TU& tu, Body& global_body) {
  auto& tokens = global_body.tokens;
  auto const name = parseIdentifier(tokens);

  SymbolTable::Variable members[Settings::MAX_STRUCT_MEMBER_VARIABLES];
  tokens.expect_then_pop(TokenType::LBRACE, "Expected opening curly brace in struct definition.");
  auto i{0uz};
  if (tokens.pop_if(TokenType::RBRACE))
    return (void)tu.module->addCustomType(name, {});

  bool in_pub_block = STRUCT_MEMBERS_START_PUBLIC;
  do {
    if (in_pub_block) {
      if (tokens.pop_if(TokenType::COLON)) {
        tokens.expect_then_pop(TokenType::KEYWORD_PUB, "Expected closing 'pub' in 'pub:   :pub' block.");
        if (tokens.peek_is(TokenType::RBRACE)) break;
        in_pub_block = false;
      }
    }
    else {
      if (tokens.pop_if(TokenType::KEYWORD_PUB)) {
        tokens.expect_then_pop(TokenType::COLON, "Expected opening colon in 'pub:   :pub' block.");
        in_pub_block = true;
      }
    }

    auto member_type = parseUnqualifiedType(tokens, *tu.module);
    auto member_name = parseIdentifier(tokens);
    members[i] = {member_type, member_name, in_pub_block};
    ++i;
  }while (tokens.pop_if(TokenType::COMMA) and not tokens.peek_is(TokenType::RBRACE));
  tokens.expect_then_pop(TokenType::RBRACE, "Expected closing curly brace after struct definition.");

  tu.module->addCustomType(name, std::span(members, i));
}

void parseImports(TU& tu, Body& global_body) {
  TokenView& tokens = global_body.tokens;
  while (tokens.pop_if(TokenType::KEYWORD_IMPORT)) {
    tokens.expect(TokenType::IDENTIFIER, "Expected module name.");
    auto name = tokens.take().getString(tu.file_text);
    if (tu.module->nameof() == name)
      throw ValidationError("Cannot import from current module!", std::string(name), 0);

    tu.imports.emplace_back(name);
    tokens.expect_then_pop(TokenType::SEMI_COLON, "Expected semicolon.");
  }
}

void parseFunctionsAndStructs(TU& tu, Body& global_body, std::vector<Function>& functions) {
  TokenView& tokens = global_body.tokens;
  Module& table = global_body.table;
  std::vector<TokenView> function_tokens;

  // parse the declarations to load into symbol table
  while (not tokens.empty()) {
    if (tokens.pop_if(TokenType::DUNDER_CEXTERN)) {
      parseCExtern(tu, global_body);
      continue;
    }
    if (tokens.pop_if(TokenType::KEYWORD_STRUCT)) {
      parseStructDecl(tu, global_body);
      continue;
    }

    Function& function = functions.emplace_back();
    const bool is_public = tokens.pop_if(TokenType::KEYWORD_PUB);
    function.is_public = is_public;

    tokens.expect_then_pop(TokenType::KEYWORD_FN, "Expected function declaration.");
    function.name = parseIdentifier(tokens);
    tokens.expect_then_pop(TokenType::LPAREN, "Expected parameter list.");

    std::vector<SymbolTable::Variable> parameters; parameters.reserve(4);
    if (not tokens.pop_if(TokenType::RPAREN))
      while (true) {
        auto type = parseType(tokens, table);
        auto name = parseIdentifier(tokens);
        parameters.emplace_back(type, name, false);

        if (not tokens.pop_if(TokenType::COMMA)) {
          tokens.expect_then_pop(TokenType::RPAREN, "Expected closing parenthesis in parameter list.");
          break;
        }
      }

    const Type* return_type = Type::devoid();
    if (not tokens.peek_is(TokenType::LBRACE)) {
      InstantiatedType return_instance = parseType(tokens, table);
      if (not return_instance.isUnqualified())
        throw ParsingError("Return type may not have type qualifiers.", return_instance.toString(), 0);
      return_type = return_instance.type;
    }

    tokens.expect_then_pop(TokenType::LBRACE, "Expected function definition.");
    function_tokens.emplace_back(tokens.getTokensBetweenBraces());

    table.addFunction(function.name, std::span(parameters), return_type, is_public);
  }

  // actually parse the function bodies
  const auto sz = functions.size();
  assert(sz == function_tokens.size());
  for (auto i{0uz}; i < sz; ++i) {
    Body function_body
    (function_tokens[i], table, global_body.expression_tree);

    table.enterFunctionScope(functions[i].name);
    function_body.parseStatementsUntilEmpty();
    functions[i].body.nodes = std::move(function_body.tree);
  }
}

void printFunction(Function const& func, Module const& table, u64_t ln) {
  auto const function = table.getFunction(func.name);
  auto const parameters = function->parameters();
  auto const return_type = function->returnType();

  std::print("id: {}\nfn {} (", function->get_id(), func.name);

  for (auto &parameter : parameters) {
    std::cout << parameter.type.toString();
    std::cout << " " << parameter.name << ", ";
  }

  if (not parameters.empty())
    std::cout << "\b\b";

  std::cout << ")";
  if (not return_type->isDevoid()) {
    std::cout << " -> ";
    std::cout << return_type->toString();
  }
  std::cout << " { ";
  func.body.print(ln);
  std::cout << "\n}";
}

}

void Parser::printTU(TU& ptu) {
  for (auto& import : ptu.imports) {
    std::println("import {};", std::string_view(import));
  }
  if (not ptu.global_tree.nodes.empty()) {
    ptu.global_tree.print(1);
    std::println();
  }
  for (const auto &f : ptu.functions) {
    printFunction(f, *ptu.module, 1);
    std::println();
  }
}

void Parser::parseTokens(TU& tu, std::vector<Token>& tokens) {
  Body::ExpressionTree expression_tree;
  Body global_body(TokenView{tokens}, *tu.module, expression_tree);

  parseImports(tu, global_body);

  // parse globals here

  tu.global_tree.nodes = std::move(global_body.tree);
  parseFunctionsAndStructs(tu, global_body, tu.functions);
}