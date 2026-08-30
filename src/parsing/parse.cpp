#include "parse.hpp"

#include "edenlib/vectors/swap_vector.hpp"

#include "ast.hpp"
#include "build_system/build.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "module/table_and_module.hpp"
#include "module/types.hpp"

#include <chrono>
#include <numeric>
#include <print>
#include <utility>

using namespace LOM;
using namespace LOM::Lexer;
using namespace LOM::Parser;
using namespace LOM::AST;

namespace {

edenNoInlineCold [[noreturn]] void throw_notfound() edenThrows(0) { throw 0; }

struct Expression {
  u32_t left_idx;
  u32_t right_idx; // unary expressions do not contain right_idx
  ASTNode node;
};

// Exists to allow expression parsing w/o allocation
struct ExpressionTree {
  static constexpr u64_t max_expressiontree_size{256};
  std::array<Expression, max_expressiontree_size> data;
  u64_t begin{};

  edenNodiscardCXPR u32_t
  create(ASTNode node, u32_t left_idx = 0, u32_t right_idx = 0) noexcept {
    assert(left_idx < max_expressiontree_size); assert(right_idx < max_expressiontree_size);
    ++begin; assert(begin < max_expressiontree_size);

    auto& expr = data[begin];
    expr.left_idx = left_idx;
    expr.right_idx = right_idx;
    expr.node = node;
    return begin;
  }

  edenInlineCXPR void reset() noexcept { begin = 0; }
};

class ParserBody {
  static constexpr auto name_search = [] (std::string_view e, std::string_view key) static { return e == key; };
  eden::swap_vector<std::string_view> imports;
  eden::vector<ASTNode> nodes;
  TokenView tokens;
  File current_file;
  TU& tu;
  Module& module;
  bool has_errors{};
  ExpressionTree expression_tree;

  constexpr ParserBody(eden::vector<Token>& tokens, TU& tu) noexcept
  : tokens(tokens), current_file(tu.source_files.back()), tu(tu), module(*tu.module) {
    imports.reserve(2);
    imports.emplace_back("__C");
  }

  edenNodiscardCXPR u8_t
  current_file_idx() const noexcept {
    assert((tu.source_files.size() - 1) <= u8_max);
    return u8_t(tu.source_files.size() - 1);
  }

  // adds an uninitialized node containing only the current file idx and node_type
  // returns index of node
  edenInlineNodiscardCXPR sz_t
  insertTypedNode(ASTNode::NodeType node_type) noexcept {
    auto const new_node_idx = nodes.size();
    nodes.emplace_back(node_type, current_file_idx());
    return new_node_idx;
  }

  edenInlineNodiscardCXPR ASTNode
  newNode(Token token, ASTNode::NodeType type = ASTNode::EMPTY) const noexcept {
    return ASTNode{
            .type = type,
            .file_idx = current_file_idx(),
            .length_in_file = token.length, .position_in_file = token.position,
            .base = 0
    };
  }

  edenNoInlineCold void
  error(Token err, std::string_view msg) noexcept {
    report_error(current_file, err, std::string(msg)); has_errors = true;
  }

#define pre assert(tokens.previous().is(TokenType::LBRACKET));
  edenNodiscardCXPR TypeID parseArrayType() noexcept { pre
    auto const& current_file = tu.source_files.back();
    auto const token = tokens.take();
    u64_t array_size;

    if (not token.is(TokenType::INTEGER_LITERAL))
      error(token, "Expected integer literal in array type."), array_size = 1;
    else array_size = token.getInteger(current_file);

    if (not tokens.pop_if(TokenType::RBRACKET)) {
      error(tokens.peek(), "Expected ] while parsing array type.");
      tokens.pop();
    }

    auto const subtype = parseType();
    return module.getArrayType(subtype, array_size);
  }
#undef pre

#define pre assert(pointer_token.isPointer());
  edenNodiscardCXPR TypeID parsePointerType(Token pointer_token) noexcept { pre
    switch (pointer_token.type) {
    case TokenType::KEYWORD_RAW: return module.getRawPointerType(parseType());
    case TokenType::KEYWORD_REF: return module.getRefPointerType(parseType());
    default: edenUnreachable("Pointer type unsupported.");
    }
  }
#undef pre

#define pre assert(primitive_token.isPrimitive());
  edenNodiscardCXPR TypeID parsePrimitiveType(Token primitive_token) const noexcept { pre
    switch (primitive_token.type) {
    case TokenType::KEYWORD_i8:     return i8_literal.toTypeID();
    case TokenType::KEYWORD_i16:    return i16_literal.toTypeID();
    case TokenType::KEYWORD_i32:    return i32_literal.toTypeID();
    case TokenType::KEYWORD_i64:    return i64_literal.toTypeID();
    case TokenType::KEYWORD_u8:     return u8_literal.toTypeID();
    case TokenType::KEYWORD_u16:    return u16_literal.toTypeID();
    case TokenType::KEYWORD_u32:    return u32_literal.toTypeID();
    case TokenType::KEYWORD_u64:    return u64_literal.toTypeID();
    case TokenType::KEYWORD_f32:    return f32_literal.toTypeID();
    case TokenType::KEYWORD_f64:    return f64_literal.toTypeID();

    case TokenType::KEYWORD_CHAR:   return char_literal.toTypeID();
    case TokenType::KEYWORD_BOOL:   return bool_literal.toTypeID();
    case TokenType::KEYWORD_STRING: return string_literal.toTypeID();
    case TokenType::KEYWORD_DEVOID: return devoid_literal.toTypeID();
    default: edenUnreachable("Primitive type not supported.");
    }
  }
#undef pre

  edenNodiscardCXPR TypeID parseType() noexcept {
    auto const& current_file = tu.source_files.back();
    auto const token = tokens.take();

    switch (token.type) { using enum TokenType;
    TOKENTYPE_PRIMITIVES_CASES return parsePrimitiveType(token);
    TOKENTYPE_POINTERS_CASES   return parsePointerType(token);
    case LBRACKET:             return parseArrayType();

    case IDENTIFIER: {
      auto const type = module.getCustomType(token.originalString(current_file));
      if (type == error_literal.toTypeID())
        error(token, "Expected typename.");
      return type;
    }

    default:
      error(token, "Expected typename.");
      return error_literal.toTypeID();
    }
  }

  edenNodiscardCXPR std::string_view parseIdentifier() noexcept {
    if (not tokens.peek_is(TokenType::IDENTIFIER)) {
      error(tokens.take_if_valid(), "Expected identifier.");
      return "!ERROR!";
    }
    return tokens.take().originalString(tu.source_files.back());
  }

  // opening parenthesis must be popped, and the next token must not be closing parenthesis (?)
#define pre assert(not tokens.previous().is(TokenType::LPAREN));
  edenNodiscardCXPR u32_t generateParameters() { pre
    auto const parameter = generateAssignmentExpression();
    if (tokens.pop_if(TokenType::RPAREN))
      return expression_tree.create(PLACEHOLDER_NODE, parameter, 0);

    if (tokens.pop_if(TokenType::COMMA))
      return expression_tree.create(PLACEHOLDER_NODE, parameter, generateParameters());

    error(tokens.peek(), "Expected comma in call expression.");
    return expression_tree.create(PLACEHOLDER_NODE, parameter, generateParameters());
  }
#undef pre

#define pre assert(tokens.peek().isLiteral());
  template <bool negate = false>
  edenNodiscardCXPR u32_t generateLiteral() { pre
    auto const token = tokens.take();
    ASTNode node = newNode(token);

    switch (token.type) {
    case TokenType::INTEGER_LITERAL: {
      if constexpr (negate) {
        node.type = ASTNode::SIGNED_LITERAL;
        node.signed_data.value = -static_cast<i64_t>(token.getInteger(current_file));
        break;
      }
      else {
        node.type = ASTNode::UNSIGNED_LITERAL;
        node.unsigned_data.value = token.getInteger(current_file);
        break;
      }
    }
    case TokenType::FLOAT_LITERAL:  node.type = ASTNode::FLOAT_LITERAL;
      node.float_data.value = negate ? -token.getFloat(current_file) : token.getFloat(current_file);
      break;
    case TokenType::DOUBLE_LITERAL: node.type = ASTNode::DOUBLE_LITERAL;
      node.double_data.value = negate ? -token.getDouble(current_file) : token.getDouble(current_file);
      break;
    case TokenType::BOOL_LITERAL:   node.type = ASTNode::BOOL_LITERAL;
      node.bool_data.value = negate ? not token.getBool(current_file) : token.getBool(current_file);
      break;
    case TokenType::CHAR_LITERAL:   node.type = ASTNode::CHAR_LITERAL;
      node.char_data.value = negate ? -token.getChar(current_file) : token.getChar(current_file);
      break;

    case TokenType::STRING_LITERAL:           node.type = ASTNode::STRING_LITERAL; assert(not negate); break;
    case TokenType::ESCAPED_STRING_LITERAL:   node.type = ASTNode::ESCAPED_STRING_LITERAL; assert(not negate); break;
    default: edenUnreachable("Invalid literal token type.");
    }

    return expression_tree.create(node);
  }
#undef pre

#define pre assert(module_name_token.isIdentifier() or module_name_token.is(TokenType::DUNDER_CEXTERN)); assert(tokens.peek_is(TokenType::DOT));
  edenNodiscardCXPR u32_t generateModuleAccess(Token module_name_token) { pre
    auto const dot_token = tokens.take();
    auto module_access_node = newNode(dot_token, ASTNode::MODULE_ACCESS);
    module_access_node.module_access_data.module_length = module_name_token.length;

    if (not tokens.peek_is(TokenType::IDENTIFIER))
      error(tokens.peek(), "Expected identifier.");

    auto const member_token = tokens.take(); assert(member_token.position == dot_token.position + 1);
    module_access_node.module_access_data.member_length = member_token.length;

    static_assert(not Settings::SUBMODULE_SUPPORT); // does not allow for submodules TODO: FIX WHEN ADDING SUBMODULES
    return expression_tree.create(module_access_node);
  }
#undef pre

  edenNodiscardCXPR u32_t generatePrimaryExpression() {
    switch (tokens.peek().type) { using enum TokenType;
    TOKENTYPE_LITERALS_CASES return generateLiteral();
    case DUNDER_CEXTERN:     return generateModuleAccess(tokens.take());
    case LPAREN: {
      tokens.pop();
      auto const res = generateAssignmentExpression();
      if (not tokens.pop_if(RPAREN)) error(tokens.take_if_valid(), "Expected closing ).");
      return res;
    }

    case IDENTIFIER: {
      auto const identifier_node = tokens.take();
      if (not tokens.peek_is(DOT))
        return expression_tree.create( newNode(identifier_node, ASTNode::IDENTIFIER) );

      if (imports.search(name_search, identifier_node.originalString(current_file)))
        return generateModuleAccess(identifier_node);

      auto const dot_token = tokens.take();
      auto const identifier_idx = expression_tree.create( newNode(identifier_node, ASTNode::IDENTIFIER) );
      auto const member_idx = generatePrimaryExpression();
      auto const member_access_node = newNode(dot_token, ASTNode::MEMBER_ACCESS);
      return expression_tree.create(member_access_node, identifier_idx, member_idx);
    }

    default: throw_notfound();
    }
  }

  edenNodiscardCXPR u32_t generatePostfixExpression() {
    auto left = generatePrimaryExpression();
    while (true) {
      auto const token = tokens.peek();
      Operator opr;
      switch (token.type) {
      case TokenType::PLUSPLUS: opr = Operator::POST_INCREMENT; break;
      case TokenType::MINUSMINUS: opr = Operator::POST_DECREMENT; break;
      case TokenType::ARROW:
        if (not tokens.peek_is(TokenType::IDENTIFIER)) { opr = Operator::ARROW; break; }

        tokens.pop();
        left = expression_tree.create(newNode(token, ASTNode::MEMBER_ACCESS), left, generatePrimaryExpression()); // identifier following arrow should only occur when accessing a member
        continue;

        case TokenType::DOT:
        tokens.pop();
        left = expression_tree.create(newNode(token, ASTNode::MEMBER_ACCESS), left, generatePostfixExpression());
        continue;

      case TokenType::LPAREN:
        tokens.pop();
        left = expression_tree.create(
          newNode(token, ASTNode::CALLING),
          left,
          tokens.pop_if(TokenType::RPAREN) ? 0 : generateParameters()
          );
        continue;

      case TokenType::LBRACKET:
        tokens.pop();
        left = expression_tree.create(
          newNode(token, ASTNode::SUBSCRIPT),
          left,
          generateAssignmentExpression()
          );

        if (not tokens.pop_if(TokenType::RBRACKET)) {
          error(tokens.take_if_valid(), "Expected closing ].");
          sync_to_semicolon();
        }

        continue;

      default: return left;
      }

      auto node = newNode(token, ASTNode::UNARY); node.unary_data.opr = opr;
      tokens.pop();
      left = expression_tree.create(node, left);
    }
  }

  edenNodiscardCXPR u32_t generatePrefixExpression() {
    auto const token = tokens.peek();
    Operator opr;
    switch (token.type) {
    case TokenType::PLUSPLUS:           opr = Operator::PRE_INCREMENT; break;
    case TokenType::MINUSMINUS:         opr = Operator::PRE_DECREMENT; break;
    case TokenType::ADDR:               opr = Operator::ADDRESS_OF; break;
    case TokenType::AMPERSAND:          opr = Operator::REF_TO; break;
    case TokenType::KEYWORD_NOT:        opr = Operator::NOT; break;
    case TokenType::KEYWORD_BITNOT:     opr = Operator::BITNOT; break;
    case TokenType::KEYWORD_CAST: {
      auto node = newNode(token, ASTNode::CAST); tokens.pop();
      if (not tokens.pop_if(TokenType::LESS)) error(tokens.peek(), "Expected opening < in cast.");
      node.cast_data.cast_type = parseType();
      if (not tokens.pop_if(TokenType::GTR)) error(tokens.peek(), "Expected opening > in cast.");

      return expression_tree.create(node, generatePrefixExpression(), 0);
    }

    case TokenType::MINUS: {
      if (tokens.peek_ahead(1).isNumericLiteral()) {
        tokens.pop();
        return generateLiteral<true>();
      }

      opr = Operator::UNARY_MINUS; break;
    }

    default: return generatePostfixExpression();
    }

    auto node = newNode(token, ASTNode::UNARY); node.unary_data.opr = opr;
    tokens.pop();
    return expression_tree.create(node, generatePrefixExpression());
  }

  edenNodiscardCXPR u32_t generateFactorExpression() {
    auto left = generatePrefixExpression();
    while (true) {
      auto const token = tokens.peek();
      Operator opr;
      switch (token.type) {
      case TokenType::STAR: opr = Operator::MULTIPLY; break;
      case TokenType::SLASH: opr = Operator::DIVIDE; break;
      case TokenType::MOD: opr = Operator::MODULUS; break;
      default: return left;
      }

      auto node = newNode(token, ASTNode::BINARY); node.unary_data.opr = opr;
      tokens.pop();
      left = expression_tree.create(node, left, generatePrefixExpression());
    }
  }

  edenNodiscardCXPR u32_t generateTermExpression() {
    auto left = generateFactorExpression();
    while (true) {
      auto const token = tokens.peek();
      Operator opr;
      switch (token.type) {
      case TokenType::PLUS: opr = Operator::ADD; break;
      case TokenType::MINUS: opr = Operator::SUBTRACT; break;
      default: return left;
      }

      auto node = newNode(token, ASTNode::BINARY); node.binary_data.opr = opr;
      tokens.pop();
      left = expression_tree.create(node, left, generateFactorExpression());
    }
  }

  edenNodiscardCXPR u32_t generateRelationalExpression() {
    auto left = generateTermExpression();
    while (true) {
      auto const token = tokens.peek();
      Operator opr;
      switch (token.type) {
      case TokenType::KEYWORD_EQUALS: opr = Operator::EQUAL; break;
      case TokenType::KEYWORD_NOT_EQUAL: opr = Operator::NOT_EQUAL; break;
      case TokenType::LESS: opr = Operator::LESS; break;
      case TokenType::GTR: opr = Operator::GREATER; break;
      case TokenType::LESSEQ: opr = Operator::LESS_EQUAL; break;
      case TokenType::GTREQ: opr = Operator::GREATER_EQUAL; break;
      default: return left;
      }

      auto node = newNode(token, ASTNode::BINARY); node.binary_data.opr = opr;
      tokens.pop();
      left = expression_tree.create(node, left, generateTermExpression());
    }
  }

  edenNodiscardCXPR u32_t generateBitwiseExpression() {
    auto left = generateRelationalExpression();
    while (true) {
      auto const token = tokens.peek();
      Operator opr;
      switch (token.type) {
      case TokenType::KEYWORD_BITAND: opr = Operator::BITAND; break;
      case TokenType::KEYWORD_BITOR: opr = Operator::BITOR; break;
      case TokenType::KEYWORD_BITXOR: opr = Operator::BITXOR; break;
      default: return left;
      }

      auto node = newNode(token, ASTNode::BINARY); node.binary_data.opr = opr;
      tokens.pop();
      left = expression_tree.create(node, left, generateRelationalExpression());
    }
  }

  edenNodiscardCXPR u32_t generateLogicalExpression() {
    auto left = generateBitwiseExpression();
    while (true) {
      auto const token = tokens.peek();
      Operator opr;
      switch (token.type) {
      case TokenType::KEYWORD_AND: opr = Operator::AND; break;
      case TokenType::KEYWORD_OR: opr = Operator::OR; break;
      case TokenType::KEYWORD_XOR: opr = Operator::XOR; break;
      default: return left;
      }

      auto node = newNode(token, ASTNode::BINARY); node.binary_data.opr = opr;
      tokens.pop();
      left = expression_tree.create(node, left, generateBitwiseExpression());
    }
  }

  edenNodiscardCXPR u32_t generateAssignmentExpression() {
    auto const left = generateLogicalExpression();
    if (tokens.peek_is(TokenType::ASSIGN)) {
      auto const token = tokens.take();
      auto node = newNode(token, ASTNode::BINARY);
      node.binary_data.opr = Operator::ASSIGN;

      return expression_tree.create(node, left, generateAssignmentExpression());
    }

    return left;
  }

#define pre assert(idx not_eq 0);
  constexpr void translateExpression(u32_t idx) noexcept { pre
    auto expression = expression_tree.data[idx];
    switch (expression.node.type) { using enum ASTNode::NodeType;
    case EMPTY: case DECLARATION: case IF:
    case WHILE: case RETURN:
      edenUnreachable("Statements should not be contained in an expression.");

    case UNARY: case CAST:
      nodes.emplace_back(expression.node);
      translateExpression(expression.left_idx);
      return;

    case BINARY: case MEMBER_ACCESS: case SUBSCRIPT:
      nodes.emplace_back(expression.node);
      translateExpression(expression.left_idx);
      translateExpression(expression.right_idx);
      return;

    case CALLING: { // i think this might be unnecessarily complicated
      auto const calling_idx = nodes.size();
      nodes.emplace_back(expression.node);
      translateExpression(expression.left_idx);
      if (expression.right_idx == 0) {
        nodes[calling_idx].call_data.num_parameters = 0;
        return;
      }

      auto left_idx = expression_tree.data[expression.right_idx].left_idx;
      auto right_idx = expression_tree.data[expression.right_idx].right_idx;
      u64_t num_parameters{1};
      while (true) {
        translateExpression(left_idx);
        if (right_idx == 0) break;

        left_idx = expression_tree.data[right_idx].left_idx;
        right_idx = expression_tree.data[right_idx].right_idx;
        ++num_parameters;
      }

      nodes[calling_idx].call_data.num_parameters = num_parameters;
      return;
    }

    case MODULE_ACCESS:
    case IDENTIFIER: case STRING_LITERAL: case ESCAPED_STRING_LITERAL: case SIGNED_LITERAL: case UNSIGNED_LITERAL:
    case FLOAT_LITERAL: case DOUBLE_LITERAL: case BOOL_LITERAL: case CHAR_LITERAL:
      nodes.emplace_back(expression.node);
      return;

    default: edenUnreachable("Invalid ast node type in expression translation.");
    }
  }
#undef pre

  // don't access anything above this method directly through anything below this method or i'll hurt you
  template <bool expression_statement = false>
  constexpr void parseExpression() noexcept {
    expression_tree.reset();

    try { translateExpression( generateAssignmentExpression() ); }
    catch (...) {
      if constexpr (expression_statement) {
        error(tokens.peek(), "Expected expression.");
        tokens.set_peek(TokenType::SEMI_COLON); // trick into avoiding double error
      }
      else error(tokens.take_if_valid(), "Expected expression.");

      nodes.emplace_back(PLACEHOLDER_NODE);
    }
  }

  constexpr void sync_to(TokenType sync_type) noexcept {
    while (not tokens.pop_if(sync_type)) {
      if (tokens.peek_is(TokenType::INVALID_TOKEN)) return;
      tokens.pop();
    }
  }

  edenInlineCXPR void sync_to_semicolon() noexcept { return sync_to(TokenType::SEMI_COLON); }

  // parses  name: qualified_type
  // returns name_token and qualified_type
  template <bool is_parameter = false>
  edenNodiscardCXPR std::pair<Token, QualifiedTypeID>
  parseHalfDeclaration() noexcept {
    auto const identifier_token = tokens.take();
    QualifiedTypeID declaration_type;

    if (not identifier_token.isIdentifier())  error(identifier_token, "Expected identifier.");

    if (tokens.pop_if(TokenType::COLON)) declaration_type.qualifiers.writable = false;
    else if (tokens.pop_if(TokenType::DOLLAR)) {
      if constexpr(is_parameter)
        error(tokens.previous(), "Readwrite parameters are not allowed.");
      else
        declaration_type.qualifiers.writable = true;
    }
    else
      error(tokens.peek(), "Expected : or $ in declaration.");

    auto const type = parseType();
    declaration_type.module_id = type.module_id;
    declaration_type.id = type.id;
    return {identifier_token, declaration_type};
  }

#define pre assert(tokens.peek().isIdentifier());
  constexpr Token parseVarDecl(sz_t decl_node_idx) noexcept { pre
    auto& node = nodes[decl_node_idx];

    auto const [identifier_token, qualified_typeID] = parseHalfDeclaration();
    node.declaration_data.type = qualified_typeID;
    node.file_idx = current_file_idx();
    node.length_in_file = identifier_token.length;
    node.position_in_file = identifier_token.position;

    if (not tokens.pop_if(TokenType::ASSIGN)) {
      auto const err = tokens.peek();
      error(err, "Expected assignment in variable declaration. Use = junk; if you'd like to keep the variable uninitialized.");
      sync_to_semicolon();
      return err;
    }

    if (tokens.pop_if(TokenType::KEYWORD_JUNK))
      node.type = ASTNode::DECLARATION_JUNK;
    else
      parseExpression();

    if (not tokens.peek_is(TokenType::SEMI_COLON)) {
      auto const err = tokens.peek();
      error(err, "Expected semicolon ending variable declaration.");
      sync_to_semicolon();
      return err;
    }

    return tokens.take();
  }
#undef pre

#define pre assert(tokens.previous().is(TokenType::KEYWORD_IF));
  constexpr Token parseIf(sz_t if_node_idx) { pre
    parseExpression();
    if (not tokens.pop_if(TokenType::LBRACE)) {
      error(tokens.peek(), "Expected {.");
    }

    auto num_substatements{0uz};
    while (not tokens.peek_is(TokenType::RBRACE)) {
      parseStatement();
      ++num_substatements;
      if (tokens.peek().isInvalid()) {
        error(tokens.peek(), "Expected closing } in scoped statement.");
        break;
      }
    }

    auto& if_data = nodes[if_node_idx].if_data;
    if_data.num_substatements = num_substatements;
    auto const final_token = tokens.take();

    if (tokens.pop_if(TokenType::KEYWORD_ELSE)) {
      if_data.has_else = true;
      return parseStatement();
    }

    if_data.has_else = false;
    return final_token;
  }
#undef pre

#define pre assert(tokens.previous().is(TokenType::KEYWORD_WHILE));
  constexpr Token parseWhile(sz_t while_node_idx) { pre
    parseExpression();
    if (not tokens.pop_if(TokenType::LBRACE)) {
      error(tokens.peek(), "Expected {.");
    }

    sz_t num_substatements = 0;
    while (not tokens.peek_is(TokenType::RBRACE)) {
      parseStatement();
      ++num_substatements;
      if (tokens.peek().isInvalid()) {
        error(tokens.peek(), "Expected closing } in scoped statement.");
        break;
      }
    }

    nodes[while_node_idx].while_data.num_substatements = num_substatements;
    return tokens.take();
  }
#undef pre

#define pre assert(tokens.previous().is(TokenType::KEYWORD_RETURN));
  constexpr Token parseReturn(sz_t return_node_idx) { pre
    auto& return_data = nodes[return_node_idx].return_data;
    if (tokens.peek_is(TokenType::SEMI_COLON)) {
      return_data.has_value = false;
      return tokens.take();
    }

    return_data.has_value = true;
    parseExpression();
    if (not tokens.peek_is(TokenType::SEMI_COLON)) {
      error(tokens.peek(), "Expected semi-colon.");
      return tokens.peek();
    }

    return tokens.take();
  }
#undef pre

  constexpr Token parseStatement() {
    auto const first = tokens.peek();
    sz_t stmt_idx;
    Token final;

    switch (first.type) { using enum TokenType;
    case SEMI_COLON:      return tokens.take();
    case KEYWORD_IF:      tokens.pop(); stmt_idx = insertTypedNode(ASTNode::IF);      final = parseIf(stmt_idx);     break;
    case KEYWORD_WHILE:   tokens.pop(); stmt_idx = insertTypedNode(ASTNode::WHILE);   final = parseWhile(stmt_idx);  break;
    case KEYWORD_RETURN:  tokens.pop(); stmt_idx = insertTypedNode(ASTNode::RETURN);  final = parseReturn(stmt_idx); break;
    case LBRACE:          tokens.pop(); parseStatementsBetweenBraces(); return tokens.previous();

    case IDENTIFIER:
      if (tokens.peek_ahead(1).isVarQualifier()) {
        stmt_idx = insertTypedNode(ASTNode::DECLARATION);
        final = parseVarDecl(stmt_idx);
        break;
      }

      [[fallthrough]];
    default: // expression statement
      stmt_idx = nodes.size();
      parseExpression<true>();
      if (not tokens.peek_is(SEMI_COLON)) {
        error(tokens.peek(), "Expected semi-colon.");
        final = tokens.previous();
      }
      else { final = tokens.previous(); tokens.pop(); }
    }

    auto const combined = Token::combine(first, final);
    auto& stmt_node = nodes[stmt_idx];
    stmt_node.length_in_file = combined.length;
    stmt_node.position_in_file = combined.position;
    return combined;
  }

#define pre  assert(tokens.previous().is(TokenType::LBRACE));
#define post assert(tokens.previous().is(TokenType::RBRACE) or tokens.peek().isInvalid());
  void parseStatementsBetweenBraces() { pre
    while (not tokens.pop_if(TokenType::RBRACE)) {
      parseStatement();
      if (tokens.peek_is(TokenType::INVALID_TOKEN)) {
        error(tokens.peek(), "Expected closing }.");
        break;
      }
    }
  post }
#undef pre
#undef post

#define pre assert(tokens.previous().is(TokenType::DUNDER_CEXTERN));
  void parseCExtern() noexcept { pre
    auto const name = parseIdentifier();
    if (not tokens.pop_if(TokenType::LPAREN)) {
      error(tokens.peek(), "Expected opening ( for parameter list.");
      return sync_to_semicolon();
    }

    eden::swap_vector16<Module::Variable> parameters; parameters.reserve(4);
    bool is_variadic = false;
    if (tokens.peek_is(TokenType::RPAREN)) goto end_params;

    while (true) {
      if (tokens.pop_if(TokenType::DUNDER_VA)) {
        is_variadic = true;
        break;
      }

      auto const [identifier_token, type] = parseHalfDeclaration<true>();
      auto const parameter_idx = parameters.size();
      parameters.emplace_back(type, identifier_token.originalString(current_file), false, parameter_idx);

      if (not tokens.pop_if(TokenType::COMMA)) break;
      if (parameter_idx + 1 == Settings::MAX_FUNCTION_PARAMETERS) {
        error(tokens.peek(), std::format("Functions may have no more than {} parameters.", Settings::MAX_FUNCTION_PARAMETERS));
        sync_to_semicolon();
        return;
      }
    }

    end_params:
    if (not tokens.pop_if(TokenType::RPAREN))
      error(tokens.peek(), "Expected closing parenthesis in parameter list.");

    auto returnTypeID = devoid_literal.toTypeID();
    if (not tokens.peek_is(TokenType::SEMI_COLON))
      returnTypeID = parseType();

    if (not tokens.pop_if(TokenType::SEMI_COLON))
      return error(tokens.peek(), "Expected semi-colon.");

    static_assert(Settings::MULTITHREADING_SUPPORT == false); // needs to be protected
    auto& cModule = getCModule();
    auto const functionTypeID = cModule.getFunctionType(parameters, returnTypeID, is_variadic);
    cModule.addFunction(name, std::move(parameters), functionTypeID, true);
  }
#undef pre

#define pre assert(tokens.previous().is(TokenType::KEYWORD_IMPORT));
  void parseImport() noexcept { pre
    auto const& current_file = tu.source_files.back();
    auto const name_token = tokens.take();
    if (not name_token.isIdentifier()) {
      error(name_token, "Expected module name.");
      return;
    }

    auto name = name_token.originalString(current_file);
    if (tu.name == name)
      error(name_token, "Cannot import from current module.");
    else
      imports.emplace_back(name);

    if (not tokens.pop_if(TokenType::SEMI_COLON)) {
      error(tokens.peek(), "Expected semicolon.");
    }

  }
#undef pre

#define pre assert(tokens.previous().isVarQualifier());
  void parseStructDecl(std::string_view name, [[maybe_unused]] bool is_public) noexcept { pre
    if (tokens.pop_if(TokenType::RBRACE))
      return (void)module.addCustomType(name, {});

    eden::swap_vector<SymbolTable::Variable> members; members.reserve(2);
    do {
      auto const [member_name, member_type] = parseHalfDeclaration();
      members.emplace_back(member_type, member_name.originalString(current_file), true, members.size());
    } while (tokens.pop_if(TokenType::COMMA) and not tokens.peek_is(TokenType::RBRACE));

    if (not tokens.pop_if(TokenType::RBRACE))
      error(tokens.peek(), "Expected closing curly brace after struct definition.");

    (void)module.addCustomType(name, std::move(members));
  }

  void parseFunctionDecl(std::string_view name, bool is_public) noexcept { pre
    Function current_function;
    current_function.file_idx = u8_t(tu.source_files.size() - 1);
    current_function.is_public = is_public;
    current_function.name_len = name.length();
    current_function.name_ptr = name.data();

    if (not tokens.pop_if(TokenType::LPAREN))
      error(tokens.peek(), "Expected parameter list.");

    // parameters
    {
      eden::swap_vector16<SymbolTable::Variable> parameters;
      if (tokens.peek_is(TokenType::RPAREN)) goto end_params;

      while (true) {
        auto const [name_token, type] = parseHalfDeclaration<true>();
        auto const parameter_idx = parameters.size();
        parameters.emplace_back(type, name_token.originalString(current_file), false, parameter_idx);
        if (not tokens.pop_if(TokenType::COMMA)) break;
        if (parameter_idx + 1 == Settings::MAX_FUNCTION_PARAMETERS) {
          error(tokens.peek(), std::format("Functions may have no more than {} parameters.", Settings::MAX_FUNCTION_PARAMETERS));
          break;
        }
      }

      end_params:
      if(not tokens.pop_if(TokenType::RPAREN))
        error(tokens.peek(), "Expected closing parenthesis in parameter list.");

      auto returnTypeID = devoid_literal.toTypeID();
      if (not tokens.peek_is(TokenType::LBRACE))
        returnTypeID = parseType();

      auto const functionTypeID = module.getFunctionType(parameters, returnTypeID, false);
      module.addFunction(
        current_function.nameof(),
        std::move(parameters),
        functionTypeID,
        current_function.is_public);
      module.enterFunctionScope(current_function.nameof());
    }

    if (not tokens.pop_if(TokenType::LBRACE)) {
      error(tokens.peek(), "Expected function definition.");
      tu.functions.emplace_back(std::move(current_function));
      return;
    }

    // body
    {
      parseStatementsBetweenBraces();
      current_function.body = std::move(nodes);
    }

    tu.functions.emplace_back(std::move(current_function));
  }
#undef pre

  void parseGlobalLevelDeclaration(bool is_public) noexcept {
      auto const name = parseIdentifier();
      if (not tokens.peek().isVarQualifier())
        error(tokens.peek(), "Expected declaration qualifier ( : or $ ).");
      else if (tokens.peek_is(TokenType::DOLLAR))
        error(tokens.take(), "$ qualifiers currently not supported on functions or structs, sorry!");
      else
        tokens.pop();

      if (tokens.pop_if(TokenType::LBRACE))
        parseStructDecl(name, is_public);
      else
        parseFunctionDecl(name, is_public);
  }

public:
  [[nodiscard]] static bool
  parse(TU& tu, eden::vector<Token>& tokens) {
    ParserBody parser(tokens, tu);

    while (not parser.tokens.peek_is(TokenType::INVALID_TOKEN)) {
      switch (parser.tokens.peek().type) { using enum TokenType;
      case KEYWORD_IMPORT: parser.tokens.pop(); parser.parseImport(); break;
      case DUNDER_CEXTERN: parser.tokens.pop(); parser.parseCExtern(); break;
      case KEYWORD_PUB:    parser.tokens.pop(); parser.parseGlobalLevelDeclaration(true); break;
      case IDENTIFIER:     parser.parseGlobalLevelDeclaration(false); break;
      default:
        parser.error(parser.tokens.take(), "Expected struct or function declaration.");
        break;
      }
    }

    return parser.has_errors;
  }
};

edenNoInlineCold void printFunction(Function const& func, TU const& tu) noexcept {
  std::print("{}{}: (",
    func.is_public ? "pub " : "",
    func.nameof());

  auto const& module = *tu.module;
  auto const function = module.getFunction(func.nameof()); assert(function);
  auto const num_parameters = function->num_parameters();
  auto const returnTypeID = function->returnType(module.getID());

  for (auto i{0uz}; i<num_parameters; ++i) {
    auto const& parameter = function->getLocal(i);
    std::print("{}", parameter.type.toString(module));
    std::print(" {}, ", parameter.nameof());
  }

  if (num_parameters not_eq 0) std::print("\b\b");

  std::print(") ");
  if (returnTypeID.derived not_eq Type::DEVOID)
    std::print("{}", returnTypeID.toString(module));

  std::print(" {{ ");
  print_ast(func.body, tu.source_files.back());
  std::print(" \n}} ");
}

}

void Parser::printTU(TU const& tu) noexcept {
  for (auto const& f : tu.functions) {
    printFunction(f, tu);
    std::println();
  }
}

#include <chrono>
static void output_benchmark([[maybe_unused]] auto begin_time) {
#ifdef STAGE_BENCHMARKS
  auto end_time = std::chrono::high_resolution_clock::now();
  std::println("{:>10}, {:>10} | Parsing {}",
    end_time - begin_time,
    std::chrono::duration_cast<std::chrono::microseconds>(end_time - begin_time),
    tu.source_files.back().path()
  );
#endif
}

bool Parser::parseTokens(TU& out_tu, eden::vector<Token>& tokens) noexcept {
  auto const begin_time = std::chrono::high_resolution_clock::now();

  auto const has_errors = ParserBody::parse(out_tu, tokens);

  output_benchmark(begin_time);
  return has_errors;
}

