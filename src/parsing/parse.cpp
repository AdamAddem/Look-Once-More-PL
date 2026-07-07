#include "parse.hpp"

#include "ast.hpp"
#include "build_system/build.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "semantic_analysis/symbol_table.hpp"

#include <chrono>
#include <numeric>
#include <print>
#include <utility>
#include <chrono>

using namespace LOM;
using namespace LOM::Lexer;
using namespace LOM::Parser;
using namespace LOM::AST;

namespace {

[[noreturn]] void throw_notfound() eden_throws(0 if empty) { throw 0; }

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

  u32_t create(ASTNode node, u32_t left_idx = 0, u32_t right_idx = 0) noexcept {
    assume_assert(left_idx < max_expressiontree_size); assume_assert(right_idx < max_expressiontree_size);
    ++begin; assume_assert(begin < max_expressiontree_size);

    auto& expr = data[begin];
    expr.left_idx = left_idx;
    expr.right_idx = right_idx;
    expr.node = node;
    return begin;
  }

  void reset() noexcept { begin = 0; }
};

class ParserBody {

  [[nodiscard]] u8_t
  current_file_idx() const noexcept {
    assert((tu.source_files.size() - 1) <= u8_max);
    return u8_t(tu.source_files.size() - 1);
  }

  // adds an uninitialized node containing only the current file idx and node_type
  // returns index of node
  [[nodiscard]] sz_t
  insertTypedNode(ASTNode::NodeType node_type) noexcept {
    auto const new_node_idx = nodes.size();
    nodes.emplace_back(node_type, current_file_idx());
    return new_node_idx;
  }

  eden_always_inline [[nodiscard]] ASTNode
  newNode(Token token, ASTNode::NodeType type = ASTNode::EMPTY) const noexcept {
    return ASTNode{
    ASTNode::CommonData{
            .type = type,
            .file_idx = current_file_idx(),
            .length_in_file = token.length, .position_in_file = token.position}};
  }

  eden_always_inline [[nodiscard]] ASTNode
  newNode(u16_t length_in_file, u32_t position_in_file, ASTNode::NodeType type = ASTNode::EMPTY) const noexcept {
    return ASTNode{
    ASTNode::CommonData{
      .type = type,
      .file_idx = current_file_idx(),
      .length_in_file = length_in_file, .position_in_file = position_in_file}};
  }

  eden_noinline_cold void
  error(Token err, std::string_view msg) noexcept {
    report_error(current_file, err, std::string(msg)); has_errors = true;
  }

  std::vector<ASTNode> nodes;
  TokenView tokens;
  File current_file;
  TU& tu;
  bool has_errors{};
  ExpressionTree expression_tree;

  ParserBody(std::vector<Token>& tokens, TU& tu)
  : tokens(tokens), current_file(tu.source_files.back()), tu(tu) {}

  #define pre assert(tokens.peek().isTypeQualifier());
  [[nodiscard]] Type::Qualifiers
  parseTypeQualifiers() noexcept { pre
    Type::Qualifiers qualifiers;
    if (tokens.pop_if(TokenType::KEYWORD_MUT))
      qualifiers.is_mutable = true;
    else eden_unreachable("Type qualifier not supported.");

    return qualifiers;
  }
  #undef pre

  #define pre assert(tokens.previous().is(TokenType::LBRACKET));
  [[nodiscard]] ArrayType const*
  parseArrayType() noexcept { pre
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

    auto const subtype = parseUnqualifiedType();
    return tu.module->getArrayType(array_size, subtype);
  }
  #undef pre

  #define pre assert(pointer_token.isPointer());
  [[nodiscard]] PointerType const*
  parsePointerType(Token pointer_token) noexcept { pre
    if (pointer_token.is(TokenType::KEYWORD_VAGUE)) {
      auto const subtype_qualifiers = parseTypeQualifiers();
      return PointerType::vague(subtype_qualifiers.is_mutable);
    }

    auto const subtype_instance = parseType();

    switch (pointer_token.type) {
    case TokenType::KEYWORD_RAW:    return tu.module->getRawPointerType(subtype_instance);
    case TokenType::KEYWORD_UNIQUE: return tu.module->getUniquePointerType(subtype_instance);
    default: eden_unreachable("Pointer type unsupported.");
    }

  }
  #undef pre

  #define pre assert(primitive_token.isPrimitive() and not primitive_token.isPointer());
  [[nodiscard]] Type const*
  parsePrimitiveType(Token primitive_token) noexcept { pre
    switch (primitive_token.type) {
    case TokenType::KEYWORD_i8:     return PrimitiveType::i8();
    case TokenType::KEYWORD_i16:    return PrimitiveType::i16();
    case TokenType::KEYWORD_i32:    return PrimitiveType::i32();
    case TokenType::KEYWORD_i64:    return PrimitiveType::i64();
    case TokenType::KEYWORD_u8:     return PrimitiveType::u8();
    case TokenType::KEYWORD_u16:    return PrimitiveType::u16();
    case TokenType::KEYWORD_u32:    return PrimitiveType::u32();
    case TokenType::KEYWORD_u64:    return PrimitiveType::u64();
    case TokenType::KEYWORD_f32:    return PrimitiveType::f32();
    case TokenType::KEYWORD_f64:    return PrimitiveType::f64();

    case TokenType::KEYWORD_CHAR:   return PrimitiveType::char_();
    case TokenType::KEYWORD_BOOL:   return PrimitiveType::bool_();
    case TokenType::KEYWORD_STRING: return PrimitiveType::string();
    case TokenType::KEYWORD_DEVOID: return Type::devoid();
    default:
    eden_unreachable("Pointer type not supported.");
    }
  }
  #undef pre

  [[nodiscard]] Type const*
  parseUnqualifiedType() noexcept {
    auto const& current_file = tu.source_files.back();
    auto const token = tokens.take();

    static_assert(isCategoryPRIMITIVES(TokenType::KEYWORD_RAW));
    static_assert(isCategoryPRIMITIVES(TokenType::KEYWORD_UNIQUE));
    static_assert(isCategoryPRIMITIVES(TokenType::KEYWORD_VAGUE));

    switch (token.type) { using enum TokenType;
    TOKENTYPE_PRIMITIVES_CASES
        if (token.isPointer()) return parsePointerType(token);
        return parsePrimitiveType(token);

    case IDENTIFIER: {
        Type const* type = tu.module->getCustomType(token.getString(current_file));
        if (type == nullptr)
          type = Type::error(), error(token, "Expected typename.");
        return type;
      }

    case LBRACKET: return parseArrayType();
    default:
      error(token, "Expected typename.");
      return Type::error();
    }
  }

  [[nodiscard]] QualifiedType
  parseType() noexcept {
    Type::Qualifiers qualifiers;
    if (tokens.peek().isTypeQualifier())
      qualifiers = parseTypeQualifiers();
    return {parseUnqualifiedType(), qualifiers};
  }

  [[nodiscard]] std::string_view
  parseIdentifier() noexcept {
    if (not tokens.peek_is(TokenType::IDENTIFIER)) {
      error(tokens.take_if_valid(), "Expected identifier.");
      return "?";
    }
    return tokens.take().getString(tu.source_files.back());
  }

  // opening parenthesis must be popped, and the next token must not be closing parenthesis
  u32_t generateParameters() {
    auto const parameter = generateAssignmentExpression();
    if (tokens.pop_if(TokenType::RPAREN))
      return expression_tree.create(PLACEHOLDER_NODE, parameter, 0);

    if (tokens.pop_if(TokenType::COMMA))
      return expression_tree.create(PLACEHOLDER_NODE, parameter, generateParameters());

    error(tokens.peek(), "Expected comma in call expression.");
    return expression_tree.create(PLACEHOLDER_NODE, parameter, generateParameters());
  }

#define pre assert(tokens.peek().isLiteral());
  template <bool negate = false>
  u32_t generateLiteral() { pre
    auto const token = tokens.take();
    ASTNode node = newNode(token);

    switch (token.type) {
    case TokenType::INTEGER_LITERAL: {
      if constexpr (negate) {
        node.m.type = ASTNode::SIGNED_LITERAL;
        node.signed_data.value = -static_cast<i64_t>(token.getInteger(current_file));
        break;
      }
      else {
        node.m.type = ASTNode::UNSIGNED_LITERAL;
        node.unsigned_data.value = token.getInteger(current_file);
        break;
      }
    }
    case TokenType::FLOAT_LITERAL:  node.m.type = ASTNode::FLOAT_LITERAL;
      node.float_data.value = negate ? -token.getFloat(current_file) : token.getFloat(current_file);
      break;
    case TokenType::DOUBLE_LITERAL: node.m.type = ASTNode::DOUBLE_LITERAL;
      node.double_data.value = negate ? -token.getDouble(current_file) : token.getDouble(current_file);
      break;
    case TokenType::BOOL_LITERAL:   node.m.type = ASTNode::BOOL_LITERAL;
      node.bool_data.value = negate ? not token.getBool(current_file) : token.getBool(current_file);
      break;
    case TokenType::CHAR_LITERAL:   node.m.type = ASTNode::CHAR_LITERAL;
      node.char_data.value = negate ? -token.getChar(current_file) : token.getChar(current_file);
      break;

    case TokenType::STRING_LITERAL:   node.m.type = ASTNode::STRING_LITERAL; assert(not negate); break;
    case TokenType::ESCAPED_STRING_LITERAL:   node.m.type = ASTNode::ESCAPED_STRING_LITERAL; assert(not negate); break;
    default: eden_unreachable("Invalid literal token type.");
    }

    return expression_tree.create(node);
  }
#undef pre

  u32_t generatePrimaryExpression() {
    switch (tokens.peek().type) { using enum TokenType;
    TOKENTYPE_LITERALS_CASES return generateLiteral();
    case LPAREN: {
      tokens.pop();
      auto const res = generateAssignmentExpression();
      if (not tokens.pop_if(RPAREN)) error(tokens.take_if_valid(), "Expected closing ).");
      return res;
    }

    case IDENTIFIER:
    case DUNDER_CEXTERN: {
      auto const identifier_idx = expression_tree.create( newNode(tokens.take(), ASTNode::IDENTIFIER) );

      if (not tokens.peek_is(DOT))
        return identifier_idx;

      tokens.pop();
      auto const member_idx = generatePrimaryExpression();

      auto const member_node = expression_tree.data[member_idx].node;
      auto const identifier_node = expression_tree.data[identifier_idx].node;
      auto const combined = combine_spans(
         member_node.m.position_in_file,
         identifier_node.m.length_in_file,
         identifier_node.m.position_in_file
        );

      auto const total_data = newNode(combined.first, combined.second, ASTNode::MEMBER_ACCESS);
      return expression_tree.create(total_data, identifier_idx, member_idx);
    }

    default:
      // Throwing seems to be almost exactly as performant as just returning 0, while making things somewhat easier to reason about.
      // This does unfortunately invalidate any imperfect expressions rather than attempting to fix them, but I probably wasn't going to do that anyways.
      throw_notfound();
    }
  }

  u32_t generatePostfixExpression() {
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

  u32_t generatePrefixExpression() {
    auto const token = tokens.peek();
    Operator opr;
    switch (token.type) {
    case TokenType::PLUSPLUS:           opr = Operator::PRE_INCREMENT; break;
    case TokenType::MINUSMINUS:         opr = Operator::PRE_DECREMENT; break;
    case TokenType::ADDR:               opr = Operator::ADDRESS_OF; break;
    case TokenType::KEYWORD_NOT:        opr = Operator::NOT; break;
    case TokenType::KEYWORD_BITNOT:     opr = Operator::BITNOT; break;
    case TokenType::KEYWORD_CAST: {
      auto node = newNode(token, ASTNode::CAST); tokens.pop();
      if (not tokens.pop_if(TokenType::LESS)) error(tokens.peek(), "Expected opening < in cast.");
      node.cast_data.cast_type = parseUnqualifiedType();
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

  u32_t generateFactorExpression() {
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

  u32_t generateTermExpression() {
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

  u32_t generateRelationalExpression() {
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

  u32_t generateBitwiseExpression() {
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

  u32_t generateLogicalExpression() {
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

  u32_t generateAssignmentExpression() {
    auto const left = generateLogicalExpression();
    if (tokens.peek_is(TokenType::ASSIGN)) {
      auto const token = tokens.take();
      auto node = newNode(token, ASTNode::BINARY);
      node.binary_data.opr = Operator::ASSIGN;

      return expression_tree.create(node, left, generateAssignmentExpression());
    }

    return left;
  }

  void translateExpression(u32_t idx) noexcept {
    assert(idx not_eq 0);

    auto expression = expression_tree.data[idx];
    switch (expression.node.m.type) { using enum ASTNode::NodeType;
    case EMPTY: case DECLARATION: case IF:
    case WHILE: case RETURN:
      eden_unreachable("Statements should not be contained in an expression.");

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
        nodes[calling_idx].calling_data.num_parameters = 0;
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

      nodes[calling_idx].calling_data.num_parameters = num_parameters;
      return;
    }

    case IDENTIFIER: case STRING_LITERAL: case ESCAPED_STRING_LITERAL: case SIGNED_LITERAL: case UNSIGNED_LITERAL:
    case FLOAT_LITERAL: case DOUBLE_LITERAL: case BOOL_LITERAL: case CHAR_LITERAL:
      nodes.emplace_back(expression.node);
      return;

    default: eden_unreachable("Invalid ast node type in expression translation.");
    }
  }

  // don't access anything above this method directly through anything below this method or i'll hurt you
  template <bool expression_statement = false>
  void parseExpression() noexcept {
    expression_tree.reset();

    try { translateExpression( generateAssignmentExpression() ); }
    catch (...) {
      if constexpr (expression_statement) {
        error(tokens.peek(), "Expected expression.");
        tokens.set_peek(TokenType::SEMI_COLON); // trick into avoiding double error
      }
      else error(tokens.take_if_valid(), "Expected expression.");

      nodes.emplace_back(ASTNode::EMPTY, current_file_idx());
    }
  }

  void sync_to(TokenType sync_type) noexcept {
    while (not tokens.pop_if(sync_type)) {
      if (tokens.peek_is(TokenType::INVALID_TOKEN)) return;
      tokens.pop();
    }
  }

  eden_always_inline void sync_to_semicolon() noexcept { return sync_to(TokenType::SEMI_COLON); }

  Token parseVarDecl(sz_t decl_node_idx, QualifiedType declaration_type) noexcept {
    bool has_init;

    // identifier
    {
      auto const identifier_token = tokens.take();
      if (not identifier_token.is(TokenType::IDENTIFIER)) {
        error(identifier_token, "Expected identifier in variable declaration.");
        sync_to(TokenType::SEMI_COLON);
        return identifier_token;
      }

      auto identifier_node = newNode(identifier_token, ASTNode::IDENTIFIER);
      identifier_node.identifier_data.decl_type = declaration_type.type;
      nodes.emplace_back( identifier_node );
    }

    if (not tokens.pop_if(TokenType::ASSIGN)) {
      auto err = tokens.peek();
      error(err, "Expected assignment in variable declaration. Use = junk; if you'd like to keep the variable uninitialized.");
      sync_to_semicolon();
      return err;
    }

    if (tokens.pop_if(TokenType::KEYWORD_JUNK))
      has_init = false;
    else {
      has_init = true;
      parseExpression();
    }

    if (not tokens.peek_is(TokenType::SEMI_COLON)) {
      auto const err = tokens.peek();
      error(err, "Expected semicolon ending variable declaration.");
      sync_to_semicolon();
      return err;
    }

    auto& data = nodes[decl_node_idx].declaration_data;
    data.has_init = has_init;
    data.qualifiers = declaration_type.qualifiers;
    return tokens.take();
  }

#define pre assert(not tokens.peek_is(TokenType::KEYWORD_IF));
  Token parseIf(sz_t if_node_idx) { pre
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

#define pre assert(not tokens.peek_is(TokenType::KEYWORD_WHILE));
  Token parseWhile(sz_t while_node_idx) { pre
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

#define pre assert(not tokens.peek_is(TokenType::KEYWORD_RETURN));
  Token parseReturn(sz_t return_node_idx) { pre
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

  Token parseStatement() {
    auto const first = tokens.peek();
    sz_t stmt_idx;
    Token final;

    switch (first.type) { using enum TokenType;
    case SEMI_COLON:      return tokens.take();
    case KEYWORD_IF:      tokens.pop(); stmt_idx = insertTypedNode(ASTNode::IF);      final = parseIf(stmt_idx);     break;
    case KEYWORD_WHILE:   tokens.pop(); stmt_idx = insertTypedNode(ASTNode::WHILE);   final = parseWhile(stmt_idx);  break;
    case KEYWORD_RETURN:  tokens.pop(); stmt_idx = insertTypedNode(ASTNode::RETURN);  final = parseReturn(stmt_idx); break;
    case LBRACE:          tokens.pop(); parseStatementsBetweenBraces(); return tokens.previous();

    case LBRACKET:
    TOKENTYPE_PRIMITIVES_CASES
    TOKENTYPE_TYPE_QUALIFIERS_CASES
                          stmt_idx = insertTypedNode(ASTNode::DECLARATION);           final = parseVarDecl(stmt_idx, parseType()); break;

    case IDENTIFIER:
      if (tokens.peek_ahead(1).isIdentifier())
      {                   stmt_idx = insertTypedNode(ASTNode::DECLARATION);           final = parseVarDecl(stmt_idx, parseType()); break; }

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
    auto& stmt_data = nodes[stmt_idx].m;
    stmt_data.length_in_file = combined.length;
    stmt_data.position_in_file = combined.position;

    return combined;
  }

#define pre  assert(not tokens.peek_is(TokenType::LBRACE));
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
    has_errors = true;
  }


  Module::Variable parameters[Settings::MAX_FUNCTION_PARAMETERS];
  bool is_variadic = false; auto num_parameters{0uz};
  if (tokens.peek_is(TokenType::RPAREN)) goto end_params;

  while (true) {
    if (tokens.pop_if(TokenType::DUNDER_VA)) {
      is_variadic = true;
      break;
    }

    auto type = parseType();
    parameters[num_parameters] = {type, parseIdentifier(), false};
    ++num_parameters;

    if (not tokens.pop_if(TokenType::COMMA)) break;
    if (num_parameters >= Settings::MAX_FUNCTION_PARAMETERS) {
      error(tokens.peek(), std::format("Functions may have no more than {} parameters.", Settings::MAX_FUNCTION_PARAMETERS));
      has_errors = true;
      break;
    }
  }

  end_params:
  if (not tokens.pop_if(TokenType::RPAREN)) {
    error(tokens.peek(), "Expected closing parenthesis in parameter list.");
  }

  Type const* return_type = Type::devoid();
  if (not tokens.peek_is(TokenType::SEMI_COLON))
    return_type = parseUnqualifiedType();

  if (not tokens.pop_if(TokenType::SEMI_COLON)) {
    error(tokens.peek(), "Expected semi-colon.");
    return;
  }

  auto const c_module = getModule("__C");
  auto const parameter_span = std::span(parameters, num_parameters);
  c_module->addFunction(name, parameter_span, return_type, true, is_variadic);
}
#undef pre

#define pre assert(tokens.previous().is(TokenType::KEYWORD_STRUCT));
void parseStructDecl() noexcept { pre
  auto const name = parseIdentifier();

  SymbolTable::Variable members[Settings::MAX_STRUCT_MEMBER_VARIABLES];

  if (not tokens.pop_if(TokenType::LBRACE)) {
    error(tokens.peek(), "Expected opening curly brace in struct definition.");
  }

  auto i{0uz};
  if (tokens.pop_if(TokenType::RBRACE))
    return (void)tu.module->addCustomType(name, {});

  bool in_pub_block = STRUCT_MEMBERS_START_PUBLIC;
  do {
    if (in_pub_block) {
      if (tokens.pop_if(TokenType::COLON)) {
        if (not tokens.pop_if(TokenType::KEYWORD_PUB)) {
          error(tokens.peek(), "Expected closing 'pub' in 'pub:  :pub' block.");
        }

        if (tokens.peek_is(TokenType::RBRACE)) break;
        in_pub_block = false;
      }
    } else {
      if (tokens.pop_if(TokenType::KEYWORD_PUB)) {
        if (not tokens.pop_if(TokenType::COLON)) {
          error(tokens.peek(), "Expected opening colon in 'pub:  :pub' block.");
        }

        in_pub_block = true;
      }
    }

    auto member_type = parseUnqualifiedType();
    auto member_name = parseIdentifier();
    members[i] = {member_type, member_name, in_pub_block};
    ++i;
  } while (tokens.pop_if(TokenType::COMMA) and not tokens.peek_is(TokenType::RBRACE));

  if (not tokens.pop_if(TokenType::RBRACE)) {
    error(tokens.peek(), "Expected closing curly brace after struct definition.");
  }

  tu.module->addCustomType(name, std::span(members, i));
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

  auto name = name_token.getString(current_file);
  if (tu.module->nameof() == name) {
    error(name_token, "Cannot import from current module.");
  }
  else
    tu.imports.emplace_back(name);

  if (not tokens.pop_if(TokenType::SEMI_COLON)) {
    error(tokens.peek(), "Expected semicolon.");
  }

}
#undef pre

#define pre assert(tokens.peek().isIdentifier() or tokens.peek().is(TokenType::KEYWORD_PUB));
void parseFunction() noexcept { pre
  Function current_function;
  current_function.file_idx = static_cast<u8_t>(tu.source_files.size() - 1);
  current_function.is_public = tokens.pop_if(TokenType::KEYWORD_PUB);

  /* if (not tokens.pop_if(TokenType::KEYWORD_FN)) {
    report_error(current_file, tokens.peek(), "Expected function declaration.");
    return current_function;
  } */

  // name
  {
    auto const function_name = parseIdentifier();
    current_function.name_len = function_name.length();
    current_function.name_ptr = function_name.data();
  }

  if (not tokens.pop_if(TokenType::LPAREN)) {
    error(tokens.peek(), "Expected parameter list.");
  }

  // parameters
  {
    SymbolTable::Variable parameters[Settings::MAX_FUNCTION_PARAMETERS];
    auto i{0uz};
    if (tokens.peek_is(TokenType::RPAREN)) goto end_params;

    while (true) {
      auto type = parseType();
      auto name = parseIdentifier();
      parameters[i] = {type, name, false};
      ++i;
      if (not tokens.pop_if(TokenType::COMMA)) break;
      if (i >= Settings::MAX_FUNCTION_PARAMETERS) {
        error(tokens.peek(), std::format("Functions may have no more than {} parameters.", Settings::MAX_FUNCTION_PARAMETERS));
        break;
      }
    }

    end_params:
    if(not tokens.pop_if(TokenType::RPAREN)) {
      error(tokens.peek(), "Expected closing parenthesis in parameter list.");
    }

    Type const* return_type = Type::devoid();
    if (not tokens.peek_is(TokenType::LBRACE))
      return_type = parseUnqualifiedType();

    tu.module->addFunction(
      current_function.nameof(),
      std::span(parameters, i),
      return_type,
      current_function.is_public);
    tu.module->enterFunctionScope(current_function.nameof());
  }

  if (not tokens.pop_if(TokenType::LBRACE)) {
    error(tokens.peek(), "Expected function definition.");
    tu.functions.emplace_back(current_function);
    return;
  }

  // body
  {
    parseStatementsBetweenBraces();
    current_function.body = std::move(nodes);
  }

  tu.functions.emplace_back(current_function);
}
#undef pre

public:
  static bool parse(TU& tu, std::vector<Token>& token_list) {
    ParserBody parser(token_list, tu);

    while (not parser.tokens.peek_is(TokenType::INVALID_TOKEN)) {
      switch (parser.tokens.peek().type) { using enum TokenType;
      case KEYWORD_IMPORT: parser.tokens.pop(); parser.parseImport();     break;
      case DUNDER_CEXTERN: parser.tokens.pop(); parser.parseCExtern();    break;
      case KEYWORD_STRUCT: parser.tokens.pop(); parser.parseStructDecl(); break;
      case KEYWORD_PUB:
      case IDENTIFIER:                          parser.parseFunction();   break;
      default:
        parser.error(parser.tokens.take(), "Expected struct or function declaration.");
        break;
      }
    }

    return parser.has_errors;
  }
};

void printFunction(Function const& func, TU const& tu) noexcept {
  auto const function = tu.module->getFunction(func.nameof());

  auto const parameters = function->parameters();
  auto const return_type = function->returnType();

  std::print("{}fn {} (",
    func.is_public ? "pub " : "",
    func.nameof());

  for (auto& parameter : parameters) {
    std::print("{}", parameter.type.toString());
    std::print(" {}, ", parameter.nameof());
  }

  if (not parameters.empty()) std::print("\b\b");

  std::print(") ");
  if (not return_type->isDevoid())
    std::print("{}", return_type->toString());

  std::print(" {{ ");
  print_ast(func.body, tu.source_files.back());
  std::print(" \n}} ");

}

} // namespace

void Parser::printTU([[maybe_unused]] TU const& tu) noexcept {
  for (auto const& import : tu.imports)
    std::println("import {};", import);

  for (auto const& f : tu.functions) {
    printFunction(f, tu);
    std::println();
  }
}

bool Parser::parseTokens(TU& tu, std::vector<Token>& token_list) noexcept {
#ifdef STAGE_BENCHMARKS
  auto begin_time = std::chrono::high_resolution_clock::now();
#endif

  auto const has_errors = ParserBody::parse(tu, token_list);

#ifdef STAGE_BENCHMARKS
  auto end_time = std::chrono::high_resolution_clock::now();
  std::println("Parsing {}: {} | {} | {}",
    tu.source_files.back().path(),
    end_time - begin_time,
    std::chrono::duration_cast<std::chrono::microseconds>(end_time - begin_time),
    std::chrono::duration_cast<std::chrono::milliseconds>(end_time - begin_time)
  );
#endif
  return has_errors;
}

