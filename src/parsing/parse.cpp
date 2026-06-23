#include "parse.hpp"

#include "ast.hpp"
#include "build_system/build.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "semantic_analysis/symbol_table.hpp"

#include <print>
#include <unordered_set>
#include <utility>

using namespace LOM;
using namespace LOM::Lexer;
using namespace LOM::Parser;
using namespace LOM::AST;
using eden::releasing_string;

namespace {
[[nodiscard]] InstantiatedType parseType(TokenView& tokens, TU& tu);

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

[[nodiscard]] const Type*
parseUnqualifiedType(TokenView& tokens, TU& tu) {
  auto const& current_file = tu.source_files.back();
  auto const token = tokens.take();
  auto const& module = *tu.module;

  if (token.isPrimitive()) {
    if (token.isPointer()) {

      if (not tokens.pop_if(TokenType::LESS)) {
        report_parsing_error(current_file, tokens.peek(), "Expected opening < in pointer declaration.");
        return Type::error();
      }

      if (token.is(TokenType::KEYWORD_VAGUE)) {
        auto const subtype_qualifiers = parseTypeQualifiers(tokens);
        if (not tokens.pop_if(TokenType::GTR)) {
          report_parsing_error(current_file, tokens.peek(), "Expected closing < in pointer declaration.");
          return Type::error();
        }

        return PointerType::vague(subtype_qualifiers.is_mutable);
      }

      auto const subtype_instance = parseType(tokens, tu);

      if (not tokens.pop_if(TokenType::GTR)) {
        report_parsing_error(current_file, tokens.peek(), "Expected closing < in pointer declaration.");
        return Type::error();
      }

      switch (token.type) {
      case TokenType::KEYWORD_RAW:    return module.getRawPointerType(subtype_instance);
      case TokenType::KEYWORD_UNIQUE: return module.getUniquePointerType(subtype_instance);
      default:
        eden_unreachable("Pointer type unsupported.");
      }
    }

    switch (token.type) {
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

  if (token.is(TokenType::IDENTIFIER)) {
    auto const type = module.getCustomType(token.getString(current_file));
    if (not type) report_parsing_error(current_file, token, "Expected typename.");
    return type;
  }

  if (token.is(TokenType::LESS)) {
    TokenView variant_types_tokens = tokens.getTokensBetweenAngleBrackets();
    auto const variant_types_tokens_cpy = variant_types_tokens;

    bool nullable{false};
    std::vector<const Type *> subtypes;
    std::unordered_set<const Type *> typenames; // prevent duplicate types

    do {
      auto const subtype_token = variant_types_tokens.peek();
      auto subtype = parseUnqualifiedType(variant_types_tokens, tu);
      if (subtype->isVariant())
        report_parsing_error(current_file, subtype_token, "Nested variant types not allowed.");

      if (subtype->isDevoid()) {
        if (nullable) report_parsing_error(current_file, subtype_token, "Devoid may not be specified more than once in variant declaration.");

        nullable = true;
      } else if (typenames.contains(subtype))
        report_parsing_error(current_file, subtype_token,"Duplicate types specified in variant declaration.");
      else
        typenames.emplace(subtype);

      subtypes.push_back(subtype);
    } while (variant_types_tokens.pop_if(TokenType::COMMA));

    if (subtypes.size() < 2)
      report_parsing_error(current_file, variant_types_tokens_cpy.viewAsStringToken(), "Two or more types must be specified in variant type list.");

    return module.getVariantType(subtypes, nullable);
  }

  report_parsing_error(current_file, token, "Expected typename.");
  return Type::error();
}

template <bool allow_qualifiers>
[[maybe_unused]] [[nodiscard]] auto
parseTypeList(TokenView& tokens, TU& tu) {
  using type = std::conditional_t<allow_qualifiers, InstantiatedType, const Type *>;
  std::vector<type> list;
  list.reserve(2);

  do {
    if constexpr (allow_qualifiers) list.emplace_back(parseType(tokens, tu));
    else                            list.emplace_back(parseUnqualifiedType(tokens, tu));
  } while (tokens.pop_if(TokenType::COMMA));

  if (not tokens.pop_if(TokenType::GTR))
    report_parsing_error(tu.source_files.back(), tokens.peek(), "Expected closing > in type list.");

  return list;
}

[[nodiscard]] InstantiatedType
parseType(TokenView& tokens, TU& tu) {
  const auto qualifiers = parseTypeQualifiers(tokens);
  return {parseUnqualifiedType(tokens, tu), qualifiers};
}

[[nodiscard]] std::string_view
parseIdentifier(TokenView& tokens, TU const& tu) noexcept {
  auto const token = tokens.take();
  if (not token.is(TokenType::IDENTIFIER)) {
    report_parsing_error(tu.source_files.back(), token, "Expected identifier.");
    return "?";
  }

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

    u32_t create(ASTNode::NodeData node_data, u32_t left_idx = 0, u32_t right_idx = 0) noexcept {
      assume_assert(left_idx < max_expressiontree_size); assume_assert(right_idx < max_expressiontree_size);
      ++begin; assume_assert(begin < max_expressiontree_size);

      auto &expr = data[begin];
      expr.left_idx = left_idx;
      expr.right_idx = right_idx;
      expr.data = node_data;
      return begin;
    }

    void reset() noexcept { begin = 0; }
  };

  [[nodiscard]] ASTNode::NodeData
  newNodeData(Token token, ASTNode::Type type = ASTNode::EMPTY) const noexcept {
    assert((tu.source_files.size() - 1) <= u8_max);
    return {.type = type,
            .file_idx = static_cast<u8_t>(tu.source_files.size() - 1),
            .length_in_file = token.length, .position_in_file = token.position,
            .bool_value = false}; // silences warning
  }

  std::vector<ASTNode> tree;
  TokenView tokens;
  TU& tu;
  File const& current_file;
  ExpressionTree& expression_tree;

  Body(TokenView tokens, TU &tu, ExpressionTree &expression_tree)
  : tokens(tokens), tu(tu), current_file(tu.source_files.back()), expression_tree(expression_tree) {}

  u32_t generateParameters() {
    const auto parameter = generateAssignmentExpression();
    if (tokens.pop_if(TokenType::RPAREN)) return expression_tree.create(EMPTY_NODE_DATA, parameter, 0);

    if (not tokens.pop_if(TokenType::COMMA))
      report_parsing_error(current_file, tokens.peek(), "Expected comma in call expression.");
    return expression_tree.create(EMPTY_NODE_DATA, parameter, generateParameters());
  }

  u32_t generateSubscript() {
    auto const res = generateAssignmentExpression();
    if (not tokens.pop_if(TokenType::RBRACKET))
      report_parsing_error(current_file, tokens.peek(), "const Expected closing bracked in subscript expression.");
    return res;
  }

  u32_t generateIdentifier() {
    auto const token = tokens.take();
    if (token.is(TokenType::IDENTIFIER) or token.is(TokenType::DUNDER_CEXTERN))
      return expression_tree.create(newNodeData(token, ASTNode::IDENTIFIER));

    report_parsing_error(current_file, token, "Expected identifier.");
    return expression_tree.create(newNodeData({}, ASTNode::IDENTIFIER)); // dummy data
  }

  u32_t generatePrimaryExpression() {
    if (tokens.empty()) return 0;

    if (tokens.peek().isLiteral()) {
      auto const token = tokens.take();
      ASTNode::NodeData data = newNodeData(token);

      switch (token.type) {
      case TokenType::SIGNED_LITERAL:   data.type = ASTNode::SIGNED_LITERAL; data.signed_value = token.getSigned(current_file); break;
      case TokenType::UNSIGNED_LITERAL: data.type = ASTNode::UNSIGNED_LITERAL; data.unsigned_value = token.getUnsigned(current_file); break;
      case TokenType::FLOAT_LITERAL:    data.type = ASTNode::FLOAT_LITERAL; data.float_value = token.getFloat(current_file); break;
      case TokenType::DOUBLE_LITERAL:   data.type = ASTNode::DOUBLE_LITERAL; data.double_value = token.getDouble(current_file); break;
      case TokenType::BOOL_LITERAL:     data.type = ASTNode::BOOL_LITERAL; data.bool_value = token.getBool(current_file); break;
      case TokenType::CHAR_LITERAL:     data.type = ASTNode::CHAR_LITERAL; data.char_value = token.getChar(current_file); break;
      case TokenType::STRING_LITERAL:   data.type = ASTNode::STRING_LITERAL; break;
      default: eden_unreachable("Invalid literal token type.");
      }

      return expression_tree.create(data);
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
      auto const token = tokens.take();
      auto data = newNodeData(token);
      switch (token.type) {
      case TokenType::PLUSPLUS: data.opr = Operator::POST_INCREMENT; break;
      case TokenType::MINUSMINUS: data.opr = Operator::POST_DECREMENT; break;
      case TokenType::ARROW:
        if (tokens.peek_is(TokenType::IDENTIFIER)) { // should only occur when accessing a member.
          left = expression_tree.create(data, left, 0);
          data.type = ASTNode::MEMBER_ACCESS;
          left = expression_tree.create(data, left, generatePostfixExpression());
          continue;
        }
        data.opr = Operator::ARROW;
        break;
      case TokenType::LPAREN: {
        u32_t const num_parameters = tokens.pop_if(TokenType::RPAREN) ? 0 : generateParameters();
        data.type = ASTNode::CALLING;
        left = expression_tree.create( data, left, num_parameters );
        continue;
      }
      case TokenType::LBRACKET:
        data.type = ASTNode::SUBSCRIPT;
        if (tokens.pop_if(TokenType::RBRACKET)) {
          report_parsing_error(current_file, tokens.peek(), "Expected expression within brackets.");
          auto fake = newNodeData({}, ASTNode::UNSIGNED_LITERAL);
          fake.unsigned_value = 1;
          left = expression_tree.create(data, left, expression_tree.create(fake));
          continue;
        }
        left = expression_tree.create(data,left, generateSubscript());
        continue;
      case TokenType::DOT:
        data.type = ASTNode::MEMBER_ACCESS;
        left = expression_tree.create(data, left, generatePostfixExpression());
        continue;
      default: tokens.undo(); return left;
      }

      data.type = ASTNode::UNARY;
      left = expression_tree.create(data, left);
    }
  }

  u32_t generatePrefixExpression() {
    auto const token = tokens.take();
    auto data = newNodeData(token);
    switch (token.type) {
    case TokenType::PLUSPLUS:           data.opr = Operator::PRE_INCREMENT; break;
    case TokenType::MINUSMINUS:         data.opr = Operator::PRE_DECREMENT; break;
    case TokenType::MINUS:              data.opr = Operator::UNARY_MINUS; break;
    case TokenType::ADDR:               data.opr = Operator::ADDRESS_OF; break;
    case TokenType::KEYWORD_CAST: {
      data.type = ASTNode::CAST;
      if (not tokens.pop_if(TokenType::LESS))
        report_parsing_error(current_file, tokens.peek(), "Expected opening < in cast.");
      data.cast_type = parseUnqualifiedType(tokens, tu);
      if (not tokens.pop_if(TokenType::GTR))
        report_parsing_error(current_file, tokens.peek(), "Expected opening > in cast.");

      return expression_tree.create(data, generatePrefixExpression(), 0);
    }
    case TokenType::KEYWORD_NOT:        data.opr = Operator::NOT; break;
    case TokenType::KEYWORD_BITNOT:     data.opr = Operator::BITNOT; break;
    default:
      tokens.undo(); return generatePostfixExpression();
    }

    data.type = ASTNode::UNARY;
    return expression_tree.create(data, generatePrefixExpression());
  }

  u32_t generateFactorExpression() {
    auto left = generatePrefixExpression();
    while (true) {
      auto const token = tokens.take();
      auto data = newNodeData(token);
      switch (token.type) {
      case TokenType::STAR: data.opr = Operator::MULTIPLY; break;
      case TokenType::SLASH: data.opr = Operator::DIVIDE; break;
      case TokenType::MOD: data.opr = Operator::MODULUS; break;
      default:
        tokens.undo(); return left;
      }
      data.type = ASTNode::BINARY;
      left = expression_tree.create(data, left, generatePrefixExpression());
    }
  }

  u32_t generateTermExpression() {
    auto left = generateFactorExpression();
    while (true) {
      auto const token = tokens.take();
      auto data = newNodeData(token);
      switch (token.type) {
      case TokenType::PLUS: data.opr = Operator::ADD; break;
      case TokenType::MINUS: data.opr = Operator::SUBTRACT; break;
      default:
        tokens.undo(); return left;
      }

      data.type = ASTNode::BINARY;
      left = expression_tree.create(data, left, generateFactorExpression());
    }
  }

  u32_t generateRelationalExpression() {
    auto left = generateTermExpression();
    while (true) {
      auto const token = tokens.take();
      auto data = newNodeData(token);

      switch (token.type) {
      case TokenType::KEYWORD_EQUALS: data.opr = Operator::EQUAL; break;
      case TokenType::KEYWORD_NOT_EQUAL: data.opr = Operator::NOT_EQUAL; break;
      case TokenType::LESS: data.opr = Operator::LESS; break;
      case TokenType::GTR: data.opr = Operator::GREATER; break;
      case TokenType::LESSEQ: data.opr = Operator::LESS_EQUAL; break;
      case TokenType::GTREQ: data.opr = Operator::GREATER_EQUAL; break;
      default:
        tokens.undo(); return left;
      }

      data.type = ASTNode::BINARY;
      left = expression_tree.create(data, left, generateTermExpression());
    }
  }

  u32_t generateBitwiseExpression() {
    auto left = generateRelationalExpression();
    while (true) {
      auto const token = tokens.take();
      auto data = newNodeData(token);

      switch (token.type) {
      case TokenType::KEYWORD_BITAND: data.opr = Operator::BITAND; break;
      case TokenType::KEYWORD_BITOR: data.opr = Operator::BITOR; break;
      case TokenType::KEYWORD_BITXOR: data.opr = Operator::BITXOR; break;
      default:
        tokens.undo(); return left;
      }

      data.type = ASTNode::BINARY;
      left = expression_tree.create(data, left, generateRelationalExpression());
    }
  }

  u32_t generateLogicalExpression() {
    auto left = generateBitwiseExpression();
    while (true) {
      auto const token = tokens.take();
      auto data = newNodeData(token);

      switch (token.type) {
      case TokenType::KEYWORD_AND: data.opr = Operator::AND; break;
      case TokenType::KEYWORD_OR: data.opr = Operator::OR; break;
      case TokenType::KEYWORD_XOR: data.opr = Operator::XOR; break;
      default:
        tokens.undo();
        return left;
      }

      data.type = ASTNode::BINARY;
      left = expression_tree.create(data, left, generateBitwiseExpression());
    }
  }

  u32_t generateAssignmentExpression() {
    const auto left = generateLogicalExpression();
    if (tokens.peek_is(TokenType::ASSIGN)) {
      auto const token = tokens.take();
      auto data = newNodeData(token, ASTNode::BINARY);
      data.opr = Operator::ASSIGN;
      return expression_tree.create(data, left, generateAssignmentExpression());
    }

    return left;
  }

  // assumes ( is popped, returns with no )
  u32_t generateExpressionBetweenParenthesis() {
    const TokenView until_ending = tokens.getTokensBetweenParenthesis();
    const TokenView after_ending = tokens;
    tokens = until_ending;
    const auto res = generateAssignmentExpression();
    if (not tokens.empty()) report_parsing_error(current_file, tokens.peek(), "Expected ending parenthesis.");
    tokens = after_ending;
    return res;
  }

  // assumes { is popped, returns with no }
  u32_t generateExpressionBetweenBraces() {
    auto const until_ending = tokens.getTokensBetweenBraces();
    auto const after_ending = tokens;
    tokens = until_ending;
    auto const res = generateAssignmentExpression();
    if (not tokens.empty()) report_parsing_error(current_file, tokens.peek(), "Expected ending brace.");
    tokens = after_ending;
    return res;
  }

  u32_t generateExpressionUntil(TokenType ending_token) {
    auto const until_ending = tokens.getAllTokensUntilFirstOf(ending_token);
    tokens.pop();
    auto const after_ending = tokens;
    tokens = until_ending;
    auto const res = generateAssignmentExpression();
    if (not tokens.empty()) report_parsing_error(current_file, tokens.peek(), std::format("Expected {}.", tokenTypeToString(ending_token)));
    tokens = after_ending;
    return res;
  }

  void translateExpression(u32_t idx) {
    if (idx == 0) return;

    auto expression = expression_tree.data[idx];
    switch (expression.data.type) {
      using enum ASTNode::Type;
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
      if (expression.right_idx == 0)
        return;

      u32_t left_idx = expression_tree.data[expression.right_idx].left_idx;
      u32_t right_idx = expression_tree.data[expression.right_idx].right_idx;
      u64_t num_parameters{1};
      while (true) {
        translateExpression(left_idx);
        if (right_idx == 0)
          break;
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

  // assumes { is popped, returns with no }
  void parseExpressionBetweenBraces() {
    expression_tree.reset();
    auto const idx = generateExpressionBetweenBraces();
    translateExpression(idx);
  }

  // returns with no ending token
  void parseExpressionUntil(TokenType ending_token) {
    expression_tree.reset();
    const auto idx = generateExpressionUntil(ending_token);
    translateExpression(idx);
  }

  void parseExpressionStatement() {
    auto data = newNodeData(tokens.peek(), ASTNode::EXPR_STMT);
    auto const expr_stmt_idx = tree.size();
    tree.emplace_back(EMPTY_NODE_DATA);

    parseExpressionUntil(TokenType::SEMI_COLON);
    auto const expr_end = tree.back().m;
    data.length_in_file = static_cast<u16_t>(expr_end.position_in_file - data.position_in_file) + expr_end.length_in_file;
    tree[expr_stmt_idx].m = data;
  }

  void parseVarDecl() {
    auto data = newNodeData(tokens.peek(), ASTNode::DECLARATION);
    auto const decl_idx = tree.size();
    tree.emplace_back(EMPTY_NODE_DATA);

    tree.emplace_back(parseType(tokens, tu));
    {
      auto const identifier_token = tokens.take();
      if (not identifier_token.is(TokenType::IDENTIFIER))
        report_parsing_error(current_file, identifier_token, "Expected identifier in variable declaration.");

      tree.emplace_back(newNodeData(identifier_token, ASTNode::IDENTIFIER));
    }

    if (not tokens.pop_if(TokenType::ASSIGN))
      report_parsing_error(current_file, tokens.peek(), "Expected assignment in variable declaration. Use = junk; if you'd like to keep the variable uninitialized.");

    if (tokens.peek_is(TokenType::KEYWORD_JUNK)) {
      tree.emplace_back(newNodeData(tokens.take()));
      if (not tokens.pop_if(TokenType::SEMI_COLON))
        report_parsing_error(current_file, tokens.peek(), "Expected semicolon ending junk variable declaration.");

    } else {
      if (tokens.pop_if(TokenType::SEMI_COLON))
        report_parsing_error(current_file, tokens.peek(), "Expected initializing expression in variable declaration.");
      parseExpressionUntil(TokenType::SEMI_COLON);
    }

    auto const decl_end = tree.back().m;
    data.length_in_file = static_cast<u16_t>(decl_end.position_in_file - data.position_in_file) + decl_end.length_in_file;
    tree[decl_idx].m = data;
  }

  void parseIf() {
    if (tokens.pop_if(TokenType::LBRACE))
      report_parsing_error(current_file, tokens.peek(), "Expected expression in if statement condition.");
    else
      parseExpressionUntil(TokenType::LBRACE);

    parseScoped();
    if (tokens.pop_if(TokenType::KEYWORD_ELSE))
      parseStatement();
    else
      tree.emplace_back(EMPTY_NODE_DATA);
  }

  void parseFor() { assert(false and "Sorry, unimplemented."); }

  void parseWhile() {
    parseExpressionUntil(TokenType::LBRACE);
    parseScoped();
  }

  // call after { is popped
  void parseScoped() {
    auto const token = tokens.peek();
    auto data = newNodeData(token, ASTNode::SCOPED); data.num_substatements = 0;

    auto const scoped_idx = tree.size();
    tree.emplace_back(EMPTY_NODE_DATA);

    while (not tokens.pop_if(TokenType::RBRACE)) {
      parseStatement();
      ++data.num_substatements;
      if (tokens.empty()) {
        tokens.undo();
        report_parsing_error(current_file, tokens.take(), "Expected closing } in scoped statement.");
        break;
      }
    }

    auto const scoped_end = tree.back().m;
    data.length_in_file = static_cast<u16_t>(scoped_end.position_in_file - data.position_in_file) + scoped_end.length_in_file;
    tree[scoped_idx].m = data;
  }

  void parseReturn() {
    auto const return_idx = tree.size() - 1;
    parseExpressionUntil(TokenType::SEMI_COLON);

    auto const return_end = tree.back().m;
    auto const return_start = tree[return_idx].m;
    auto const length_in_file = static_cast<u16_t>(return_end.position_in_file - return_start.position_in_file) + return_end.length_in_file;
    tree[return_idx].m.length_in_file = length_in_file;
  }

  void parseStatement() {
    Token const first = tokens.peek();
    if (first.isPrimitive() or first.isTypeModifier())
      return parseVarDecl();

    switch (first.type) {
    case TokenType::KEYWORD_IF:
      tree.emplace_back( newNodeData(tokens.take(), ASTNode::IF) );
      return parseIf();
    case TokenType::KEYWORD_FOR:
      tree.emplace_back( newNodeData(tokens.take(), ASTNode::FOR) );
      return parseFor();
    case TokenType::KEYWORD_WHILE:
      tree.emplace_back( newNodeData(tokens.take(), ASTNode::WHILE) );
      return parseWhile();
    case TokenType::KEYWORD_RETURN:
      tree.emplace_back( newNodeData(tokens.take(), ASTNode::RETURN) );
      return parseReturn();
    case TokenType::KEYWORD_SWITCH: eden_unreachable("Switch keyword not yet supported.");
    case TokenType::LBRACE: return parseScoped();
    case TokenType::LESS:   return parseVarDecl(); // variant declaration
    case TokenType::IDENTIFIER:
      // There should exist no other scenario where there exist two identifier in a row (hopefully) .
      // If that ever changes, could be disambiguated by the '=' in a declaration.
      // Also, this needs to change if array types can be declared via typename[] varname;
      if (tokens.peek_ahead(1).is(TokenType::IDENTIFIER)) return parseVarDecl();
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
  auto const& current_file = tu.source_files.back();
  auto &tokens = global_body.tokens;
  auto const name = parseIdentifier(tokens, tu);
  if (not tokens.pop_if(TokenType::LPAREN))
    report_parsing_error(current_file, tokens.peek(), "Expected opening ( for parameter list.");


  Module::Variable parameters[Settings::MAX_FUNCTION_PARAMETERS];
  bool is_variadic = false; auto num_parameters{0uz};
  if (tokens.peek_is(TokenType::RPAREN)) goto end_params;

  while (true) {
    if (tokens.pop_if(TokenType::DUNDER_VA)) {
      is_variadic = true;
      break;
    }

    auto type = parseType(tokens, tu);
    parameters[num_parameters] = {type, parseIdentifier(tokens, tu), false};
    ++num_parameters;

    if (not tokens.pop_if(TokenType::COMMA)) break;
    if (num_parameters >= Settings::MAX_FUNCTION_PARAMETERS) {
      report_parsing_error(tu.source_files.back(), tokens.peek(), std::format("Functions may have no more than {} parameters.", Settings::MAX_FUNCTION_PARAMETERS));
      break;
    }
  }

  end_params:
  if (not tokens.pop_if(TokenType::RPAREN))
    report_parsing_error(current_file, tokens.peek(), "Expected closing parenthesis in parameter list.");

  const Type *return_type = Type::devoid();
  if (not tokens.peek_is(TokenType::SEMI_COLON))
    return_type = parseUnqualifiedType(tokens, tu);

  if (not tokens.pop_if(TokenType::SEMI_COLON))
    report_parsing_error(current_file, tokens.peek(), "Expected semi-colon.");

  auto const c_module = getModule("__C");
  auto const parameter_span = std::span(parameters, num_parameters);
  c_module->addFunction(name, parameter_span, return_type, true, is_variadic);
}

void parseStructDecl(TU& tu, Body& global_body) {
  auto const& current_file = tu.source_files.back();
  auto &tokens = global_body.tokens;
  auto const name = parseIdentifier(tokens, tu);

  SymbolTable::Variable members[Settings::MAX_STRUCT_MEMBER_VARIABLES];

  if (not tokens.pop_if(TokenType::LBRACE))
    report_parsing_error(current_file, tokens.peek(), "Expected opening curly brace in struct definition.");

  auto i{0uz};
  if (tokens.pop_if(TokenType::RBRACE))
    return (void)tu.module->addCustomType(name, {});

  bool in_pub_block = STRUCT_MEMBERS_START_PUBLIC;
  do {
    if (in_pub_block) {
      if (tokens.pop_if(TokenType::COLON)) {
        if (not tokens.pop_if(TokenType::KEYWORD_PUB))
          report_parsing_error(current_file, tokens.peek(), "Expected closing 'pub' in 'pub:  :pub' block.");

        if (tokens.peek_is(TokenType::RBRACE)) break;
        in_pub_block = false;
      }
    } else {
      if (tokens.pop_if(TokenType::KEYWORD_PUB)) {
        if (not tokens.pop_if(TokenType::COLON))
          report_parsing_error(current_file, tokens.peek(), "Expected opening colon in 'pub:  :pub' block.");

        in_pub_block = true;
      }
    }

    auto member_type = parseUnqualifiedType(tokens, tu);
    auto member_name = parseIdentifier(tokens, tu);
    members[i] = {member_type, member_name, in_pub_block};
    ++i;
  } while (tokens.pop_if(TokenType::COMMA) and not tokens.peek_is(TokenType::RBRACE));

  if (not tokens.pop_if(TokenType::RBRACE))
    report_parsing_error(current_file, tokens.peek(), "Expected closing curly brace after struct definition.");

  tu.module->addCustomType(name, std::span(members, i));
}

void parseImports(TU& tu, TokenView& tokens) {
  auto const& current_file = tu.source_files.back();
  while (tokens.pop_if(TokenType::KEYWORD_IMPORT)) {

    if (not tokens.peek_is(TokenType::IDENTIFIER))
      report_parsing_error(current_file, tokens.peek(), "Expected module name.");

    auto name = tokens.take().getString(current_file);
    if (tu.module->nameof() == name)
      report_validation_error(current_file, "Cannot import from current module!");
    else
      tu.imports.emplace_back(name);

    if (not tokens.pop_if(TokenType::SEMI_COLON))
      report_parsing_error(current_file, tokens.peek(), "Expected semicolon.");
  }
}

void parseFunctionsAndStructs(TU& tu, Body& global_body, std::vector<Function>& functions) {
  auto const& current_file = tu.source_files.back();
  TokenView& tokens = global_body.tokens;

  while (not tokens.empty()) {
    if (tokens.pop_if(TokenType::DUNDER_CEXTERN)) {
      parseCExtern(tu, global_body);
      continue;
    }
    if (tokens.pop_if(TokenType::KEYWORD_STRUCT)) {
      parseStructDecl(tu, global_body);
      continue;
    }

    Function& current_function = functions.emplace_back();
    current_function.file_idx = static_cast<u8_t>(tu.source_files.size() - 1);
    bool const is_public = tokens.pop_if(TokenType::KEYWORD_PUB);

    if (not tokens.pop_if(TokenType::KEYWORD_FN))
      report_parsing_error(current_file, tokens.peek(), "Expected function declaration.");

    {
    auto const function_name = parseIdentifier(tokens, tu);
    current_function.name_ptr = function_name.data();
    current_function.name_len = function_name.length();
    }

    if (not tokens.pop_if(TokenType::LPAREN))
      report_parsing_error(current_file, tokens.peek(), "Expected parameter list.");

    {
      SymbolTable::Variable parameters[Settings::MAX_FUNCTION_PARAMETERS];
      auto i{0uz};
      if (tokens.peek_is(TokenType::RPAREN)) goto end_params;

      while (true) {
        auto type = parseType(tokens, tu);
        auto name = parseIdentifier(tokens, tu);
        parameters[i] = {type, name, false};
        ++i;
        if (not tokens.pop_if(TokenType::COMMA)) break;
        if (i >= Settings::MAX_FUNCTION_PARAMETERS) {
          report_parsing_error(tu.source_files.back(), tokens.peek(), std::format("Functions may have no more than {} parameters.", Settings::MAX_FUNCTION_PARAMETERS));
          break;
        }
      }

      end_params:
      if(not tokens.pop_if(TokenType::RPAREN))
        report_parsing_error(current_file, tokens.peek(), "Expected closing parenthesis in parameter list.");

      const Type *return_type = Type::devoid();
      if (not tokens.peek_is(TokenType::LBRACE))
        return_type = parseUnqualifiedType(tokens, tu);

      tu.module->addFunction(current_function.nameof(), std::span(parameters, i), return_type, is_public);
      tu.module->enterFunctionScope(current_function.nameof());
    }

    if (not tokens.pop_if(TokenType::LBRACE))
      report_parsing_error(current_file, tokens.peek(), "Expected function definition.");

    {
      Body function_body(tokens.getTokensBetweenBraces(), tu, global_body.expression_tree);
      function_body.parseStatementsUntilEmpty();
      current_function.body = std::move(function_body.tree);
    }
  }
}

void printFunction(Function const& func, TU const& tu) {
  auto const function = tu.module->getFunction(func.nameof());
  auto const parameters = function->parameters();
  auto const return_type = function->returnType();

  std::print("id: {}\nfn {} (", function->get_id(), func.nameof());

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

void Parser::printTU(TU const& tu) {

  for (auto const& import : tu.imports)
    std::println("import {};", import);

  for (auto const& f : tu.functions) {
    printFunction(f, tu);
    std::println();
  }
}

void Parser::parseTokens(TU& tu, std::vector<Token> const& tokens) {
  Body::ExpressionTree expression_tree; // the stack can have 6kb as a treat :)

  TokenView global_view{tokens};
  parseImports(tu, global_view);

  Body global_body(global_view, tu, expression_tree);
  parseFunctionsAndStructs(tu, global_body, tu.functions);
}

