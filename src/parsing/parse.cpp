#include "parse.hpp"

#include "ast.hpp"
#include "build_system/build.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "semantic_analysis/symbol_table.hpp"

#include <print>
#include <utility>

using namespace LOM;
using namespace LOM::Lexer;
using namespace LOM::Parser;
using namespace LOM::AST;
using eden::releasing_string;

namespace {

[[noreturn]] void throw_notfound() eden_throws(0 if empty) { throw 0; }

[[nodiscard]] InstantiatedType parseType(TokenView& tokens, TU& tu);

// assumes tokens starts with type qualifier
[[nodiscard]] InstantiatedType::Qualifiers
parseTypeQualifiers(TokenView& tokens) {
  InstantiatedType::Qualifiers qualifiers;
  if (tokens.peek_is(TokenType::KEYWORD_MUT))
    qualifiers.is_mutable = true;
  else eden_unreachable("Type modifier not supported.");
  tokens.pop();
  return qualifiers;
}

[[nodiscard]] const Type*
parseUnqualifiedType(TokenView& tokens, TU& tu) {
  auto const& current_file = tu.source_files.back();
  auto const token = tokens.take();
  auto const& module = *tu.module;

  static_assert(isCategoryPRIMITIVES(TokenType::KEYWORD_RAW));
  static_assert(isCategoryPRIMITIVES(TokenType::KEYWORD_VAGUE));
  if (token.isPrimitive()) {
    if (token.isPointer()) {

      if (token.is(TokenType::KEYWORD_VAGUE)) {
        auto const subtype_qualifiers = parseTypeQualifiers(tokens);
        return PointerType::vague(subtype_qualifiers.is_mutable);
      }

      auto const subtype_instance = parseType(tokens, tu);

      switch (token.type) {
      case TokenType::KEYWORD_RAW:    return module.getRawPointerType(subtype_instance);
      case TokenType::KEYWORD_UNIQUE: return module.getUniquePointerType(subtype_instance);
      default: eden_unreachable("Pointer type unsupported.");
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

  if (token.isIdentifier()) {
    const Type* type = module.getCustomType(token.getString(current_file));
    if (type == nullptr)
      type = Type::error(), report_error(current_file, token, "Expected typename.");

    return type;
  }

  report_error(current_file, token, "Expected typename.");
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
    report_error(tu.source_files.back(), tokens.peek(), "Expected closing > in type list.");

  return list;
}

// assumes tokens are not empty
[[nodiscard]] InstantiatedType
parseType(TokenView& tokens, TU& tu) {
  InstantiatedType::Qualifiers qualifiers;
  if (tokens.peek().isTypeQualifier())
    qualifiers = parseTypeQualifiers(tokens);
  return {parseUnqualifiedType(tokens, tu), qualifiers};
}

[[nodiscard]] std::string_view
parseIdentifier(TokenView& tokens, TU const& tu) noexcept {
  auto const token = tokens.take();
  if (not token.is(TokenType::IDENTIFIER)) {
    report_error(tu.source_files.back(), token, "Expected identifier.");
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

  // assumes that the parameter list isn't empty ()
  // needs ( popped
  u32_t generateParameters() {
    auto const parameter = generateAssignmentExpression();
    if (tokens.pop_if(TokenType::RPAREN))
      return expression_tree.create(EMPTY_NODE_DATA, parameter, 0);

    if (tokens.pop_if(TokenType::COMMA))
      return expression_tree.create(EMPTY_NODE_DATA, parameter, generateParameters());

    report_error(current_file, tokens.peek(), "Expected comma in call expression.");
    return expression_tree.create(EMPTY_NODE_DATA, parameter, generateParameters());
  }

  u32_t generatePrimaryExpression() {
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

    if (tokens.peek_is(TokenType::LPAREN)) {
      tokens.pop();
      auto const res = generateAssignmentExpression();
      if (not tokens.peek_is(TokenType::RPAREN)) report_error(current_file, tokens.take(), "Expected closing ).");
      return res;
    }

    if (tokens.peek_is(TokenType::IDENTIFIER) or tokens.peek_is(TokenType::DUNDER_CEXTERN))
      return expression_tree.create(newNodeData(tokens.take(), ASTNode::IDENTIFIER));

    throw_notfound();
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
        tokens.pop();
        if (not tokens.peek_is(TokenType::IDENTIFIER)) { opr = Operator::ARROW; break; }
        left = expression_tree.create(newNodeData(token, ASTNode::MEMBER_ACCESS), left, generatePostfixExpression()); // identifier following arrow should only occur when accessing a member
        continue;

      case TokenType::LPAREN:
        left = expression_tree.create(
          newNodeData(token, ASTNode::CALLING),
          left,
          tokens.pop_if(TokenType::RPAREN) ? 0 : generateParameters()
          );
        continue;

      case TokenType::LBRACKET: throw std::runtime_error("Subscript unsupported.");

      case TokenType::DOT:
        left = expression_tree.create(newNodeData(token, ASTNode::MEMBER_ACCESS), left, generatePostfixExpression());
        continue;
      default: return left;
      }

      auto data = newNodeData(token, ASTNode::UNARY); data.opr = opr;
      tokens.pop();
      left = expression_tree.create(data, left);
    }
  }

  u32_t generatePrefixExpression() {
    auto const token = tokens.peek();
    Operator opr;
    switch (token.type) {
    case TokenType::PLUSPLUS:           opr = Operator::PRE_INCREMENT; break;
    case TokenType::MINUSMINUS:         opr = Operator::PRE_DECREMENT; break;
    case TokenType::MINUS:              opr = Operator::UNARY_MINUS; break;
    case TokenType::ADDR:               opr = Operator::ADDRESS_OF; break;
    case TokenType::KEYWORD_NOT:        opr = Operator::NOT; break;
    case TokenType::KEYWORD_BITNOT:     opr = Operator::BITNOT; break;
    case TokenType::KEYWORD_CAST: {
      auto data = newNodeData(token, ASTNode::CAST); tokens.pop();
      if (not tokens.pop_if(TokenType::LESS)) report_error(current_file, tokens.peek(), "Expected opening < in cast.");
      data.cast_type = parseUnqualifiedType(tokens, tu);
      if (not tokens.pop_if(TokenType::GTR)) report_error(current_file, tokens.peek(), "Expected opening > in cast.");
      return expression_tree.create(data, generatePrefixExpression(), 0);
    }

    default: return generatePostfixExpression();
    }

    auto data = newNodeData(token, ASTNode::UNARY); data.opr = opr;
    tokens.pop();
    return expression_tree.create(data, generatePrefixExpression());
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

      auto data = newNodeData(token, ASTNode::BINARY); data.opr = opr;
      tokens.pop();
      left = expression_tree.create(data, left, generatePrefixExpression());
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

      auto data = newNodeData(token, ASTNode::BINARY); data.opr = opr;
      tokens.pop();
      left = expression_tree.create(data, left, generateFactorExpression());
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

      auto data = newNodeData(token, ASTNode::BINARY); data.opr = opr;
      tokens.pop();
      left = expression_tree.create(data, left, generateTermExpression());
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
      auto data = newNodeData(token, ASTNode::BINARY); data.opr = opr;
      tokens.pop();
      left = expression_tree.create(data, left, generateRelationalExpression());
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
      auto data = newNodeData(token, ASTNode::BINARY); data.opr = opr;
      tokens.pop();
      left = expression_tree.create(data, left, generateBitwiseExpression());
    }
  }

  u32_t generateAssignmentExpression() {
    auto const left = generateLogicalExpression();
    if (tokens.peek_is(TokenType::ASSIGN)) {
      auto const token = tokens.take();
      auto data = newNodeData(token, ASTNode::BINARY);
      data.opr = Operator::ASSIGN;
      return expression_tree.create(data, left, generateAssignmentExpression());
    }

    return left;
  }

  void translateExpression(u32_t idx) noexcept {
    assert(idx not_eq 0);

    auto expression = expression_tree.data[idx];
    switch (expression.data.type) {
      using enum ASTNode::Type;
    case EMPTY: case DECLARATION: case IF:
    case WHILE: case SCOPED: case RETURN: case EXPR_STMT:
      eden_unreachable("Statements should not be contained in an expression.");

    case UNARY: case CAST:
      tree.emplace_back(expression.data);
      translateExpression(expression.left_idx);
      return;

    case BINARY: case SUBSCRIPT: case MEMBER_ACCESS:
      tree.emplace_back(expression.data);
      translateExpression(expression.left_idx);
      translateExpression(expression.right_idx);
      return;

    case CALLING: { // i think this might be unnecessarily complicated
      tree.emplace_back(expression.data);
      auto const calling_idx = tree.size() - 1;
      translateExpression(expression.left_idx);
      if (expression.right_idx == 0) return;

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

      tree[calling_idx].m.num_parameters = num_parameters;
      return;
    }

    case IDENTIFIER: case STRING_LITERAL: case SIGNED_LITERAL: case UNSIGNED_LITERAL:
    case FLOAT_LITERAL: case DOUBLE_LITERAL: case BOOL_LITERAL: case CHAR_LITERAL:
      tree.emplace_back(expression.data);
      return;

    default: eden_unreachable("Invalid ast node type in expression translation.");
    }
  }

  void parseExpression() noexcept {
    expression_tree.reset();

    u32_t idx;
    try { idx = generateAssignmentExpression(); }
    catch (...) {
      report_error(current_file, tokens.peek_ahead(1), "Expected expression.");
      return;
    }

    translateExpression(idx);
  }

  void parseExpressionStatement() {
    auto data = newNodeData(tokens.peek(), ASTNode::EXPR_STMT);
    auto const expr_stmt_idx = tree.size();
    tree.emplace_back(EMPTY_NODE_DATA);

    parseExpression();
    auto const ending_semicolon = tokens.peek();
    if (not ending_semicolon.is(TokenType::SEMI_COLON))
      return report_error(current_file, ending_semicolon, "Expected semicolon.");

    tokens.pop();

    data.length_in_file = static_cast<u16_t>(ending_semicolon.position - data.position_in_file) + ending_semicolon.length;
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
        return report_error(current_file, identifier_token, "Expected identifier in variable declaration.");

      tree.emplace_back( newNodeData(identifier_token, ASTNode::IDENTIFIER) );
    }

    if (not tokens.pop_if(TokenType::ASSIGN))
      return report_error(current_file, tokens.peek(), "Expected assignment in variable declaration. Use = junk; if you'd like to keep the variable uninitialized.");

    if (tokens.peek_is(TokenType::KEYWORD_JUNK)) {
      tree.emplace_back(newNodeData(tokens.take()));
    } else parseExpression();

    if (not tokens.pop_if(TokenType::SEMI_COLON))
      return report_error(current_file, tokens.peek(), "Expected semicolon ending variable declaration.");

    auto const decl_end = tree.back().m;
    data.length_in_file = static_cast<u16_t>(decl_end.position_in_file - data.position_in_file) + decl_end.length_in_file;
    tree[decl_idx].m = data;
  }

  // call after if is popped
  void parseIf() {
    parseExpression();
    if (not tokens.pop_if(TokenType::LBRACE))
      return report_error(current_file, tokens.peek(), "Expected {.");

    parseScoped();
    if (tokens.pop_if(TokenType::KEYWORD_ELSE))
      parseStatement();
    else
      tree.emplace_back(EMPTY_NODE_DATA);
  }

  // call after while is popped
  void parseWhile() {
    parseExpression();
    if (not tokens.pop_if(TokenType::LBRACE))
      return report_error(current_file, tokens.peek(), "Expected {.");
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
      if (tokens.peek().type == TokenType::INVALID_TOKEN)
        return report_error(current_file, tokens.take(), "Expected closing } in scoped statement.");
    }

    auto const scoped_end = tree.back().m;
    data.length_in_file = static_cast<u16_t>(scoped_end.position_in_file - data.position_in_file) + scoped_end.length_in_file;
    tree[scoped_idx].m = data;
  }

  // call after return is popped
  void parseReturn() {
    auto const return_idx = tree.size() - 1;
    parseExpression();
    if (not tokens.pop_if(TokenType::SEMI_COLON))
      return report_error(current_file, tokens.peek(), "Expected semi-colon.");

    auto const return_end = tree.back().m;
    auto const return_start = tree[return_idx].m;
    auto const length_in_file = static_cast<u16_t>(return_end.position_in_file - return_start.position_in_file) + return_end.length_in_file;
    tree[return_idx].m.length_in_file = length_in_file;
  }

  void parseStatement() {
    auto const first = tokens.peek();
    if (first.isPrimitive() or first.isTypeQualifier())
      return parseVarDecl();

    switch (first.type) {
    case TokenType::KEYWORD_IF:
      tree.emplace_back( newNodeData(tokens.take(), ASTNode::IF) );
      return parseIf();
    case TokenType::KEYWORD_WHILE:
      tree.emplace_back( newNodeData(tokens.take(), ASTNode::WHILE) );
      return parseWhile();
    case TokenType::KEYWORD_RETURN:
      tree.emplace_back( newNodeData(tokens.take(), ASTNode::RETURN) );
      return parseReturn();
    case TokenType::LBRACE: return parseScoped();
    case TokenType::IDENTIFIER:
      // There should exist no other scenario where there exist two identifiers in a row (hopefully) .
      // If that ever changes, could be disambiguated by the '=' in a declaration.
      // Also, this needs to change if array types can be declared via typename[] varname;
      if (tokens.peek_ahead(1).is(TokenType::IDENTIFIER)) return parseVarDecl();
      [[fallthrough]];
    default:
      return parseExpressionStatement();
    }
  }

  // Expects { is popped, will pop ending }
  void parseStatementsBetweenBraces() {
    while (not tokens.pop_if(TokenType::RBRACE))
      parseStatement();
  }
};

void parseCExtern(TU& tu, TokenView& tokens) {
  auto const& current_file = tu.source_files.back();
  auto const name = parseIdentifier(tokens, tu);
  if (not tokens.pop_if(TokenType::LPAREN))
    report_error(current_file, tokens.peek(), "Expected opening ( for parameter list.");


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
      report_error(tu.source_files.back(), tokens.peek(), std::format("Functions may have no more than {} parameters.", Settings::MAX_FUNCTION_PARAMETERS));
      break;
    }
  }

  end_params:
  if (not tokens.pop_if(TokenType::RPAREN))
    report_error(current_file, tokens.peek(), "Expected closing parenthesis in parameter list.");

  Type const* return_type = Type::devoid();
  if (not tokens.peek_is(TokenType::SEMI_COLON))
    return_type = parseUnqualifiedType(tokens, tu);

  if (not tokens.pop_if(TokenType::SEMI_COLON))
    return report_error(current_file, tokens.peek(), "Expected semi-colon.");

  auto const c_module = getModule("__C");
  auto const parameter_span = std::span(parameters, num_parameters);
  c_module->addFunction(name, parameter_span, return_type, true, is_variadic);
}

void parseStructDecl(TU& tu, TokenView& tokens) {
  auto const& current_file = tu.source_files.back();
  auto const name = parseIdentifier(tokens, tu);

  SymbolTable::Variable members[Settings::MAX_STRUCT_MEMBER_VARIABLES];

  if (not tokens.pop_if(TokenType::LBRACE))
    report_error(current_file, tokens.peek(), "Expected opening curly brace in struct definition.");

  auto i{0uz};
  if (tokens.pop_if(TokenType::RBRACE))
    return (void)tu.module->addCustomType(name, {});

  bool in_pub_block = STRUCT_MEMBERS_START_PUBLIC;
  do {
    if (in_pub_block) {
      if (tokens.pop_if(TokenType::COLON)) {
        if (not tokens.pop_if(TokenType::KEYWORD_PUB))
          report_error(current_file, tokens.peek(), "Expected closing 'pub' in 'pub:  :pub' block.");

        if (tokens.peek_is(TokenType::RBRACE)) break;
        in_pub_block = false;
      }
    } else {
      if (tokens.pop_if(TokenType::KEYWORD_PUB)) {
        if (not tokens.pop_if(TokenType::COLON))
          report_error(current_file, tokens.peek(), "Expected opening colon in 'pub:  :pub' block.");

        in_pub_block = true;
      }
    }

    auto member_type = parseUnqualifiedType(tokens, tu);
    auto member_name = parseIdentifier(tokens, tu);
    members[i] = {member_type, member_name, in_pub_block};
    ++i;
  } while (tokens.pop_if(TokenType::COMMA) and not tokens.peek_is(TokenType::RBRACE));

  if (not tokens.pop_if(TokenType::RBRACE))
    report_error(current_file, tokens.peek(), "Expected closing curly brace after struct definition.");

  tu.module->addCustomType(name, std::span(members, i));
}

void parseImports(TU& tu, TokenView& tokens) {
  auto const& current_file = tu.source_files.back();
  while (tokens.pop_if(TokenType::KEYWORD_IMPORT)) {
    auto name_token = tokens.take();
    if (not name_token.isIdentifier())
      return report_error(current_file, name_token, "Expected module name.");

    auto name = name_token.getString(current_file);
    if (tu.module->nameof() == name)
      report_error(current_file, name_token, "Cannot import from current module");
    else
      tu.imports.emplace_back(name);

    if (not tokens.pop_if(TokenType::SEMI_COLON))
      report_error(current_file, tokens.peek(), "Expected semicolon.");
  }
}

Function parseFunction(TU& tu, Body& global_body) {
  auto& tokens = global_body.tokens;
  auto const& current_file = tu.source_files.back();

  Function current_function;
  current_function.file_idx = static_cast<u8_t>(tu.source_files.size() - 1);
  bool const is_public = tokens.pop_if(TokenType::KEYWORD_PUB);

  if (not tokens.pop_if(TokenType::KEYWORD_FN))
    report_error(current_file, tokens.peek(), "Expected function declaration.");

  // name
  {
    auto const function_name = parseIdentifier(tokens, tu);
    current_function.name_ptr = function_name.data();
    current_function.name_len = function_name.length();
  }

  if (not tokens.pop_if(TokenType::LPAREN))
    report_error(current_file, tokens.peek(), "Expected parameter list.");

  // parameters
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
        report_error(tu.source_files.back(), tokens.peek(), std::format("Functions may have no more than {} parameters.", Settings::MAX_FUNCTION_PARAMETERS));
        break;
      }
    }

    end_params:
    if(not tokens.pop_if(TokenType::RPAREN))
      report_error(current_file, tokens.peek(), "Expected closing parenthesis in parameter list.");

    const Type *return_type = Type::devoid();
    if (not tokens.peek_is(TokenType::LBRACE))
      return_type = parseUnqualifiedType(tokens, tu);

    tu.module->addFunction(current_function.nameof(), std::span(parameters, i), return_type, is_public);
    tu.module->enterFunctionScope(current_function.nameof());
  }

  if (not tokens.pop_if(TokenType::LBRACE))
    report_error(current_file, tokens.peek(), "Expected function definition.");

  // body
  {
    Body function_body(tokens, tu, global_body.expression_tree);
    function_body.parseStatementsBetweenBraces();
    current_function.body = std::move(function_body.tree);
    tokens = function_body.tokens;
  }

  return current_function;
}

void parseFunctionsAndStructs(TU& tu, Body& global_body, std::vector<Function>& functions) {
  auto& tokens = global_body.tokens;

  while (not tokens.peek_is(TokenType::INVALID_TOKEN)) {
    if (tokens.pop_if(TokenType::DUNDER_CEXTERN))
      parseCExtern(tu, tokens);
    else if (tokens.pop_if(TokenType::KEYWORD_STRUCT))
      parseStructDecl(tu, tokens);
    else
      functions.emplace_back( parseFunction(tu, global_body) );
  }

}

void printFunction(Function const& func, TU const& tu) {
  auto const function = tu.module->getFunction(func.nameof());
  auto const parameters = function->parameters();
  auto const return_type = function->returnType();

  std::print("fn {} (", func.nameof());

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

