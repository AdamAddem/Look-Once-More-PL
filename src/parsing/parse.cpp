#include "parse.hpp"

#include "edenlib/vectors/swap_vector.hpp"

#include "ast.hpp"
#include "build_system/build.hpp"
#include "error.hpp"
#include "lexing/lex.hpp"
#include "module/table_and_module.hpp"
#include "module/types.hpp"

#include <numeric>
#include <utility>

using namespace LOM;
using namespace LOM::Lexer;
using namespace LOM::Parser;

namespace {
NoInlineCold [[noreturn]] void throw_notfound() Throws(0) { throw 0; }

struct Expression {
  u32_t left_idx;
  u32_t right_idx; // unary expressions do not contain right_idx

  union { ASTNode node; };

  constexpr Expression() noexcept {}
};

// Exists to allow expression parsing w/o allocation
struct ExpressionTree {
  static constexpr u64_t max_expressiontree_size{256};
  Expression data[max_expressiontree_size];
  sz_t begin{};

  constexpr ExpressionTree() noexcept {}

  NodiscardCXPR u32_t
  create(ASTNode node, u32_t left_idx = 0, u32_t right_idx = 0) noexcept {
    assert(left_idx < max_expressiontree_size); assert(right_idx < max_expressiontree_size);
    ++begin; assert(begin < max_expressiontree_size);

    auto& expr = data[begin];
    expr.left_idx = left_idx;
    expr.right_idx = right_idx;
    expr.node = node;
    return begin;
  }

  InlineCXPR void reset() noexcept { begin = 0; }
};

class ParsingContext {
  // static constexpr auto name_search = [] (std::string_view e, std::string_view key) static { return e == key; };
  eden::vector<ASTNode> nodes;
  TokenView tokens;
  File current_file;
  TU& tu;
  Module& module;
  bool has_errors{};
  ExpressionTree expression_tree; // the stack can have 6kb as a treat :)

  constexpr ParsingContext(std::span<Token> tokens, TU& tu) noexcept : tokens(tokens), current_file(tu.source_files.back()), tu(tu), module(tu.module) {}

  NodiscardCXPR u8_t
  current_file_idx() const noexcept {
    assert((tu.source_files.size() - 1) <= u8_max);
    return u8_t(tu.source_files.size() - 1);
  }

  // adds an uninitialized node containing only the current file idx and node_type
  // returns index of node
  InlineNodiscardCXPR sz_t
  insertTypedNode(ASTNode::NodeType node_type) noexcept {
    auto const new_node_idx = nodes.size();
    nodes.emplace_back(
      ASTNode {
      .type = node_type,
      .file_idx = current_file_idx(),
      .length = 0,
      .position = 0,
      ._ = 0
      }
      );
    return new_node_idx;
  }

  InlineNodiscardCXPR ASTNode
  newNode(Token token, ASTNode::NodeType type = ASTNode::EMPTY) const noexcept {
    return ASTNode{
      .type = type,
      .file_idx = current_file_idx(),
      .length = token.length, .position = token.position,
      ._ = 0
    };
  }

  NoInlineCold void
  error(Token err, std::string_view msg) noexcept {
    reportError(current_file, err, std::string(msg)); has_errors = true;
  }

#define pre assert(tokens.previous().is(TokenType::LBRACKET));
  NodiscardCXPR TypeID parseArrayType() noexcept { pre
    auto const& current_file = tu.source_files.back();
    auto const token = tokens.take();
    u64_t array_size;

    if (not token.is(TokenType::INTEGER_LITERAL))
      error(token, "Expected integer literal in array type."), array_size = 1;
    else array_size = token.getInt(current_file);

    if (not tokens.pop_if(TokenType::RBRACKET)) {
      error(tokens.peek(), "Expected ] while parsing array type.");
      tokens.pop();
    }

    auto const subtype = parseType();
    return module.addArrayType(subtype, array_size);
  }
#undef pre

#define pre assert(pointer_token.isPointer());
  NodiscardCXPR TypeID parsePointerType(Token pointer_token) noexcept { pre
    auto x = parseType();
    x.addPointer(pointer_token.is(TokenType::KEYWORD_RAW));
    return x;
  }
#undef pre

#define pre assert(primitive_token.isPrimitive());
  InlineNodiscardCXPR static TypeID parsePrimitiveType(Token primitive_token) noexcept { pre
    switch (primitive_token.type) { using enum TokenType;
    case KEYWORD_u8:      return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::U8)};
    case KEYWORD_u16:     return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::U16)};
    case KEYWORD_u32:     return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::U32)};
    case KEYWORD_u64:     return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::U64)};
    case KEYWORD_i8:      return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::I8)};
    case KEYWORD_i16:     return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::I16)};
    case KEYWORD_i32:     return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::I32)};
    case KEYWORD_i64:     return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::I64)};
    case KEYWORD_f32:     return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::F32)};
    case KEYWORD_f64:     return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::F64)};
    case KEYWORD_CHAR:    return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::CHAR)};
    case KEYWORD_STRING:  return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::STRING)};
    case KEYWORD_BOOL:    return {.derived = Type::PRIMITIVE, .id = std::to_underlying(PrimitiveType::BOOL)};
    default: Unreachable("Invalid primitive token.");
    }
  }
#undef pre

  NodiscardCXPR TypeID parseType() noexcept {
    auto const& current_file = tu.source_files.back();
    auto const token = tokens.take();

    switch (token.type) { using enum TokenType;
    TOKENTYPE_PRIMITIVES_CASES return parsePrimitiveType(token);
    TOKENTYPE_POINTERS_CASES   return parsePointerType(token);
    case LBRACKET:             return parseArrayType();

    case IDENTIFIER: {
      auto const type = module.getCustomTypeID(token.originalString(current_file));
      if (type == devoidID)
        error(token, "Expected typename.");
      return type;
    }

    default:
      error(token, "Expected typename.");
      return errorID;
    }
  }

  NodiscardCXPR std::string_view parseIdentifier() noexcept {
    if (not tokens.peek_is(TokenType::IDENTIFIER)) {
      error(tokens.take_if_valid(), "Expected identifier.");
      return "!ERROR!";
    }
    return tokens.take().originalString(tu.source_files.back());
  }

  // opening parenthesis must be popped, and the next token must not be closing parenthesis (?) commented out cause idk if this is true TODO: check
  //#define pre assert(tokens.previous().is(TokenType::LPAREN));
  NodiscardCXPR u32_t generateParameters() { //pre
    auto const parameter = generateAssignmentExpression();
    if (tokens.pop_if(TokenType::RPAREN))
      return expression_tree.create(PLACEHOLDER_NODE, parameter, 0);

    if (tokens.pop_if(TokenType::COMMA))
      return expression_tree.create(PLACEHOLDER_NODE, parameter, generateParameters());

    error(tokens.peek(), "Expected comma in call expression.");
    return expression_tree.create(PLACEHOLDER_NODE, parameter, generateParameters());
  }
  //#undef pre

#define pre assert(tokens.peek().isLiteral());
  template <bool negate = false>
  NodiscardCXPR u32_t generateLiteral() { pre
    auto const token = tokens.take();
    ASTNode node = newNode(token);

    switch (token.type) {
    case TokenType::INTEGER_LITERAL:
      if constexpr (negate)
        node.type = ASTNode::SIGNED_LITERAL,
        node.signed_data.value = -token.getInt(current_file);
      else
        node.type = ASTNode::UNSIGNED_LITERAL,
        node.unsigned_data.value = token.getInt(current_file);
      break;

    case TokenType::FLOAT_LITERAL:
      node.type = ASTNode::FLOAT_LITERAL;
      node.float_data.value = negate ? -token.getFloat(current_file) : token.getFloat(current_file);
      break;
    case TokenType::DOUBLE_LITERAL:
      node.type = ASTNode::DOUBLE_LITERAL;
      node.double_data.value = negate ? -token.getDouble(current_file) : token.getDouble(current_file);
      break;
    case TokenType::BOOL_LITERAL:
      node.type = ASTNode::BOOL_LITERAL;
      node.bool_data.value = negate ? not token.getBool(current_file) : token.getBool(current_file);
      break;
    case TokenType::CHAR_LITERAL:
      node.type = ASTNode::CHAR_LITERAL;
      node.char_data.value = negate ? -token.getChar(current_file) : token.getChar(current_file);
      break;

    case TokenType::STRING_LITERAL:           node.type = ASTNode::STRING_LITERAL; assert(not negate); break;
    case TokenType::ESCAPED_STRING_LITERAL:   node.type = ASTNode::ESCAPED_STRING_LITERAL; assert(not negate); break;
    default: Unreachable("Invalid literal token type.");
    }

    return expression_tree.create(node);
  }
#undef pre

#define pre assert(module_name_token.isIdentifier() or module_name_token.is(TokenType::DUNDER_CEXTERN));
  NodiscardCXPR u32_t generateModuleAccess(Token module_name_token) { pre
    auto const module_name = module_name_token.originalString(current_file);
    auto access_node = newNode(module_name_token, ASTNode::MODULE_ACCESS);

    {
      auto const module_id_opt = getModuleID(module_name);
      if (module_id_opt.has_value()) access_node.module_access_data.module_id = module_id_opt.value();
      else error(module_name_token, "Module not found.");
    }

    if (not tokens.peek_is(TokenType::IDENTIFIER))
      error(tokens.peek(), "Expected identifier.");

    {
      auto const member_token = tokens.take();
      access_node.length = member_token.position + member_token.length - access_node.position;
      access_node.module_access_data.member_offset = eden::narrow<u16_t>(member_token.position - access_node.position);
    }

    static_assert(not Settings::SUBMODULE_SUPPORT);
    return expression_tree.create(access_node);
  }
#undef pre

  NodiscardCXPR u32_t generatePrimaryExpression() {
    switch (tokens.peek().type) { using enum TokenType;
    TOKENTYPE_LITERALS_CASES return generateLiteral();
    case DUNDER_CEXTERN: {
      auto const cmodule_token = tokens.take();
      if (not tokens.pop_if(DOUBLE_COLON)) {
        error(tokens.peek(), "Expected '::' when accessing __C module.");
        syncToSemicolon();
        return 0;
      }

      return generateModuleAccess(cmodule_token);
    }

    case LPAREN: {
      tokens.pop();
      auto const res = generateAssignmentExpression();
      if (not tokens.pop_if(RPAREN)) error(tokens.take_if_valid(), "Expected closing ')'");
      return res;
    }

    case IDENTIFIER: {
      auto const identifier_node = tokens.take();

      if (tokens.pop_if(DOUBLE_COLON))
        return generateModuleAccess(identifier_node);

      if (not tokens.peek_is(DOT))
        return expression_tree.create( newNode(identifier_node, ASTNode::IDENTIFIER) );

      auto const dot_token = tokens.take();
      auto const identifier_idx = expression_tree.create( newNode(identifier_node, ASTNode::IDENTIFIER) );
      auto const member_idx = generatePrimaryExpression();
      auto const member_access_node = newNode(dot_token, ASTNode::MEMBER_ACCESS);
      return expression_tree.create(member_access_node, identifier_idx, member_idx);
    }

    default: throw_notfound();
    }
  }

  NodiscardCXPR u32_t generatePostfixExpression() {
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
          syncToSemicolon();
        }

        continue;

      default: return left;
      }

      auto node = newNode(token, ASTNode::UNARY); node.unary_data.opr = opr;
      tokens.pop();
      left = expression_tree.create(node, left);
    }
  }

  NodiscardCXPR u32_t generatePrefixExpression() {
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

  NodiscardCXPR u32_t generateFactorExpression() {
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

  NodiscardCXPR u32_t generateTermExpression() {
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

  NodiscardCXPR u32_t generateRelationalExpression() {
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

  NodiscardCXPR u32_t generateBitwiseExpression() {
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

  NodiscardCXPR u32_t generateLogicalExpression() {
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

  NodiscardCXPR u32_t generateAssignmentExpression() {
    auto const left = generateLogicalExpression();
    if (tokens.peek_is(TokenType::ASSIGN)) {
      auto const token = tokens.take();
      auto node = newNode(token, ASTNode::BINARY);
      node.binary_data.opr = Operator::ASSIGN;

      return expression_tree.create(node, left, generateAssignmentExpression());
    }

    return left;
  }

#define pre assert(idx != 0 || expression_tree.data[idx].node.type == ASTNode::EMPTY);
  constexpr void translateExpression(u32_t idx) noexcept { pre
    auto expression = expression_tree.data[idx];
    switch (expression.node.type) { using enum ASTNode::NodeType;
    case EMPTY: assert(has_errors); nodes.emplace_back(PLACEHOLDER_NODE); return;

    case DECLARATION_JUNK_RO: case DECLARATION_JUNK_RW: case DECLARATION_RO: case DECLARATION_RW:
    case IF:
    case WHILE: case RETURN:
      Unreachable("Statements should not be contained in an expression.");

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

  InlineCXPR void syncToSemicolon() noexcept {
    while (not tokens.pop_if(TokenType::SEMI_COLON)) {
      if (tokens.peek_is(TokenType::INVALID_TOKEN)) return;
      tokens.pop();
    }
  }

  InlineCXPR void syncToMatchingBraces() noexcept {
    sz_t level{};
    while (not tokens.peek_is(TokenType::INVALID_TOKEN)) {
      if (tokens.pop_if(TokenType::LBRACE)) ++level;
      else if (tokens.pop_if(TokenType::RBRACE)) { if (level <= 1) return; --level; }
      tokens.pop();
    }
  }


  // parses  `name  : or $  type`
  // returns name token, qualifiers, and typeID
  template <bool is_parameter = false>
  NodiscardCXPR std::tuple<Token, Type::Qualifiers, TypeID>
  parseHalfDeclaration() noexcept {
    auto const identifier_token = tokens.take();
    Type::Qualifiers qualifiers;

    if (not identifier_token.isIdentifier())  error(identifier_token, "Expected identifier.");

    if (tokens.pop_if(TokenType::COLON)) qualifiers.writable = false;
    else if (tokens.pop_if(TokenType::DOLLAR)) {
      if constexpr(is_parameter)
        error(tokens.previous(), "Readwrite parameters are not allowed.");
      else
        qualifiers.writable = true;
    }
    else
      error(tokens.peek(), "Expected : or $ in declaration.");

    auto const typeID = parseType();
    return {identifier_token, qualifiers, typeID};
  }

#define pre assert(tokens.peek().isIdentifier());
  constexpr Token parseVarDecl(sz_t decl_node_idx) noexcept { pre
    auto const [identifier_token, qualifiers, typeID] = parseHalfDeclaration();
    auto const decl_name = identifier_token.originalString(current_file);
    if (module.containsLocal(decl_name))
      error(identifier_token, "Redefinition of symbol name in variable declaration.");
    else
      module.addLocal(typeID, qualifiers, decl_name);

    {
    auto& node = nodes[decl_node_idx];
    node.declaration_data.typeID = typeID;
    node.file_idx = current_file_idx();
    node.length = identifier_token.length;
    node.position = identifier_token.position;

    if (not tokens.pop_if(TokenType::ASSIGN)) {
      auto const err = tokens.peek();
      error(err, "Expected assignment in variable declaration. Use = junk; if you'd like to keep the variable uninitialized.");
      syncToSemicolon();
      return err;
    }

    if (tokens.pop_if(TokenType::KEYWORD_JUNK)) {
      node.type = qualifiers.writable ? ASTNode::DECLARATION_JUNK_RW : ASTNode::DECLARATION_JUNK_RO;
    }
    else {
      node.type = qualifiers.writable ? ASTNode::DECLARATION_RW : ASTNode::DECLARATION_RO;
      parseExpression();
    }
    }

    if (not tokens.peek_is(TokenType::SEMI_COLON)) {
      auto const err = tokens.peek();
      error(err, "Expected semi-colon ending variable declaration.");
      syncToSemicolon();
      return err;
    }

    return tokens.take();
  }
#undef pre

#define pre assert(tokens.previous().is(TokenType::KEYWORD_IF));
  constexpr Token parseIf(sz_t if_node_idx) noexcept { pre
    parseExpression();
    if (not tokens.pop_if(TokenType::LBRACE)) {
      error(tokens.peek(), "Expected {.");
    }

    sz_t num_substatements{};
    while (not tokens.peek_is(TokenType::RBRACE)) {
      parseStatement();
      ++num_substatements;
      if (tokens.peek().isInvalid()) {
        error(tokens.peek(), "Expected closing } in scoped statement.");
        break;
      }
    }

    if (num_substatements == 0)
      error(tokens.peek(), "if statement with empty body.");

    auto& if_data = nodes[if_node_idx].if_data;
    if_data.num_substatements = num_substatements;
    if_data.has_else = false;

    auto final_token = tokens.take();
    if (tokens.pop_if(TokenType::KEYWORD_ELSE)) {
      if_data.has_else = true;
      final_token = parseStatement();
    }

    return final_token;
  }
#undef pre

#define pre assert(tokens.previous().is(TokenType::KEYWORD_WHILE));
  constexpr Token parseWhile(sz_t while_node_idx) noexcept { pre
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
  constexpr Token parseReturn(sz_t return_node_idx) noexcept { pre
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

  constexpr Token parseStatement() noexcept {
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
        stmt_idx = insertTypedNode(ASTNode::EMPTY); // type will be filled later
        return parseVarDecl(stmt_idx); // avoid getting the combined token for the whole statement
      }

    [[fallthrough]];
    default: // expression statement
      stmt_idx = nodes.size();
      parseExpression<true>();
      final = tokens.previous();
      if (not tokens.peek_is(SEMI_COLON)) {
        if (tokens.peek_ahead(1).is(ASSIGN))
          error(tokens.peek(), "Expected declaration qualifier ':' or '$'.");
        else
          error(tokens.peek(), "Expected semi-colon.");
        syncToSemicolon();
      }
      else tokens.pop();
    }

    auto const combined = Token::combine(first, final);
    auto& stmt_node = nodes[stmt_idx];
    stmt_node.length = combined.length;
    stmt_node.position = combined.position;
    return combined;
  }

#define pre assert(tokens.previous().is(TokenType::LBRACE));
  constexpr void parseStatementsBetweenBraces() noexcept { pre
    while (not tokens.pop_if(TokenType::RBRACE)) {
      parseStatement();
      if (tokens.peek_is(TokenType::INVALID_TOKEN)) {
        error(tokens.peek(), "Expected closing }.");
        break;
      }
    }
  }
#undef pre

#define pre assert(tokens.previous().is(TokenType::KEYWORD_IMPORT));
  constexpr void parseImport() noexcept { pre
    auto const& current_file = tu.source_files.back();
    auto const name_token = tokens.take();
    if (not name_token.isIdentifier()) {
      error(name_token, "Expected module name.");
      return;
    }

    error(name_token, "Imports not functional.");
    auto name = name_token.originalString(current_file);
    if (tu.name == name) error(name_token, "Cannot import from current module.");
    if (not tokens.pop_if(TokenType::SEMI_COLON))
      error(tokens.peek(), "Expected semicolon.");

  }
#undef pre

#define pre assert(tokens.previous().is(TokenType::LBRACE));
  constexpr void parseStructDecl(std::string_view name, [[maybe_unused]] bool is_public) noexcept { pre
    if (tokens.pop_if(TokenType::RBRACE))
      return (void)module.addCustomType(name, {});

    eden::swap_vector<Symbol::Variable> members{eden::flags::reserve_initial<2>};
    do { // makes it so structs must have a member, which is lowk dumb but whatever. TODO: Change
      auto const [member_name, member_qualifiers, member_type] = parseHalfDeclaration();
      members.emplace_back(member_type, member_qualifiers, member_name.originalString(current_file), true, members.size());
    } while (tokens.pop_if(TokenType::SEMI_COLON) and not tokens.peek_is(TokenType::RBRACE));

    if (not tokens.pop_if(TokenType::RBRACE))
      error(tokens.peek(), "Expected closing curly brace after struct definition.");

    (void)module.addCustomType(name, std::move(members));
  }
#undef pre

#define pre assert(tokens.previous().is(TokenType::DUNDER_CEXTERN));
  constexpr void parseCExtern() noexcept { pre
    auto const name = parseIdentifier();
    if (not tokens.pop_if(TokenType::LPAREN)) {
      error(tokens.peek(), "Expected opening ( for parameter list.");
      return syncToSemicolon();
    }

    eden::swap_vector16<Symbol::Variable> arguments;
    bool is_variadic = false;
    if (tokens.peek_is(TokenType::RPAREN)) goto end_params;

    while (true) {
      if (tokens.pop_if(TokenType::DUNDER_VA)) {
        is_variadic = true;
        break;
      }

      auto const [identifier_token, qualifiers, typeID] = parseHalfDeclaration<true>();
      auto const parameter_idx = arguments.size();
      arguments.emplace_back(typeID, qualifiers, identifier_token.originalString(current_file), false, parameter_idx);

      if (not tokens.pop_if(TokenType::COMMA)) break;

    }

    end_params:
    if (not tokens.pop_if(TokenType::RPAREN))
      error(tokens.peek(), "Expected closing parenthesis in parameter list.");

    auto return_typeID = devoidID;
    if (not tokens.peek_is(TokenType::SEMI_COLON))
      return_typeID = parseType();

    if (not tokens.pop_if(TokenType::SEMI_COLON))
      return error(tokens.peek(), "Expected semi-colon.");

    addExternDeclaration( name, std::move(arguments), return_typeID, is_variadic );
  }
#undef pre

#define pre assert(tokens.previous().is(TokenType::LPAREN));
  constexpr void parseFunctionDecl(std::string_view name, bool is_public) noexcept { pre
    Function current_function;
    eden::swap_vector16<Symbol::Variable> parameters;

    // parameters
    if (not tokens.pop_if(TokenType::RPAREN)) {
      while (true) {
        auto const [name_token, qualifiers, typeID] = parseHalfDeclaration<true>();
        auto const parameter_idx = parameters.size();
        parameters.emplace_back(typeID, qualifiers, name_token.originalString(current_file), false, parameter_idx);
        if (not tokens.pop_if(TokenType::COMMA)) break;

      }
      if(not tokens.pop_if(TokenType::RPAREN)) error(tokens.peek(), "Expected closing parenthesis in parameter list.");
    }

    auto return_typeID = devoidID;
    if (not tokens.peek_is(TokenType::LBRACE))
      return_typeID = parseType();

    auto const function_id = module.addFunction( name, std::move(parameters), return_typeID, is_public );
    module.enterFunctionScope(function_id);

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

#define pre assert(tokens.peek().isIdentifier());
  constexpr void parseGlobalLevelDeclaration(bool is_public) noexcept { pre
    auto const name = parseIdentifier();
    if (tokens.peek_is(TokenType::COLON))
      tokens.pop();
    else {
      error(tokens.peek(), "Expected ':'");
      if (tokens.peek_is(TokenType::DUNDER_CEXTERN)) { syncToSemicolon(); return; }
    }

    switch (tokens.peek().type) {
    case TokenType::LPAREN: tokens.pop(); parseFunctionDecl(name, is_public); break;
    case TokenType::LBRACE: tokens.pop(); parseStructDecl(name, is_public); break;
    default:
      error(tokens.peek(), "Expected '{' or '(', globals unsupported, sorry!");
      syncToSemicolon();
    }
  }
#undef pre

public:
  NodiscardCXPR static bool
  parse(TU& tu, std::span<Token> tokens) noexcept {
    ParsingContext parser(tokens, tu);

    while (true) {
      switch (parser.tokens.peek().type) { using enum TokenType;
      case KEYWORD_IMPORT: parser.tokens.pop(); parser.parseImport(); break;
      case DUNDER_CEXTERN: parser.tokens.pop(); parser.parseCExtern(); break;

      case KEYWORD_PUB:
        parser.tokens.pop();

        if (parser.tokens.peek().isIdentifier())
          parser.parseGlobalLevelDeclaration(true);
        else
          parser.error(parser.tokens.take(), "Expected identifier in public declaration."),
          parser.syncToMatchingBraces();
        break;

      case IDENTIFIER: parser.parseGlobalLevelDeclaration(false); break;

      default: [[unlikely]]
        if (parser.tokens.peek_is(INVALID_TOKEN)) return parser.has_errors;
        parser.error(parser.tokens.take(), "Expected struct or function declaration.");
        break;
      }
    }
  }
};
}

bool Parser::parseTokens(TU& out_tu, std::span<Token> tokens) noexcept {
  return ParsingContext::parse(out_tu, tokens);
}

