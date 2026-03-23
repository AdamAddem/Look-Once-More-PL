#include "parse.hpp"

#include "ast/ast.hpp"
#include "error.hpp"
#include "semantic_analysis/symbol_table.hpp"

#include <iostream>
#include <unordered_set>
#include <utility>

#include "lexing/lex.hpp"
#include "settings.hpp"

using namespace LOM;
using namespace LOM::Lexer;
using namespace LOM::Parser;
using namespace LOM::AST;


namespace {
InstantiatedType parseType(TokenView& tokens, SymbolTable& table) {
  Token token = tokens.take();
  bool is_mutable{false};
  if (token.isTypeModifier()) {
    if (token.is(TokenType::KEYWORD_MUT))
      is_mutable = true;
    else
      throw ParsingError("Type modifier not implemented yet, soz.", token);

    token = tokens.take();
  }

  if (token.isPrimitive() || token.is(TokenType::IDENTIFIER)) {
    if (token.is(TokenType::KEYWORD_DEVOID))
      return devoid_instance;

    if (token.isPointer()) { //add reference support eventually
      tokens.expect_then_pop(TokenType::ARROW, "Expected arrow in pointer declaration.");

      const InstantiatedType subtype_instance = parseType(tokens, table);
      switch (token.type) {
      case TokenType::KEYWORD_RAW:
        return InstantiatedType(table.addRawPointer(subtype_instance.type, subtype_instance.details.is_mutable), is_mutable);
      case TokenType::KEYWORD_UNIQUE:
        return InstantiatedType(table.addUniquePointer(subtype_instance.type, subtype_instance.details.is_mutable), is_mutable);
      case TokenType::KEYWORD_VAGUE:
        return InstantiatedType(&vague_pointer, is_mutable);

      default:
        assert(false);
      }

    }

    switch (token.type) {
    case TokenType::KEYWORD_i8:
      return InstantiatedType(&i8_type, is_mutable);
    case TokenType::KEYWORD_i16:
      return InstantiatedType(&i16_type, is_mutable);
    case TokenType::KEYWORD_i32:
      return InstantiatedType(&i32_type, is_mutable);
    case TokenType::KEYWORD_i64:
      return InstantiatedType(&i64_type, is_mutable);
    case TokenType::KEYWORD_u8:
      return InstantiatedType(&u8_type, is_mutable);
    case TokenType::KEYWORD_u16:
      return InstantiatedType(&u16_type, is_mutable);
    case TokenType::KEYWORD_u32:
      return InstantiatedType(&u32_type, is_mutable);
    case TokenType::KEYWORD_u64:
      return InstantiatedType(&u64_type, is_mutable);
    case TokenType::KEYWORD_f32:
      return InstantiatedType(&f32_type, is_mutable);
    case TokenType::KEYWORD_f64:
      return InstantiatedType(&f64_type, is_mutable);

    case TokenType::KEYWORD_CHAR:
      return InstantiatedType(&char_type, is_mutable);
    case TokenType::KEYWORD_BOOL:
      return InstantiatedType(&bool_type, is_mutable);
    case TokenType::KEYWORD_STRING:
      return InstantiatedType(&string_type, is_mutable);
    case TokenType::KEYWORD_DEVOID:
      assert(is_mutable == false && "Mutable devoid? Is that allowed?");
      return devoid_instance;
    default:
      assert(false);
    }
  }

  if (token.is(TokenType::LESS)) {
    TokenView variant_types_tokens = tokens.getTokensBetweenAngleBrackets();

    bool nullable{false};
    std::vector<const Type*> subtypes;
    std::unordered_set<const Type* > typenames; //prevent duplicate types

    const unsigned variant_ln = variant_types_tokens.peek().line_number;
    do {
      const unsigned subtype_ln = variant_types_tokens.peek().line_number;
      const InstantiatedType subtype = parseType(tokens, table);
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

    return InstantiatedType(table.addVariant(subtypes, nullable), is_mutable);
  }

  throw ParsingError("Expected typename.", token);
}

std::string parseIdentifier(TokenView& tokens) {
  Token token = tokens.take();

  if (token.is(TokenType::IDENTIFIER))
    return token.takeString();

  throw ParsingError("Expected identifier.", token);
}


struct Body {
  std::vector<ASTNode> tree;
  TokenView tokens;
  SymbolTable& table;

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

    void reset() {
      begin = 1;
    }
  };

  ExpressionTree& expression_tree;

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

  u16_t generatePrimaryExpression() {
    if (tokens.empty())
      return 0;

    if (tokens.peek().isLiteral()) {
      auto token = tokens.take();
      auto literal_type = ASTNode::EMPTY;
      u64_t literal_value;

      switch (token.type) {
      case TokenType::INT_LITERAL:
        literal_type = ASTNode::INT_LITERAL;
        literal_value = std::bit_cast<u64_t>(token.getInt());
        break;
      case TokenType::UINT_LITERAL:
        literal_type = ASTNode::UINT_LITERAL;
        literal_value = token.getUint();
        break;
      case TokenType::FLOAT_LITERAL:
        literal_type = ASTNode::FLOAT_LITERAL;
        literal_value = token.getUint();
        break;
      case TokenType::DOUBLE_LITERAL:
        literal_type = ASTNode::DOUBLE_LITERAL;
        literal_value = token.getUint();
        break;
      case TokenType::BOOL_LITERAL:
        literal_type = ASTNode::BOOL_LITERAL;
        literal_value = token.getUint();
        break;
      case TokenType::CHAR_LITERAL:
        literal_type = ASTNode::CHAR_LITERAL;
        literal_value = token.getUint();
        break;
      case TokenType::STRING_LITERAL:
        literal_type = ASTNode::STRING_LITERAL;
        literal_value = std::bit_cast<u64_t>(
          new std::string(token.takeString()));
        break;
      default:
        assert(false);
      }

      return expression_tree.create(literal_type, literal_value);
    }

    if (tokens.pop_if(TokenType::LPAREN)) {
      const auto res = generateExpressionUntil(TokenType::LPAREN);
      return res;
    }

    std::string identifier = tokens.take().takeString();
    const auto identifier_value = std::bit_cast<u64_t>(new std::string(std::move(identifier)));
    return expression_tree.create(ASTNode::IDENTIFIER, identifier_value);
  }

  u16_t generatePostfixExpression() {
    auto left = generatePrimaryExpression();
    while (true) {
      u64_t value;
      switch (tokens.peek().type) {
      case TokenType::PLUSPLUS:
        value = static_cast<u64_t>(Operator::POST_INCREMENT);
        break;
      case TokenType::MINUSMINUS:
        value = static_cast<u64_t>(Operator::POST_DECREMENT);
        break;

      case TokenType::LPAREN: {
        tokens.pop();
        const u16_t parameters = tokens.pop_if(TokenType::RPAREN) ? 0 : generateParameters();
        left = expression_tree.create(
          ASTNode::CALLING, 0,
          left, parameters);
        continue;
      }
      case TokenType::LBRACKET:
        tokens.pop();
        tokens.reject(TokenType::RBRACKET, "Expected expression within brackets.");
        left = expression_tree.create(
          ASTNode::SUBSCRIPT, 0,
          left, generateSubscript());
        continue;
      default:
        return left;
      }

      tokens.pop();
      left = expression_tree.create(
        ASTNode::UNARY, value,
        generatePrimaryExpression(), 0);
    }
  }

  u16_t generatePrefixExpression() {
    u64_t value;
    switch (tokens.peek().type) {
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

  u16_t generateExponentExpression() {
    auto left = generatePrefixExpression();
    while (tokens.pop_if(TokenType::POW)) {
      left = expression_tree.create(
        ASTNode::BINARY, static_cast<u64_t>(Operator::POWER),
        left, generatePrefixExpression());
    }

    return left;
  }

  u16_t generateFactorExpression() {
    auto left = generateExponentExpression();
    while (true) {
      u64_t value;
      switch (tokens.peek().type) {
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
        left, generateExponentExpression());
    }
  }

  u16_t generateTermExpression() {
    auto left = generateFactorExpression();
    while (true) {
      u64_t value;
      switch (tokens.peek().type) {
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
      switch (tokens.peek().type) {
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
      switch (tokens.peek().type) {
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
      switch (tokens.peek().type) {
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

  u16_t generateExpressionUntil(TokenType ending_token) {
    const TokenView until_ending = tokens.getAllTokensUntilFirstOf(ending_token); tokens.pop();
    const TokenView after_ending = tokens;
    tokens = until_ending;
    const auto res = generateAssignmentExpression();
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
      assert(false);

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
      u16_t left_idx = expression.left_idx;
      u16_t right_idx = expression.right_idx;
      u64_t num_parameters{};
      while (true) {
        translateExpression(left_idx);
        left_idx = expression_tree.data[right_idx].left_idx;
        right_idx = expression_tree.data[right_idx].right_idx;
        if (right_idx == 0)
          break;
        ++num_parameters;
      }

      tree[calling_idx].value = num_parameters - 1;
      return;
    }
      assert(false);
    case SUBSCRIPT:
      tree.emplace_back(SUBSCRIPT, expression.value);
      translateExpression(expression.left_idx);
      translateExpression(expression.right_idx);
      return;

    case IDENTIFIER:
    case INT_LITERAL:
    case UINT_LITERAL:
    case FLOAT_LITERAL:
    case DOUBLE_LITERAL:
    case BOOL_LITERAL:
    case CHAR_LITERAL:
    case STRING_LITERAL:
      tree.emplace_back(expression.type, expression.value);
      return;

    default:
      assert(false);
    }
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
    parseExpressionUntil(TokenType::SEMI_COLON);
  }

  void parseVarDecl(bool is_global = false) {
    tree.emplace_back(ASTNode::DECLARATION, tokens.current_line_number());
    InstantiatedType variable_type = parseType(tokens, table);
    std::string ident = parseIdentifier(tokens);
    tree.emplace_back(ASTNode::IDENTIFIER,std::bit_cast<u64_t>(new std::string(ident)));
    tokens.expect_then_pop(TokenType::ASSIGN, "Expected assignment in variable declaration.");

    if (tokens.peek_is(TokenType::IDENTIFIER)) {
      if (tokens.peek().toString() == ident)
        throw ParsingError("Variable may not be initialized using itself.", tokens.peek());

      assert(false && "custom types unimplemented");
    }

    if (tokens.pop_if(TokenType::KEYWORD_JUNK)) {
      tree.emplace_back(ASTNode::EMPTY);
      tokens.expect_then_pop(TokenType::SEMI_COLON, "Expected semicolon ending junk variable declaration.");
    }
    else {
      tokens.reject(TokenType::SEMI_COLON, "Expected initializing expression in variable declaration.");
      parseExpressionUntil(TokenType::SEMI_COLON);
    }

    if (is_global)
      table.addGlobalVariable(std::move(ident), variable_type);
    else
      table.addLocalVariable(std::move(ident), variable_type);
  }

  void parseIf() {
    tree.emplace_back(ASTNode::IF, tokens.current_line_number());
    tokens.expect_then_pop(TokenType::LPAREN, "Expected opening parenthesis in if statement condition.");
    tokens.reject(TokenType::RPAREN, "Expected condition in if statement.");
    parseExpressionUntil(TokenType::RPAREN);
    parseScoped();
    if (tokens.pop_if(TokenType::KEYWORD_ELSE))
      parseStatement();
    else
      tree.emplace_back(ASTNode::EMPTY);
  }

  void parseFor() {
    tree.emplace_back(ASTNode::FOR, tokens.current_line_number());
    tokens.expect_then_pop(TokenType::LPAREN, "Expected opening patenthesis in for loop statement.");
    tokens.reject(TokenType::RPAREN, "Expected for loop header.");

    parseVarDecl();
    parseExpressionUntil(TokenType::SEMI_COLON);
    parseExpressionUntil(TokenType::RPAREN);
    parseScoped();
  }

  void parseWhile() {
    tree.emplace_back(ASTNode::WHILE, tokens.current_line_number());
    tokens.expect_then_pop(TokenType::LPAREN,"Expected open parenthesis in while loop condition.");
    tokens.reject(TokenType::RPAREN, "Expected while loop condition.");

    parseExpressionUntil(TokenType::RPAREN);
    parseScoped();
  }

  void parseScoped() {
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
      tree[scoped_idx].value = num_statements;
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

    switch (first.type) {
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
      assert(false);
    case TokenType::KEYWORD_RETURN:
      tokens.pop();
      return parseReturn();
    case TokenType::KEYWORD_SWITCH:
      assert(false);
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

bool parseGlobal(Body& global_body) {
  TokenView& tokens = global_body.tokens;
  if (tokens.peek_is(TokenType::KEYWORD_FN) || tokens.empty())
    return false;

  tokens.expect_then_pop(TokenType::KEYWORD_GLOBAL, "Expected global keyword before declaration.");
  global_body.parseVarDecl(true);
  return true;
}

void parseFunctions(Body& global_body, std::vector<ParsedFunction>& functions) {
  TokenView& tokens = global_body.tokens;
  SymbolTable& table = global_body.table;
  std::vector<TokenView> function_tokens;

  //just parses the declarations to load into symbol table
  while (not tokens.empty()) {
    ParsedFunction& function = functions.emplace_back();
    tokens.expect_then_pop(TokenType::KEYWORD_FN, "Expected function declaration.");
    function.name = parseIdentifier(tokens);
    tokens.expect_then_pop(TokenType::LPAREN, "Expected parameter list.");

    SymbolTable::Variable parameters[Settings::MAX_FUNCTION_PARAMETERS];
    u64_t num_parameters{};
    if (not tokens.pop_if(TokenType::RPAREN))
      while (true) {
        parameters[num_parameters].type = parseType(tokens, table);
        parameters[num_parameters].name = parseIdentifier(tokens);
        ++num_parameters;
        if (not tokens.pop_if(TokenType::COMMA)) {
          tokens.expect_then_pop(TokenType::RPAREN, "Expected closing parenthesis in parameter list.");
          break;
        }
      }

    const Type* return_type = &devoid_type;
    if (tokens.pop_if(TokenType::ARROW)) {
      InstantiatedType type = parseType(tokens, table);
      if (not type.isPlain())
        throw ParsingError("Return type may not have type qualifiers.", type.toString(), tokens.current_line_number());
      return_type = type.type;
    }

    tokens.expect_then_pop(TokenType::LBRACE, "Expected function definition.");
    function_tokens.emplace_back(tokens.getTokensBetweenBraces());
    table.addFunction(function.name, std::span(parameters, num_parameters), return_type);
  }

  //actually parse the function bodies
  const auto sz = functions.size();
  assert(sz == function_tokens.size());
  for (auto i{0uz}; i<sz; ++i) {
    Body function_body
    {{}, function_tokens[i], table, global_body.expression_tree};

    table.enterFunctionScope(functions[i].name);
    function_body.parseStatementsUntilEmpty();
    functions[i].function_body.nodes = std::move(function_body.tree);
  }
}

void printFunction(const ParsedFunction& func, SymbolTable& table) {
  std::cout << "\nfn " << func.name << "(";
  const auto parameters = table.parametersOfFunction(func.name);
  const auto return_type = table.returnTypeOfFunction(func.name);

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
  std::cout << " {\n";
  func.function_body.print();
  std::cout << "\n}";
}

void printTU(ParsedTU& ptu) {
  ptu.global_tree.print();
  for (const auto &f : ptu.functions) {
    printFunction(f, ptu.table);
    std::cout << "\n";
  }

}

}
ParsedTU Parser::parseTokens(std::vector<Token> &&token_list) {
  Body::ExpressionTree expression_tree;
  ParsedTU ptu;
  Body global_body{{}, TokenView{token_list}, ptu.table,
    expression_tree};


  while (parseGlobal(global_body)) {}
  parseFunctions(global_body, ptu.functions);

  if (Settings::doOutputParser()) {
    std::cout << "\n--- Parser Output ---\n";
    printTU(ptu);
    std::cout << "--- Parser Output ---\n";
    std::quick_exit(0);
  }

  return ptu;
}







