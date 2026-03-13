#include "parse.hpp"
#include "ast/statements.hpp"

#include <iostream>
#include <unordered_set>
#include <utility>

#include "lexing/lex.hpp"
#include "settings.hpp"

using namespace Lexer;
using namespace Parser;
using namespace AST;


namespace {
/* Type and Identifier */
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
      tokens.expect_then_pop(TokenType::ARROW, "Expected arrow in pointer declaration.", LOMError::Stage::ParsingError);

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
/* Type and Identifier */

/* Expressions */
Expression* parseExpression(TokenView& tokens);

std::vector<Expression*> parseParameters(TokenView& between_parenthesis) {
  if (between_parenthesis.empty())
    return {};

  std::vector<Expression*> retval;

  while (true) {
    retval.push_back(parseExpression(between_parenthesis));
    if (between_parenthesis.empty())
      return retval;

    between_parenthesis.expect_then_pop(TokenType::COMMA, "Expected comma between function parameters in calling expression.", LOMError::Stage::ParsingError);
  }
}

Expression* parsePrimaryExpression(TokenView& tokens) {
  const unsigned ln = tokens.peek().line_number;
  if (tokens.peek().isLiteral()) {
    LiteralExpression::LiteralType type;
    switch (tokens.peek().type) {
    case TokenType::INT_LITERAL:
      type = LiteralExpression::INT;
      break;
    case TokenType::UINT_LITERAL:
      type = LiteralExpression::UINT;
      break;
    case TokenType::FLOAT_LITERAL:
      type = LiteralExpression::FLOAT;
      break;
    case TokenType::DOUBLE_LITERAL:
      type = LiteralExpression::DOUBLE;
      break;
    case TokenType::BOOL_LITERAL:
      type = LiteralExpression::BOOL;
      break;
    case TokenType::CHAR_LITERAL:
      type = LiteralExpression::CHAR;
      break;
    case TokenType::STRING_LITERAL:
      type = LiteralExpression::STRING;
      break;

    default:
      throw ParsingError("Impossible error in parsePrimaryExpression function in secondparse", tokens.peek());
    }

    return new Expression(std::in_place_type<LiteralExpression>, std::move(tokens.take().value), type, ln);
  }

  if (tokens.pop_if(TokenType::LPAREN)) {
    TokenView t = tokens.getTokensBetweenParenthesis();
    return parseExpression(t);
  }


  std::string ident = parseIdentifier(tokens);
  return new Expression(std::in_place_type<IdentifierExpression>, std::move(ident), ln);
}

Expression* parsePostfixExpression(TokenView& tokens) {
  Expression* left = parsePrimaryExpression(tokens);
  if (tokens.empty())
    return left;



  const unsigned ln = tokens.peek().line_number;
  while (true) {
    if (tokens.pop_if(TokenType::PLUSPLUS))
      left = new Expression(std::in_place_type<UnaryExpression>, left, Operator::POST_INCREMENT, ln);

    else if (tokens.pop_if(TokenType::MINUSMINUS))
      left = new Expression(std::in_place_type<UnaryExpression>, left, Operator::POST_DECREMENT, ln);

    else if (tokens.pop_if(TokenType::LPAREN)) {
      TokenView t = tokens.getTokensBetweenParenthesis();
      left = new Expression(std::in_place_type<CallingExpression>, left,  parseParameters(t), ln);
    } else if (tokens.pop_if(TokenType::LBRACKET)) {
      TokenView t = tokens.getTokensBetweenBrackets();
      left = new Expression(std::in_place_type<SubscriptExpression>, left, parseExpression(t), ln);
    } else
      break;
  };

  return left;
}

Expression* parsePrefixExpression(TokenView& tokens) {
  const unsigned ln = tokens.peek().line_number;
  if (tokens.pop_if(TokenType::PLUSPLUS))
    return new Expression{
      std::in_place_type<UnaryExpression>, parsePrefixExpression(tokens), Operator::PRE_INCREMENT, ln
    };

  if (tokens.pop_if(TokenType::MINUSMINUS))
    return new Expression{
      std::in_place_type<UnaryExpression>, parsePrefixExpression(tokens), Operator::PRE_DECREMENT, ln
    };

  if (tokens.pop_if(TokenType::MINUS))
    return new Expression{
      std::in_place_type<UnaryExpression>, parsePrefixExpression(tokens), Operator::UNARY_MINUS, ln
    };

  if (tokens.pop_if(TokenType::ADDR))
    return new Expression{
        std::in_place_type<UnaryExpression>, parsePrefixExpression(tokens), Operator::ADDRESS_OF, ln
    };

  if (tokens.pop_if(TokenType::KEYWORD_NOT))
    return new Expression{
        std::in_place_type<UnaryExpression>, parsePrefixExpression(tokens), Operator::NOT, ln
    };

  if (tokens.pop_if(TokenType::KEYWORD_BITNOT))
    return new Expression{
      std::in_place_type<UnaryExpression>, parsePrefixExpression(tokens), Operator::BITNOT, ln
  };


  return parsePostfixExpression(tokens);
}

Expression* parseExponentExpression(TokenView& tokens) {
  Expression* left = parsePrefixExpression(tokens);
  if (tokens.empty())
    return left;

  const unsigned ln = tokens.peek().line_number;
  while (true) {
    if (tokens.pop_if(TokenType::POW))
      left = new Expression{
        std::in_place_type<BinaryExpression>,left, parsePrefixExpression(tokens), Operator::POWER, ln
      };
    else
      break;
  }

  return left;
}

Expression* parseFactorExpression(TokenView& tokens) {
  Expression* left = parseExponentExpression(tokens);
  if (tokens.empty())
    return left;


  const unsigned ln = tokens.peek().line_number;
  while (true) {
    if (tokens.pop_if(TokenType::STAR))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseFactorExpression(tokens), Operator::MULTIPLY, ln
      };

    else if (tokens.pop_if(TokenType::SLASH))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseFactorExpression(tokens), Operator::DIVIDE, ln
      };

    else if (tokens.pop_if(TokenType::MOD))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseFactorExpression(tokens), Operator::MODULUS, ln
      };

    else
      break;
  }

  return left;
}

Expression* parseTermExpression(TokenView& tokens) {
  Expression* left = parseFactorExpression(tokens);
  if (tokens.empty())
    return left;

  const unsigned ln = tokens.peek().line_number;
  while (true) {
    if (tokens.pop_if(TokenType::PLUS))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseFactorExpression(tokens), Operator::ADD, ln
      };

    else if (tokens.pop_if(TokenType::MINUS))
      left = new Expression{
        std::in_place_type<BinaryExpression>,left, parseFactorExpression(tokens), Operator::SUBTRACT, ln
      };

    else
      break;
  }
  return left;
}

Expression* parseRelationalExpression(TokenView& tokens) {
  Expression* left = parseTermExpression(tokens);
  if (tokens.empty())
    return left;

  const unsigned ln = tokens.peek().line_number;
  while (true) {
    if (tokens.pop_if(TokenType::KEYWORD_EQUALS))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseTermExpression(tokens), Operator::EQUAL, ln
      };
    else if (tokens.pop_if(TokenType::KEYWORD_NOT_EQUAL))
      left = new Expression{
        std::in_place_type<BinaryExpression>,left, parseTermExpression(tokens), Operator::NOT_EQUAL, ln
      };
    else if (tokens.pop_if(TokenType::LESS))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseTermExpression(tokens), Operator::LESS, ln
      };
    else if (tokens.pop_if(TokenType::GTR))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseTermExpression(tokens), Operator::GREATER, ln
      };
    else if (tokens.pop_if(TokenType::LESSEQ))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseTermExpression(tokens), Operator::LESS_EQUAL, ln
      };
    else if (tokens.pop_if(TokenType::GTREQ))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseTermExpression(tokens), Operator::GREATER_EQUAL, ln
      };
    else
      break;
  }

  return left;
}

Expression* parseBitwiseExpression(TokenView& tokens) {
  Expression* left = parseRelationalExpression(tokens);
  if (tokens.empty())
    return left;

  const unsigned ln = tokens.peek().line_number;
  while (true) {
    if (tokens.pop_if(TokenType::KEYWORD_BITAND))
      left = new Expression{
        std::in_place_type<BinaryExpression>,left, parseRelationalExpression(tokens), Operator::BITAND, ln
      };

    else if (tokens.pop_if(TokenType::KEYWORD_BITOR))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseRelationalExpression(tokens), Operator::BITOR, ln
      };

    else if (tokens.pop_if(TokenType::KEYWORD_BITXOR))
      left = new Expression{
        std::in_place_type<BinaryExpression>,left, parseRelationalExpression(tokens), Operator::BITXOR, ln
      };


    else
      break;
  }

  return left;
}

Expression* parseLogicalExpression(TokenView& tokens) {
  Expression* left = parseBitwiseExpression(tokens);
  if (tokens.empty())
    return left;

  const unsigned ln = tokens.peek().line_number;
  while (true) {
    if (tokens.pop_if(TokenType::KEYWORD_AND))
      left = new Expression{
        std::in_place_type<BinaryExpression>, left, parseBitwiseExpression(tokens), Operator::AND, ln
      };
    else if (tokens.pop_if(TokenType::KEYWORD_OR))
      left = new Expression{
          std::in_place_type<BinaryExpression>, left, parseBitwiseExpression(tokens), Operator::OR, ln
      };
    else if (tokens.pop_if(TokenType::KEYWORD_XOR))
      left = new Expression{
          std::in_place_type<BinaryExpression>, left, parseBitwiseExpression(tokens), Operator::XOR, ln
      };
    else
      break;
  }

  return left;
}

Expression* parseAssignmentExpression(TokenView& tokens) {
  Expression* const left = parseLogicalExpression(tokens);
  if (tokens.empty())
    return left;

  const unsigned ln = tokens.peek().line_number;
  if (tokens.pop_if(TokenType::ASSIGN))
    return new Expression{
      std::in_place_type<BinaryExpression>, left, parseAssignmentExpression(tokens), Operator::ASSIGN, ln
    };

  return left;
}

// assumes tokens holds only the tokens relevant to the expression
Expression* parseExpression(TokenView& tokens) {
  if (tokens.empty())
    return nullptr;

  return parseAssignmentExpression(tokens);
}
/* Expressions */



Statement *parseStatement(TokenView& tokens, SymbolTable& table);
Statement *parseScoped(TokenView& tokens, SymbolTable& table);

Statement *parseExpressionStatement(TokenView& tokens) {
  const unsigned ln = tokens.peek().line_number;
  if (tokens.pop_if(TokenType::SEMI_COLON))
    return new Statement{std::in_place_type<ExpressionStatement>, ln};

  TokenView until_semi = tokens.getAllTokensUntilFirstOf(TokenType::SEMI_COLON);
  tokens.pop();
  return new Statement{std::in_place_type<ExpressionStatement>, ln, parseExpression(until_semi)};
}

Statement *parseVarDecl(TokenView& tokens, SymbolTable& table) {
  const unsigned ln = tokens.peek().line_number;
  InstantiatedType variable_type = parseType(tokens, table);
  std::string ident = parseIdentifier(tokens);
  tokens.expect_then_pop(TokenType::ASSIGN, "Expected assignment in variable declaration.", LOMError::Stage::ParsingError);

  if (tokens.peek_is(TokenType::IDENTIFIER)) {
    if (tokens.peek().toString() == ident)
      throw ParsingError("Variable may not be initialized using itself.", tokens.peek());
  }

  if (tokens.pop_if(TokenType::KEYWORD_JUNK)) {
    if (!variable_type.details.is_mutable)
      throw ParsingError("Non-mutable variables may not be junk initialized.", variable_type.toString(), ln);

    tokens.expect_then_pop(TokenType::SEMI_COLON, "Expected semicolon ending variable declaration.", LOMError::Stage::ParsingError);
    return new Statement{
      std::in_place_type<VarDeclaration>, variable_type, std::move(ident), ln
    };
  }


  TokenView expression_tokens = tokens.getAllTokensUntilFirstOf(TokenType::SEMI_COLON);
  tokens.pop();
  if (expression_tokens.empty())
    throw ParsingError("Expected initializing expression in variable declaration.", ";", ln);

  Expression * const expr = parseExpression(expression_tokens);

  return new Statement{std::in_place_type<VarDeclaration>, variable_type, std::move(ident), ln, expr};
}

// for the statements below starting with a keyword,
// that keyword has already been eaten
Statement *parseIf(TokenView& tokens, SymbolTable& table) {
  const unsigned ln = tokens.peek().line_number;
  tokens.expect_then_pop(TokenType::LPAREN, "Expected opening parenthesis in if statement condition.", LOMError::Stage::ParsingError);

  TokenView condition_tokens = tokens.getTokensBetweenParenthesis();
  if (condition_tokens.empty())
    throw ParsingError("Expected condition in if statement.", "(...)", ln);

  Expression* const condition = parseExpression(condition_tokens);

  Statement *const true_branch = parseScoped(tokens, table);
  Statement *false_branch = nullptr;

  if (tokens.pop_if(TokenType::KEYWORD_ELSE))
    false_branch = parseScoped(tokens, table);

  return new Statement{std::in_place_type<IfStatement>, condition, true_branch, ln, false_branch};
}

Statement *parseFor(TokenView& tokens, SymbolTable& table) {
  tokens.expect_then_pop(TokenType::LPAREN, "Expected opening patenthesis in for loop statement.", LOMError::Stage::ParsingError);

  const unsigned ln = tokens.peek().line_number;
  TokenView betweenParen = tokens.getTokensBetweenParenthesis();
  if (betweenParen.empty())
    throw ParsingError("Expected for loop header.", "(...)", ln);

  auto& first = betweenParen.peek();
  Statement *declOrAssignment{nullptr};
  if (first.isTypeModifier() ||
      first.isPrimitive() ||
    (betweenParen.peek_is(TokenType::IDENTIFIER) && betweenParen.peek_ahead(1).is(TokenType::IDENTIFIER))
    )
    declOrAssignment = parseVarDecl(betweenParen, table);
  else if (first.is(TokenType::SEMI_COLON)) {betweenParen.pop();}
  else
    declOrAssignment = parseExpressionStatement(betweenParen);


  Expression* const condition = betweenParen.peek_is(TokenType::SEMI_COLON) ? nullptr : parseExpression(betweenParen);
  betweenParen.expect_then_pop(TokenType::SEMI_COLON, "Expected semicolon after condition in for loop header.", LOMError::Stage::ParsingError);

  Expression* const iteration = parseExpression(betweenParen);
  Statement *const loop_body = parseScoped(tokens, table);

  return new Statement{std::in_place_type<ForLoop>, declOrAssignment, condition, iteration, loop_body, ln};
}

Statement *parseWhile(TokenView& tokens, SymbolTable& table) {
  tokens.expect_then_pop(TokenType::LPAREN,"Expected open parenthesis in while loop condition", LOMError::Stage::ParsingError);

  const unsigned ln = tokens.peek().line_number;
  TokenView condition_tokens = tokens.getTokensBetweenParenthesis();
  if (condition_tokens.empty())
    throw ParsingError("Expected condition between parenthesis in while loop", "(...)", ln);
  Expression* const condition = parseExpression(condition_tokens);

  Statement *const loop_body = parseScoped(tokens, table);

  return new Statement{std::in_place_type<WhileLoop>, condition, loop_body, ln};
}

//unsupported currently
Statement *parseDoWhile(TokenView&) { assert(false && "dowhile currently unsupported"); }

// scoped may be {...} or one statement ;
Statement *parseScoped(TokenView& tokens, SymbolTable& table) {
  std::vector<Statement*> statements;

  const unsigned ln = tokens.peek().line_number;
  if (tokens.pop_if(TokenType::LBRACE)) {
    TokenView scopedTokens = tokens.getTokensBetweenBraces();
    while (!scopedTokens.empty()) {
      statements.emplace_back(parseStatement(scopedTokens, table));
    }

    return new Statement{std::in_place_type<ScopedStatement>, std::move(statements), ln};
  }

  statements.push_back(parseStatement(tokens, table));
  return new Statement{std::in_place_type<ScopedStatement>, std::move(statements), ln};
}

Statement *parseReturn(TokenView& tokens) {
  const unsigned ln = tokens.peek().line_number;
  if (tokens.pop_if(TokenType::SEMI_COLON))
    return new Statement{std::in_place_type<ReturnStatement>, ln};

  TokenView retval = tokens.getAllTokensUntilFirstOf(TokenType::SEMI_COLON);
  tokens.pop();
  return new Statement{std::in_place_type<ReturnStatement>, ln, parseExpression(retval)};
}

//unsupported currently
Statement *parseSwitch(TokenView& ) { assert(false && "switch currently unsupported"); }

Statement *parseStatement(TokenView& tokens, SymbolTable& table) {
  const Token &first = tokens.peek();
  if (first.isPrimitive())
    return parseVarDecl(tokens, table);

  switch (first.type) {
  case TokenType::KEYWORD_IF:
    tokens.pop();
    return parseIf(tokens, table);
  case TokenType::KEYWORD_FOR:
    tokens.pop();
    return parseFor(tokens, table);
  case TokenType::KEYWORD_WHILE:
    tokens.pop();
    return parseWhile(tokens, table);
  case TokenType::KEYWORD_DO:
    tokens.pop();
    return parseDoWhile(tokens);
  case TokenType::KEYWORD_RETURN:
    tokens.pop();
    return parseReturn(tokens);
  case TokenType::KEYWORD_SWITCH:
    tokens.pop();
    return parseSwitch(tokens);

  case TokenType::LBRACE:
    return parseScoped(tokens, table);

  case TokenType::KEYWORD_MUT:
  case TokenType::LESS:
    return parseVarDecl(tokens, table);

  case TokenType::IDENTIFIER: //this is problematic as FUCK
    if (tokens.peek_ahead(1).is(TokenType::IDENTIFIER))
      return parseVarDecl(tokens, table);
    [[fallthrough]];
  default:
    return parseExpressionStatement(tokens);
  }
}

std::vector<Statement *> parseStatements(TokenView& body_tokens, SymbolTable& table) {
  std::vector<Statement *> body_statements;

  while (!body_tokens.empty())
    body_statements.push_back(parseStatement(body_tokens, table));

  return body_statements;
}


struct UnparsedFunction {
  const Type* return_type;
  std::string name;
  std::vector<VarDeclaration> parameter_list;
  TokenView body_tokens;

  UnparsedFunction(const Type* _return_type, std::string &&_name, std::vector<VarDeclaration> &&_parameter_list, const TokenView _body_tokens)
  : return_type(_return_type), name(std::move(_name)), parameter_list(std::move(_parameter_list)), body_tokens(_body_tokens) {}
};

struct UnparsedTU {
  std::vector<VarDeclaration> globals;
  std::vector<UnparsedFunction> functions;

  void registerFunction(const Type* _return_type, std::string _name, std::vector<VarDeclaration> _decl, TokenView _body) {
    functions.emplace_back(_return_type, std::move(_name), std::move(_decl), _body);
  }
};


ParsedFunction parseFunction(UnparsedFunction &func, SymbolTable& table) {
  return {
    func.return_type, std::move(func.name),
    std::move(func.parameter_list), parseStatements(func.body_tokens, table)
  };
}

ParsedTU secondPassParsing(UnparsedTU &&unparsedtu, SymbolTable&& table) {
  std::vector<ParsedFunction> parsed_funcs;
  for (auto &f : unparsedtu.functions)
    parsed_funcs.emplace_back(parseFunction(f, table));

  return {std::move(table), std::move(unparsedtu.globals), std::move(parsed_funcs)};
}

std::vector<VarDeclaration> parseParameterDecl(TokenView decl_tokens, SymbolTable& table) {
  if (decl_tokens.empty())
    return {};

  std::vector<VarDeclaration> parameter_list;

  while (true) {
    const unsigned ln = decl_tokens.peek().line_number;
    InstantiatedType type = parseType(decl_tokens, table);
    std::string ident = parseIdentifier(decl_tokens);

    parameter_list.emplace_back(std::move(type), std::move(ident), ln);

    if (decl_tokens.empty())
      return parameter_list;

    decl_tokens.expect_then_pop(TokenType::COMMA, "Expected ending parenthesis or comma in parameter list.", LOMError::Stage::ParsingError);
  }
}

// returns false when we're done
bool parseGlobals(UnparsedTU &tu, TokenView& tokens, SymbolTable& table) {
  if (tokens.peek_is(TokenType::KEYWORD_FN))
    return false;

  tokens.expect_then_pop(TokenType::KEYWORD_GLOBAL, "Expected global keyword before declaration.", LOMError::Stage::ParsingError);

  Statement* v = parseVarDecl(tokens, table);

  tu.globals.emplace_back(std::get<VarDeclaration>(std::move(*v)));
  delete v;
  return true;
}

void parseGlobalFunctions(UnparsedTU &tu, TokenView& tokens, SymbolTable& table) {
  tokens.expect_then_pop(TokenType::KEYWORD_FN, "Expected function.", LOMError::Stage::ParsingError);

  std::string ident = parseIdentifier(tokens);
  tokens.expect_then_pop(TokenType::LPAREN, "Expected opening parenthesis in function declaration.", LOMError::Stage::ParsingError);

  std::vector<VarDeclaration> parameter_list = parseParameterDecl(tokens.getTokensBetweenParenthesis(), table);

  const Type* return_type = devoid_type;
  if (tokens.pop_if(TokenType::ARROW))
    return_type = parseType(tokens, table).type;

  tokens.expect_then_pop(TokenType::LBRACE, "Expected lbrace in function declaration", LOMError::Stage::ParsingError);

  tu.registerFunction(return_type, std::move(ident),
                      std::move(parameter_list),
                      tokens.getTokensBetweenBraces());
}

UnparsedTU firstPassParsing(TokenView tokens, SymbolTable& table) {
  UnparsedTU pass_one_tu;

  while (parseGlobals(pass_one_tu, tokens, table)) {}

  while (!tokens.empty())
    parseGlobalFunctions(pass_one_tu, tokens, table);

  return pass_one_tu;
}
}

static void printTU(const ParsedTU& tu);
ParsedTU Parser::parseTokens(std::vector<Token> &&tokens) {
  SymbolTable table;
  ParsedTU ptu = secondPassParsing( firstPassParsing({tokens.begin(), tokens.end()}, table), std::move(table));

  if (Settings::doOutputParser()) {
    std::cout << "\n--- Parser Output ---\n";
    printTU(ptu);
    std::cout << "--- Parser Output ---\n";
    std::quick_exit(0);
  }

  return ptu;
}

static void printFunction(const ParsedFunction& func) {
  std::cout << "\nfn " << func.name << "(";
  for (auto &decl : func.parameter_list) {
    std::cout << decl.type.toString();
    std::cout << " " << decl.ident << ", ";
  }

  if (!func.parameter_list.empty())
    std::cout << "\b\b";

  std::cout << ")";
  if (!func.return_type->isDevoid()) {
    std::cout << " -> ";
    std::cout << func.return_type->toString();
  }
  std::cout << " {\n";

  for (const auto s : func.function_body) {
    std::visit(PrintStatementVisitor{1}, *s);
    std::cout << "\n";
  }

  std::cout << "}";
}

static void printTU(const ParsedTU& tu) {
  if (tu.globals.empty())
    goto noglobals;

  for (const auto &decl : tu.globals) {
    PrintStatementVisitor{}(decl);
    std::cout << "\n";
  }

  noglobals:

  for (const auto &f : tu.functions) {
    printFunction(f);
    std::cout << "\n";
  }

}




