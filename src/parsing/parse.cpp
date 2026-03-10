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
Type parseType(TokenView& tokens) {
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
      return devoid_type;

    if (token.isPointer()) { // add reference support eventually
      tokens.expect_then_pop(TokenType::ARROW, "Expected arrow in pointer declaration.", LOMError::Stage::ParsingError);

      PointerType t;
      switch (token.type) {
      case TokenType::KEYWORD_RAW: t = PointerType::RAW; break;
      case TokenType::KEYWORD_UNIQUE: t = PointerType::UNIQUE; break;
      case TokenType::KEYWORD_VAGUE: t = PointerType::VAGUE; break;

      default:
        assert(false);
      }

      return Type(Type::pointer, t,new Type(parseType(tokens)));
    }


    return Type(Type::normal, token.toString(), is_mutable);
  }

  if (token.is(TokenType::LESS)) {
    TokenView variant_types_tokens = tokens.getTokensBetweenAngleBrackets();

    Type variant_types(Type::variant);
    variant_types.details.is_mutable = is_mutable;

    std::unordered_set<std::string> typenames; //prevent duplicate types

    const unsigned variant_ln = variant_types_tokens.peek().line_number;
    do {
      const unsigned subtype_ln = variant_types_tokens.peek().line_number;
      Type type = parseType(variant_types_tokens);
      if (type.isVariant())
        throw ParsingError("Nested variant types not allowed.", type, subtype_ln);

      if (type.details.is_mutable)
        throw ParsingError("Mutability cannot be specified within variant type list, must be specified prior to type list.", type, subtype_ln);

      if (typenames.contains(type.getTypename()))
        throw ParsingError("Duplicate types specified in variant declaration.", type, subtype_ln);

      if (type.isDevoid())
        typenames.emplace("");
      else
        typenames.emplace(type.getTypename());

      variant_types.addTypeToVariantList(std::move(type));
    } while (variant_types_tokens.pop_if(TokenType::COMMA));

    if (variant_types.numVariantTypes() < 2)
      throw ParsingError("Two or more types must be specified in variant type list.", variant_types, variant_ln);
    return variant_types;
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

  std::vector<Expression*>  retval;

  while (true) {
    retval.push_back(parseExpression(between_parenthesis));
    if (between_parenthesis.empty())
      return retval;

    between_parenthesis.expect_then_pop(TokenType::COMMA, "Expected comma between function parameters in function call.", LOMError::Stage::ParsingError);
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

    return new Expression(std::in_place_type<LiteralExpression>, tokens.take().value, type, ln);
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

    else if (tokens.pop_if(TokenType::KEYWORD_BITNOT))
      left = new Expression{
        std::in_place_type<BinaryExpression>,left, parseRelationalExpression(tokens), Operator::BITNOT, ln
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



Statement *parseStatement(TokenView& tokens);
Statement *parseScoped(TokenView& tokens);

Statement *parseExpressionStatement(TokenView& tokens) {
  const unsigned ln = tokens.peek().line_number;
  if (tokens.pop_if(TokenType::SEMI_COLON))
    return new Statement{std::in_place_type<ExpressionStatement>, ln};

  TokenView until_semi = tokens.getAllTokensUntilFirstOf(TokenType::SEMI_COLON);
  tokens.pop();
  return new Statement{std::in_place_type<ExpressionStatement>, ln, parseExpression(until_semi)};
}

Statement *parseVarDecl(TokenView& tokens) {
  const unsigned ln = tokens.peek().line_number;
  Type type = parseType(tokens);
  std::string ident = parseIdentifier(tokens);
  tokens.expect_then_pop(TokenType::ASSIGN, "Expected assignment in variable declaration.", LOMError::Stage::ParsingError);

  if (tokens.peek_is(TokenType::IDENTIFIER)) {
    if (tokens.peek().toString() == ident)
      throw ParsingError("Variable may not be initialized using itself.", tokens.peek());
  }


  if (tokens.pop_if(TokenType::KEYWORD_JUNK)) {
    if (!type.details.is_mutable)
      throw ParsingError("Non-mutable variables may not be junk initialized.", type, ln);

    tokens.expect_then_pop(TokenType::SEMI_COLON, "Expected semicolon ending variable declaration.", LOMError::Stage::ParsingError);
    return new Statement{
      std::in_place_type<VarDeclaration>, std::move(type), std::move(ident), ln
    };
  }


  TokenView expression_tokens = tokens.getAllTokensUntilFirstOf(TokenType::SEMI_COLON);
  tokens.pop();
  if (expression_tokens.empty())
    throw ParsingError("Expected initializing expression in variable declaration.", ";", ln);

  Expression * const expr = parseExpression(expression_tokens);

  return new Statement{std::in_place_type<VarDeclaration>, std::move(type), std::move(ident), ln, expr};
}

// for the statements below starting with a keyword,
// that keyword has already been eaten
Statement *parseIf(TokenView& tokens) {
  const unsigned ln = tokens.peek().line_number;
  tokens.expect_then_pop(TokenType::LPAREN, "Expected opening parenthesis in if statement condition.", LOMError::Stage::ParsingError);

  TokenView condition_tokens = tokens.getTokensBetweenParenthesis();
  if (condition_tokens.empty())
    throw ParsingError("Expected condition in if statement.", "(...)", ln);

  Expression* const condition = parseExpression(condition_tokens);

  Statement *const true_branch = parseScoped(tokens);
  Statement *false_branch = nullptr;

  if (tokens.pop_if(TokenType::KEYWORD_ELSE))
    false_branch = parseScoped(tokens);

  return new Statement{std::in_place_type<IfStatement>, condition, true_branch, ln, false_branch};
}

Statement *parseFor(TokenView& tokens) {
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
    declOrAssignment = parseVarDecl(betweenParen);
  else if (first.is(TokenType::SEMI_COLON)) {betweenParen.pop();}
  else
    declOrAssignment = parseExpressionStatement(betweenParen);


  Expression* const condition = betweenParen.peek_is(TokenType::SEMI_COLON) ? nullptr : parseExpression(betweenParen);
  betweenParen.expect_then_pop(TokenType::SEMI_COLON, "Expected semicolon after condition in for loop header.", LOMError::Stage::ParsingError);

  Expression* const iteration = parseExpression(betweenParen);
  Statement *const loop_body = parseScoped(tokens);

  return new Statement{std::in_place_type<ForLoop>, declOrAssignment, condition, iteration, loop_body, ln};
}

Statement *parseWhile(TokenView& tokens) {
  tokens.expect_then_pop(TokenType::LPAREN,"Expected open parenthesis in while loop condition", LOMError::Stage::ParsingError);

  const unsigned ln = tokens.peek().line_number;
  TokenView condition_tokens = tokens.getTokensBetweenParenthesis();
  if (condition_tokens.empty())
    throw ParsingError("Expected condition between parenthesis in while loop", "(...)", ln);
  Expression* const condition = parseExpression(condition_tokens);

  Statement *const loop_body = parseScoped(tokens);

  return new Statement{std::in_place_type<WhileLoop>, condition, loop_body, ln};
}

//unsupported currently
Statement *parseDoWhile(TokenView&) { assert(false && "dowhile currently unsupported"); }

// scoped may be {...} or one statement ;
Statement *parseScoped(TokenView& tokens) {
  std::vector<Statement*> statements;

  const unsigned ln = tokens.peek().line_number;
  if (tokens.pop_if(TokenType::LBRACE)) {
    TokenView scopedTokens = tokens.getTokensBetweenBraces();
    while (!scopedTokens.empty()) {
      statements.emplace_back(parseStatement(scopedTokens));
    }

    return new Statement{std::in_place_type<ScopedStatement>, std::move(statements), ln};
  }

  statements.push_back(parseStatement(tokens));
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

Statement *parseStatement(TokenView& tokens) {
  const Token &first = tokens.peek();
  if (first.isPrimitive())
    return parseVarDecl(tokens);

  switch (first.type) {
  case TokenType::KEYWORD_IF:
    tokens.pop();
    return parseIf(tokens);
  case TokenType::KEYWORD_FOR:
    tokens.pop();
    return parseFor(tokens);
  case TokenType::KEYWORD_WHILE:
    tokens.pop();
    return parseWhile(tokens);
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
    return parseScoped(tokens);

  case TokenType::KEYWORD_MUT:
  case TokenType::LESS:
    return parseVarDecl(tokens);

  case TokenType::IDENTIFIER: //this is problematic as FUCK
    if (tokens.peek_ahead(1).is(TokenType::IDENTIFIER))
      return parseVarDecl(tokens);
    [[fallthrough]];
  default:
    return parseExpressionStatement(tokens);
  }
}

std::vector<Statement *> parseStatements(TokenView& body_tokens) {
  std::vector<Statement *> body_statements;

  while (!body_tokens.empty())
    body_statements.push_back(parseStatement(body_tokens));

  return body_statements;
}


struct UnparsedFunction {
  Type return_type;
  std::string name;
  std::vector<VarDeclaration> parameter_list;
  TokenView body_tokens;

  UnparsedFunction(Type &&_return_type, std::string &&_name, std::vector<VarDeclaration> &&_parameter_list, const TokenView _body_tokens)
  : return_type(std::move(_return_type)), name(std::move(_name)), parameter_list(std::move(_parameter_list)), body_tokens(_body_tokens) {}
};

struct UnparsedTU {
  std::vector<VarDeclaration> globals;
  std::vector<UnparsedFunction> functions;

  void registerFunction(Type&& _return_type, std::string _name, std::vector<VarDeclaration> _decl, TokenView _body) {
    functions.emplace_back(std::move(_return_type), std::move(_name), std::move(_decl), _body);
  }
};


ParsedFunction parseFunction(UnparsedFunction &func) {
  return {
    std::move(func.return_type), std::move(func.name),
    std::move(func.parameter_list), parseStatements(func.body_tokens)
  };
}

ParsedTU secondPassParsing(UnparsedTU &&unparsedtu) {

  std::vector<ParsedFunction> parsed_funcs;
  for (auto &f : unparsedtu.functions)
    parsed_funcs.emplace_back(parseFunction(f));

  ParsedTU ptu{std::move(unparsedtu.globals), std::move(parsed_funcs)};


  return ptu;
}

std::vector<VarDeclaration> parseParameterDecl(TokenView&& tokens) {
  if (tokens.empty())
    return {};

  std::vector<VarDeclaration> parameter_list;

  while (true) {
    const unsigned ln = tokens.peek().line_number;
    Type type = parseType(tokens);
    std::string ident = parseIdentifier(tokens);

    parameter_list.emplace_back(std::move(type), std::move(ident), ln);

    if (tokens.empty())
      return parameter_list;

    tokens.expect_then_pop(TokenType::COMMA, "Expected ending parenthesis or comma in parameter list.", LOMError::Stage::ParsingError);
  }
}

// returns false when we're done
bool parseGlobals(UnparsedTU &tu, TokenView& tokens) {
  if (tokens.peek_is(TokenType::KEYWORD_FN))
    return false;

  tokens.expect_then_pop(TokenType::KEYWORD_GLOBAL, "Expected global keyword before declaration.", LOMError::Stage::ParsingError);

  Statement* v = parseVarDecl(tokens);

  tu.globals.emplace_back(std::get<VarDeclaration>(std::move(*v)));
  delete v;
  return true;
}

void parseGlobalFunctions(UnparsedTU &tu, TokenView& tokens) {
  tokens.expect_then_pop(TokenType::KEYWORD_FN, "Expected function.", LOMError::Stage::ParsingError);

  std::string ident = parseIdentifier(tokens);
  tokens.expect_then_pop(TokenType::LPAREN, "Expected opening parenthesis in function declaration.", LOMError::Stage::ParsingError);

  std::vector<VarDeclaration> parameter_list = parseParameterDecl(tokens.getTokensBetweenParenthesis());

  Type return_type{devoid_type};
  if (tokens.pop_if(TokenType::ARROW))
    return_type = parseType(tokens);

  tokens.expect_then_pop(TokenType::LBRACE, "Expected lbrace in function declaration", LOMError::Stage::ParsingError);

  tu.registerFunction(std::move(return_type), std::move(ident),
                      std::move(parameter_list),
                      tokens.getTokensBetweenBraces());
}

UnparsedTU firstPassParsing(TokenView tokens) {
  UnparsedTU pass_one_tu;

  while (parseGlobals(pass_one_tu, tokens)) {}

  while (!tokens.empty())
    parseGlobalFunctions(pass_one_tu, tokens);

  return pass_one_tu;
}
}

static void printTU(const ParsedTU& tu);
ParsedTU Parser::parseTokens(std::vector<Token> &&tokens) {

  ParsedTU ptu = secondPassParsing( firstPassParsing({tokens.begin(), tokens.end()}));

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
  if (!func.return_type.isDevoid()) {
    std::cout << " -> ";
    std::cout << func.return_type.toString();
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




