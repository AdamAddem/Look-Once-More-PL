#include "../grammar/expressions.hpp"
#include "../grammar/statements.hpp"
#include "parse.hpp"

#include <iostream>
#include <stdexcept>
#include <unordered_set>

#include "../debug_flags.hpp"
#include "lexing/lex.hpp"

using namespace Parser;
using namespace Lexer;


/* Type and Identifier */
static Type parseType(TokenHandler &tokens) {
  Token token = tokens.eat();
  bool is_mutable{false};
  if (token.is(TokenType::KEYWORD_MUT)) {
    token = tokens.eat();
    is_mutable = true;
  }

  if (token.isPrimitive() || token.is(TokenType::IDENTIFIER)) {
    if (token.is(TokenType::KEYWORD_DEVOID))
      return devoid_type;

    if (token.isPointer()) { // add reference support eventually
      tokens.expect_then_pop(TokenType::ARROW, "Expected arrow in pointer declaration");
      return {token.toString(), is_mutable, new Type(parseType(tokens))};
    }

    std::string s = token.toString();
    return {std::move(s), is_mutable};
  }


  if (token.is(TokenType::LESS)) {
    TokenHandler variant_types_tokens = tokens.getTokensBetweenAngleBrackets();

    Type variant_types(Type::variant, parseType(variant_types_tokens));
    variant_types.is_mutable = is_mutable;

    std::unordered_set<std::string> typenames; //prevent duplicate types

    while (variant_types_tokens.pop_if(TokenType::COMMA)) {
      Type type = parseType(variant_types_tokens);
      if (type.isVariant())
        throw std::runtime_error("Nested variant types not allowed");

      if (type.isDevoid()) {
        typenames.emplace("");
        variant_types.addTypeToVariantList(std::move(type));
        continue;
      }

      if (type.is_mutable)
        throw std::runtime_error("Mutability cannot be specified within variant type list, must be specified prior to type list");

      if (typenames.contains(type.getTypename()))
        throw std::runtime_error("Duplicate types specified in variant declaration");

      typenames.emplace(type.getTypename());

      variant_types.addTypeToVariantList(std::move(type));
    }

    if (variant_types.numVariantTypes() < 2)
      throw std::runtime_error("Two or more types must be specified in variant type list");
    return variant_types;
  }

  std::string error_msg = "Expected typename, got: ";
  error_msg += token.toString();
  throw std::runtime_error(error_msg);
}

static std::string parseIdentifier(TokenHandler &tokens) {
  Token token = tokens.eat();

  if (token.is(TokenType::IDENTIFIER))
    return token.takeString();

  std::cout << token.toString() << std::endl;
  throw std::runtime_error("Expected identifier");
}
/* Type and Identifier */

/* Expressions */
static Expression *parseExpression(TokenHandler &tokens);

static std::vector<Expression *> parseParameters(TokenHandler &tokens) {
  if (tokens.empty())
    return {};

  std::vector<Expression *> retval;

  while (true) {
    retval.push_back(parseExpression(tokens));
    if (tokens.empty())
      return retval;

    if (!tokens.pop_if(TokenType::COMMA))
      throw std::runtime_error(
          "Expected comma between function parameters in call");
  }
}

static Expression *parsePrimaryExpression(TokenHandler &tokens) {
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
      throw std::runtime_error(
          "Impossible error in parsePrimaryExpression function in secondparse");
    }

    return new Expression(LiteralExpression(tokens.eat().value, type));
  }

  if (tokens.pop_if(TokenType::LPAREN)) {
    TokenHandler t = tokens.getTokensBetweenParenthesis();
    return parseExpression(t);
  }

  std::string ident = parseIdentifier(tokens);
  return new Expression(IdentifierExpression(std::move(ident)));
}

static Expression *parsePostfixExpression(TokenHandler &tokens) {
  Expression *left = parsePrimaryExpression(tokens);

  while (true) {
    if (tokens.pop_if(TokenType::PLUSPLUS))
      left = new Expression(UnaryExpression(left, Operator::PRE_INCREMENT));

    else if (tokens.pop_if(TokenType::MINUSMINUS))
      left = new Expression(UnaryExpression(left, Operator::POST_DECREMENT));

    else if (tokens.pop_if(TokenType::LPAREN)) {
      TokenHandler t = tokens.getTokensBetweenParenthesis();
      left = new Expression(CallingExpression(left, parseParameters(t)));
    } else if (tokens.pop_if(TokenType::LBRACKET)) {
      TokenHandler t = tokens.getTokensBetweenBrackets();
      left = new Expression(SubscriptExpression(left, parseExpression(t)));
    } else
      break;
  }

  return left;
}

static Expression *parsePrefixExpression(TokenHandler &tokens) {
  if (tokens.pop_if(TokenType::PLUSPLUS))
    return new Expression(UnaryExpression(parsePrefixExpression(tokens),
                                          Operator::PRE_INCREMENT));

  if (tokens.pop_if(TokenType::MINUSMINUS))
    return new Expression(UnaryExpression(parsePrefixExpression(tokens),
                                          Operator::PRE_DECREMENT));

  if (tokens.pop_if(TokenType::ADDR))
    return new Expression(
        UnaryExpression(parsePrefixExpression(tokens), Operator::ADDRESS_OF));

  return parsePostfixExpression(tokens);
}

static Expression *parseExponentExpression(TokenHandler &tokens) {
  Expression *left = parsePrefixExpression(tokens);

  while (true) {
    if (tokens.pop_if(TokenType::POW))
      left = new Expression(BinaryExpression(
          left, parsePrefixExpression(tokens), Operator::POWER));
    else
      break;
  }

  return left;
}

static Expression *parseFactorExpression(TokenHandler &tokens) {
  Expression *left = parseExponentExpression(tokens);

  while (true) {
    if (tokens.pop_if(TokenType::STAR))
      left = new Expression(BinaryExpression(
          left, parseFactorExpression(tokens), Operator::MULTIPLY));

    else if (tokens.pop_if(TokenType::SLASH))
      left = new Expression(BinaryExpression(
          left, parseFactorExpression(tokens), Operator::DIVIDE));

    else if (tokens.pop_if(TokenType::MOD))
      left = new Expression(BinaryExpression(
          left, parseFactorExpression(tokens), Operator::MODULUS));

    else
      break;
  }

  return left;
}

static Expression *parseTermExpression(TokenHandler &tokens) {
  Expression *l = parseFactorExpression(tokens);

  while (true) {
    if (tokens.pop_if(TokenType::PLUS))
      l = new Expression(
          BinaryExpression(l, parseFactorExpression(tokens), Operator::ADD));

    else if (tokens.pop_if(TokenType::MINUS))
      l = new Expression(BinaryExpression(l, parseFactorExpression(tokens),
                                          Operator::SUBTRACT));

    else
      break;
  }
  return l;
}

static Expression *parseRelationalExpression(TokenHandler &tokens) {
  Expression *left = parseTermExpression(tokens);

  while (true) {
    if (tokens.pop_if(TokenType::KEYWORD_EQUALS))
      left = new Expression(
          BinaryExpression(left, parseTermExpression(tokens), Operator::EQUAL));
    else if (tokens.pop_if(TokenType::KEYWORD_NOT_EQUAL))
      left = new Expression(BinaryExpression(left, parseTermExpression(tokens),
                                             Operator::NOT_EQUAL));
    else if (tokens.pop_if(TokenType::LESS))
      left = new Expression(
          BinaryExpression(left, parseTermExpression(tokens), Operator::LESS));
    else if (tokens.pop_if(TokenType::GTR))
      left = new Expression(BinaryExpression(left, parseTermExpression(tokens),
                                             Operator::GREATER));
    else if (tokens.pop_if(TokenType::LESSEQ))
      left = new Expression(BinaryExpression(left, parseTermExpression(tokens),
                                             Operator::LESS_EQUAL));
    else if (tokens.pop_if(TokenType::GTREQ))
      left = new Expression(BinaryExpression(left, parseTermExpression(tokens),
                                             Operator::GREATER_EQUAL));
    else
      break;
  }

  return left;
}

static Expression *parseBitwiseExpression(TokenHandler &tokens) {
  Expression *left = parseRelationalExpression(tokens);

  while (true) {
    if (tokens.pop_if(TokenType::KEYWORD_BITAND))
      left = new Expression(BinaryExpression(
          left, parseRelationalExpression(tokens), Operator::BITAND));

    else if (tokens.pop_if(TokenType::KEYWORD_BITOR))
      left = new Expression(BinaryExpression(
          left, parseRelationalExpression(tokens), Operator::BITOR));

    else if (tokens.pop_if(TokenType::KEYWORD_BITXOR))
      left = new Expression(BinaryExpression(
          left, parseRelationalExpression(tokens), Operator::BITXOR));

    else if (tokens.pop_if(TokenType::KEYWORD_BITNOT))
      left = new Expression(BinaryExpression(
          left, parseRelationalExpression(tokens), Operator::BITNOT));

    else
      break;
  }

  return left;
}

static Expression *parseLogicalExpression(TokenHandler &tokens) {
  Expression *left = parseBitwiseExpression(tokens);

  while (true) {
    if (tokens.pop_if(TokenType::KEYWORD_AND))
      left = new Expression(BinaryExpression(
          left, parseBitwiseExpression(tokens), Operator::AND));
    else if (tokens.pop_if(TokenType::KEYWORD_OR))
      left = new Expression(
          BinaryExpression(left, parseBitwiseExpression(tokens), Operator::OR));
    else
      break;
  }

  return left;
}

static Expression *parseAssignmentExpression(TokenHandler &tokens) {
  Expression *const left = parseLogicalExpression(tokens);
  if (tokens.pop_if(TokenType::ASSIGN))
    return new Expression(BinaryExpression(
        left, parseAssignmentExpression(tokens), Operator::ASSIGN));

  return left;
}

// assumes tokens holds only the tokens relevant to the expression
static Expression *parseExpression(TokenHandler &tokens) {
  if (tokens.empty())
    return nullptr;
  return parseAssignmentExpression(tokens);
}
/* Expressions */



static Statement *parseStatement(TokenHandler &tokens);

static Statement *parseScoped(TokenHandler &tokens);

static Statement *parseExpressionStatement(TokenHandler &tokens) {
  if (tokens.pop_if(TokenType::SEMI_COLON))
    return new Statement(ExpressionStatement());

  TokenHandler until_semi =
      tokens.getAllTokensUntilFirstOf(TokenType::SEMI_COLON);
  tokens.pop();
  return new Statement(ExpressionStatement(parseExpression(until_semi)));
}


static Statement *parseVarDecl(TokenHandler &tokens) {

  Type type = parseType(tokens);
  std::string ident = parseIdentifier(tokens);
  if (!tokens.pop_if(TokenType::ASSIGN))
    throw std::runtime_error("Expected assignment in variable declaration");

  if (tokens.pop_if(TokenType::KEYWORD_JUNK)) {
    if (!type.is_mutable)
      throw std::runtime_error("Non-mutable variables may not be junk initialized");

    if (!tokens.pop_if(TokenType::SEMI_COLON))
      throw std::runtime_error(
          "Expected semicolon ending variable declaration");

    return new Statement(VarDeclaration(std::move(type), std::move(ident)));
  }

  TokenHandler expression_tokens =
      tokens.getAllTokensUntilFirstOf(TokenType::SEMI_COLON);
  if (expression_tokens.empty())
    throw std::runtime_error(
        "Expected initializing expression in variable declaration");

  tokens.pop();
  Expression *const expr = parseExpression(expression_tokens);

  return new Statement(VarDeclaration(std::move(type), std::move(ident), expr));
}

// for the statements below starting with a keyword,
// that keyword has already been eaten
static Statement *parseIf(TokenHandler &tokens) {
  if (!tokens.pop_if(TokenType::LPAREN))
    throw std::runtime_error("Expected lparen in if statement condition");

  TokenHandler condition_tokens = tokens.getTokensBetweenParenthesis();
  if (condition_tokens.empty())
    throw std::runtime_error("Expected condition in if statement");

  Expression *const condition = parseExpression(condition_tokens);

  Statement *const true_branch = parseScoped(tokens);
  Statement *false_branch = nullptr;

  if (tokens.pop_if(TokenType::KEYWORD_ELSE))
    false_branch = parseScoped(tokens);

  return new Statement(IfStatement(condition, true_branch, false_branch));
}

static Statement *parseFor(TokenHandler &tokens) {
  if (!tokens.pop_if(TokenType::LPAREN))
    throw std::runtime_error("Expected opening parenthesis in for statement");

  TokenHandler betweenParen = tokens.getTokensBetweenParenthesis();

  auto& first = betweenParen.peek();
  Statement *declOrAssignment;
  if (first.isTypeModifier() ||
    (betweenParen.peek_is(TokenType::IDENTIFIER) && betweenParen.peek_ahead(1).is(TokenType::IDENTIFIER))
    )
    declOrAssignment = parseVarDecl(betweenParen);
  else
    declOrAssignment = parseExpressionStatement(betweenParen);

  Expression *const condition = parseExpression(betweenParen);
  betweenParen.expect_then_pop(TokenType::SEMI_COLON, "Expected semicolon after condition in for loop");

  Expression *const iteration = parseExpression(betweenParen);
  Statement *const loop_body = parseScoped(tokens);

  return new Statement(
      ForLoop(declOrAssignment, condition, iteration, loop_body));
}

static Statement *parseWhile(TokenHandler &tokens) {
  if (!tokens.pop_if(TokenType::LPAREN))
    throw std::runtime_error(
        "Expected open parenthesis in while loop condition");

  TokenHandler condition_tokens = tokens.getTokensBetweenParenthesis();
  if (condition_tokens.empty())
    throw std::runtime_error("Expected condition in while loop");
  Expression *const condition = parseExpression(condition_tokens);

  Statement *const loop_body = parseScoped(tokens);

  return new Statement(WhileLoop(condition, loop_body));
}

static Statement *parseDoWhile([[maybe_unused]] TokenHandler &tokens) {
  throw std::runtime_error("Do While Loop unsupported");
}

// scoped may be {...} or one statement ;
static Statement *parseScoped(TokenHandler &tokens) {
  std::vector<Statement *> statements;
  if (tokens.pop_if(TokenType::LBRACE)) {
    TokenHandler scopedTokens = tokens.getTokensBetweenBraces();
    while (!scopedTokens.empty())
      statements.push_back(parseStatement(scopedTokens));

    return new Statement(ScopedStatement(std::move(statements)));
  }

  statements.push_back(parseStatement(tokens));
  return new Statement(ScopedStatement(std::move(statements)));
}

static Statement *parseReturn(TokenHandler &tokens) {
  if (tokens.pop_if(TokenType::SEMI_COLON))
    return new Statement(ReturnStatement());

  TokenHandler retval = tokens.getAllTokensUntilFirstOf(TokenType::SEMI_COLON);
  tokens.pop();
  return new Statement(ReturnStatement(parseExpression(retval)));
}

static Statement *parseSwitch([[maybe_unused]] TokenHandler &tokens) {
  throw std::runtime_error("Switch Statement Unsupported");
}

static Statement *parseStatement(TokenHandler &tokens) {
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

static std::vector<Statement *> parseStatements(TokenHandler &body_tokens) {
  std::vector<Statement *> body_statements;

  while (!body_tokens.empty())
    body_statements.push_back(parseStatement(body_tokens));

  return body_statements;
}


struct UnparsedFunction {
  Type return_type;
  std::string name;
  std::vector<VarDeclaration> parameter_list;
  TokenHandler body_tokens;

  UnparsedFunction(Type &&_return_type, std::string &&_name,
                   std::vector<VarDeclaration> &&_parameter_list,
                   TokenHandler &&_body_tokens)
      : return_type(std::move(_return_type)), name(std::move(_name)),
        parameter_list(std::move(_parameter_list)),
        body_tokens(std::move(_body_tokens)) {}
};

struct UnparsedTU {
  bool globalsDeclared = false;
  std::vector<VarDeclaration> globals;
  std::vector<UnparsedFunction> functions;

  void registerFunction(Type&& _return_type, std::string _name,
                        std::vector<VarDeclaration> _decl,
                        TokenHandler _body) {
    std::vector<Type> param_types;
    for (auto &decl : _decl)
      param_types.emplace_back(decl.type); // this is stupid

    functions.emplace_back(std::move(_return_type), std::move(_name),
                           std::move(_decl), std::move(_body));
  }
};

static ParsedFunction parseFunction(UnparsedFunction &func) {
  return {std::move(func.return_type), std::move(func.name),
          std::move(func.parameter_list), parseStatements(func.body_tokens)};
}

static ParsedTranslationUnit secondPassParsing(UnparsedTU &&unparsedtu) {

  std::vector<ParsedFunction> parsed_funcs;
  for (auto &f : unparsedtu.functions)
    parsed_funcs.emplace_back(parseFunction(f));

  ParsedTranslationUnit ptu{std::move(unparsedtu.globals), std::move(parsed_funcs)};


  return ptu;
}



std::vector<VarDeclaration> parseParameterDecl(TokenHandler &tokens) {
  if (tokens.empty())
    return {};

  std::vector<VarDeclaration> parameter_list;

  while (true) {
    Type type = parseType(tokens);
    std::string ident = parseIdentifier(tokens);

    parameter_list.emplace_back(std::move(type), std::move(ident));

    if (tokens.empty())
      return parameter_list;

    tokens.expect_then_pop(
        TokenType::COMMA,
        "Expected ending parenthesis or comma in parameter list");
  }
}

// returns false when we're done
bool parseGlobals(UnparsedTU &tu, TokenHandler &tokens) {
  if (tokens.peek_is(TokenType::KEYWORD_FN))
    return false;

  tokens.expect_then_pop(TokenType::KEYWORD_GLOBAL, "Expected global keyword before declaration");
  Statement* v = parseVarDecl(tokens);

  tu.globals.emplace_back(std::get<VarDeclaration>(std::move(v->value)));
  delete v;
  return true;
}

void parseGlobalFunctions(UnparsedTU &tu, TokenHandler &tokens) {
  tokens.expect_then_pop(TokenType::KEYWORD_FN, "Expected function");

  std::string ident = parseIdentifier(tokens);
  tokens.expect_then_pop(TokenType::LPAREN, "Expected lparen in function declaration");

  TokenHandler parameter_tokens = tokens.getTokensBetweenParenthesis();
  std::vector<VarDeclaration> parameter_list = parseParameterDecl(parameter_tokens);

  Type return_type{devoid_type};
  if (tokens.pop_if(TokenType::ARROW))
    return_type = parseType(tokens);

  tokens.expect_then_pop(TokenType::LBRACE,
                         "Expected lbrace in function declaration");

  tu.registerFunction(std::move(return_type), std::move(ident),
                      std::move(parameter_list),
                      tokens.getTokensBetweenBraces());
}

static UnparsedTU firstPassParsing(TokenHandler &&tokens) {
  TokenHandler token_list(std::move(tokens));
  UnparsedTU pass_one_tu;

  while (parseGlobals(pass_one_tu, token_list)) {}

  while (!token_list.empty())
    parseGlobalFunctions(pass_one_tu, token_list);


  return pass_one_tu;
}

static void printTU(const ParsedTranslationUnit& tu);
ParsedTranslationUnit Parser::parseTokens(TokenHandler &&tokens) {
  ParsedTranslationUnit ptu = secondPassParsing(firstPassParsing(std::move(tokens)));

  if (lom_debug::output_parse) {
    printTU(ptu);
    std::cout << "Parsing stage passed!" << std::endl;
    std::exit(0);
  }

  return ptu;
}


static void printFunction(const ParsedFunction& func) {
  std::cout << "fn " << func.name << "(";
  for (auto &decl : func.parameter_list) {
    decl.type.print();
    std::cout << " " << decl.ident << ", ";
  }

  if (!func.parameter_list.empty())
    std::cout << "\b\b";

  std::cout << ")";
  if (!func.return_type.isDevoid()) {
    std::cout << " -> ";
    func.return_type.print();
  }
  std::cout << " {\n" << std::endl;

  for (const auto s : func.function_body) {
    std::visit(PrintStatementVisitor{1}, s->value);
    std::cout << "\n\n";
  }

  std::cout << "}" << std::endl;
}

static void printTU(const ParsedTranslationUnit& tu) {
  if (tu.globals.empty())
    goto noglobals;

  for (const auto &decl : tu.globals) {
    PrintStatementVisitor{}(decl);
    std::cout << "\n";
  }
  std::cout << "\n\n";

  noglobals:

  for (const auto &f : tu.functions) {
    printFunction(f);
    std::cout << "\n\n";
  }

}




