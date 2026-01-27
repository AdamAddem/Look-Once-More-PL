#include "secondparse.hpp"
#include "../grammar/expressions.hpp"
#include "../grammar/statements.hpp"
#include "firstparse.hpp"

#include <iostream>
#include <stdexcept>

using namespace Parser;
using namespace Lexer;

Operator tokenToOperator(TokenType t) {
  switch (t) {
  case TokenType::PLUS:
    return Operator::ADD;
  case TokenType::MINUS:
    return Operator::SUBTRACT;
  case TokenType::STAR:
    return Operator::MULTIPLY;
  case TokenType::SLASH:
    return Operator::DIVIDE;
  case TokenType::POW:
    return Operator::POWER;
  case TokenType::MOD:
    return Operator::MODULUS;

  case TokenType::ASSIGN:
    return Operator::ASSIGN;
  case TokenType::LESS:
    return Operator::LESS;
  case TokenType::GTR:
    return Operator::GREATER;
  case TokenType::LESSEQ:
    return Operator::LESS_EQUAL;
  case TokenType::GTREQ:
    return Operator::GREATER_EQUAL;
  case TokenType::KEYWORD_AND:
    return Operator::AND;
  case TokenType::KEYWORD_OR:
    return Operator::OR;
  case TokenType::KEYWORD_XOR:
    return Operator::XOR;
  case TokenType::KEYWORD_NOT:
    return Operator::NOT;
  case TokenType::KEYWORD_EQUALS:
    return Operator::EQUAL;
  case TokenType::KEYWORD_BITAND:
    return Operator::BITAND;
  case TokenType::KEYWORD_BITOR:
    return Operator::BITOR;
  case TokenType::KEYWORD_BITXOR:
    return Operator::BITXOR;
  case TokenType::KEYWORD_BITNOT:
    return Operator::BITNOT;
  case TokenType::KEYWORD_CAST:
    return Operator::CAST;
  case TokenType::KEYWORD_CAST_IF:
    return Operator::CAST_IF;
  case TokenType::KEYWORD_UNSAFE_CAST:
    return Operator::UNSAFE_CAST;
  case TokenType::KEYWORD_VERY_UNSAFE_CAST:
    return Operator::VERY_UNSAFE_CAST;
  case TokenType::ADDR:
    return Operator::ADDRESS_OF;

  default:
    throw std::runtime_error("Token to Operator conversion either unsupported, "
                             "or context dependent");
  }
}

Expression *parseExpression(TokenHandler &tokens);

std::vector<Expression *> parseParameters(TokenHandler &tokens) {
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

Expression *parsePrimaryExpression(TokenHandler &tokens) {
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

Expression *parsePostfixExpression(TokenHandler &tokens) {
  Expression *left = parsePrimaryExpression(tokens);

  while (true) {

    if (tokens.pop_if(TokenType::PLUSPLUS))
      left = new Expression(UnaryExpression(left, Operator::PRE_INCREMENT));

    else if (tokens.pop_if(TokenType::MINUSMINUS))
      left = new Expression(UnaryExpression(left, Operator::POST_DECREMENT));

    else if (tokens.pop_if(TokenType::LPAREN)) {
      TokenHandler t = tokens.getTokensBetweenParenthesis();
      left = new Expression(CallingExpression(left, parseParameters(t)));
    }

    else if (tokens.pop_if(TokenType::LBRACKET)) {
      TokenHandler t = tokens.getTokensBetweenBrackets();
      left = new Expression(SubscriptExpression(left, parseExpression(t)));
    }

    else
      break;
  }

  return left;
}

Expression *parsePrefixExpression(TokenHandler &tokens) {

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

Expression *parseExponentExpression(TokenHandler &tokens) {
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

Expression *parseFactorExpression(TokenHandler &tokens) {
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

Expression *parseTermExpression(TokenHandler &tokens) {
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

Expression *parseRelationalExpression(TokenHandler &tokens) {
  Expression *left = parseTermExpression(tokens);

  while (true) {
    if (tokens.pop_if(TokenType::KEYWORD_EQUALS))
      left = new Expression(
          BinaryExpression(left, parseTermExpression(tokens), Operator::EQUAL));

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

Expression *parseBitwiseExpression(TokenHandler &tokens) {

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

Expression *parseLogicalExpression(TokenHandler &tokens) {
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

Expression *parseAssignmentExpression(TokenHandler &tokens) {

  Expression *const left = parseLogicalExpression(tokens);
  if (tokens.pop_if(TokenType::ASSIGN))
    return new Expression(BinaryExpression(
        left, parseAssignmentExpression(tokens), Operator::ASSIGN));

  if (tokens.pop_if(TokenType::PLUS_ASSIGN))
    return new Expression(BinaryExpression(
        left, parseAssignmentExpression(tokens), Operator::ADD_ASSIGN));

  if (tokens.pop_if(TokenType::MINUS_ASSIGN))
    return new Expression(BinaryExpression(
        left, parseAssignmentExpression(tokens), Operator::SUB_ASSIGN));

  if (tokens.pop_if(TokenType::DIV_ASSIGN))
    return new Expression(BinaryExpression(
        left, parseAssignmentExpression(tokens), Operator::DIV_ASSIGN));

  if (tokens.pop_if(TokenType::MULT_ASSIGN))
    return new Expression(BinaryExpression(
        left, parseAssignmentExpression(tokens), Operator::MULT_ASSIGN));

  if (tokens.pop_if(TokenType::MOD_ASSIGN))
    return new Expression(BinaryExpression(
        left, parseAssignmentExpression(tokens), Operator::MOD_ASSIGN));

  if (tokens.pop_if(TokenType::POW_ASSIGN))
    return new Expression(BinaryExpression(
        left, parseAssignmentExpression(tokens), Operator::POW_ASSIGN));

  return left;
}

// assumes tokens holds only the tokens relevant to the expression
Expression *parseExpression(TokenHandler &tokens) {
  if (tokens.empty())
    return nullptr;
  return parseAssignmentExpression(tokens);
}

Statement *parseStatement(TokenHandler &tokens);
Statement *parseScoped(TokenHandler &tokens);

Statement *parseExpressionStatement(TokenHandler &tokens) {
  if (tokens.pop_if(TokenType::SEMI_COLON))
    return new Statement(ExpressionStatement());

  TokenHandler until_semi =
      tokens.getAllTokensUntilFirstOf(TokenType::SEMI_COLON);
  tokens.pop();
  return new Statement(ExpressionStatement(parseExpression(until_semi)));
}

Statement *parseVarDecl(TokenHandler &tokens) {
  Type type = parseType(tokens);
  std::string ident = parseIdentifier(tokens);
  if (!tokens.pop_if(TokenType::ASSIGN))
    throw std::runtime_error("Expected assignment in variable declaration");

  if (tokens.pop_if(TokenType::KEYWORD_JUNK)) {
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
Statement *parseIf(TokenHandler &tokens) {
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

Statement *parseFor(TokenHandler &tokens) {
  if (!tokens.pop_if(TokenType::LPAREN))
    throw std::runtime_error("Expected opening parenthesis in for statement");

  TokenHandler betweenParen = tokens.getTokensBetweenParenthesis();
  Statement *declOrAssignment;
  if (betweenParen.peek().isPrimitive())
    declOrAssignment = parseVarDecl(betweenParen);
  else if (betweenParen.peek_is(TokenType::IDENTIFIER)) {
    if (betweenParen.peek_ahead(1).is(TokenType::IDENTIFIER))
      declOrAssignment = parseVarDecl(betweenParen);
    else
      declOrAssignment = parseExpressionStatement(betweenParen);
  } else if (betweenParen.peek_is(TokenType::LESS)) {
    declOrAssignment = parseVarDecl(betweenParen);
  } else
    throw std::runtime_error("Expected variable declaration or assignment in "
                             "first for loop statement");

  Expression *const condition = parseExpression(betweenParen);
  if (!betweenParen.pop_if(TokenType::SEMI_COLON))
    throw std::runtime_error("Expected semicolon after condition in forloop");

  Expression *const iteration = parseExpression(betweenParen);
  Statement *const loop_body = parseScoped(tokens);

  return new Statement(
      ForLoop(declOrAssignment, condition, iteration, loop_body));
}

Statement *parseWhile(TokenHandler &tokens) {

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

Statement *parseDoWhile(TokenHandler &tokens) {
  throw std::runtime_error("Do While Loop unsupported");
}

// scoped may be {...} or one statement ;
Statement *parseScoped(TokenHandler &tokens) {
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

Statement *parseReturn(TokenHandler &tokens) {
  if (tokens.pop_if(TokenType::SEMI_COLON))
    return new Statement(ReturnStatement());

  TokenHandler retval = tokens.getAllTokensUntilFirstOf(TokenType::SEMI_COLON);
  tokens.pop();
  return new Statement(ReturnStatement(parseExpression(retval)));
}

Statement *parseSwitch(TokenHandler &tokens) {
  throw std::runtime_error("Switch Statement Unsupported");
}

Statement *parseStatement(TokenHandler &tokens) {

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

  case TokenType::LESS:
    return parseVarDecl(tokens);

  case TokenType::IDENTIFIER:
    if (tokens.peek_ahead(1).is(TokenType::IDENTIFIER))
      return parseVarDecl(tokens);
  default:
    return parseExpressionStatement(tokens);
  }
}

std::vector<Statement *> parseStatements(TokenHandler &body_tokens) {

  std::vector<Statement *> body_statements;

  while (!body_tokens.empty())
    body_statements.push_back(parseStatement(body_tokens));

  return body_statements;
}

ParsedGlobals parseGlobals(UnparsedGlobals &globals) {

  return {std::move(globals.declarations),
          parseStatements(globals.global_init_body)};
}

ParsedFunction parseFunction(UnparsedFunction &func) {

  return {std::move(func.return_value), std::move(func.name),
          std::move(func.parameter_list), parseStatements(func.body_tokens)};
}

ParsedTranslationUnit Parser::secondPassParsing(UnparsedTU &&unparsedtu) {

  ParsedGlobals globals = parseGlobals(unparsedtu.globals);

  std::vector<ParsedFunction> parsed_funcs;
  for (auto &f : unparsedtu.functions)
    parsed_funcs.emplace_back(parseFunction(f));

  ParsedTranslationUnit ptu{std::move(globals), std::move(parsed_funcs)};

  return ptu;
}

void ParsedTranslationUnit::print() {
  global.print();
  std::cout << "\n\n";

  for (auto &f : functions) {
    f.print();
    std::cout << "\n\n";
  }
}

void ParsedGlobals::print() {
  if (declarations.empty())
    return;

  for (auto &decl : declarations) {
    PrintStatementVisitor{}(decl);
    std::cout << "\n";
  }
  std::cout << "\n";

  std::cout << "globals{\n\n";

  for (auto s : global_init_body) {
    std::visit(PrintStatementVisitor{1}, s->value);
    std::cout << "\n";
  }

  std::cout << "\n}\n" << std::endl;
}

void ParsedFunction::print() {
  printType(return_type);
  std::cout << " " << name << "(";
  for (auto &decl : parameter_list) {
    printType(decl.type);
    std::cout << " " << decl.ident << ", ";
  }

  if (!parameter_list.empty())
    std::cout << "\b\b";

  std::cout << ") {\n" << std::endl;

  for (auto s : function_body) {
    std::visit(PrintStatementVisitor{1}, s->value);
    std::cout << "\n\n";
  }

  std::cout << "}" << std::endl;
}
