#include "secondparse.hpp"
#include "expressions.hpp"
#include "firstparse.hpp"
#include "lex.hpp"
#include "statements.hpp"
#include <stdexcept>

using namespace Parser;
using namespace Lexer;

bool isLeftAssociative(TokenType op) {
  switch (op) {
  case ADDR:
  case KEYWORD_CAST:
  case KEYWORD_CAST_IF:
  case KEYWORD_UNSAFE_CAST:
  case KEYWORD_VERY_UNSAFE_CAST:
  case KEYWORD_NOT:
  case ASSIGN:
  case PLUS_ASSIGN:
  case MINUS_ASSIGN:
  case MULT_ASSIGN:
  case DIV_ASSIGN:
  case POW_ASSIGN:
  case MOD_ASSIGN:
    return false;

  default:
    return true;
  }
}

int precedenceOf(TokenType op) {
  switch (op) {

  case KEYWORD_CAST:
  case KEYWORD_CAST_IF:
  case KEYWORD_UNSAFE_CAST:
  case KEYWORD_VERY_UNSAFE_CAST:
  case ADDR:
    return 1;

  case POW:
    return 2;

  case MULT:
  case DIV:
  case MOD:
    return 3;

  case PLUS:
  case MINUS:
    return 4;

  case LESS:
  case GTR:
  case LESSEQ:
  case GTREQ:
  case KEYWORD_EQUALS:
    return 5;

  case KEYWORD_BITAND:
  case KEYWORD_BITOR:
  case KEYWORD_BITXOR:
  case KEYWORD_BITNOT:
    return 6;

  case KEYWORD_AND:
  case KEYWORD_OR:
  case KEYWORD_XOR:
  case KEYWORD_NOT:
    return 7;

  case ASSIGN:
  case PLUS_ASSIGN:
  case MINUS_ASSIGN:
  case MULT_ASSIGN:
  case DIV_ASSIGN:
  case POW_ASSIGN:
  case MOD_ASSIGN:
    return 8;

  default:
    return 0;
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

    if (!tokens.pop_if(COMMA))
      throw std::runtime_error(
          "Expected comma between function parameters in call");
  }
}

Expression *parsePrimaryExpression(TokenHandler &tokens) {
  if (tokens.peek().isLiteral())
    return new LiteralExpression(tokens.eat());

  if (tokens.pop_if(LPAREN)) {
    TokenHandler t = tokens.getTokensBetweenParenthesis();
    return parseExpression(t);
  }

  return new IdentifierExpression(parseIdentifier(tokens));
}

Expression *parsePostfixExpression(TokenHandler &tokens) {
  Expression *left = parsePrimaryExpression(tokens);

  while (true) {

    if (tokens.pop_if(PLUSPLUS))
      left = new UnaryExpression(left, PLUSPLUS);

    else if (tokens.pop_if(MINUSMINUS))
      left = new UnaryExpression(left, MINUSMINUS);

    else if (tokens.pop_if(LPAREN)) {
      TokenHandler t = tokens.getTokensBetweenParenthesis();
      left = new CallingExpression(left, parseParameters(t));
    }

    else if (tokens.pop_if(LBRACKET)) {
      TokenHandler t = tokens.getTokensBetweenBrackets();
      left = new SubscriptExpression(left, parseExpression(t));
    }

    else
      break;
  }

  return left;
}

Expression *parsePrefixExpression(TokenHandler &tokens) {

  if (tokens.pop_if(PLUSPLUS))
    return new UnaryExpression(parsePrefixExpression(tokens), PLUSPLUS);

  if (tokens.pop_if(MINUSMINUS))
    return new UnaryExpression(parsePrefixExpression(tokens), MINUSMINUS);

  if (tokens.pop_if(ADDR))
    return new UnaryExpression(parsePrefixExpression(tokens), ADDR);

  return parsePostfixExpression(tokens);
}

Expression *parseExponentExpression(TokenHandler &tokens) {
  Expression *left = parsePrefixExpression(tokens);

  while (true) {
    if (tokens.pop_if(POW))
      left =
          new BinaryExpression(left, parsePrefixExpression(tokens), Token(POW));
    else
      break;
  }

  return left;
}

Expression *parseFactorExpression(TokenHandler &tokens) {
  Expression *left = parseExponentExpression(tokens);

  while (true) {
    if (tokens.pop_if(MULT))
      left = new BinaryExpression(left, parseFactorExpression(tokens),
                                  Token(MULT));

    else if (tokens.pop_if(DIV))
      left =
          new BinaryExpression(left, parseFactorExpression(tokens), Token(DIV));

    else if (tokens.pop_if(MOD))
      left =
          new BinaryExpression(left, parseFactorExpression(tokens), Token(MOD));

    else
      break;
  }

  return left;
}

Expression *parseTermExpression(TokenHandler &tokens) {
  Expression *l = parseFactorExpression(tokens);

  while (true) {

    if (tokens.pop_if(PLUS))
      l = new BinaryExpression(l, parseFactorExpression(tokens), Token(PLUS));

    else if (tokens.pop_if(MINUS))
      l = new BinaryExpression(l, parseFactorExpression(tokens), Token(MINUS));

    else
      break;
  }

  return l;
}

Expression *parseRelationalExpression(TokenHandler &tokens) {
  Expression *left = parseTermExpression(tokens);

  while (true) {
    if (tokens.pop_if(KEYWORD_EQUALS))
      left = new BinaryExpression(left, parseTermExpression(tokens),
                                  Token(KEYWORD_EQUALS));

    else if (tokens.pop_if(LESS))
      left =
          new BinaryExpression(left, parseTermExpression(tokens), Token(LESS));

    else if (tokens.pop_if(GTR))
      left =
          new BinaryExpression(left, parseTermExpression(tokens), Token(GTR));

    else if (tokens.pop_if(LESSEQ))
      left = new BinaryExpression(left, parseTermExpression(tokens),
                                  Token(LESSEQ));

    else if (tokens.pop_if(GTREQ))
      left =
          new BinaryExpression(left, parseTermExpression(tokens), Token(GTREQ));
    else
      break;
  }

  return left;
}

Expression *parseBitwiseExpression(TokenHandler &tokens) {

  Expression *left = parseRelationalExpression(tokens);

  while (true) {
    if (tokens.pop_if(KEYWORD_BITAND))
      left = new BinaryExpression(left, parseRelationalExpression(tokens),
                                  Token(KEYWORD_BITAND));

    else if (tokens.pop_if(KEYWORD_BITOR))
      left = new BinaryExpression(left, parseRelationalExpression(tokens),
                                  Token(KEYWORD_BITOR));

    else if (tokens.pop_if(KEYWORD_BITXOR))
      left = new BinaryExpression(left, parseRelationalExpression(tokens),
                                  Token(KEYWORD_BITXOR));

    else if (tokens.pop_if(KEYWORD_BITNOT))
      left = new BinaryExpression(left, parseRelationalExpression(tokens),
                                  Token(KEYWORD_BITNOT));

    else
      break;
  }

  return left;
}

Expression *parseLogicalExpression(TokenHandler &tokens) {
  Expression *left = parseBitwiseExpression(tokens);

  while (true) {
    if (tokens.pop_if(KEYWORD_AND))
      left = new BinaryExpression(left, parseBitwiseExpression(tokens),
                                  Token(KEYWORD_AND));
    else if (tokens.pop_if(KEYWORD_OR))
      left = new BinaryExpression(left, parseBitwiseExpression(tokens),
                                  Token(KEYWORD_OR));
    else
      break;
  }

  return left;
}

Expression *parseAssignmentExpression(TokenHandler &tokens) {

  Expression *left = parseLogicalExpression(tokens);
  if (tokens.pop_if(ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(tokens),
                                Token(ASSIGN));

  if (tokens.pop_if(PLUS_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(tokens),
                                Token(PLUS_ASSIGN));

  if (tokens.pop_if(MINUS_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(tokens),
                                Token(MINUS_ASSIGN));

  if (tokens.pop_if(DIV_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(tokens),
                                Token(DIV_ASSIGN));

  if (tokens.pop_if(MULT_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(tokens),
                                Token(MULT_ASSIGN));

  if (tokens.pop_if(MOD_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(tokens),
                                Token(MOD_ASSIGN));

  if (tokens.pop_if(POW_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(tokens),
                                Token(POW_ASSIGN));

  return left;
}

// assumes tokens holds only the tokens relevant to the expression
Expression *parseExpression(TokenHandler &tokens) {
  return parseAssignmentExpression(tokens);
}

Statement *parseStatement(TokenHandler &tokens);

Statement *parseExpressionStatement(TokenHandler &tokens) {
  if (tokens.pop_if(SEMI_COLON))
    return new ExpressionStatement();

  TokenHandler until_semi = tokens.getAllTokensUntilFirstOf(SEMI_COLON);
  tokens.pop();
  return new ExpressionStatement(parseExpression(until_semi));
}

Statement *parseVarDecl(TokenHandler &tokens) {
  Type type = parseType(tokens);
  std::string ident = parseIdentifier(tokens);
  if (!tokens.pop_if(ASSIGN))
    throw std::runtime_error("Expected assignment in variable declaration");

  if (tokens.pop_if(KEYWORD_JUNK)) {
    if (!tokens.pop_if(SEMI_COLON))
      throw std::runtime_error(
          "Expected semicolon ending variable declaration");

    return new VarDeclaration(std::move(type), std::move(ident));
  }

  TokenHandler expression_tokens = tokens.getAllTokensUntilFirstOf(SEMI_COLON);
  tokens.pop();
  Expression *expr = parseExpression(expression_tokens);

  return new VarDeclaration(std::move(type), std::move(ident), expr);
}

// for the statements below starting with a keyword,
// that keyword has already been eaten
Statement *parseIf(TokenHandler &tokens) {
  if (!tokens.pop_if(LPAREN))
    throw std::runtime_error("Expected lparen in if statement condition");

  TokenHandler condition_tokens = tokens.getTokensBetweenParenthesis();
  Expression *condition = parseExpression(condition_tokens);

  Statement *true_branch = parseStatement(tokens);
  Statement *false_branch = nullptr;

  if (tokens.pop_if(KEYWORD_ELSE))
    false_branch = parseStatement(tokens);

  return new IfStatement(condition, true_branch, false_branch);
}

Statement *parseFor(TokenHandler &tokens) {
  if (!tokens.pop_if(LPAREN))
    throw std::runtime_error("Expected opening parenthesis in for statement");

  TokenHandler betweenParen = tokens.getTokensBetweenParenthesis();
  Statement *first_stmt = parseExpressionStatement(betweenParen);

  TokenHandler until_semi = betweenParen.getAllTokensUntilFirstOf(SEMI_COLON);
  betweenParen.pop();
  Expression *condition = parseExpression(until_semi);

  Expression *iteration = parseExpression(betweenParen);
  Statement *loop_body = parseStatement(tokens);

  return new ForLoop(first_stmt, condition, iteration, loop_body);
}

Statement *parseWhile(TokenHandler &tokens) {

  if (!tokens.pop_if(LPAREN))
    throw std::runtime_error(
        "Expected open parenthesis in while loop condition");

  TokenHandler condition_tokens = tokens.getTokensBetweenParenthesis();
  Expression *condition = parseExpression(condition_tokens);
  Statement *loop_body = parseStatement(tokens);

  return new WhileLoop(condition, loop_body);
}

Statement *parseDoWhile(TokenHandler &tokens) {
  throw std::runtime_error("Do While Loop unsupported");
}

Statement *parseScoped(TokenHandler &tokens) {
  std::vector<Statement *> statements;
  while (!tokens.pop_if(RBRACE)) {
    if (tokens.empty())
      throw std::runtime_error("Expected ending rbrace in scoped statement");
    statements.push_back(parseStatement(tokens));
  }

  return new ScopedStatement(std::move(statements));
}

Statement *parseReturn(TokenHandler &tokens) {
  if (tokens.pop_if(SEMI_COLON))
    return new ReturnStatement();

  TokenHandler retval = tokens.getAllTokensUntilFirstOf(SEMI_COLON);
  tokens.pop();
  return new ReturnStatement(parseExpression(retval));
}

Statement *parseSwitch(TokenHandler &tokens) {
  throw std::runtime_error("Switch Statement Unsupported");
}

Statement *parseStatement(TokenHandler &tokens) {

  const Token &first = tokens.peek();
  if (first.isPrimitive())
    return parseVarDecl(tokens);

  switch (first.type) {
  case KEYWORD_IF:
    tokens.pop();
    return parseIf(tokens);
  case KEYWORD_FOR:
    tokens.pop();
    return parseFor(tokens);
  case KEYWORD_WHILE:
    tokens.pop();
    return parseWhile(tokens);
  case KEYWORD_DO:
    tokens.pop();
    return parseDoWhile(tokens);
  case KEYWORD_RETURN:
    tokens.pop();
    return parseReturn(tokens);
  case KEYWORD_SWITCH:
    tokens.pop();
    return parseSwitch(tokens);

  case LBRACE:
    tokens.pop();
    return parseScoped(tokens);

  case IDENTIFIER:
    if (tokens.peek_ahead(1).is(IDENTIFIER))
      return parseVarDecl(tokens);
  default:
    return parseExpressionStatement(tokens);
  }
}

ParsedFunction parseFunction(UnparsedFunction &func) {
  std::vector<Statement *> func_body;

  while (!func.body_tokens.empty())
    func_body.push_back(parseStatement(func.body_tokens));

  return {std::move(func.return_value), std::move(func.name),
          std::move(func.parameter_list), std::move(func_body)};
}

void Parser::secondPassParsing(UnparsedTU &&unparsedtu) {
  UnparsedTU tu = std::move(unparsedtu);

  for (auto &f : tu.functions)
    parseFunction(f).print();
}

void ParsedFunction::print() {
  std::string return_type_name;
  if (std::holds_alternative<StrictType>(return_value))
    return_type_name = std::get<StrictType>(return_value).type_name;
  else
    return_type_name = std::get<VariantType>(return_value).type_name;
  std::cout << return_type_name << " " << name << "(";
  for (auto &decl : parameter_list) {
    std::string param_type;
    if (std::holds_alternative<StrictType>(decl.type))
      param_type = std::get<StrictType>(decl.type).type_name;
    else
      param_type = std::get<VariantType>(decl.type).type_name;
    std::cout << param_type << " " << decl.ident << ", ";
  }

  std::cout << ") {" << std::endl;

  for (auto s : function_body) {
    s->print();
    std::cout << "\n\n";
  }

  std::cout << "}" << std::endl;
}
