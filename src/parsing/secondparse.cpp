#include "secondparse.hpp"
#include "../analysis/symbol_table.hpp"
#include "../grammar/expressions.hpp"
#include "../grammar/statements.hpp"
#include "firstparse.hpp"

#include <iostream>
#include <stdexcept>

using namespace Parser;
using namespace Lexer;

Operator tokenToOperator(TokenType t) {
  switch (t) {
  case PLUS:
    return ADD;
  case MINUS:
    return SUBTRACT;
  case STAR:
    return MULTIPLY;
  case SLASH:
    return DIVIDE;
  case POW:
    return POWER;
  case MOD:
    return MODULUS;
  case PLUS_ASSIGN:
    return ADD_ASSIGN;
  case MINUS_ASSIGN:
    return SUB_ASSIGN;
  case Lexer::MULT_ASSIGN:
    return Operator::MULT_ASSIGN;
  case Lexer::DIV_ASSIGN:
    return Operator::DIV_ASSIGN;
  case Lexer::POW_ASSIGN:
    return Operator::POW_ASSIGN;
  case Lexer::MOD_ASSIGN:
    return Operator::MOD_ASSIGN;
  case Lexer::ASSIGN:
    return Operator::ASSIGN;
  case Lexer::LESS:
    return Operator::LESS;
  case GTR:
    return GREATER;
  case LESSEQ:
    return LESS_EQUAL;
  case GTREQ:
    return GREATER_EQUAL;
  case KEYWORD_AND:
    return AND;
  case KEYWORD_OR:
    return OR;
  case KEYWORD_XOR:
    return XOR;
  case KEYWORD_NOT:
    return NOT;
  case KEYWORD_EQUALS:
    return EQUAL;
  case KEYWORD_BITAND:
    return BITAND;
  case KEYWORD_BITOR:
    return BITOR;
  case KEYWORD_BITXOR:
    return BITXOR;
  case KEYWORD_BITNOT:
    return BITNOT;
  case KEYWORD_CAST:
    return CAST;
  case KEYWORD_CAST_IF:
    return CAST_IF;
  case KEYWORD_UNSAFE_CAST:
    return UNSAFE_CAST;
  case KEYWORD_VERY_UNSAFE_CAST:
    return VERY_UNSAFE_CAST;
  case ADDR:
    return ADDRESS_OF;

  default:
    std::cout << t << std::endl;
    throw std::runtime_error("Token to Operator conversion either unsupported, "
                             "or context dependent");
  }
}

Expression *parseExpression(SymbolTable &table, TokenHandler &tokens);

std::vector<Expression *> parseParameters(SymbolTable &table,
                                          TokenHandler &tokens) {
  if (tokens.empty())
    return {};

  std::vector<Expression *> retval;

  while (true) {
    retval.push_back(parseExpression(table, tokens));
    if (tokens.empty())
      return retval;

    if (!tokens.pop_if(COMMA))
      throw std::runtime_error(
          "Expected comma between function parameters in call");
  }
}

Expression *parsePrimaryExpression(SymbolTable &table, TokenHandler &tokens) {
  if (tokens.peek().isLiteral()) {
    LiteralExpression::LiteralType type;
    switch (tokens.peek().type) {
    case INT_LITERAL:
      type = LiteralExpression::INT;
      break;
    case FLOAT_LITERAL:
      type = LiteralExpression::FLOAT;
      break;
    case DOUBLE_LITERAL:
      type = LiteralExpression::DOUBLE;
      break;
    case BOOL_LITERAL:
      type = LiteralExpression::BOOL;
      break;
    case CHAR_LITERAL:
      type = LiteralExpression::CHAR;
      break;
    case STRING_LITERAL:
      type = LiteralExpression::STRING;
      break;

    default:
      throw std::runtime_error(
          "Impossible error in parsePrimaryExpression function in secondparse");
    }

    return new LiteralExpression(tokens.eat().value, type);
  }

  if (tokens.pop_if(LPAREN)) {
    TokenHandler t = tokens.getTokensBetweenParenthesis();
    return parseExpression(table, t);
  }

  return new IdentifierExpression(parseIdentifier(tokens));
}

Expression *parsePostfixExpression(SymbolTable &table, TokenHandler &tokens) {
  Expression *left = parsePrimaryExpression(table, tokens);

  while (true) {

    if (tokens.pop_if(PLUSPLUS))
      left = new UnaryExpression(left, PRE_INCREMENT);

    else if (tokens.pop_if(MINUSMINUS))
      left = new UnaryExpression(left, POST_DECREMENT);

    else if (tokens.pop_if(LPAREN)) {
      TokenHandler t = tokens.getTokensBetweenParenthesis();
      left = new CallingExpression(left, parseParameters(table, t));
    }

    else if (tokens.pop_if(LBRACKET)) {
      TokenHandler t = tokens.getTokensBetweenBrackets();
      left = new SubscriptExpression(left, parseExpression(table, t));
    }

    else
      break;
  }

  return left;
}

Expression *parsePrefixExpression(SymbolTable &table, TokenHandler &tokens) {

  if (tokens.pop_if(PLUSPLUS))
    return new UnaryExpression(parsePrefixExpression(table, tokens),
                               PRE_INCREMENT);

  if (tokens.pop_if(MINUSMINUS))
    return new UnaryExpression(parsePrefixExpression(table, tokens),
                               PRE_DECREMENT);

  if (tokens.pop_if(ADDR))
    return new UnaryExpression(parsePrefixExpression(table, tokens),
                               ADDRESS_OF);

  return parsePostfixExpression(table, tokens);
}

Expression *parseExponentExpression(SymbolTable &table, TokenHandler &tokens) {
  Expression *left = parsePrefixExpression(table, tokens);

  while (true) {
    if (tokens.pop_if(POW))
      left = new BinaryExpression(left, parsePrefixExpression(table, tokens),
                                  POWER);
    else
      break;
  }

  return left;
}

Expression *parseFactorExpression(SymbolTable &table, TokenHandler &tokens) {
  Expression *left = parseExponentExpression(table, tokens);

  while (true) {
    if (tokens.pop_if(STAR))
      left = new BinaryExpression(left, parseFactorExpression(table, tokens),
                                  MULTIPLY);

    else if (tokens.pop_if(SLASH))
      left = new BinaryExpression(left, parseFactorExpression(table, tokens),
                                  DIVIDE);

    else if (tokens.pop_if(MOD))
      left = new BinaryExpression(left, parseFactorExpression(table, tokens),
                                  MODULUS);

    else
      break;
  }

  return left;
}

Expression *parseTermExpression(SymbolTable &table, TokenHandler &tokens) {
  Expression *l = parseFactorExpression(table, tokens);

  while (true) {

    if (tokens.pop_if(PLUS))
      l = new BinaryExpression(l, parseFactorExpression(table, tokens), ADD);

    else if (tokens.pop_if(MINUS))
      l = new BinaryExpression(l, parseFactorExpression(table, tokens),
                               SUBTRACT);

    else
      break;
  }

  return l;
}

Expression *parseRelationalExpression(SymbolTable &table,
                                      TokenHandler &tokens) {
  Expression *left = parseTermExpression(table, tokens);

  while (true) {
    if (tokens.pop_if(KEYWORD_EQUALS))
      left =
          new BinaryExpression(left, parseTermExpression(table, tokens), EQUAL);

    else if (tokens.pop_if(Lexer::LESS))
      left = new BinaryExpression(left, parseTermExpression(table, tokens),
                                  Operator::LESS);

    else if (tokens.pop_if(GTR))
      left = new BinaryExpression(left, parseTermExpression(table, tokens),
                                  GREATER);

    else if (tokens.pop_if(LESSEQ))
      left = new BinaryExpression(left, parseTermExpression(table, tokens),
                                  LESS_EQUAL);

    else if (tokens.pop_if(GTREQ))
      left = new BinaryExpression(left, parseTermExpression(table, tokens),
                                  GREATER_EQUAL);
    else
      break;
  }

  return left;
}

Expression *parseBitwiseExpression(SymbolTable &table, TokenHandler &tokens) {

  Expression *left = parseRelationalExpression(table, tokens);

  while (true) {
    if (tokens.pop_if(KEYWORD_BITAND))
      left = new BinaryExpression(
          left, parseRelationalExpression(table, tokens), BITAND);

    else if (tokens.pop_if(KEYWORD_BITOR))
      left = new BinaryExpression(
          left, parseRelationalExpression(table, tokens), BITOR);

    else if (tokens.pop_if(KEYWORD_BITXOR))
      left = new BinaryExpression(
          left, parseRelationalExpression(table, tokens), BITXOR);

    else if (tokens.pop_if(KEYWORD_BITNOT))
      left = new BinaryExpression(
          left, parseRelationalExpression(table, tokens), BITNOT);

    else
      break;
  }

  return left;
}

Expression *parseLogicalExpression(SymbolTable &table, TokenHandler &tokens) {
  Expression *left = parseBitwiseExpression(table, tokens);

  while (true) {
    if (tokens.pop_if(KEYWORD_AND))
      left = new BinaryExpression(left, parseBitwiseExpression(table, tokens),
                                  AND);
    else if (tokens.pop_if(KEYWORD_OR))
      left =
          new BinaryExpression(left, parseBitwiseExpression(table, tokens), OR);
    else
      break;
  }

  return left;
}

Expression *parseAssignmentExpression(SymbolTable &table,
                                      TokenHandler &tokens) {

  Expression *left = parseLogicalExpression(table, tokens);
  if (tokens.pop_if(Lexer::ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(table, tokens),
                                Operator::ASSIGN);

  if (tokens.pop_if(PLUS_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(table, tokens),
                                ADD_ASSIGN);

  if (tokens.pop_if(MINUS_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(table, tokens),
                                SUB_ASSIGN);

  if (tokens.pop_if(Lexer::DIV_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(table, tokens),
                                Operator::DIV_ASSIGN);

  if (tokens.pop_if(Lexer::MULT_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(table, tokens),
                                Operator::MULT_ASSIGN);

  if (tokens.pop_if(Lexer::MOD_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(table, tokens),
                                Operator::MOD_ASSIGN);

  if (tokens.pop_if(Lexer::POW_ASSIGN))
    return new BinaryExpression(left, parseAssignmentExpression(table, tokens),
                                Operator::POW_ASSIGN);

  return left;
}

// assumes tokens holds only the tokens relevant to the expression
Expression *parseExpression(SymbolTable &table, TokenHandler &tokens) {
  return parseAssignmentExpression(table, tokens);
}

Statement *parseStatement(SymbolTable &table, TokenHandler &tokens);
Statement *parseScoped(SymbolTable &table, TokenHandler &tokens);

Statement *parseExpressionStatement(SymbolTable &table, TokenHandler &tokens) {
  if (tokens.pop_if(SEMI_COLON))
    return new ExpressionStatement();

  TokenHandler until_semi = tokens.getAllTokensUntilFirstOf(SEMI_COLON);
  tokens.pop();
  return new ExpressionStatement(parseExpression(table, until_semi));
}

Statement *parseVarDecl(SymbolTable &table, TokenHandler &tokens) {
  Type type = parseType(tokens);
  std::string ident = parseIdentifier(tokens);
  table.addLocalVariable(ident, type);
  if (!tokens.pop_if(Lexer::ASSIGN))
    throw std::runtime_error("Expected assignment in variable declaration");

  if (tokens.pop_if(KEYWORD_JUNK)) {
    if (!tokens.pop_if(SEMI_COLON))
      throw std::runtime_error(
          "Expected semicolon ending variable declaration");

    return new VarDeclaration(std::move(type), std::move(ident));
  }

  TokenHandler expression_tokens = tokens.getAllTokensUntilFirstOf(SEMI_COLON);
  tokens.pop();
  Expression *expr = parseExpression(table, expression_tokens);

  return new VarDeclaration(std::move(type), std::move(ident), expr);
}

// for the statements below starting with a keyword,
// that keyword has already been eaten
Statement *parseIf(SymbolTable &table, TokenHandler &tokens) {
  if (!tokens.pop_if(LPAREN))
    throw std::runtime_error("Expected lparen in if statement condition");

  TokenHandler condition_tokens = tokens.getTokensBetweenParenthesis();
  Expression *condition = parseExpression(table, condition_tokens);

  Statement *true_branch = parseScoped(table, tokens);
  Statement *false_branch = nullptr;

  if (tokens.pop_if(KEYWORD_ELSE))
    false_branch = parseScoped(table, tokens);

  return new IfStatement(condition, true_branch, false_branch);
}

Statement *parseFor(SymbolTable &table, TokenHandler &tokens) {
  if (!tokens.pop_if(LPAREN))
    throw std::runtime_error("Expected opening parenthesis in for statement");

  TokenHandler betweenParen = tokens.getTokensBetweenParenthesis();
  Statement *declOrAssignment;
  table.enterScope();
  if (betweenParen.peek().isPrimitive())
    declOrAssignment = parseVarDecl(table, betweenParen);
  else if (betweenParen.peek_is(IDENTIFIER)) {
    if (betweenParen.peek_ahead(1).is(IDENTIFIER))
      declOrAssignment = parseVarDecl(table, betweenParen);
    else
      declOrAssignment = parseExpressionStatement(table, betweenParen);
  } else if (betweenParen.peek_is(Lexer::LESS)) {
    declOrAssignment = parseVarDecl(table, betweenParen);
  } else
    throw std::runtime_error("Expected variable declaration or assignment in "
                             "first for loop statement");

  Statement *condition = parseExpressionStatement(table, betweenParen);
  Expression *iteration = parseExpression(table, betweenParen);
  Statement *loop_body = parseScoped(table, tokens);
  table.leaveScope();

  return new ForLoop(declOrAssignment, condition, iteration, loop_body);
}

Statement *parseWhile(SymbolTable &table, TokenHandler &tokens) {

  if (!tokens.pop_if(LPAREN))
    throw std::runtime_error(
        "Expected open parenthesis in while loop condition");

  TokenHandler condition_tokens = tokens.getTokensBetweenParenthesis();
  Expression *condition = parseExpression(table, condition_tokens);

  table.enterScope();
  Statement *loop_body = parseScoped(table, tokens);
  table.leaveScope();

  return new WhileLoop(condition, loop_body);
}

Statement *parseDoWhile(SymbolTable &table, TokenHandler &tokens) {
  throw std::runtime_error("Do While Loop unsupported");
}

// scoped may be {...} or one statement ;
Statement *parseScoped(SymbolTable &table, TokenHandler &tokens) {
  std::vector<Statement *> statements;
  if (tokens.pop_if(LBRACE)) {
    TokenHandler scopedTokens = tokens.getTokensBetweenBraces();
    while (!scopedTokens.empty())
      statements.push_back(parseStatement(table, scopedTokens));

    return new ScopedStatement(std::move(statements));
  }

  statements.push_back(parseStatement(table, tokens));
  return new ScopedStatement(std::move(statements));
}

Statement *parseReturn(SymbolTable &table, TokenHandler &tokens) {
  if (tokens.pop_if(SEMI_COLON))
    return new ReturnStatement();

  TokenHandler retval = tokens.getAllTokensUntilFirstOf(SEMI_COLON);
  tokens.pop();
  return new ReturnStatement(parseExpression(table, retval));
}

Statement *parseSwitch(SymbolTable &table, TokenHandler &tokens) {
  throw std::runtime_error("Switch Statement Unsupported");
}

Statement *parseStatement(SymbolTable &table, TokenHandler &tokens) {

  const Token &first = tokens.peek();
  if (first.isPrimitive())
    return parseVarDecl(table, tokens);

  switch (first.type) {
  case KEYWORD_IF:
    tokens.pop();
    return parseIf(table, tokens);
  case KEYWORD_FOR:
    tokens.pop();
    return parseFor(table, tokens);
  case KEYWORD_WHILE:
    tokens.pop();
    return parseWhile(table, tokens);
  case KEYWORD_DO:
    tokens.pop();
    return parseDoWhile(table, tokens);
  case KEYWORD_RETURN:
    tokens.pop();
    return parseReturn(table, tokens);
  case KEYWORD_SWITCH:
    tokens.pop();
    return parseSwitch(table, tokens);

  case LBRACE: {
    table.enterScope();
    Statement *scoped = parseScoped(table, tokens);
    table.leaveScope();
    return scoped;
  }

  case Lexer::LESS:
    return parseVarDecl(table, tokens);

  case IDENTIFIER:
    if (tokens.peek_ahead(1).is(IDENTIFIER))
      return parseVarDecl(table, tokens);
  default:
    return parseExpressionStatement(table, tokens);
  }
}

std::vector<Statement *> parseStatements(SymbolTable &table,
                                         TokenHandler &body_tokens) {

  std::vector<Statement *> body_statements;

  while (!body_tokens.empty())
    body_statements.push_back(parseStatement(table, body_tokens));

  return body_statements;
}

ParsedGlobals parseGlobals(SymbolTable &table, UnparsedGlobals &globals) {

  for (auto &decl : globals.declarations)
    table.addGlobalVariable(decl.ident, decl.type, false);

  return {std::move(globals.declarations),
          parseStatements(table, globals.global_init_body)};
}

ParsedFunction parseFunction(SymbolTable &table, UnparsedFunction &func) {

  for (auto &decl : func.parameter_list)
    table.addLocalVariable(decl.ident, decl.type);

  return {std::move(func.return_value), std::move(func.name),
          std::move(func.parameter_list),
          parseStatements(table, func.body_tokens)};
}

void Parser::secondPassParsing(UnparsedTU &&unparsedtu) {
  SymbolTable table;

  table.enterScope();
  ParsedGlobals globals = parseGlobals(table, unparsedtu.globals);
  table.leaveScope();

  std::vector<ParsedFunction> parsed_funcs;
  for (auto &f : unparsedtu.functions) {
    table.enterScope();
    parsed_funcs.emplace_back(parseFunction(table, f));
    table.leaveScope();
  }

  ParsedTranslationUnit ptu{std::move(globals), std::move(parsed_funcs)};

  unparsedtu.table.printGlobals();
  // ptu.print();
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
    decl.print();
    std::cout << "\n";
  }
  std::cout << "\n";

  std::cout << "globals{\n\n";

  for (auto s : global_init_body) {
    s->print(1);
    std::cout << "\n";
  }

  std::cout << "\n}\n" << std::endl;
}

void ParsedFunction::print() {
  printType(return_value);
  std::cout << " " << name << "(";
  for (auto &decl : parameter_list) {
    printType(decl.type);
    std::cout << " " << decl.ident << ", ";
  }

  if (!parameter_list.empty())
    std::cout << "\b\b";

  std::cout << ") {\n" << std::endl;

  for (auto s : function_body) {
    s->print(1);
    std::cout << "\n\n";
  }

  std::cout << "}" << std::endl;
}
