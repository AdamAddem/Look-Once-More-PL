#pragma once
#include "lex.hpp"
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <variant>
#include <vector>

struct StrictType {
  std::string type_name;
  explicit StrictType(std::string &&_type_name)
      : type_name(std::move(_type_name)) {}

  StrictType(const StrictType &) = default;
  StrictType &operator=(const StrictType &) = default;

  StrictType(StrictType &&other) noexcept
      : type_name(std::move(other.type_name)) {}
};

struct VariantType {
  std::vector<StrictType> types;
  std::string type_name;
  bool devoid;

  explicit VariantType(std::vector<StrictType> &&_types, bool _devoid)
      : types(std::move(_types)), type_name("<"), devoid(_devoid) {
    std::unordered_set<std::string> seen;

    // should make types unique
    std::erase_if(types, [&seen](StrictType &type) {
      return !seen.insert(type.type_name).second;
    });

    int d = devoid ? 1 : 0;
    if ((types.size() + d) < 2)
      throw std::runtime_error("Expected two or more unique types");

    if (devoid)
      type_name += "devoid";

    for (auto &type : types) {
      type_name += ", ";
      type_name += type.type_name;
    }

    type_name.push_back('>');
  }

  VariantType(const VariantType &) = default;

  VariantType(VariantType &&other) noexcept
      : types(std::move(other.types)), type_name(std::move(other.type_name)),
        devoid(other.devoid) {}
};
using Type = std::variant<StrictType, VariantType>;

/* Expressions: */
struct Expression {
  virtual ~Expression() = default;
  virtual void print() = 0;
};

struct UnaryExpression : Expression {
  Expression *expr;
  Lexer::TokenType opr;

  UnaryExpression(Expression *_expr, Lexer::TokenType _opr)
      : expr(_expr), opr(_opr) {}

  virtual void print() override;
};

struct BinaryExpression : Expression {
  Expression *expr_left;
  Expression *expr_right;
  Lexer::Token opr;

  BinaryExpression(Expression *_expr_left, Expression *_expr_right,
                   Lexer::Token &&_opr)
      : expr_left(_expr_left), expr_right(_expr_right), opr(std::move(_opr)) {}

  virtual void print() override;
};

struct CallingExpression : Expression {
  Expression *func;
  std::vector<Expression *> parameters;

  CallingExpression(Expression *_f, std::vector<Expression *> &&_params)
      : func(_f), parameters(std::move(_params)) {}

  virtual void print() override;
};

struct SubscriptExpression : Expression {
  Expression *arr;
  Expression *inside;

  SubscriptExpression(Expression *_arr, Expression *_inside)
      : arr(_arr), inside(_inside) {};

  virtual void print() override;
};

struct IdentifierExpression : Expression {
  std::string ident;

  explicit IdentifierExpression(std::string &&_ident)
      : ident(std::move(_ident)) {}

  virtual void print() override;
};

struct LiteralExpression : Expression {
  Lexer::Token literal;

  explicit LiteralExpression(Lexer::Token &&_t) : literal(std::move(_t)) {}

  virtual void print() override;
};

struct TemporaryExpr : Expression { // finish

  virtual void print() override;
};
