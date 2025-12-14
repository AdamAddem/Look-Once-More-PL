#include <stdexcept>
#include <string>
#include <variant>
#include <vector>

struct StrictType {
  std::string type_name;
  explicit StrictType(std::string &&_type_name)
      : type_name(std::move(_type_name)) {}

  StrictType(const StrictType &) = default;

  StrictType(StrictType &&other) noexcept
      : type_name(std::move(other.type_name)) {}
};

struct VariantType {

  std::vector<StrictType> types;
  bool devoid;
  explicit VariantType(std::vector<StrictType> &&_types, bool _devoid)
      : types(std::move(_types)), devoid(_devoid) {}

  VariantType(const VariantType &) = default;

  VariantType(VariantType &&other) noexcept
      : types(std::move(other.types)), devoid(other.devoid) {}
};
using Type = std::variant<StrictType, VariantType>;

enum CastType { Cast, CastIf, UnsafeCast, VeryUnsafeCast };

/* Operators: */
enum Operator {

  None,

  Call,      // (...)
  Subscript, // []
  Address,   // @

  Plus,
  Minus,
  Divide,
  Multiply,
  Exponent,
  Modulus,

  Assign,
  PlusAssign,
  MinusAssign,
  DivideAssign,
  MultiplyAssign,
  ExponentAssign,
  ModulusAssign,

  PrefixIncrement,
  PrefixDecrement,
  PostfixIncrement,
  PostfixDecrement,
  UnaryMinus,

  Less,
  Greater,
  LessEqual,
  GreaterEqual,

  And,
  Or,
  Xor,
  Not,
  Equals,
  Bitand,
  Bitor,
  Bitxor,
  Bitnot

};
/* Operators: */

/* Expressions: */
struct Expression {
  virtual ~Expression() = default;
};

struct UnaryExpression : Expression {
  Expression *expr;
  Operator opr;
  bool isPrefix;

  UnaryExpression(Expression *_expr, Operator _opr)
      : expr(_expr), opr(_opr),
        isPrefix(_opr == PrefixDecrement || _opr == PrefixIncrement) {}
};

struct BinaryExpression : Expression {
  Expression *expr_left;
  Expression *expr_right;
  Operator opr;
  enum BinaryExpressionType {
    Arithmetic,
    Assignment,
    Conditional,
    Bitwise,
  };
  BinaryExpressionType type;

  BinaryExpression(Expression *_expr_left, Expression *_expr_right,
                   Operator _opr)
      : expr_left(_expr_left), expr_right(_expr_right), opr(_opr) {
    switch (_opr) {
    case And:
    case Or:
    case Xor:
    case Equals:
    case Less:
    case Greater:
    case GreaterEqual:
    case LessEqual:
      type = Conditional;
      break;

    case Plus:
    case Minus:
    case Multiply:
    case Divide:
    case Exponent:
    case Modulus:
      type = Arithmetic;
      break;

    case Bitand:
    case Bitor:
    case Bitxor:
      type = Bitwise;
      break;

    default:
      throw std::runtime_error("Operator not supported by Binary Expression");
    }
  }
};

struct CastExpression : Expression {
  CastType cast;
  Expression *expr;

  CastExpression(CastType _type, Expression *_expr)
      : cast(_type), expr(_expr) {}
};

struct CallingExpression : Expression {
  Expression *expr;
  std::vector<Expression *> parameters;

  CallingExpression(Expression *_expr, std::vector<Expression *> &&_parameters)
      : expr(_expr), parameters(std::move(_parameters)) {}
};
