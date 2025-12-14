#include "expressions.hpp"

struct Statement {
  virtual ~Statement() = default;
};

struct VarDeclaration : Statement {

  Type type;
  std::string ident;
  Expression *expr; // nullptr indicates junk initialization
  VarDeclaration(Type &&_type, std::string &&_ident,
                 Expression *_expr = nullptr)
      : type(std::move(_type)), ident(std::move(_ident)), expr(_expr) {}

  VarDeclaration(VarDeclaration &&other) noexcept
      : type(std::move(other.type)), ident(std::move(other.ident)) {}
};

struct VarAssignment : Statement {

  std::string ident;
  Expression *expr; // cannot be nullptr
  VarAssignment(std::string &&_ident, Expression *_expr)
      : ident(std::move(_ident)), expr(_expr) {}
};

struct FunctionCall : Statement {
  std::string function_name;
  std::vector<std::string> parameters;
  FunctionCall(std::string &&_function_name,
               std::vector<std::string> &&_parameters)
      : function_name(std::move(_function_name)),
        parameters(std::move(_parameters)) {}
};

struct IfStatement : Statement {
  Expression *condition;
  std::vector<Statement *> true_path;
  Statement *false_path;

  IfStatement(Expression *_condition, std::vector<Statement *> &&_true_path,
              Statement *_false_path = nullptr)
      : condition(_condition), true_path(_true_path),
        false_path(std::move(_false_path)) {}
};

struct ForLoop : Statement {
  Statement *decl_statement;
  Expression *condition;
  Expression *iteration;

  std::vector<Statement *> loop_body;

  ForLoop(Statement *_decl, Expression *_condition, Expression *_iteration,
          std::vector<Statement *> &&_loop_body)
      : decl_statement(_decl), condition(_condition), iteration(_iteration),
        loop_body(std::move(_loop_body)) {}
};

struct WhileLoop : Statement {
  Expression *condition;
  bool doWhile;
  std::vector<Statement *> loop_body;
  WhileLoop(Expression *_condition, bool _doWhile,
            std::vector<Statement *> &&_loop_body)
      : condition(_condition), doWhile(_doWhile), loop_body(_loop_body) {}
};

struct ReturnStatement : Statement {
  Expression *return_value;
  explicit ReturnStatement(Expression *_return_value)
      : return_value(_return_value) {}
};

struct SwitchCase : Statement {}; // TBD
