#include "ast.hpp"
#include "edenlib/macros.hpp"
#include <print>
#include <iostream>

using namespace LOM::AST;

[[nodiscard]] static constexpr bool has_ln(ASTNode::Type type) {
  return eden::enumBetween(type, ASTNode::DECLARATION, ASTNode::EXPR_STMT) &&
         type not_eq ASTNode::SCOPED;
}

static void print(std::vector<ASTNode>::const_iterator &node,
                  u64_t &ln) noexcept {
  //if (has_ln(node->type())) {
    //while (ln <= node->line_number())
      //std::print("\n{}:\t\t", ln++);
  //}

  using enum ASTNode::Type;
  switch (node->type()) {
  case EMPTY:
    eden_unreachable("Empty should not be printed.");
  case DECLARATION:
    std::print("\n\t{} ", (++node)->instance_type().toString());
    std::print("{} = ", (++node)->identifier());

    if ((++node)->type() == EMPTY)
      std::print("junk");
    else
      print(node, ln);
    return;
  case IF:
    std::print("\n\tif ( ");
    print(++node, ln);
    std::print(" ) ");
    print(++node, ln);
    if ((++node)->type() not_eq EMPTY) {
      std::print(" else ");
      print(node, ln);
    }
    return;
  case WHILE:
    std::print("\n\twhile ( ");
    print(++node, ln);
    std::print(" )");
    print(++node, ln);
    return;
  case SCOPED: {
    const u64_t num_children = node->sub_statements();
    if (num_children == 0)
      return;

    std::print("\n\t{{");
    for (auto i{0uz}; i < num_children; ++i) {
      print(++node, ln);
      std::print(";");
    }
    return std::print("\n\t}}");
  }
  case RETURN:
    std::print("\n\treturn ");
    if ((++node)->type() not_eq EMPTY)
      print(node, ln);
    return;
  case EXPR_STMT:
    std::print("\n\t");
    print(++node, ln);
    return;
  case MEMBER_ACCESS:
    print(++node, ln);
    std::print(".");
    print(++node, ln);
    return;
  case UNARY: {
    const auto opr = node->operator_val();
    const char *opr_str = operatorToString(opr);
    assert(isCategoryUNARY_OPS(opr));

    std::print("(");
    if (isCategoryPREFIX_OPS(opr)) {
      std::print("{}", opr_str);
      if (opr == Operator::NOT or opr == Operator::BITNOT)
        std::print(" ");
      print(++node, ln);
    } else {
      print(++node, ln);
      std::print("{}", opr_str);
    }
    std::print(")");
    return;
  }
  case BINARY: {
    const auto opr = node->operator_val();
    const char *opr_str = operatorToString(opr);
    assert(isCategoryBINARY_OPS(opr));

    std::print("(");
    print(++node, ln);
    std::print(" {} ", opr_str);
    print(++node, ln);
    return std::print(")");
  }
  case CALLING: {
    u64_t num_params = node->parameter_count();
    print(++node, ln);
    std::print("(");
    if (num_params == 0)
      return std::print(")");

    while (num_params--) {
      print(++node, ln);
      std::print(", ");
    }
    return std::print("\b\b)");
  }
  case SUBSCRIPT:
    eden_unreachable("Subscript operator unsupported.");
  case IDENTIFIER:
    return std::print("{}", node->identifier());
  case CAST:
    std::print("cast<{}>(", node->cast_type()->toString());
    print(++node, ln);
    return std::print(")");
  case SIGNED_LITERAL:
    return std::print("{}", node->signed_val());
  case UNSIGNED_LITERAL:
    return std::print("{}", node->unsigned_val());
  case FLOAT_LITERAL:
    return std::print("{}", node->float_val());
  case DOUBLE_LITERAL:
    return std::print("{}", node->double_val());
  case BOOL_LITERAL:
    return std::print("{}", (node->bool_val() ? "true" : "false"));
  case CHAR_LITERAL:
    return std::print("{}", node->char_val());
  case STRING_LITERAL:
    return std::print("\"{}\"", node->string_val());
  default:
    eden_unreachable("Invalid literal astnode type.");
  }
}

void SyntaxTree::print([[maybe_unused]] u64_t starting_line_number) const noexcept {
  if (nodes.empty())
    return;

  auto curr = nodes.begin();
  const auto end = nodes.end();
  std::print("\n");

#define PRINT_PURE_AST
#ifdef PRINT_PURE_AST
  while (curr not_eq end) {
    using enum ASTNode::Type;
    std::print("\t");
    switch (curr->type()) {
    case EMPTY: std::println("EMPTY"); break;
    case DECLARATION: std::println("DECLARATION WITH TYPE {}", (++curr)->instance_type().toString()); break;
    case IF: std::println("IF, LN: {}", curr->line_number()); break;
    case WHILE: std::println("WHILE, LN: {}", curr->line_number()); break;
    case SCOPED: std::println("SCOPED W/ {} SUB_STATEMENTS", curr->sub_statements()); break;
    case RETURN: std::println("RETURN, LN: {}", curr->line_number()); break;
    case EXPR_STMT: std::println("EXPR_STMT, LN: {}", curr->line_number()); break;
    case MEMBER_ACCESS: std::println("MEMBER_ACCESS"); break;
    case UNARY: std::println("UNARY: {}", operatorToString(curr->operator_val())); break;
    case BINARY: std::println("BINARY: {}", operatorToString(curr->operator_val())); break;
    case CALLING: std::println("CALLING W/ {} PARAMETERS", curr->parameter_count()); break;
    case IDENTIFIER: std::println("IDENTIFIER: {}", curr->identifier()); break;
    case CAST: std::println("CAST TO {}", curr->cast_type()->toString()); break;
    case SIGNED_LITERAL: std::println("SIGNED_LITERAL: {}", curr->signed_val()); break;
    case UNSIGNED_LITERAL: std::println("UNSIGNED_LITERAL: {}", curr->unsigned_val()); break;
    case FLOAT_LITERAL: std::println("FLOAT_LITERAL: {}", curr->float_val()); break;
    case DOUBLE_LITERAL: std::println("DOUBLE_LITERAL: {}", curr->double_val()); break;
    case BOOL_LITERAL: std::println("BOOL_LITERAL: {}", curr->bool_val()); break;
    case CHAR_LITERAL: std::println("CHAR_LITERAL: {}", curr->char_val()); break;
    case STRING_LITERAL: std::println("STRING_LITERAL: {}", curr->string_val()); break;
    default:
      eden_unreachable("Invalid ASTNode Type.");
    }
    ++curr;
  }
#else
  while (curr not_eq end) {
    ::print(curr, starting_line_number);
    std::cout << "; ";
    ++curr;
  }
#endif
}
