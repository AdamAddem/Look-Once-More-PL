#include "ast.hpp"
#include "edenlib/macros.hpp"
#include <print>


namespace LOM::AST {
void print_ast(std::vector<ASTNode> const& nodes, File const& file) noexcept {

  auto curr = nodes.begin();
  const auto end = nodes.end();
  std::print("\n");

  while (curr not_eq end) {
    using enum ASTNode::Type;
    std::print("\t");
    switch (curr->m.type) {
    case EMPTY: std::println("EMPTY"); break;
    case DECLARATION: std::println("DECLARATION WITH TYPE {}", (++curr)->instance_type().toString()); break;
    case IF: std::println("IF"); break;
    case WHILE: std::println("WHILE"); break;
    case SCOPED: std::println("SCOPED W/ {} SUB_STATEMENTS", curr->sub_statements()); break;
    case RETURN: std::println("RETURN"); break;
    case EXPR_STMT: std::println("EXPR_STMT"); break;
    case MEMBER_ACCESS: std::println("MEMBER_ACCESS"); break;
    case UNARY: std::println("UNARY: {}", operatorToString(curr->operator_val())); break;
    case BINARY: std::println("BINARY: {}", operatorToString(curr->operator_val())); break;
    case CALLING: std::println("CALLING W/ {} PARAMETERS", curr->parameter_count()); break;
    case IDENTIFIER: std::println("IDENTIFIER: {}", curr->identifier(file)); break;
    case CAST: std::println("CAST TO {}", curr->cast_type()->toString()); break;
    case SIGNED_LITERAL: std::println("SIGNED_LITERAL: {}", curr->signed_val()); break;
    case UNSIGNED_LITERAL: std::println("UNSIGNED_LITERAL: {}", curr->unsigned_val()); break;
    case FLOAT_LITERAL: std::println("FLOAT_LITERAL: {}", curr->float_val()); break;
    case DOUBLE_LITERAL: std::println("DOUBLE_LITERAL: {}", curr->double_val()); break;
    case BOOL_LITERAL: std::println("BOOL_LITERAL: {}", curr->bool_val()); break;
    case CHAR_LITERAL: std::println("CHAR_LITERAL: {}", curr->char_val()); break;
    case STRING_LITERAL: std::println("STRING_LITERAL: {}", curr->string_val(file)); break;
    default:
      eden_unreachable("Invalid ASTNode Type.");
    }
    ++curr;
  }
}
}