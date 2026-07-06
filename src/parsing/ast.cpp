#include "ast.hpp"
#include "edenlib/macros.hpp"
#include <print>


namespace LOM::AST {
void print_ast(std::vector<ASTNode> const& nodes, File const& file) noexcept {

  auto curr = nodes.begin();
  const auto end = nodes.end();
  std::print("\n\t");

  while (curr not_eq end) {
    using enum ASTNode::NodeType;
    switch (curr->m.type) {
    case EMPTY: std::print("EMPTY"); break;
    case DECLARATION: std::print("DECLARATION WITH TYPE {}{}",
          QualifiedType{(curr + 1)->declaration_identifier_val(), curr->declaration_qualifiers()}.toString(),
          curr->declaration_has_init() ? "" : ", JUNK INITIALIZED"); break;
    case IF: std::print("IF{} W/ {} SUB_STATEMENTS", curr->if_has_else() ? " W/ ELSE" : "", curr->if_numstatements()); break;
    case WHILE: std::print("WHILE W/ {} SUB_STATEMENTS", curr->while_numstatements()); break;
    case RETURN: std::print("RETURN{}", curr->return_has_value() ? "" : " W/ NO VALUE"); break;

    case MEMBER_ACCESS: std::print("MEMBER_ACCESS"); break;
    case UNARY: std::print("UNARY: {}", operatorToString(curr->unary_operator())); break;
    case BINARY: std::print("BINARY: {}", operatorToString(curr->binary_operator())); break;
    case CALLING: std::print("CALLING W/ {} PARAMETERS", curr->parameter_count()); break;
    case IDENTIFIER: std::print("IDENTIFIER: {}", curr->identifier_val(file)); break;
    case CAST: std::print("CAST TO {}", curr->cast_type()->toString()); break;
    case SUBSCRIPT: std::print("SUBSCRIPT"); break;

    case SIGNED_LITERAL: std::print("SIGNED_LITERAL: {}", curr->signed_val()); break;
    case UNSIGNED_LITERAL: std::print("UNSIGNED_LITERAL: {}", curr->unsigned_val()); break;
    case FLOAT_LITERAL: std::print("FLOAT_LITERAL: {}", curr->float_val()); break;
    case DOUBLE_LITERAL: std::print("DOUBLE_LITERAL: {}", curr->double_val()); break;
    case BOOL_LITERAL: std::print("BOOL_LITERAL: {}", curr->bool_val()); break;
    case CHAR_LITERAL: std::print("CHAR_LITERAL: {}", curr->char_val()); break;
    case STRING_LITERAL: std::print("STRING_LITERAL: {}", curr->string_val(file)); break;
    case ESCAPED_STRING_LITERAL: std::print("ESCAPED_STRING_LITERAL: {}", curr->string_val(file)); break;

    default: eden_unreachable("Invalid ASTNode Type.");
    }
    std::print("\n\t");
    ++curr;
  }
}
}