#include "ast.hpp"
#include <iostream>

using namespace LOM::AST;

[[nodiscard]] static constexpr
bool has_ln(ASTNode::Type type) {
  return std::to_underlying(type) >= ASTNode::Type::DECLARATION &&
    std::to_underlying(type) <= ASTNode::Type::EXPR_STMT && type not_eq ASTNode::Type::SCOPED;
}

static void print(std::vector<ASTNode>::const_iterator& node, u64_t& ln) noexcept {
  if (has_ln(node->type())) {
    while (ln <= node->line_number())
      std::cout << '\n' << ln++ << ":\t\t";
  }

  using enum ASTNode::Type;
  switch (node->type()) {
  case EMPTY:
    assert(false);
  case DECLARATION:
    std::cout << (++node)->instance_type().toString() << " ";
    std::cout << *(++node)->getIdentifier() << " = ";
    if ((++node)->type() == EMPTY)
      std::cout << "junk";
    else
      print(node, ln);
    return;

  case IF:
    std::cout << "if ( ";
    print(++node, ln);
    std::cout << " ) ";
    print(++node, ln);
    if ((++node)->type() not_eq EMPTY) {
      std::cout << " else ";
      print(node, ln);
    }
    return;

  case FOR:
    std::cout << "for ( " << std::flush;
    print(++node, ln);
    std::cout << "; ";
    print(++node, ln);
    std::cout << "; ";
    print(++node, ln);
    std::cout << " )";
    print(++node, ln);
    return;

  case WHILE:
    std::cout << "while ( ";
    print(++node, ln);
    std::cout << " )";
    return print(++node, ln);

  case SCOPED: {
    const u64_t num_children = node->sub_statements();
    if (num_children == 0)
      return;

    std::cout << "{ ";
    for (auto i{0uz}; i<num_children; ++i) {
      print(++node, ln);
      std::cout << ';';
    }
    std::cout << " }";
    return;
  }

  case RETURN:
    std::cout << "return ";
    if ((++node)->type() not_eq EMPTY)
      print(node, ln);
    return;

  case EXPR_STMT:
    return print(++node, ln);

  case UNARY: {
    auto opr = node->getOperator();
    const char* opr_str = operatorToString(opr);
    assert(isCategoryUNARY_OPS(opr));

    std::cout << '(';
    if (isCategoryPREFIX_OPS(opr)) {
      std::cout << opr_str;
      if (opr == Operator::NOT || opr == Operator::BITNOT)
        std::cout << ' ';

      print(++node, ln);
    }
    else {
      print(++node, ln);
      std::cout << opr_str;
    }
    std::cout << ')';
    return;
  }

  case BINARY: {
    auto opr = node->getOperator();
    const char* opr_str = operatorToString(opr);
    assert(isCategoryBINARY_OPS(opr));

    std::cout << '(';
    print(++node, ln);
    std::cout << ' ' << opr_str << ' ';
    print(++node, ln);
    std::cout << ')';
    return;
  }

  case CALLING: {
    u64_t num_params = node->parameter_count();
    print(++node, ln);

    std::cout << '(';
    if (num_params == 0) {
      std::cout << ')';
      return;
    }

    while (num_params--) {
      print(++node, ln);
      std::cout << ", ";
    }
    std::cout << "\b\b)";
    return;
  }

  case SUBSCRIPT:
    assert(false);

  case IDENTIFIER:
    std::cout << *node->getIdentifier();
    return;

  case INT_LITERAL:
    std::cout << node->getInt();
    return;

  case UINT_LITERAL:
    std::cout << node->getUint();
    return;

  case FLOAT_LITERAL:
    std::cout << node->getFloat();
    return;

  case DOUBLE_LITERAL:
    std::cout << node->getDouble();
    return;

  case BOOL_LITERAL:
    std::cout << (node->getBool() ? "true" : "false");
    return;

  case CHAR_LITERAL:
    std::cout << node->getChar();
    return;

  case STRING_LITERAL:
    std::cout << *node->getString();
    return;

  default:
    assert(false);
  }
}

void SyntaxTree::print(u64_t starting_line_number) const noexcept {
  if (nodes.empty())
    return;

  auto curr = nodes.begin();
  const auto end = nodes.end();
  while (curr != end) {
    ::print(curr, starting_line_number);
    std::cout << "; ";
    ++curr;
  }
}
