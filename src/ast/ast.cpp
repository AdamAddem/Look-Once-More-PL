#include "ast.hpp"
#include <iostream>

using namespace LOM::AST;

[[nodiscard]] constexpr
bool has_ln(ASTNode::Type type) {
  return std::to_underlying(type) >= ASTNode::Type::DECLARATION &&
    std::to_underlying(type) <= ASTNode::Type::RETURN && type not_eq ASTNode::Type::SCOPED;
}

void print(std::vector<ASTNode>::const_iterator& node, u64_t& ln) noexcept {
  if (has_ln(node->type) && node->value > ln) {
    ln = node->value;
    std::cout << '\n' << ln << ": ";
  }

  using enum ASTNode::Type;
  switch (node->type) {
  case EMPTY:
    assert(false);
  case DECLARATION:
    std::cout << "decl " << *(++node)->getIdentifier() << " = ";
    if ((++node)->type == EMPTY)
      std::cout << "junk";
    else
      print(node, ln);
    std::cout << ';';
    return;

  case IF:
    std::cout << "if (";
    print(++node, ln);
    std::cout << ")";
    return print(++node, ln);

  case FOR:
    std::cout << "for (";
    print(++node, ln);
    std::cout << ' ';
    print(++node, ln);
    std::cout << ';';
    return print(++node, ln);

  case WHILE:
    std::cout << "while (";
    print(++node, ln);
    std::cout << ')';
    return print(++node, ln);

  case SCOPED:
    if (node->value == 1)
      return print(++node, ln);

    if (node->value == 0) {
      std::cout << ';';
      return;
    }
    std::cout << '{';
    print(++node, ln);
    std::cout << '}';
    return;

  case RETURN:
    std::cout << "return ";
    if ((++node)->type not_eq EMPTY)
      print(node, ln);
    std::cout << ';';
    return;

  case UNARY: {
    auto opr = node->getOperator();
    const char* opr_str = operatorToString(opr);
    assert(isCategoryUNARY_OPS(opr));

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

    return;
  }

  case BINARY: {
    auto opr = node->getOperator();
    const char* opr_str = operatorToString(opr);
    assert(isCategoryBINARY_OPS(opr));

    print(++node, ln);
    std::cout << ' ' << opr_str << ' ';
    return print(++node, ln);
  }

  case CALLING: {
    u64_t num_params = node->value;
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



void SyntaxTree::print() const noexcept {
  if (nodes.empty())
    return;

  auto curr = nodes.begin();
  const auto end = nodes.end();
  u64_t ln = 0;
  while (curr != end) {
    ::print(curr, ln);
    ++curr;
  }
}
