#include "secondparse.hpp"
#include "firstparse.hpp"

using namespace Parser;

void Parser::secondPassParsing(UnparsedTU &&unparsedtu) {
  UnparsedTU tu = std::move(unparsedtu);
}
