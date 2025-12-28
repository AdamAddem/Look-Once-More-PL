#pragma once

namespace Parser {
struct ParsedTranslationUnit;
}

namespace Validation {

void validateTU(Parser::ParsedTranslationUnit &&);

}; // namespace Validation
