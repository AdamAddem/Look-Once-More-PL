#include "print.hpp"
#include "build_system/build.hpp"
#include "parsing/parse.hpp"
#include "peepir/peepir.hpp"

using namespace LOM;
namespace {
class IndentationManager {
  eden::vector<sz_t> indents{eden::flags::reserve_initial<4>};
public:

  NoInlineCold void
  print() noexcept {
    while (indents.back()-- == 0) indents.pop_back();
    for ([[maybe_unused]] auto _ : indents) std::print("  ");
  }

  NoInlineCold void add(sz_t num_subexpressions) noexcept { indents.push_back(num_subexpressions); }
};

NoInlineCold [[nodiscard]]
std::string functionToString(Symbol::Function const& fn) noexcept {
  std::string res;
  if (fn.is_public) res.append("pub ");
  res.append(fn.nameof());
  res.append(": (");

  for (u16_t i{}; i<fn.num_parameters; ++i) {
    auto const& parameter = fn.getLocal(i);
    res.append( std::format("{}: {}, " , parameter.nameof(), parameter.typeID.toString()) );
  }

  if (fn.is_variadic)
    res.append("...");
  else if (fn.num_parameters not_eq 0)
    res.pop_back(), res.pop_back();

  auto const return_typeID = fn.return_typeID;
  res.append(") ");
  if (return_typeID.derived not_eq Type::DEVOID)
    res.append( std::format("{} ", return_typeID.toString()) );

  res.append("{\n");
  return res;
}
}

namespace LOM::Parser { namespace {

NoInlineCold NodiscardCXPR
char const* operatorToString(Operator e) noexcept {
  static constexpr char const* toString[] = {
    "+","-","*",
    "/","%","=",
    "<",">","<=",
    ">=","and","or",
    "xor","bitand","bitor",
    "bitxor","eq","not_eq",
    "++","--","-",
    "@", "&", "bitnot","not",
    "++","--","->", "."
};
  return toString[std::to_underlying(e)];
}

NoInlineCold sz_t
printNode(ASTNode node, File const& file) noexcept {
  using enum ASTNode::NodeType;
  switch (node.type) {
  case EMPTY: std::println("EMPTY"); return 0;
  case DECLARATION_RO: std::println("{}: {} = ", node.original_string(file), node.declaration_type().toString()); return 1;
  case DECLARATION_RW: std::println("{}$ {} = ", node.original_string(file), node.declaration_type().toString()); return 1;
  case DECLARATION_JUNK_RO: std::println("{}: {} = junk", node.original_string(file), node.declaration_type().toString()); return 0;
  case DECLARATION_JUNK_RW: std::println("{}$ {} = junk", node.original_string(file), node.declaration_type().toString()); return 0;
  case IF: std::println("if"); return node.if_numstatements() + 1;
  case WHILE: std::println("while"); return node.while_numstatements() + 1;
  case RETURN: std::println("return"); return (sz_t)node.return_has_value();

  case MEMBER_ACCESS: std::println("custom type member access"); return 2;
  case MODULE_ACCESS: std::println("{} from module {}", node.module_member_name(file), node.module_name(file)); return 0;
  case UNARY: std::println("operator {}", operatorToString(node.unary_operator())); return 1;
  case BINARY: std::println("operator {}", operatorToString(node.binary_operator())); return 2;
  case CALLING: std::println("calling function with {} argument(s)", node.parameter_count()); return node.parameter_count() + 1;
  case IDENTIFIER: std::println("identifier {}", node.identifier_val(file)); return 0;
  case CAST: std::println("cast<{}>", node.cast_type().toString()); return 0;
  case SUBSCRIPT: std::println("operator [] on "); return 2;

  case SIGNED_LITERAL: std::println("signed literal {}", node.signed_val()); return 0;
  case UNSIGNED_LITERAL: std::println("unsigned literal: {}", node.unsigned_val()); return 0;
  case FLOAT_LITERAL: std::println("f32 literal: {}", node.float_val()); return 0;
  case DOUBLE_LITERAL: std::println("f64 literal: {}", node.double_val()); return 0;
  case BOOL_LITERAL: std::println("boolean literal: {}", node.bool_val()); return 0;
  case CHAR_LITERAL: std::println("character literal: {}", node.char_val()); return 0;
  case STRING_LITERAL: std::println("string literal: \"{}\"", node.string_val(file)); return 0;
  case ESCAPED_STRING_LITERAL: std::println("escaped string literal: \"{}\"", node.string_val(file)); return 0;

  default: Unreachable("Invalid ASTNode Type.");
  }
}

NoInlineCold void
printFunction(TU const& tu, u16_t fn_id) noexcept {
  std::print("{}", functionToString( tu.module.getFunction(fn_id)) );

  auto const& fn = tu.functions[fn_id];
  IndentationManager indents; indents.add(sz_max);
  for (auto const& node : fn.body) {
    indents.print();
    indents.add(printNode(node, tu.source_files[node.file_idx]));
  }

  std::print("\n}} ");
}

}}

NoInlineCold void
LOM::printParsed() noexcept {
  for (sz_t i{C_TU_IDX + 1}; i<getNumTUs(); ++i) {
    auto const& tu = getParserTU(i);
    std::println("\n--- Parser Output --- {}", tu.name);
    for (sz_t j{}; j<tu.functions.size(); ++j) {
      Parser::printFunction(tu, eden::narrow<u16_t>(j));
      std::println();
    }
    std::println("\n--- Parser Output ---");
  }
}


namespace LOM::PeepIR { namespace {

// returns number of sub expressions for indentation purposes
sz_t printInstruction(Instruction instruction, File file, std::string_view module_name) {
  switch (instruction.type) { using enum Instruction::InstructionType;
  case GLOBAL: std::println("Global {}", instruction.original_string(file)); return 0;
  case FUNCTION: std::println("Function {}", instruction.original_string(file)); return 0;

  case MODULE_GLOBAL: std::println("Global {} from module {}", instruction.module_variable_name(), instruction.module_name(file)); return 0;
  case MODULE_FUNCTION: {
    auto const& fn = getModule(instruction.module_member_data.module_id).getFunction(instruction.module_member_data.member_id);
    std::println("Function {} from module {}", fn.nameof(), module_name);
    return 0;
  }

  // this doesn't have to be one line but its really funny
  case TYPE_VARIABLE: std::println("Type Variable {}", getModule(instruction.type_member_data.custom_type_module_id).getCustomType( TypeID{ .derived = Type::CUSTOM, .module_id = instruction.type_member_data.custom_type_module_id, .id = instruction.type_member_data.custom_type_id } ).member_table().getVariable(instruction.type_member_data.member_id).nameof()); return 0;

  case LOCAL: std::println("Local {}", instruction.local_data.idx); return 0;

  case I8_LITERAL: std::println("i8 literal {}", instruction.signed_literal_data.value); return 0;
  case I16_LITERAL: std::println("i16 literal {}", instruction.signed_literal_data.value); return 0;
  case I32_LITERAL: std::println("i32 literal {}", instruction.signed_literal_data.value); return 0;
  case I64_LITERAL: std::println("i64 literal {}", instruction.signed_literal_data.value); return 0;
  case U8_LITERAL: std::println("u8 literal {}", instruction.unsigned_literal_data.value); return 0;
  case U16_LITERAL: std::println("u16 literal {}", instruction.unsigned_literal_data.value); return 0;
  case U32_LITERAL: std::println("u32 literal {}", instruction.unsigned_literal_data.value); return 0;
  case U64_LITERAL: std::println("u64 literal {}", instruction.unsigned_literal_data.value); return 0;
  case FLOAT_LITERAL: std::println("f32 literal {}", instruction.float_literal_data.value); return 0;
  case DOUBLE_LITERAL: std::println("f64 literal {}", instruction.double_literal_data.value); return 0;
  case BOOL_LITERAL: std::println("Boolean literal {}", instruction.bool_literal_data.value); return 0;
  case CHAR_LITERAL: std::println("Character literal {}", instruction.char_literal_data.value); return 0;
  case STRING_LITERAL: std::println("String literal {}", instruction.original_string(file)); return 0;
  case ESCAPED_STRING_LITERAL: std::println("Escaped string literal {}", instruction.original_string(file)); return 0;

  case ADD: std::println("Add"); return 2;
  case FADD: std::println("Add floats"); return 2;
  case SUB:  std::println("Subtract"); return 2;
  case FSUB:  std::println("Subtract floats"); return 2;
  case MULT:  std::println("Multiply"); return 2;
  case FMULT:  std::println("Multiply floats"); return 2;
  case UDIV:  std::println("Unsigned division"); return 2;
  case SDIV:  std::println("Signed division"); return 2;
  case FDIV:  std::println("Floating division"); return 2;
  case UMOD:  std::println("Unsigned modulo"); return 2;
  case SMOD:  std::println("Signed modulo"); return 2;
  case FMOD:  std::println("Floating modulo"); return 2;
  case ASSIGN:  std::println("Assign"); return 2;
  case UCAST_ASSIGN:  std::println("Assign but cast source to u{} first", instruction.cast_assign_data.bitwidth); return 2;
  case SCAST_ASSIGN:  std::println("Assign but cast source to i{} first", instruction.cast_assign_data.bitwidth); return 2;
  case ULESS:  std::println("Unsigned less than"); return 2;
  case SLESS:  std::println("Signed less than"); return 2;
  case FLESS:  std::println("Float less than"); return 2;
  case UGTR:  std::println("Unsigned greater than"); return 2;
  case SGTR:  std::println("Signed greater than"); return 2;
  case FGTR:  std::println("Float greater than"); return 2;
  case ULEQ:  std::println("Unsigned less than or equal"); return 2;
  case SLEQ:  std::println("Signed less than or equal"); return 2;
  case FLEQ:  std::println("Float less than or equal"); return 2;
  case UGEQ:  std::println("Unsigned greater than or equal"); return 2;
  case SGEQ:  std::println("Signed greater than or equal"); return 2;
  case FGEQ:  std::println("Float greater than or equal"); return 2;
  case EQ:  std::println("Equal"); return 2;
  case NEQ:  std::println("Not Equal"); return 2;
  case AND:  std::println("Logical and"); return 2;
  case OR:  std::println("Logical or"); return 2;
  case BITAND:  std::println("Bitwise and"); return 2;
  case BITOR:  std::println("Bitwise or"); return 2;
  case BITXOR:  std::println("Bitwise xor"); return 2;
  case SUBSCRIPT:  std::println("Subscript on array type {}", instruction.subscript_data.array_typeID.toString()); return 2;

  case PRE_INC: std::println("Prefix increment"); return 1;
  case FPRE_INC: std::println("Prefix increment float"); return 1;
  case PRE_DEC: std::println("Prefix decrement"); return 1;
  case FPRE_DEC: std::println("Prefix decrement float"); return 1;
  case ADDRESS_OF: std::println("Address of"); return 1;
  case NEGATE: std::println("Negate"); return 1;
  case FNEGATE: std::println("Negate float"); return 1;
  case BITNOT: std::println("Bitnot"); return 1;
  case POST_INC: std::println("Postfix increment"); return 1;
  case FPOST_INC: std::println("Postfix increment float"); return 1;
  case POST_DEC: std::println("Postfix decrement"); return 1;
  case FPOST_DEC: std::println("Postfix decrement float"); return 1;
  case DEREFERENCE: std::println("Dereference to get type {}", instruction.dereference_data.dereference_typeID.toString());  return 1;

  case UCAST: std::println("Cast to {} on unsigned", instruction.cast_data.destination_typeID.toString()); return 1;
  case SCAST: std::println("Cast to {} on signed", instruction.cast_data.destination_typeID.toString()); return 1;
  case FCAST: std::println("Cast to {} on float", instruction.cast_data.destination_typeID.toString()); return 1;
  case PCAST: std::println("Cast to {} on pointer", instruction.cast_data.destination_typeID.toString()); return 1;

  case CALL: std::println("Function call with {} arguments(s)", instruction.call_data.num_parameters); return instruction.call_data.num_parameters + 1;
  default:
    Unreachable("Invalid peep instruction.");
  }
}

void printBlocks(std::span<Block const> blocks, std::span<Instruction const> instructions, std::span<File const> files, std::string_view module_name) {
  auto const num_blocks = blocks.size();
  sz_t current_instruction_idx{};

  IndentationManager indents; indents.add(sz_max);
  for (sz_t i{}; i<num_blocks; ++i) {
    auto const& block = blocks[i];
    std::println("Block {}, {} instruction(s): ", i, block.num_instructions);
    for (sz_t j{}; j<block.num_instructions; ++j) {
      indents.print();
      auto const instruction = instructions[current_instruction_idx];
      indents.add(printInstruction(instruction, files[instruction.file_idx], module_name));
      ++current_instruction_idx;
    }

    switch (block.terminator_type) { using enum Block::Terminator;
    case BR:  std::println("  BR {}\n", block.br.next_block_offset + i); break;
    case BRC: std::println("  BRC TRUE {}, FALSE {}\n", block.brc.true_block_offset + i, block.brc.false_block_offset + i); break;
    case RET: std::println("  RET\n"); break;
    default: Unreachable("Invalid block terminator type.");
    }

  }

}

NoInlineCold void
printFunction(TU const& tu, u16_t fn_id) noexcept {
  std::print("{}", functionToString( tu.module.getFunction(fn_id)) );

  auto const& fn = tu.functions[fn_id];
  std::println("Block count: {}", fn.blocks.size());

  sz_t instruction_count{};
  for (auto block : fn.blocks) instruction_count += (sz_t) block.num_instructions;
  std::println("Instruction Count: {}", instruction_count);

  std::print("Locals: ");

  auto const& fn_symbol = tu.module.getFunction(fn_id);
  auto const num_locals = fn_symbol.locals.size();
  for (sz_t i{}; i<num_locals; ++i) {
    std::print("{}: {} | ", i, fn_symbol.locals[i].typeID.toString());
  }

  std::println();

  printBlocks(fn.blocks.to_span(), fn.instructions.to_span(), tu.source_files.to_span(), tu.name);
  std::println("}}");
}

}}

NoInlineCold void
LOM::printPeeped() noexcept {
  for (sz_t i{C_TU_IDX + 1}; i<getNumTUs(); ++i) {
    auto const& tu = getPeepTU(i);
    std::println("\n--- PeepIR --- {}", tu.name);
    for (sz_t j{}; j<tu.functions.size(); ++j)
      PeepIR::printFunction(tu, j);
    std::println("\n--- PeepIR ---");
  }
}

NoInlineCold void
LOM::printFileErrors(File file) noexcept {
  if (not doesFileHaveErrors(file)) return;

  std::println("\n--- Errors --- {}", file.path());
  std::println("{}", getFileErrors(file));
  std::println("\n--- Errors ---");
}
