#include <cctype>
#include <fstream>
#include <string>
#include <unordered_map>
#include <vector>

struct Entry {
  Entry(std::string &&enum_, std::string &&string)
      : enum_representation(std::move(enum_)),
        string_representation(std::move(string)) {}

  std::string enum_representation;
  std::string string_representation;
};

struct Category {
  Category(std::string &&n, unsigned b, unsigned e)
      : name(std::move(n)), begin(b), end(e) {}
  std::string name;
  unsigned begin;
  unsigned end;
};

static std::ifstream input_file;
static std::ofstream output_file;
static std::vector<Entry> entries;
static std::vector<Category> parsed_categories;
static std::unordered_map<std::string, unsigned> categories_being_parsed;

static std::string enum_name;
static std::string requested_namespace;

static void parseCommand(const std::string &command, unsigned current_index) {
  const char c = command.at(1);
  if (c == 'F') [[unlikely]]
    return;

  static std::string next;
  input_file >> next;
  if (c == 'B') {
    if (categories_being_parsed.contains(next)) {
      const std::string error = "duplicate category name: " + next;
      throw std::runtime_error(error);
    }

    categories_being_parsed.emplace(next, current_index);
    return;
  }

  if (c == 'E') {
    unsigned begin = categories_being_parsed.at(next);
    categories_being_parsed.erase(next);
    parsed_categories.emplace_back(std::move(next), begin, current_index);
    return;
  }
}

static void parseLoop() {

  unsigned num_enums{};
  while (!input_file.eof()) {
    static std::string next;
    input_file >> next;
    if (next.front() == '!') {
      parseCommand(next, num_enums);
      continue;
    }

    ++num_enums;

    static std::string string_rep;
    input_file >> string_rep;

    if (string_rep.front() == '-') {
      string_rep.erase(0, 1);
    }

    entries.emplace_back(std::move(next), std::move(string_rep));
  }

  if (!categories_being_parsed.empty())
    throw std::runtime_error("Unfinished category");
}

static void printDefinition() {
  output_file << "#pragma once\n"
              << "#include <unordered_map>\n"
              << "#include <string>\n"
              << "#include <utility>\n\n";

  output_file << "namespace " << requested_namespace << " {\n";

  output_file << "enum class " << enum_name << " : unsigned"
              << " {\n";

  for (const auto &entry : entries)
    output_file << "  " << entry.enum_representation << ",\n";

  output_file << "};\n\n\n";
}

static void printMapping() {
  output_file << "inline const std::unordered_map<std::string_view, " << enum_name
              << "> stringTo" << enum_name << "{\n";

  static constexpr unsigned num_per_row{3};
  unsigned i{};
  for (const auto &entry : entries) {
    bool newline = (i % num_per_row == 0);

    if (newline)
      output_file << "\n\t";

    ++i;

    output_file << "{\"" << entry.string_representation << "\", " << enum_name
                << "::" << entry.enum_representation << "}, ";
  }

  output_file << "\n};\n\n\n";
}

static void printArray() {
  std::string enumFirstLowercase = enum_name;
  enumFirstLowercase.front() = std::tolower(enumFirstLowercase.front());

  output_file << "constexpr const char* " << enumFirstLowercase
              << "ToString(const " << enum_name << " e) {\n"
              << "constexpr const char* " << "toString[] = {\n";

  static constexpr unsigned num_per_row{3};
  unsigned i{};
  for (const auto &entry : entries) {
    bool newline = (i % num_per_row == 0);

    if (newline)
      output_file << "\n\t";

    ++i;

    output_file << "\"" << entry.string_representation << "\",";
  }

  output_file << "\n};\n\treturn toString[std::to_underlying(e)];\n}\n";
}

static void printHelpers() {

  for (auto &category : parsed_categories) {
    output_file << "constexpr bool isCategory" << category.name << "(const "
                << enum_name << " e) { return ";

    output_file << "std::to_underlying(e) >= " << std::to_string(category.begin)
                << " && "
                << "std::to_underlying(e) < " << std::to_string(category.end)
                << "; }\n\n";
  }
}

int main(int argc, char *argv[]) {
  if (argc < 2)
    throw std::runtime_error("Path to the input file not specified");

  entries.reserve(30);
  input_file.open(argv[1]);
  if (!input_file.is_open())
    throw std::runtime_error("File not found");

  input_file >> std::ws;
  input_file >> enum_name;
  input_file >> requested_namespace;

  parseLoop();
  input_file.close();

  if (argc == 3) {
    output_file.open(argv[2]);
  } else {
    std::string output_file_name(enum_name);
    output_file_name.append(".hpp");
    output_file.open(output_file_name);
  }

  if (!output_file.is_open())
    throw std::runtime_error("Error creating enum generation file");

  printDefinition();
  printMapping();
  printArray();
  printHelpers();

  output_file << "}; //namespace " << requested_namespace;

  return 0;
}
