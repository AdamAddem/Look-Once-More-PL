import sys

class Category:
    name: str
    begin: int
    end: int
    def __init__(self, name: str, begin: int, end: int):
        self.name = name
        self.begin = begin
        self.end = end
    def __repr__(self):
        return f'{self.name}: ({self.begin}, {self.end})'

class Entry:
    enum_rep: str
    string_rep: str
    def __init__(self, enum_rep: str, string_rep: str):
        self.enum_rep = enum_rep
        self.string_rep = string_rep
    def __repr__(self):
        return f'{self.enum_rep}: {self.string_rep}'

def file_output(lines, out_filename, enum_name, namespace, underlying_type):
    categories: list[Category] = []
    entries: list[Entry] = []
    active_categories: dict[str, Category] = {}

    for raw_line in lines:
        strip_line = raw_line.strip()
        if len(strip_line) == 0:
            continue

        first, second = strip_line.split()
        if first == "!BEGIN":
            if active_categories.__contains__(second):
                print(f'Active category {second} duplicated')
                exit(1)

            new_category = Category(second, len(entries), 0)
            active_categories[second] = new_category
            categories.append(new_category)
        elif first == "!END":
            if not active_categories.__contains__(second):
                print(f'Ended category {second} not active')
                exit(1)

            active_categories[second].end = len(entries)
            active_categories.pop(second)
        else:
            entries.append( Entry(first, second.removeprefix('!')) )

    for unended_category in active_categories.values():
        unended_category.end = len(entries)

    out = open(out_filename, "w")
    out.write("#pragma once\n")
    out.write("#include <string_view>\n")
    out.write("#include <unordered_map>\n")
    out.write("#include <utility>\n\n")
    out.write(f'namespace {namespace} {{\n')

    out.write(f'enum class {enum_name} : {underlying_type} {{\n')
    for entry in entries:
        out.write(f'\t{entry.enum_rep},\n')
    out.write("};\n")

    out.write(f'inline const std::unordered_map<std::string_view, {enum_name}> stringTo{enum_name}{{\n\t')
    i: int = 0
    for entry in entries:
        out.write(f'{{"{entry.string_rep}", {enum_name}::{entry.enum_rep}}}, ')
        if i == 2:
            out.write("\n\t")
            i = 0
        i += 1
    out.write("\n};\n\n")

    i = 0
    out.write(f'constexpr std::string_view {enum_name}ToString({enum_name} e) {{\n\t')
    out.write("static constexpr std::string_view toString[] = {\n\t")
    for entry in entries:
        out.write(f'"{entry.string_rep}",')
        if i == 2:
            out.write("\n\t")
            i = 0
        i += 1
    out.write("\n};\n\treturn toString[std::to_underlying(e)];\n}\n")

    for category in categories:
        out.write(f'constexpr bool isCategory{category.name}({enum_name} e) {{ return std::to_underlying(e) >= {category.begin} && std::to_underlying(e) < {category.end}; }}\n')
    out.write("\n")

    for category in categories:
        out.write(f'#define {enum_name.upper()}_{category.name}_CASES ')
        for i in range(category.begin, category.end):
            out.write(f'case {entries[i].enum_rep}: ')
        out.write("\n")

    out.write(f"\n}}; //namespace {namespace}")
    out.close()
    pass

def file_generate(filename):
    file = open(filename)
    lines = list(file)
    file.close()

    if len(lines) == 0:
        print(f'Empty file: {filename}')
        exit(1)

    header = lines[0].strip()
    if len(header) == 0:
        print("First line must contain file name, enum name, namespace, and underlying type.")
        exit(1)

    fn, en, ns, un = header.split()
    file_output(lines[1:], fn, en, ns, un)
    pass

arg_len = len(sys.argv)
if arg_len == 1:
    print("Provide filename(s) as argument(s).")
    exit(1)

filenames = sys.argv[1:]

for filename in filenames:
    file_generate(filename)
