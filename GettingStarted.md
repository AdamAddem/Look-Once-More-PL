### Getting Started
Run `lom init` to set up a project template within the current directory. <br>
You should see src, and build folders. <br>
The src folder holds all source files organized via module. <br>
The build folder contains the outputs of all 'emit' commands. <br>

To build your project, run `lom build` within the project directory.

### Modules
LookOnceMore uses a folder-based module system. <br>
A module contains one or more .lom files, which are all compiled together into one translation unit. <br>
To create a module, simply create a directory within 'src'. The module's name is the directory's name. <br>
All .lom files within that directory will be compiled as part of the module. <br>
Any directory prefixed with '.' will not be considered. <br>
No individual module may exceed 256 files. <br>

In the top level source directory, it is required that there be one file named 'main.lom'. <br>
Only main.lom may exist in the top level src directory. <br>
This is a temporary requirement while I establish a more formal build system. <br>

Within a module, all non-public globals and functions are available only to other source files within the module. <br>
To publish a function, place the keyword 'pub' before 'fn'. <br>
To import a module, just type 'import <module_name>;'. <br>
Access the members of a module by appending the module name with a dot. <br>

### Error messages
Error messages are currently stage-based

### C Interop
Calling C functions can be done using the '__C' keyword. <br>
You must first declare the function before usage. Place '__C' before the function name, then provide a forward declaration (note, the 'fn' keyword is not needed). <br>
Ensure that the type of the C function has been appropriately translated to LOM. <br>
Ex: `__C puts(raw char str) i32;` <br>
This will place the function as part of the internal '__C' module, and as such only one declaration can be present throughout the entire program. <br>
If variadic arguments are needed, use the '__va' keyword as the last parameter. <br>
Ex: `__C printf(raw char fmt, __va) i32;` <br>
Calling these functions can be done as if through the '__C' module, which is imported by default. <br>
C Interop using structs is untested and probably won't function as expected. I'm working on it. <br>
The C standard library (+ math) is linked to by default. Linking to anything else requires outputting object files and doing so manually. <br>

### Current Limitations, Missing Features, and Known Bugs
- Functions may not have greater than 8 parameters, structs may not have greater than 256 members, modules may have no more than 256 files, and no token may exceed the u16 integer limit in length.
  - Violating any of these rules will cause a crash, assertion failure, or logical error rather than a standard error message.
  - These limits exist to increase compilation speed.
- Globals are unsupported until a constant evaluator is made to enforce constant initialization.
- Private functions (and public functions within main.lom) with the same name as one declared with the __C keyword will be all sorts of messed up. Avoid for now.
- _= operators do not exist (+=, -=, etc).
- String literals are accessable only through raw pointer to immutable char.
- No support for creating or importing precompiled LOM libraries directly.
- Import names currently shadow local variabels when accessing members. (local variable named foo cannot access any members if a module named foo has been imported).
- Parsing errors don't sync very well. One error will likely cause many many more.
- No pointer arithmetic.
- No arrays.
- No working variants or tuples.
- No standard library.
- No nothing.