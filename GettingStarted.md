### Getting Started
Run `LookOnceMore init` to set up a project template within the current directory. <br>
You should see src, external, and build folders. <br>
The src folder holds all source files organized via module. <br>
The external folder is not implemented currently. <br>
The build folder contains the outputs of all 'emit' commands. <br>

To build your project, just do `LookOnceMore build` within the project directory.

### Modules
LookOnceMore uses a module system. <br>
A module contains one or more .lom files, which are all compiled together into one translation unit. <br>
To create a module, simply create a directory within 'src'. The module's name is taken directly from the directory's name. <br>
All files within that directory will be compiled as part of the module. <br>

In the top level source directory, it is required that there be one file named 'main.lom'. <br>
Only one .lom file may exist in the top level src directory. <br>
In the future main.lom will not be required, this is a temporary measure. <br>

Within a module, all non-public globals and functions are available only to other source files within the module. <br>
To publish a global or function, place the keyword 'pub' before 'fn' or after 'global'. <br>
To import a module, just type 'import <module_name>' before any globals have been declared. <br>
Access the members of a module by appending the module name with a dot. <br>

### 

### C Interop
Calling C functions can be done using the '__C' intrinsic. <br>
You must first declare the function before usage. Place '__C' before the function name, then provide a forward declaration (Note: the 'fn' keyword is not needed). <br>
Ensure that the type of the C function has been appropriately translated to LOM. <br>
Ex: `__C puts(raw -> char str) -> i32;` <br>
This will place the function as part of the internal '__C' module, and as such only one declaration can be present throughout the entire program. <br>
If variadic arguments are needed, use the '__va' intrinsic as the last type. <br>
Ex: `__C printf(raw -> char fmt, __va) -> i32;` <br>
Calling these functions can be done as if through the '__C' module, which is implicitly imported in every module by default. <br>

### Current Limitations
The following is a list of all missing features that are soon to be implemented.
- No form of (explicit) casting.
- No pointer arithmetic.
- No arrays.
- No string type.
- No working variants or tuples.
- No standard library.
- No user defined structures.
- No nothing.