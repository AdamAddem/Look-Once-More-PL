### Getting Started
Run `lom init` to set up a project template within the current directory. <br>
You should see src, extern, and build folders. <br>
The src folder holds all source files organized via module. <br>
The extern folder holds all external c files you'd like to be compiled with LOM. <br>
The build folder contains the outputs of all 'emit' commands, alongside the object files output by the compiler. <br>

To build your project, run `lom build` within the project directory. <br>
Building requires an external compiler (only clang or gcc currently) available via the command line. <br>
Top level .c files within the 'extern' folder will automatically be compiled and linked alongside the project. <br>
To specify flags for use with the external compiler pass '--extern_flags:' as an argument. All remaining arguments will be passed along. <br>
If not using clang or gcc, you can output object files via --emit-obj and manually link them, although no files within 'extern' will be compiled :(. <br>
* Note that the external compiler is invoked from within the build/obj folder to make it place the object files in the correct location. <br>

### Modules
LookOnceMore uses a folder-based module system. <br>
A module contains one or more .lom files, which are all compiled into one translation unit. <br>
To create a module, simply create a directory within 'src'. The module's name is the directory's name. <br>
All .lom files within that directory will be compiled as part of the module. <br>
No individual module may exceed 256 files. <br>

In the top level source directory, it is required that there be one file named 'main.lom'. <br>
Only main.lom may exist in the top level src directory. <br>
This is a temporary requirement while I establish a more formal build system. <br>

Within a module, all non-public functions are available only to other source files within the module. <br>
To publish a function, place the keyword 'pub' before its declaration. <br>
All modules are 'imported' by default. <br>
Access the members of a module by appending the module name with a '::'. <br>

### Error messages
Error messages are currently stage-based. <br>
Any errors during parsing will prevent all other modules from entering the validation stage. <br>
Likewise, any errors during validation will prevent all other modules from being lowered. <br>

### C Interop
Calling C functions can be done using the '__C' keyword. <br>
You must first declare the function before usage. Place '__C' before the function name, then provide a forward declaration. <br>
Ex: `__C puts(str: ref u8) i32;` <br>
This will place the function as part of the internal '__C' module, and as such only one declaration can be present throughout the entire program. <br>
If variadic arguments are needed, use the '__va' keyword as the last parameter. <br>
Ex: `__C printf(fmt: ref u8, __va) i32;` <br>
C Interop using structs or arrays passed by value is incorrect won't function as expected. Pass by pointer instead. <br>

### Missing Features and Known Bugs
- Globals are unsupported until a constant evaluator is made to enforce constant initialization.
- _= operators do not exist yet (+=, -=, etc).
- Strings are accessable only through ref u8.
- No support for creating or importing precompiled libraries directly.
- Errors have some synchronization problems;
- No pointer arithmetic.
- No working variants or tuples.
- No standard library.
- No nothing.

### Code Limitations
- Modules may not have greater than 256 files.
- Modules / Structs may not have greater than u16_max globals/member variables.
- Modules may not have greater than u16_max functions.
- Files may not exceed u32_max in size.
- Tokens may not exceed the u16_max in length.
- Expressions have a reasonably large size limit, do not nest too hard.
- Violating any of these rules may or may not cause a crash, assertion failure, or logical error rather than a standard error message.
- These limits exist to increase compilation speed and reduce memory usage.
