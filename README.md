### Look Once More
LOM is an ahead-of-time compiled, statically typed, systems level language inspired by C++ and supported by
a LLVM backend; it is written as a passion project, and successfully compiles with a limited featureset.

### Language Priorities
My main goal is to create a language that mirrors the performance and freedom that C++ provides while ditching many of the legacy practices.
I want to prioritize improvements to the language over all else without being held back by tradition or long term backwards compatability.
Compilation speed is also a priority.

### Features of Look Once More (Subject to Change)
* Improved defaults
    ```
    i32 const_num = 5;  
    $i32 mutable_num = 4;
    
    $f32 uninitialized = junk;     // explicit junk initialization required
    $f32 not_allowed;              // error
    f32 const_not_allowed = junk;  // error
* Simple, intuitive pointer syntax
    ```
    // Pointer declarations are simply read left to right
    raw i32 x = null;              // Raw pointer to an integer
    vague $ z = null;              // Pointer to anything mutable (void* equivalent)
    
    // Uniform dereference syntax
    raw $Rectangle p = @my_rect;   // GenZ address-of operator (don't @ me bro)
    p->length = 2;                 // Dereference to access member
    p-> = getSquare();             // Dereference to access object (*p equivalent)
* Native variant, tuple, and nullable types (planned)
    ```
    <string, u32> name_or_id = 5;
    <string, devoid> first_member = getFirstClubMember();    // Nullable type represented via 'devoid' keyword
    
    [string first, string last] person = ["Gabe", "Newell"]; // Tuple / Anonymous struct
* Strict and explicit global variables (planned)
    ```
    // Global variables must be declared before functions and after imports.
    // Global variables may not be junk initialized.
    // Global variables must be initialized at compile time.

    global $i32 x = junk;    // error
    global $f32 y = 4.0f;
  
    fn foo() {...}  
    
    global i32 too_late = 5;    // error
* Strong typing and simple promotion rules
    ```
    // No implicit conversions, with the exception of unsigned to signed conversions where the signed type is of greater size
    $i8 signed_8 = 0; $u8 unsigned_8 = 0; $i32 signed_32 = 0; $u32 unsigned_32 = 0;
    unsigned_32 = signed_8;   // error
    unsigned_32 = unsigned_8;
    signed_32 = unsigned_8;
    signed_8 = unsigned_8;    // error
  
    // Cast operator with prefix precedence
    unsigned_32 = cast<u32> signed_8;
    f32 res = cast<f32> (signed_8 + 3);
* An actual module system (100% adoption rate)
  ```
  import whatever;
  pub fn bar() {...}

  fn foo() i32 { // visible only within current module
    return whatever.getNum() + 2;
  }
---
### How to Build
Currently the only dependencies are LLVM 22 and my own library which is included as a submodule. <br>
Clone and compile as such:
```
git clone --recurse-submodules https://github.com/AdamAddem/Look-Once-More-PL
mkdir build && cd build && cmake .. && make
```
Either clang or gcc are required to support linking objects into an executable. <br> 
Windows support hasn't been tested but feel free to try. <br>

### Running
The executable can be ran with the following arguments:
```
    -init                Creates a project template.
    -build               Builds, compiles, and links the project.
    -o <output>          Specifies output file name.
    -O0, O1, O2, O3      Sets optimization levels (No effect currently).
    
    -emit-obj            Produces object files.
    -emit-llvm           Produces the LLVM IR representation of the source code.
    -emit-asm            Produces the assembly representation of the source code.
    
    Prints a textual representation of the respective stage to cout (Don't expect it to be pretty!).
    -emit-<lexer, parser, peep> 
```
Visit [GettingStarted](GettingStarted.md) for more information on setting up a project.

---
### The Name
The name 'C++' is a play on 'C', implying that it is the increment of C. <br>
I've always thought it was funny that the postfix ++ operator was used rather than the prefix. <br>
Ironically this implies that when someone uses 'C++', all they're really getting is C, and the addition is an afterthought - an idea I find consistent with my experience of the language. <br>
So while C++ is really C again, I encourage us all to Look Once More. (badumtss)