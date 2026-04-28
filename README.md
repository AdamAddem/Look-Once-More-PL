## Look Once More
LOM is an ahead-of-time compiled, statically typed, systems level language inspired by C++ and supported by
a LLVM backend; It is written as a passion project, and successfully compiles with a limited featureset.

My main goal is to create a language that mirrors the performance and freedom that C++ provides while ditching many of the legacy practices.
I want to prioritize improvements to the language over all else without being held back by tradition or long term backwards compatability.
Below are language features, only some of which are implemented currently, that intend to improve upon C++.

### Features of Look Once More (Subject to Change)

* Improved defaults
    ```
    i32 const_num = 5;  
    mut i32 mutable_num = 4;
    
    mut f32 not_initialized = junk;   // explicit junk initialization required
    mut f32 not_allowed;              // error
    f32 const_not_allowed = junk;     // error
  
* Simple, intuitive pointer syntax
    ```
    // Pointer declarations are simply read left to right
    raw -> i32 x = null;              // Raw pointer to an integer
    vague -> mut z = null;            // Pointer to anything mutable (void* equivalent)
    
    // Uniform dereference syntax
    raw -> mut Rectangle p;
    p->length = 2;                    // Dereference to access member
    p-> = getSquare();                // Dereference to access object (*p equivakent)
  
* Native variant, tuple, and nullable types
    ```
    <string, u32> name_or_id = 5;
    <string, devoid> first_member = getFirstClubMember();    // Nullable type represented 'devoid' keyword
    
    [string first, string last] person = ["Gabe", "Newell"]; // Tuple / Anonymous struct
  
* Strict and explicit global variables
    ```
    // Global variables must be declared before functions and after imports.
    // Global variables may not be junk initialized.
    // Global variables must be initialized at compile time.

    global mut i32 x = junk;    // error
    global mut f32 y = 4.0f;
  
    fn foo() {...}  
    
    global i32 too_late = 5;    // error
  
* Steal semantics
    ```
    fn foo(Resource param) {...}
  
    fn bar() {
        Resource x = ...;
        foo(steal x);
        x.doThing() //error: x no longer usable
    }
  
* Strong typing and simple promotion rules
    ```
    // No implicit conversions, with the exception of unsigned -> signed conversions where the signed type is of greater size
  
    mut i8 signed_8 = 0; mut u8 unsigned_8 = 0; mut i32 signed_32 = 0; mut u32 unsigned_32 = 0;
    unsigned_32 = signed_8;   // error
    unsigned_32 = unsigned_8;
    signed_32 = unsigned_8;
    signed_8 = unsigned_8;    // error
  
    // In math expressions, types are promoted to the leftmost type if such conversion is legal
    signed_32 / unsigned_8;   // signed 32bit division
    unsigned_32 + unsigned_8; 
    unsigned_8 + unsigned_32; // error

* An actual module system (100% adoption rate)
  ```
  import whatever;
  
  pub fn bar() {...}

  fn foo() -> i32 {
    return whatever.getNum() + 2;
  }

---
### How to Build
Currently the only dependencies are LLVM 21 and my own library which is included as a submodule. <br>
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
    -init                       Creates a project template.
    -build                      Builds the project.
    -o <output>                 Specifies output file name.
    -validate                   Prints whether the files are legal LOM programs.
    -O0, O1, O2, O3             Does absolutely nothing for now.
    
    -emit-obj                   Produces object files.
    -emit-llvm                  Produces the LLVM IR representation of the source code.
    -emit-asm                   Produces the assembly representation of the source code.
```
Visit [GettingStarted](GettingStarted.md) for more information on setting up a project.

---
### The Name
The name 'C++' is a play on 'C', implying that it is the increment of C. <br>
I've always thought it was funny that the postfix ++ operator was used rather than the prefix. <br>
Ironically this implies that when someone uses 'C++', all they're really getting is C, and the addition is an afterthought - an idea I find consistent with my experience of the language. <br>
So while C++ is really C again, I encourage us all to Look Once More. (badumtss)