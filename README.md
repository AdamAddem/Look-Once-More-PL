### Look Once More
LOM is an ahead-of-time compiled, statically typed, systems level language inspired by C++ and supported by
a LLVM backend; it is written as a passion project, and successfully compiles with a limited featureset.

### Language Priorities
My main goal is to create a language that mirrors the performance and freedom that C++ provides while ditching many of the legacy practices.
I want to prioritize improvements to the language over all else without being held back by tradition or long term backwards compatability.
Compilation speed is also a priority.

### Features of Look Once More (Subject to Change)
* Improved defaults, readonly/readwrite semantics
    ```
    ro_num: i32 = 5; # (const int equivalent)
    rw_num$ i32 = 4; # (int equivalent)
    
    uninitialized$  f32 = junk;   # explicit junk initialization required
    not_allowed:    f32;          # error
    ro_not_allowed: f32 = junk;   # error, readonly variable may not be junk initialized
  
    # All struct members are public
    # All members may be modified through Foo's methods if the instance is readwrite
    struct Foo {
        a: i32,
        b$ i32      # readwrite members may be modified outside the class through a readwrite instance of Foo.
    }

* Simple and explicit pointer / reference syntax
    ```
    # References are pointers
    x: raw i32 = ...;  # Raw keyword denotes a pointer to readwrite variable (int* const equivalent)
    y: ref i32 = ...;  # Ref keyword denotes a pointer to readonly variable  (const int* const equivalent)
    
    # Uniform dereference syntax
    raw_rect: raw Rectangle = @my_rect;   # GenZ address-of operator produces a pointer to readwrite variable (if possible)
    ref_rect: ref Rectangle = &my_rect;   # Ampersand operator produces a pointer to readonly variable
    raw_rect->length = 2;                 # Dereference to access member
    raw_rect-> = getSquare();             # Dereference to access object (*raw_rect equivalent)
* Native variant, tuple, and nullable types (planned)
    ```
    <string, u32> name_or_id = 5;
    <string, devoid> first_member = getFirstClubMember();    # Nullable type represented via 'devoid' keyword
    
    [string first, string last] person = ["Gabe", "Newell"]; # Tuple / Anonymous struct
* Strict and explicit global variables (planned)
    ```
    #{ 
       Global variables must be declared before functions and after imports.
       Global variables may not be junk initialized.
       Global variables must be initialized at compile time.
    }#

    global x$ i32 = junk;    # error
    global y$ f32 = 4.0f;
  
    foo() {...}
    
    global too_late: i32 = 5; # error
* Strong typing and simple promotion rules
    ```
    #{
       No implicit conversions,
       With the exception of unsigned conversions where the other type is of greater size.
    }#
  
    signed_8$ i8 = 0; unsigned_8$ u8 = 0; signed_32$ i32 = 0; unsigned_32$ u32 = 0;
    unsigned_32 = signed_8;   # error
    unsigned_32 = unsigned_8;
    signed_32 = unsigned_8;
    signed_8 = unsigned_8;    # error
  
    # Cast operator with prefix precedence
    unsigned_32 = cast<u32> signed_8;
    res: f32 = cast<f32> (signed_8 + 3);
* An actual module system (100% adoption rate)
    ```
    import whatever;
    pub bar() {...}

    foo() i32 { # visible only within current module
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
Either clang or gcc are required to support linking objects into an executable or compiling within the 'extern' folder. <br>

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
    
    --extern_flags:      Will pass all remaining flags to the compiler used for linking and compiling files within the 'extern' folder.
```
Visit [GettingStarted](GettingStarted.md) for more information on setting up a project.

---
### The Name
The name 'C++' is a play on 'C', implying that it is the increment of C. <br>
I've always thought it was funny that the postfix ++ operator was used rather than the prefix. <br>
Ironically this implies that when someone uses 'C++', all they're really getting is C, and the addition is an afterthought - an idea I find consistent with my experience of the language. <br>
So while C++ is really C again, I encourage us all to Look Once More. (badumtss)