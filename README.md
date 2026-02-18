# WIP

## Look Once More
LOM is an ahead-of-time compiled, statically typed systems level language inspired by C++ and supported by
a LLVM backend; It is written as a passion project, and is currently in the AST validation phase of development.

### Problems with C++
I have a love / hate relationship with C++, although I am not special in that regard. Most who work with it have plenty of
complaints about the language. It has many quirks and features that I think make it beautiful; 
however, the beauty comes from the various colors of duct tape that has been used to keep it working.

Its overemphasis on backwards compatability leads to a mixed bag of features with an age gap larger than 
Leonardo DiCaprio and whoever his girlfriend is right now. In addition, C++ chooses to implement many of its features
through the standard library, rather than integrating them as language features. For example, the 'type trait' std::is_same_v
is a compile time constant that simply states whether two types are the same. It, and essentialy all other type traits, are implemented
by means of defining structs that exploit the template specialization system. In my opinion, if your language requires inheritance 
and template meta programming to tell if two types are the same, something is wrong. Granted, this is being addressed in C++26, but 
this is likely yet another colorful strand of duct tape that might not be widely adopted for another decade.<br>

Some of the main problems with C++ LOM intends to address are:
* Obscure / Esoteric syntax
  ```c++ 
    int (Foo::*)(int (&)[5]) ptr; 
    requires requires { typename T::foo; };
    noexcept(noexcept(a.~T()))

* Standard library features that should be features of the language
  * unique_ptr
  * Many type-traits and concepts
  * variant
  * move
  * tuple
  * string (arguably)

* Operators and Keywords with varying meanings in varying contexts
  * static and inline are probably the best examples
  * \* used for multiplication, pointer declaration, and pointer dereference
  * new, operator new, placement new

* Pointer and Reference semantics are very unintuitive
    ```c++
    int * const * x; //mutable pointer to const pointer to mutable int
    int& foo() { static int* ptr = new int; return *ptr; } //must dereference to create a reference

* Bad defaults and too many implicit features
  * noexcept, const, and explicit are everywhere and not the default
  * Implicit junk initialization if you don't specify a value when declaring a trivial type (int x;)
  * Compiler will generate move construction / assignment for you that **just copies** raw pointers

There are **many** more that I've excluded for the sake of brevity, partially because most languages already fix these issues 
and/or C++ itself is moving away from them.

### Features of Look Once More (Subject to Change)
Enough complaining, here is what I want to bring to the table:

* Improved defaults
    ```
    i32 x = 5; //const by default  
    mut i32 y = 4;
    
    f32 z = junk; //explicit junk initialization required
    f32 w; //error
  
* Improved pointer syntax, with multiple special pointer types
    ```
    //Pointer declarations are read left to right
    raw -> i32 x = null; //Raw pointer to an int
    unique -> i32 y = null; //Pointer to an int w/ compile time enforcement for ownership and destruction
    vague -> z = null; //void* equivalent
    mut raw -> raw -> mut i32 x = null; //mutable pointer to const pointer to mutable int
  
* Native variant and nullable types
    ```
    <string, u32> name_or_id = 5; // May hold a string or int value
    <string, devoid> first_member = getFirstClubMember(); //Nullable type can be represented with 'devoid'

* Strict and explicit global variables
    ```
    // Global variables must be defined at top of file
    // Global variables cannot be initialized using anything outside of the file
    // Global variables must be defined in a global initialization body

    i32 x = global;
    f32 y = global;
    globals{
        x = 2;
        y = 4.f;
    }
  
    //no global variables allowed to be declared after global body

* Scoped imports without flooding namespace
    ```
    from standard: vector as vec;
    from my_lib: all;
  
    fn foo() {
        from other_lib: only_used_here;
    }
  
* 'steal' semantics
    ```
    fn foo(Resource param) {...}
  
    fn bar() {
        Resource x = ...;
        foo(steal x);
        x.doThing() //error: x no longer exists within this scope
    }
* Miscellaneous
  * Changed void to devoid, which is a pedantic change, but I think it is a better word
  * Any functions can be called with the . operator on the first parameter
    * Ex: 
      ```
      fn doThing(Resource arg) {...} 
      fn main() -> i32 { Resource x; x.doThing(); }
  * Mutable variables may become const at some point within the same scope, syntax remains undecided
  * Native tuples
  
There exist more features, but these are the ones i'm mostly certain on. 
