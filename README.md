## Look Once More
LOM is an ahead-of-time compiled, statically typed, multi-paradigm systems level language inspired by C++ and supported by
a LLVM backend; It is written as a passion project, and successfully compiles with a very (very) limited featureset.

My main goal is to create a language that mirrors the performance and freedom that C++ provides while ditching many of the legacy practices.
I want to prioritize improvements to the language over all else without being held back by tradition or long term backwards compatability.
Below are language features, only some of which are implemented currently, that intend to improve upon C++.

### Features of Look Once More (Subject to Change)

* Improved defaults
    ```
    i32 const_num = 5;  
    mut i32 mutable_num = 4;
    
    mut f32 not_initialized = junk; //explicit junk initialization required
    mut f32 not_allowed; //error
    f32 const_not_allowed = junk; //error
* Improved pointer syntax, with multiple specialized pointer types
    ```
    //Pointer declarations are simply read left to right
    
    raw -> i32 x = null;                   //Raw pointer to an integer
    unique -> i32 y = null;                //Pointer to an integer w/ compile time enforcement for ownership and destruction
    vague -> mut z = null;                 //Pointer to anything mutable (void* equivalent)
    mut raw -> raw -> mut i32 x = null;    //Mutable pointer to const pointer to mutable int
* Native variant and nullable types
    ```
    <string, u32> name_or_id = 5;                           //May hold a string or unsigned value
    <string, devoid> first_member = getFirstClubMember();   //Nullable type can be represented with 'devoid' keyword
* Strict and explicit global variables
    ```
    //Global variables must be defined before any functions
    //Global variables cannot be initialized using any values outside the Translation Unit

    global mut i32 x = junk;
    global mut f32 y = 4.0f;
  
    fn foo() {...}  
    
    global i32 not_allowed = 5; //error

* Scoped imports
    ```
    from standard: vector as vec;
    from my_lib; //import all
  
    fn foo() {
        from other_lib: only_used_here;
    }
* 'steal' semantics
    ```
    fn foo(Resource param) {...}
  
    fn bar() {
        Resource x = ...;
        foo(steal x);
        x.doThing() //error: x no longer usable
    }
* Miscellaneous
    * Adopted devoid instead of void to better describe something without type or value
    * Any function can be called with the . operator on the first parameter
        * Ex:
          ```
          fn doThing(Resource arg) {...} 
          fn main() -> i32 { Resource x; x.doThing(); }
    * Mutable variables may become const at some point within the same scope, syntax remains undecided
    * Native tuples, strings, etc
---
### Compiling
Currently the only dependencies are LLVM version 20.1.8. Compile as such:
```
mkdir build && cd build && cmake .. && make
```
Either clang or gcc are required to support linking objects into an executable. <br> 
If you're using MSVC or any other compiler, use the -emit-obj flags to prevent the linking stage, which you can then start yourself.
Then, stop using MSVC or any other compiler. Thanks.

### Running
The executable can be ran with the following arguments:
```
    <filenames>.lom             Specifies filenames for compilation
    -o <output>                 Specifies output file name
    -build-location <location>  Specifies the folder for all outputs
    -emit-lexer                 Prints the result of the lexer and exits
    -emit-parser                Prints the result of the parser and exits
    -validate                   Prints whether the files are legal LOM programs and exits
    
    //May be used in conjunction but prevents linking
    -emit-obj                   Produces object files
    -emit-llvm                  Compiles to LLVM IR
    -emit-asm                   Compiles to assembly
```
As of right now, the module system is not implemented, and forward declarations are not a thing, so compiling multiple files is useless as they can't interact.
Actually, barely anything is implemented, so don't bother doing any of this. Come back in a month or so when it'll be somewhat useable.
---

### Disclaimer
Below is a large amount of what is colloquially referred to as 'yap'. You probably have better things to do, but
if you'd like to hear me hate on C++, feel free to read ahead. Otherwise, the TL;DR is that making a programming language is fun and C++ sucks.

### Motivation
I have a love / hate relationship with C++, and I am certainly not special in that regard. Although it has many quirks and features that I think make it beautiful,
much of that beauty comes from how clever the language is about working around itself rather than the quality of the feature itself.

C++'s overemphasis on backwards compatability has led to a mixed bag of features with a massive age gap. 
The extreme hesitancy to change/deprecate/remove what has already been added to the language leads to the creation of many new features with the sole purpose of improving the old. <br>

At best, a new feature does improve the language, but ends up coexisting with old features anyways because it never truly changed the way things were done. 
For example, concepts are one of the best recent C++ features in my opinion. However, they don't fundamentally change metaprogramming as most uses of concepts still require type traits.
At worst, a new feature will release just to end up being more inconvenient / wordy / restrictive than the old version, albiet with some very situational benefits.

Below are some major examples of the problems that C++ has accrued over the years that are a result of its 'less than progressive' development: 

* Obscure / Esoteric syntax
  ```c++ 
    int (Foo::*)(int (&)[5]) ptr; 
    requires requires { typename T::foo; };
    noexcept(noexcept(a.~T()))
* Standard library features that should be features of the language
  * unique_ptr
  * many type-traits and concepts
  * variant
  * move
  * tuple

* Operators and Keywords with varying meanings in varying contexts
  * \* used for multiplication, pointer declaration, and pointer dereference
  * new, operator new, placement new
  * 'static' and 'inline' could have their own markdown dedicated solely to this subject

* Pointers and references can be unintuitive 
    ```c++
    int * const * x; //mutable pointer to const pointer to mutable int
    int& foo() { static int* ptr = new int; return *ptr; } //dereference to create a reference but does not actually dereference
    std::move(obj) //doesn't actually do anything beyond casting
* Bad defaults and too many implicit features
  * noexcept, const, constexpr, \[\[nodiscard\]\], and explicit are everywhere in modern codebases leading to long lines of text that just declare a single function
  * Implicit junk initialization if you don't specify a value when declaring a trivially constructible type (int x;)
  * Compiler can generate move construction / assignment for you that shallow copies raw pointers, dangerous
* Header/source files and build systems
  * Modules are a thing, yes, but if a feature is released in a standard and noone is around to use it, does it really exist?

### Why not use X language instead?
As much as i've dragged C++, I really do love it. The amount of freedom it provides to the user is essentially unparalleled by anything but C and ASM.
Do you want to interpret the bits of an integer pointer as a Car? Go ahead. Matter of fact, declare that the fourth bit of that car represents whether the windshield wipers are activated, who cares.
While you're at it, use multiple layers of function-like macros to declare an enum and create mappings between that enum and its string equivalent. 
C++ won't give you an easy (or pretty) way of doing it, but if you really want to then thy will be done.
Many languages have been created in an attempt to replace or augment C++, although from what I've seen none have quite replicated the same philosophy of freedom.

Rust is likely the most popular C++ 'replacement'. When I first started developing Look Once More, I had essentially no knowledge of Rust beyond a basic understanding of the borrow checker.
Coincidentally many of the ideas I had early on for LOM shared a striking resemblance to many of Rusts features, despite the fact that I had never used the language.
Now having dug much deeper, there are many things I can admire about the language. However, its core philosophy is almost the exact opposite of C++'s in many ways; it forces you into a particular
style of programming in order to even compile. Granted, this style is well justified and has many benefits, but it is simply not enjoyable to me.

I'm sure there may exist other low-level languages that I'd enjoy programming in, although I'm not quite sure that they'd ever scratch the same itch.
With LOM, I can make sure it does.

### The Name
The name 'C++' is a play on 'C'. Funnily enough, since the ++ is used as a postfix operator, this implies that when someone uses C++ all they're really getting is C — and
the addition is an afterthought. While obviously not what the author intended, I do think it is consistent with my experience of the language as a whole.
So while C++ is really C again, I encourage us all to Look Once More (badumtss).

The name is not just a pun though. It represents the goal of the language: to not let things of the past keep their relevance purely for the purposes of comfort, 'backwards compatability', or because 'is the way that it is'.
We should actively revisit the old with new eyes and determine whether they hold up to modern standards.
