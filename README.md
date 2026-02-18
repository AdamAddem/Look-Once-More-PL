# WIP

## Look Once More
LOM is an ahead-of-time compiled, statically typed systems level language inspired by C++ and supported by
a LLVM backend; It is written as a passion project, and has just passed the AST validation phase of development.

### Motivation
I have a love / hate relationship with C++, and I am certainly not special in that regard. Most who work with it have plenty of
complaints about the language. Although it has many quirks and features that I think make it beautiful,
much of that beauty comes from the various colors of duct tape that have been layered through the years to keep it functional.

C++'s overemphasis on backwards compatability has led to a mixed bag of features with a massive age gap. 
The lack of will to change/deprecate what has already been added to the language leads to the creation of many new features with the sole purpose of improving the old.
At best, a new feature does improve the language, but ends up coexisting with old features anyways because it did not truly change the way things are done
(Ex: Concepts are great but are essentially just a convenience feature that dooesn't actually change anything; Most uses of concepts still require type_traits).
At worst a new feature will release just to end up being more inconvenient / wordy / restrictive than the old version.

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
  * string (arguably)

* Operators and Keywords with varying meanings in varying contexts
  * \* used for multiplication, pointer declaration, and pointer dereference
  * new, operator new, placement new
  * 'static' and 'inline' could have their own README dedicated solely to this subject

* Pointers and references can be unintuitive 
    ```c++
    int * const * x; //mutable pointer to const pointer to mutable int
    int& foo() { static int* ptr = new int; return *ptr; } //must dereference to create a reference (does not actually dereference)
    std::move(obj) //doesn't actually do anything beyond casting
* Bad defaults and too many implicit features
  * noexcept, const, constexpr, \[\[nodiscard\]\], and explicit are everywhere in modern codebases leading to massive walls of text that just declare a single function
  * Implicit junk initialization if you don't specify a value when declaring a trivial type (int x;)
  * Compiler will generate move construction / assignment for you that **just copies** raw pointers, very dangerous
* Header/source files and build systems
  * Modules exist, yes, but if a feature is released in a standard and noone is around to use it, did it really make an impact? 

### Why not use X language instead?
As much as i've dragged C++, I really do love it. The amount of freedom it provides to the user is essentially unparalleled by anything but C and ASM.
Do you want to interpret the bits of an integer pointer as a Car? Go ahead. Matter of fact, declare that the fourth bit of that car represents whether the windshield wipers are activated, who cares.
While you're at it, use multiple layers of function-like macros to declare an enum and create mappings between that enum and its string equivalent. 
C++ won't give you an easy (or pretty) way of doing it, but if you really want to then thy will be done.
Many languages have been created in an attempt to replace or augment C++, although from what I've seen none have quite replicated the same philosophy of freedom.

Rust is likely the most popular C++ 'replacement'. When I first started developing Look Once More, I had essentially no knowledge of Rust beyond a basic understanding of the borrow checker.
Coincidentally many of the ideas I had early on for LOM shared a striking resemblance to many of Rusts features, despite the fact that I had never used the language.
Now having dug much deeper, there are many things I can admire. However, its core philosophy is almost the exact opposite of C++'s in many ways. It forces you into a particular
style of programming in order to even function. Granted, this style is well justified and has many benefits, but it is simply not enjoyable to me.

I'm sure there may exist other low-level languages that I'd enjoy programming in, although I'm not quite sure that they'd ever scratch the same itch.
With Look Once More, i'm making sure it does.

### Features of Look Once More (Subject to Change)
Here are some features of LOM, some unique to the language, and some generally accepted as best practice:

* Improved defaults
    ```
    i32 const_num = 5;  
    mut i32 mutable_num = 4;
    
    f32 not_initialized = junk; //explicit junk initialization required
    f32 not_allowed; //error
* Improved pointer syntax, with multiple special pointer types
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
    //Global variables must be defined at top of file
    //Global variables cannot be initialized using anything outside of the file
    //Global variables must be defined in a global initialization body (syntax still iffy)

    mut i32 x = global;
    mut f32 y = global;
    globals{
        x = 2;
        y = 4.f;
    }
  
    //No global variables allowed to be declared after global initialization body
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
  * Changed void to devoid, which is a very pedantic but I think it better describes something without type or value
  * Any function can be called with the . operator on the first parameter
    * Ex: 
      ```
      fn doThing(Resource arg) {...} 
      fn main() -> i32 { Resource x; x.doThing(); }
  * Mutable variables may become const at some point within the same scope, syntax remains undecided
  * Native tuples, strings, etc
  
There exist many more features, but these are the ones that are significant/distinct enough to mention.

## The Name
'C++; is a play on 'C'. Ironically, since the ++ is used as a postfix operator, this implies that when someone uses C++ all they're really getting is C,
and the addition is an afterthought. This interpretation is likely not how the author intended, but I do think its consistent with the language as a whole. 

So while C++ is really C again, I encourage us all to Look Once More (badumtss).

The name is not just a pun. It represents the goal of the language: to not let things of the past keep their relevance purely for the purposes of comfort, 'backwards compatability', or because 'is the way that it is'.
We should actively revisit the old with new eyes and determine whether they hold up to modern standards.
