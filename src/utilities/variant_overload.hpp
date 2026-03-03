#pragma once

template<class... Ts>
struct overload : Ts... {
    using Ts::operator()...;
};

template<class... Ts>
overload(Ts...) -> overload<Ts...>;



// Provide the type for the variant overload, the function to be called, and any additional arguments to that function
#define utils_callon(type, func, ...) [&] (type obj) constexpr { return func(obj __VA_OPT__(,) __VA_ARGS__);}

/*  Example:
 *
 *  void useInt(int num) {...}
 *  void useFloat(float num) {...}
 *  void useString(const string& str, bool setting) {...}
 *
 *  std::variant<int, float, string> my_variant = ...;
 *  utils_match(my_variant,
 *              utils_callon(int, useInt),
 *              utils_callon(float, useFloat),
 *              utils_callon(const string&, useString, true)
 *              );
*/
#define utils_match(variant, ...) std::visit(overload{__VA_ARGS__}, variant)

