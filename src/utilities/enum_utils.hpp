#pragma once
#include <type_traits>
#include <utility>

template<class T>
concept EnumType = std::is_enum_v<T>;

struct Exclusive { Exclusive() = delete; };
struct Inclusive { Inclusive() = delete; };

template <class T>
concept ExclusivityFlag = std::is_same_v<T, Exclusive> || std::is_same_v<T, Inclusive>;

template <EnumType T, ExclusivityFlag lower = Inclusive, ExclusivityFlag higher = Inclusive>
[[nodiscard]] constexpr bool
enumBetween(T value, T lower_bound, T higher_bound) {
  return [&]() constexpr {
    if constexpr (std::is_same_v<lower, Exclusive>)
      return std::to_underlying(value) > std::to_underlying(lower_bound);
    else
      return std::to_underlying(value) >= std::to_underlying(lower_bound);
  }()
  &&
  [&]() constexpr {
    if constexpr (std::is_same_v<higher, Exclusive>)
      return std::to_underlying(value) < std::to_underlying(higher_bound);
    else
      return std::to_underlying(value) <= std::to_underlying(higher_bound);
  }();
}