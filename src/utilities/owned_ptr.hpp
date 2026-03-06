#pragma once


template <class T>
class owned_ptr {
  T* owned{nullptr};
public:
  constexpr owned_ptr() noexcept = default;
  constexpr explicit owned_ptr(T* mine_now) noexcept : owned(mine_now) {}
  constexpr owned_ptr(owned_ptr&& other) noexcept : owned(other.owned) {other.owned = nullptr;}

  [[nodiscard]] constexpr T* get() const noexcept { return owned; }
  [[nodiscard]] constexpr T* release() noexcept { const T* retval = owned; owned = nullptr; return retval; }
  constexpr void obliterate() noexcept { delete owned; owned = nullptr; }
  [[nodiscard]] constexpr T& operator*() const noexcept {return *owned;}
  [[nodiscard]] constexpr T* operator->() const noexcept {return owned;}
};