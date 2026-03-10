#pragma once

template <class T>
class owned_ptr {
  T* owned{nullptr};
public:
  constexpr owned_ptr() noexcept = default;
  constexpr owned_ptr(T* mine_now) noexcept : owned(mine_now) {}

  owned_ptr(const owned_ptr&) = delete;
  owned_ptr& operator=(const owned_ptr&) = delete;

  constexpr owned_ptr(owned_ptr&& other) noexcept : owned(other.owned) { other.owned = nullptr; }
  constexpr owned_ptr& operator=(owned_ptr&& other) noexcept { owned = other.owned; other.owned = nullptr; return *this; }

  [[nodiscard]] constexpr T* get() const noexcept { return owned; }
  [[nodiscard]] constexpr T* release() noexcept { const T* retval = owned; owned = nullptr; return retval; }
  constexpr void destroy() noexcept { delete owned; owned = nullptr; }
  [[nodiscard]] constexpr T& operator*() const noexcept {return *owned;}
  [[nodiscard]] constexpr T* operator->() const noexcept {return owned;}

  constexpr bool operator==(const decltype(nullptr)) const noexcept { return owned == nullptr; }
  constexpr operator bool() const noexcept { return owned != nullptr; }
};

template <class T>
using nullable_owned_ptr = owned_ptr<T>;
