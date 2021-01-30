#pragma once

#include "variant_destroy_layer.h"

namespace variant_helper {
template <class TList, typename = void> struct copy_construct_layer;

template <class TList>
struct copy_construct_layer<TList, std::enable_if_t<TList::template all_of<std::is_copy_constructible> &&
                                                    !TList::template all_of<std::is_trivially_copy_constructible>>>
    : destroy_layer<TList> {
  using base_t = destroy_layer<TList>;
  using destroy_layer<TList>::destroy_layer;

  constexpr copy_construct_layer(const copy_construct_layer &other) { this->init_from(other); }
  constexpr copy_construct_layer(copy_construct_layer &&other) = default;
  constexpr copy_construct_layer &operator=(const copy_construct_layer &other) = default;
  constexpr copy_construct_layer &operator=(copy_construct_layer &&other) = default;
};

template <class TList>
struct copy_construct_layer<TList, std::enable_if_t<!TList::template all_of<std::is_copy_constructible> ||
                                                    TList::template all_of<std::is_trivially_copy_constructible>>>
    : destroy_layer<TList> {
  using base_t = destroy_layer<TList>;
  using destroy_layer<TList>::destroy_layer;

  constexpr copy_construct_layer(const copy_construct_layer &other) = default;
  constexpr copy_construct_layer(copy_construct_layer &&other) = default;
  constexpr copy_construct_layer &operator=(const copy_construct_layer &other) = default;
  constexpr copy_construct_layer &operator=(copy_construct_layer &&other) = default;
};

template <class TList, typename = void> struct move_construct_layer;

template <class TList>
struct move_construct_layer<TList, std::enable_if_t<TList::template all_of<std::is_move_constructible> &&
                                                    !TList::template all_of<std::is_trivially_move_constructible>>>
    : copy_construct_layer<TList> {
  using base_t = copy_construct_layer<TList>;
  using copy_construct_layer<TList>::copy_construct_layer;

  constexpr move_construct_layer(const move_construct_layer &other) = default;
  constexpr move_construct_layer(move_construct_layer &&other) noexcept(
      TList::template all_of<std::is_nothrow_move_constructible>) {
    this->init_from(std::move(other));
  }
  constexpr move_construct_layer &operator=(const move_construct_layer &other) = default;
  constexpr move_construct_layer &operator=(move_construct_layer &&other) = default;
};

template <class TList>
struct move_construct_layer<TList, std::enable_if_t<!TList::template all_of<std::is_move_constructible> ||
                                                    TList::template all_of<std::is_trivially_move_constructible>>>
    : copy_construct_layer<TList> {
  using base_t = copy_construct_layer<TList>;
  using copy_construct_layer<TList>::copy_construct_layer;

  constexpr move_construct_layer(const move_construct_layer &other) = default;
  constexpr move_construct_layer(move_construct_layer &&other) = default;
  constexpr move_construct_layer &operator=(const move_construct_layer &other) = default;
  constexpr move_construct_layer &operator=(move_construct_layer &&other) = default;
};

template <class TList, typename = void> struct copy_assign_layer;

template <class TList>
struct copy_assign_layer<TList, std::enable_if_t<!(TList::template all_of<std::is_trivially_copy_constructible> &&
                                                   TList::template all_of<std::is_trivially_copy_assignable> &&
                                                   TList::template all_of<std::is_trivially_destructible>) &&
                                                 TList::template all_of<std::is_copy_constructible> &&
                                                 TList::template all_of<std::is_copy_assignable>>>
    : move_construct_layer<TList> {
  using base_t = move_construct_layer<TList>;
  using move_construct_layer<TList>::move_construct_layer;

  constexpr copy_assign_layer(const copy_assign_layer &other) = default;
  constexpr copy_assign_layer(copy_assign_layer &&other) = default;
  constexpr copy_assign_layer &operator=(const copy_assign_layer &other) {
    this->init_from(other);
    return *this;
  }
  constexpr copy_assign_layer &operator=(copy_assign_layer &&other) = default;
};

template <class TList>
struct copy_assign_layer<TList, std::enable_if_t<(TList::template all_of<std::is_trivially_copy_constructible> &&
                                                  TList::template all_of<std::is_trivially_copy_assignable> &&
                                                  TList::template all_of<std::is_trivially_destructible>) ||
                                                 !(TList::template all_of<std::is_copy_constructible> &&
                                                   TList::template all_of<std::is_copy_assignable>)>>
    : move_construct_layer<TList> {
  using base_t = move_construct_layer<TList>;
  using move_construct_layer<TList>::move_construct_layer;

  constexpr copy_assign_layer(const copy_assign_layer &other) = default;
  constexpr copy_assign_layer(copy_assign_layer &&other) = default;
  constexpr copy_assign_layer &operator=(const copy_assign_layer &other) = default;
  constexpr copy_assign_layer &operator=(copy_assign_layer &&other) = default;
};

template <class TList, typename = void> struct move_assign_layer;

template <class TList>
struct move_assign_layer<TList, std::enable_if_t<TList::template all_of<std::is_move_assignable> &&
                                                 TList::template all_of<std::is_move_constructible> &&
                                                 !TList::template all_of<std::is_trivially_copyable>>>
    : copy_assign_layer<TList> {
  using base_t = copy_assign_layer<TList>;
  using copy_assign_layer<TList>::copy_assign_layer;

  constexpr move_assign_layer(const move_assign_layer &other) = default;
  constexpr move_assign_layer(move_assign_layer &&other) = default;
  constexpr move_assign_layer &operator=(const move_assign_layer &other) = default;
  constexpr move_assign_layer &
  operator=(move_assign_layer &&other) noexcept(TList::template all_of<std::is_nothrow_move_constructible> &&
                                                TList::template all_of<std::is_nothrow_move_assignable>) {
    this->init_from(std::move(other));
    return *this;
  }
};

template <class TList>
struct move_assign_layer<TList, std::enable_if_t<TList::template all_of<std::is_trivially_copyable> ||
                                                 !TList::template all_of<std::is_move_assignable> ||
                                                 !TList::template all_of<std::is_move_constructible>>>
    : copy_assign_layer<TList> {
  using base_t = copy_assign_layer<TList>;
  using copy_assign_layer<TList>::copy_assign_layer;

  constexpr move_assign_layer(const move_assign_layer &other) = default;
  constexpr move_assign_layer(move_assign_layer &&other) = default;
  constexpr move_assign_layer &operator=(const move_assign_layer &other) = default;
  constexpr move_assign_layer &operator=(move_assign_layer &&other) = default;
};

template <class TList, typename = void> struct default_construct_layer;

template <class TList>
struct default_construct_layer<TList, std::enable_if_t<std::is_default_constructible_v<typename TList::head_t>>>
    : move_assign_layer<TList> {
  using base_t = move_assign_layer<TList>;
  using move_assign_layer<TList>::move_assign_layer;

  constexpr default_construct_layer() noexcept(std::is_nothrow_default_constructible_v<typename TList::head_t>)
      : base_t(in_place_index<0>) {}
};

template <class TList>
struct default_construct_layer<TList, std::enable_if_t<!std::is_default_constructible_v<typename TList::head_t>>>
    : move_assign_layer<TList> {
  using base_t = move_assign_layer<TList>;
  using move_assign_layer<TList>::move_assign_layer;

  default_construct_layer() = delete;
};
} // namespace variant_helper
