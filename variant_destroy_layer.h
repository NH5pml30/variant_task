#pragma once

#include <climits>

#include "variant_storage.h"

namespace variant_helper {
template <class TList, class TSuffix = TList, size_t I = 0>
struct select_type : select_type<TList, typename TSuffix::tail_t, I + 1> {
  using type = typename TSuffix::head_t;
  static constexpr size_t index = I;
  using select_type<TList, typename TSuffix::tail_t, I + 1>::get;

  template <typename T, typename TListArgs, typename = void> struct is_list_initializable : std::false_type {};

  template <typename T, typename... TArgs>
  struct is_list_initializable<T, type_list<TArgs...>, std::void_t<decltype(T{std::declval<TArgs>()...})>>
      : std::true_type {};

  template <typename T, typename... TArgs>
  static constexpr bool is_list_initializable_v = is_list_initializable<T, type_list<TArgs...>, void>::value;

  template <typename T>
  static constexpr bool is_valid = is_list_initializable_v<type[], T &&> &&
                                   (std::is_same_v<std::remove_cv_t<std::remove_reference_t<type>>, bool>
                                        ? std::is_same_v<std::remove_cv_t<std::remove_reference_t<T>>, bool>
                                        : true);

  template <typename T, typename U = TList> // dummy template parameter to apply SFINAE
  static constexpr std::enable_if_t<U::template has_unique_type<type> && is_valid<T>, select_type> get(type) noexcept;

  template <class T> using result = decltype(select_type::get<T>(std::declval<T>()));

  template <class T> using result_t = typename result<T>::type;

  template <class T> static constexpr size_t result_v = result<T>::index;
};

template <class TList, size_t I> struct select_type<TList, type_list<>, I> {
  template <typename T>
  static constexpr select_type get(...) noexcept {}
};

struct optional_index {
  size_t index;
  static constexpr size_t NO_VALUE = variant_npos;

  constexpr explicit optional_index(size_t index = NO_VALUE) noexcept : index(index) {}
  constexpr bool has_value() const noexcept { return index != NO_VALUE; }
};

template <class TList, typename = void> struct destroy_layer_helper;

template <class TList>
struct destroy_layer_helper<TList, std::enable_if_t<!std::is_trivially_destructible_v<storage_t<TList>>>>
    : storage_t<TList>, optional_index {
  using base_t = storage_t<TList>;

  constexpr destroy_layer_helper() = default;

  template <
      size_t I, class... Args,
      typename = std::enable_if_t<I <= TList::size - 1>, // <= size -1 for MSVC compatibility
      typename = std::enable_if_t<std::is_constructible_v<argument_pack_at_t<I, TList>, Args...>>>
  constexpr explicit destroy_layer_helper(in_place_index_t<I>, Args &&... args)
      noexcept(noexcept(base_t(in_place_index<I>, std::forward<Args>(args)...)))
      : base_t(in_place_index<I>, std::forward<Args>(args)...), optional_index(I) {}

  template <class T, class... Args,
            typename = std::enable_if_t<TList::template has_unique_type<T> && std::is_constructible_v<T, Args...>>>
  constexpr explicit destroy_layer_helper(in_place_type_t<T>, Args &&... args)
      noexcept(noexcept(base_t(in_place_type<T>, std::forward<Args>(args)...)))
      : base_t(in_place_type<T>, std::forward<Args>(args)...), optional_index(TList::template unique_type_index<T>) {}

  void reset() noexcept {
    if (has_value()) {
      base_t::reset(index);
      index = NO_VALUE;
    }
  }

  ~destroy_layer_helper() { reset(); }
};

template <class TList>
struct destroy_layer_helper<TList, std::enable_if_t<std::is_trivially_destructible_v<storage_t<TList>>>>
    : storage_t<TList>, optional_index {
  using base_t = storage_t<TList>;

  constexpr destroy_layer_helper() = default;

  template <
      std::size_t I, class... Args,
      typename = std::enable_if_t<I <= TList::size - 1>, // <= size -1 for MSVC compatibility
      typename = std::enable_if_t<std::is_constructible_v<argument_pack_at_t<I, TList>, Args...>>>
  constexpr explicit destroy_layer_helper(in_place_index_t<I>, Args &&... args)
      noexcept(noexcept(base_t(in_place_index<I>, std::forward<Args>(args)...)))
      : base_t(in_place_index<I>, std::forward<Args>(args)...), optional_index(I) {}

  template <class T, class... Args,
            typename = std::enable_if_t<TList::template has_unique_type<T> && std::is_constructible_v<T, Args...>>>
  constexpr explicit destroy_layer_helper(in_place_type_t<T>, Args &&... args)
      noexcept(noexcept(base_t(in_place_type<T>, std::forward<Args>(args)...)))
      : base_t(in_place_type<T>, std::forward<Args>(args)...), optional_index(TList::template unique_type_index<T>) {}

  void reset() noexcept { index = NO_VALUE; }

  ~destroy_layer_helper() = default;
};

template <class TList> struct destroy_layer : destroy_layer_helper<TList> {
  using base_t = destroy_layer_helper<TList>;
  using destroy_layer_helper<TList>::destroy_layer_helper;

  template <class DestroyLayer>
  void init_from(DestroyLayer &&other) noexcept(
      noexcept(std::declval<base_t>().init_from(0, false, std::forward<DestroyLayer>(other)))) {
    if (other.has_value()) {
      bool assign = base_t::index == other.index;
      if (!assign)
        this->reset();
      base_t::init_from(other.index, assign, std::forward<DestroyLayer>(other));
    }
    base_t::index = other.index;
  }

  template <size_t I, class... Args>
  std::enable_if_t<std::is_constructible_v<argument_pack_at_t<I, TList>, Args...>, argument_pack_at_t<I, TList> &>
  emplace(Args &&... args) {
    this->reset();
    auto &res = base_t::template emplace<I>(std::forward<Args>(args)...);
    base_t::index = I;
    return res;
  }

  template <class T, class... Args>
  std::enable_if_t<std::is_constructible_v<T, Args...> && TList::template has_unique_type<T>, T &>
  emplace(Args &&... args) {
    return emplace<TList::template unique_type_index<T>>(std::forward<Args>(args)...);
  }

  template <size_t I> constexpr argument_pack_at_t<I, TList> &get() {
    if (!base_t::has_value() || base_t::index != I)
      throw bad_variant_access();
    return base_t::template get<I>();
  }

  template <size_t I> constexpr const argument_pack_at_t<I, TList> &get() const {
    return const_cast<destroy_layer &>(*this).get<I>();
  }
};
} // namespace variant_helper
