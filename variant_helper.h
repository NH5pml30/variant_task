#pragma once

#include <cstdint>
#include <exception>
#include <type_traits>

template <class... Types> class variant;

template <class T> struct variant_size;

template <class... Types> struct variant_size<variant<Types...>> : std::integral_constant<size_t, sizeof...(Types)> {};

template <class T> struct variant_size<const T> : variant_size<T>::type {};

template <class T> inline constexpr size_t variant_size_v = variant_size<T>::value;

namespace variant_helper {
template <class From, class To> struct copy_refs {
private:
  using obj_from_t = std::remove_reference_t<From>;
  using c_type = std::conditional_t<std::is_const_v<obj_from_t>, const To, To>;
  using cv_type = std::conditional_t<std::is_volatile_v<obj_from_t>, volatile c_type, c_type>;
  using cv_ref_type = std::conditional_t<std::is_lvalue_reference_v<From>, cv_type &,
                                         std::conditional_t<std::is_rvalue_reference_v<From>, cv_type &&, cv_type>>;

public:
  using type = cv_ref_type;
};

template <class From, class To> using copy_refs_t = typename copy_refs<From, To>::type;

template <typename... Types> struct type_list {
  static constexpr size_t size = 0;

  template <typename T> static constexpr bool has_unique_type = false;

  template <typename T, size_t Len> static constexpr size_t unique_type_index = Len;

  template <template <typename> typename Checker> static constexpr bool all_of = true;

  template <class TFunc, class CopyRefsFrom> static constexpr bool is_nothrow = true;
};

template <typename THead, typename... TTail> struct type_list<THead, TTail...> {
  using head_t = THead;
  using tail_t = type_list<TTail...>;

  static constexpr size_t size = sizeof...(TTail) + 1;

  template <typename T>
  static constexpr bool has_unique_type = tail_t::template has_unique_type<T> ^ std::is_same_v<head_t, T>;

  template <typename T, size_t Len = size>
  static constexpr size_t unique_type_index =
      std::is_same_v<head_t, T> ? Len - size : tail_t::template unique_type_index<T, Len>;

  template <template <typename> typename Checker>
  static constexpr bool all_of = Checker<head_t>::value &&tail_t::template all_of<Checker>;

  template <class TFunc, class CopyRefsFrom>
  static constexpr bool is_nothrow = std::is_nothrow_invocable_v<TFunc, copy_refs_t<CopyRefsFrom, head_t>>
      && tail_t::template is_nothrow<TFunc, CopyRefsFrom>;
};

template <size_t Ind, class TList> struct argument_pack_at : argument_pack_at<Ind - 1, typename TList::tail_t> {};

template <class TList> struct argument_pack_at<0, TList> { using type = typename TList::head_t; };

template <size_t Ind, class TList> using argument_pack_at_t = typename argument_pack_at<Ind, TList>::type;

template <class T, class RefT>
constexpr inline bool is_same_to_ref_v = std::is_same_v<T, std::remove_cv_t<std::remove_reference_t<RefT>>>;

template <class T, class RefT, class R = void> struct forw_enable_if : std::enable_if<is_same_to_ref_v<T, RefT>, R> {};

template <class T, class RefT, class R = void> using forw_enable_if_t = typename forw_enable_if<T, RefT, R>::type;

template <class T> struct is_variant : std::false_type {};
template <class... Types> struct is_variant<variant<Types...>> : std::true_type {};
template <class... Types> struct is_variant<const variant<Types...>> : std::true_type {};
template <class T> inline constexpr bool is_variant_v = is_variant<T>::value;

template <size_t index, class THead, class... TTail>
constexpr auto &&get_by_index(THead &&head, TTail &&... tail) noexcept {
  if constexpr (index == 0)
    return head;
  else
    return get_by_index<index - 1>(std::forward<TTail>(tail)...);
}
} // namespace variant_helper

template <size_t I, class T> struct variant_alternative;

template <std::size_t I, class... Types> struct variant_alternative<I, variant<Types...>> {
  static_assert(I < sizeof...(Types));

  using type = variant_helper::argument_pack_at_t<I, variant_helper::type_list<Types...>>;
};

template <std::size_t I, class T> struct variant_alternative<I, const T> {
  using type = const typename variant_alternative<I, T>::type;
};

template <size_t I, class T> using variant_alternative_t = typename variant_alternative<I, T>::type;

struct bad_variant_access : std::exception {
public:
  bad_variant_access() = default;

  const char *what() const noexcept override { return "bad variant access"; }
};

struct in_place_t {};
inline constexpr in_place_t in_place{};

template <class T> struct in_place_type_t { using type = T; };

template <class T> inline constexpr in_place_type_t<T> in_place_type{};

template <size_t I> struct in_place_index_t {
  constexpr operator size_t() const noexcept { return I; }
};

template <size_t I> inline constexpr in_place_index_t<I> in_place_index{};

inline constexpr size_t variant_npos = -1;

namespace variant_helper {
template <class T> struct is_in_place_index : std::false_type {};
template <size_t I> struct is_in_place_index<in_place_index_t<I>> : std::true_type {};
template <size_t I> struct is_in_place_index<const in_place_index_t<I>> : std::true_type {};
template <class T> inline constexpr bool is_in_place_index_v = is_in_place_index<T>::value;

template <class T> struct is_in_place_type : std::false_type {};
template <class T> struct is_in_place_type<in_place_type_t<T>> : std::true_type {};
template <class T> struct is_in_place_type<const in_place_type_t<T>> : std::true_type {};
template <class T> inline constexpr bool is_in_place_type_v = is_in_place_type<T>::value;
} // namespace variant_helper
