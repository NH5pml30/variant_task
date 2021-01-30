#pragma once

#include <optional>

#include "variant_construct_assign_layer.h"

template <class T, class BaseT>
std::enable_if_t<std::is_base_of_v<std::remove_reference_t<BaseT>, T> &&
                     variant_helper::is_variant_v<std::remove_reference_t<T>>,
                 variant_helper::copy_refs_t<T, BaseT> &&>
forward_base_(std::remove_reference_t<T> &v) noexcept;

template <size_t Index, class Variant>
constexpr variant_helper::copy_refs_t<Variant &&, variant_alternative_t<Index, std::remove_reference_t<Variant>>>
get(Variant &&v);

template <class R, class Visitor, class... Variants> constexpr R visit(Visitor &&vis, Variants &&... vars);

template <class... Types>
class variant : private variant_helper::default_construct_layer<variant_helper::type_list<Types...>> {
private:
  using TList = variant_helper::type_list<Types...>;
  using base_t = variant_helper::default_construct_layer<TList>;

public:
  using variant_helper::default_construct_layer<TList>::default_construct_layer;

  constexpr variant(const variant &) = default;
  constexpr variant(variant &&) = default;
  constexpr variant &operator=(variant &&other) = default;
  constexpr variant &operator=(const variant &other) = default;

  template <class T,
            typename = std::enable_if_t<!variant_helper::is_variant_v<std::decay_t<T>> &&
                                        !variant_helper::is_in_place_index_v<std::decay_t<T>> &&
                                        !variant_helper::is_in_place_type_v<std::decay_t<T>>>,
            typename T_j_ = typename variant_helper::select_type<TList>::template result<T>,
            typename T_j = typename T_j_::type,
            typename = std::enable_if_t<std::is_constructible_v<T_j, T>>>
  constexpr variant(T &&t) noexcept(std::is_nothrow_constructible_v<T_j, T>)
      : base_t(in_place_index<variant_helper::select_type<TList>::template result_v<T>>, std::forward<T>(t)) {}

  template <class T, typename = std::enable_if_t<!std::is_same_v<std::decay_t<T>, variant>>,
            typename T_j_ = typename variant_helper::select_type<TList>::template result<T>,
            typename T_j = typename T_j_::type, size_t j = T_j_::index,
            typename = std::enable_if_t<std::is_assignable_v<T_j &, T> && std::is_constructible_v<T_j, T>>>
  constexpr variant &
  operator=(T &&t) noexcept(std::is_nothrow_assignable_v<T_j &, T> &&std::is_nothrow_constructible_v<T_j, T>) {
    bool assigned = false;
    if (base_t::has_value()) {
      assigned = ::visit<bool>([&](auto &el) {
        if constexpr (std::is_same_v<std::remove_reference_t<decltype(el)>, T_j>) {
          el = std::forward<T>(t);
          return true;
        }
        return false;
      }, *this);
    }
    if (!assigned) {
      if constexpr (std::is_nothrow_constructible_v<T_j, T> || !std::is_nothrow_move_constructible_v<T_j>)
        this->template emplace<j>(std::forward<T>(t));
      else
        this->operator=(variant(std::forward<T>(t)));
    }
    return *this;
  }

  constexpr size_t index() const noexcept { return base_t::index; }

  constexpr bool valueless_by_exception() const noexcept { return !base_t::has_value(); }

  using base_t::emplace;

  ~variant() = default;

private:
  template <class T, class BaseT>
  friend constexpr std::enable_if_t<std::is_base_of_v<std::remove_reference_t<BaseT>, variant> &&
                                        variant_helper::is_same_to_ref_v<variant, T>,
                                    variant_helper::copy_refs_t<T, BaseT> &&>
  forward_base_(std::remove_reference_t<T> &v) noexcept {
    return static_cast<variant_helper::copy_refs_t<T, BaseT> &&>(v);
  }

  template <class R, class Visitor, class Variant>
  constexpr static variant_helper::forw_enable_if_t<variant, Variant, R> visit(Visitor &&vis, Variant &&var) {
    if (var.valueless_by_exception())
      throw bad_variant_access();
    return var.base_t::template visit<R>(std::forward<Visitor>(vis), var.index(),
                                         forward_base_<Variant &&, base_t>(var));
  }

  template <class T> friend constexpr bool holds_alternative(const variant &v) noexcept {
    static_assert(TList::template has_unique_type<T>);
    if (v.valueless_by_exception())
      return false;
    return v.visit<bool>(
        [&](auto &val) constexpr noexcept { return std::is_same_v<T, std::remove_reference_t<decltype(val)>>; },
        const_cast<variant &>(v));
  }

  template <size_t Index> friend constexpr variant_alternative_t<Index, variant> &get(variant &v) {
    return v.template get<Index>();
  }
  template <size_t Index> friend constexpr const variant_alternative_t<Index, variant> &get(const variant &v) {
    return v.template get<Index>();
  }
  template <size_t Index> friend constexpr variant_alternative_t<Index, variant> &&get(variant &&v) {
    return std::move(v.template get<Index>());
  }
  template <size_t Index> friend constexpr const variant_alternative_t<Index, variant> &&get(const variant &&v) {
    return std::move(v.template get<Index>());
  }

  template <class T> friend constexpr T &get(variant &v) {
    static_assert(TList::template has_unique_type<T>);
    return v.template get<TList::template unique_type_index<T>>();
  }
  template <class T> friend constexpr const T &get(const variant &v) {
    static_assert(TList::template has_unique_type<T>);
    return v.template get<TList::template unique_type_index<T>>();
  }
  template <class T> friend constexpr T &&get(variant &&v) { return std::move(get<T>(v)); }
  template <class T> friend constexpr const T &&get(const variant &&v) { return std::move(get<T>(v)); }

  template <class R, size_t index, class Visitor, class VariantHead, class... VariantsAndArgs>
  friend constexpr R visit_(Visitor &&vis, VariantHead &&var, VariantsAndArgs &&... vars_and_args);

public:
  void swap(variant &rhs) noexcept(((std::is_nothrow_move_constructible_v<Types> &&
                                     std::is_nothrow_swappable_v<Types>)&&...)) {
    if (index() == rhs.index()) {
      if (!valueless_by_exception())
        visit<void>(
            [&rhs](auto &left) {
              visit<void>(
                  [&left](auto &right) {
                    if constexpr (std::is_same_v<decltype(left), decltype(right)>) {
                      using std::swap;
                      swap(left, right);
                    }
                  },
                  rhs);
            },
            *this);
    } else {
      std::swap(*this, rhs);
    }
  }
};

template <class T, class... Types> constexpr bool holds_alternative(const variant<Types...> &v) noexcept;

template <class R, size_t index, class Visitor, class VariantHead, class... VariantsAndArgs>
constexpr R visit_(Visitor &&vis, VariantHead &&var, VariantsAndArgs &&... vars_and_args) {
  if constexpr (index == sizeof...(VariantsAndArgs) + 1) {
    return vis(std::forward<VariantHead>(var), std::forward<VariantsAndArgs>(vars_and_args)...);
  } else {
    return var.template visit<R>(
        [&](auto &&held) constexpr {
          return visit_<R, index + 1>(std::forward<Visitor>(vis), std::forward<VariantsAndArgs>(vars_and_args)...,
                                      std::forward<decltype(held)>(held));
        },
        std::forward<VariantHead>(var));
  }
}

template <class R, class Visitor, class... Variants> constexpr R visit(Visitor &&vis, Variants &&... vars) {
  return visit_<R, 0>(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

template <class Visitor, class... Variants>
constexpr auto visit(Visitor &&vis, Variants &&... vars) -> decltype(vis(get<0>(vars)...)) {
  return visit<decltype(vis(get<0>(vars)...))>(std::forward<Visitor>(vis), std::forward<Variants>(vars)...);
}

template <size_t I, class... Types>
constexpr std::add_pointer_t<variant_alternative_t<I, variant<Types...>>> get_if(variant<Types...> *pv) noexcept {
  return pv == nullptr || pv->index() != I ? nullptr : &get<I>(*pv);
}
template <std::size_t I, class... Types>
constexpr std::add_pointer_t<const variant_alternative_t<I, variant<Types...>>>
get_if(const variant<Types...> *pv) noexcept {
  return pv == nullptr || pv->index() != I ? nullptr : &get<I>(*pv);
}
template <class T, class... Types> constexpr std::add_pointer_t<T> get_if(variant<Types...> *pv) noexcept {
  return pv == nullptr || !holds_alternative<T>(*pv) ? nullptr : &get<T>(*pv);
}
template <class T, class... Types> constexpr std::add_pointer_t<const T> get_if(const variant<Types...> *pv) noexcept {
  return pv == nullptr || !holds_alternative<T>(*pv) ? nullptr : &get<T>(*pv);
}

namespace variant_helper {
enum class cmp_res { less, equal, greater, equal_ex };

template <class... Types> constexpr cmp_res cmp_index(const variant<Types...> &v, const variant<Types...> &w) {
  if (v.index() < w.index())
    return cmp_res::less;
  else if (v.index() > w.index())
    return cmp_res::greater;
  else // v.index() == w.index()
    return v.valueless_by_exception() ? cmp_res::equal : cmp_res::equal_ex;
}

template <class Base> struct cmp_functor : Base {
  template <class LeftT, class RightT>
  constexpr bool operator()(LeftT &&left, RightT &&right) {
    if constexpr (std::is_same_v<LeftT, RightT>)
      return Base::operator()(std::forward<LeftT>(left), std::forward<RightT>(right));
    else
      return false; // dummy
  }
};

template <class... Types> constexpr bool operator==(const variant<Types...> &v, const variant<Types...> &w) {
  cmp_res res = cmp_index(v, w);
  return res == cmp_res::equal || (res == cmp_res::equal_ex && ::visit(cmp_functor<std::equal_to<>>(), v, w));
}

template <class... Types> constexpr bool operator!=(const variant<Types...> &v, const variant<Types...> &w) {
  cmp_res res = cmp_index(v, w);
  return res != cmp_res::equal && (res != cmp_res::equal_ex || ::visit(cmp_functor<std::not_equal_to<>>(), v, w));
}

template <class... Types> constexpr bool operator<(const variant<Types...> &v, const variant<Types...> &w) {
  cmp_res res = cmp_index(v, w);
  return res == cmp_res::less || (res == cmp_res::equal_ex && ::visit(cmp_functor<std::less<>>(), v, w));
}

template <class... Types> constexpr bool operator>(const variant<Types...> &v, const variant<Types...> &w) {
  cmp_res res = cmp_index(v, w);
  return res == cmp_res::greater || (res == cmp_res::equal_ex && ::visit(cmp_functor<std::greater<>>(), v, w));
}

template <class... Types> constexpr bool operator<=(const variant<Types...> &v, const variant<Types...> &w) {
  cmp_res res = cmp_index(v, w);
  return res == cmp_res::equal || res == cmp_res::less ||
         (res == cmp_res::equal_ex && ::visit(cmp_functor<std::less_equal<>>(), v, w));
}

template <class... Types> constexpr bool operator>=(const variant<Types...> &v, const variant<Types...> &w) {
  cmp_res res = cmp_index(v, w);
  return res == cmp_res::equal || res == cmp_res::greater ||
         (res == cmp_res::equal_ex && ::visit(cmp_functor<std::greater_equal<>>(), v, w));
}
} // namespace variant_helper

using variant_helper::operator==;
using variant_helper::operator!=;
using variant_helper::operator<;
using variant_helper::operator>;
using variant_helper::operator<=;
using variant_helper::operator>=;
