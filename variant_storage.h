#pragma once

#include "variant_helper.h"
#include <cassert>

namespace variant_helper {
template <class TList, typename = void> struct storage_helper;

template <> struct storage_helper<type_list<>> {
  void reset(...) noexcept {} // dummy for runtime reset recursive call
};

template <class TList>
struct storage_helper<TList, std::enable_if_t<TList::template all_of<std::is_trivially_destructible>>>
// not enable_if on is_trivially_destrutible<head_t> because of implicitly deleted destructor
// warning
{
  using head_t = typename TList::head_t;
  using tail_t = typename TList::tail_t;
  union {
    head_t head;
    storage_helper<tail_t> tail;
  };

  constexpr storage_helper() noexcept {}

  template <class... Args, std::size_t I>
  constexpr explicit storage_helper(in_place_index_t<I>, Args &&... args) noexcept(
      std::is_nothrow_constructible_v<storage_helper<tail_t>, in_place_index_t<I - 1>, Args &&...>)
      : tail(in_place_index<I - 1>, std::forward<Args>(args)...) {}

  template <class... Args>
  constexpr explicit storage_helper(in_place_index_t<0>,
                                    Args &&... args) noexcept(std::is_nothrow_constructible_v<head_t, Args &&...>)
      : head(std::forward<Args>(args)...) {}

  template <class T, class... Args>
  constexpr explicit storage_helper(in_place_type_t<T>, Args &&... args)
      : storage_helper(in_place_index<TList::template unique_type_index<T>>, std::forward<Args>(args)...) {}

  void reset(size_t index) noexcept {
    // all trivially destructible
  }
};

template <class TList>
struct storage_helper<TList, std::enable_if_t<!TList::template all_of<std::is_trivially_destructible>>> {
  using head_t = typename TList::head_t;
  using tail_t = typename TList::tail_t;
  union {
    head_t head;
    storage_helper<tail_t> tail;
  };

  constexpr storage_helper() noexcept {}

  template <std::size_t I, class... Args>
  constexpr explicit storage_helper(in_place_index_t<I>, Args &&... args) noexcept(
      std::is_nothrow_constructible_v<storage_helper<tail_t>, in_place_index_t<I - 1>, Args &&...>)
      : tail(in_place_index<I - 1>, std::forward<Args>(args)...) {}

  template <class... Args>
  constexpr explicit storage_helper(in_place_index_t<0>,
                                    Args &&... args) noexcept(std::is_nothrow_constructible_v<head_t, Args &&...>)
      : head(std::forward<Args>(args)...) {}

  template <class T, class... Args>
  constexpr explicit storage_helper(in_place_type_t<T>, Args &&... args)
      : storage_helper(in_place_index<TList::template unique_type_index<T>>, std::forward<Args>(args)...) {}

  void reset(size_t index) noexcept {
    if (index == 0)
      head.~head_t();
    else
      // all paths are compiled => in the end call to dummy (never executed)
      tail.reset(index - 1);
  }

  ~storage_helper() {}
};

template <class TList> struct storage_t : storage_helper<TList> {
  using base_t = storage_helper<TList>;
  using storage_helper<TList>::storage_helper;
  using head_t = typename TList::head_t;
  using tail_t = typename TList::tail_t;

  template <size_t I> static constexpr argument_pack_at_t<I, TList> &get(base_t &self) noexcept {
    if constexpr (I > 0)
      return storage_t<tail_t>::template get<I - 1>(self.tail);
    else
      return self.head;
  }

  template <size_t I> constexpr argument_pack_at_t<I, TList> &get() noexcept { return get<I>(*this); }

  template <class R, class Action, class BaseT, class... OthersT> static constexpr bool is_recurse_noexcept() noexcept {
    if constexpr (TList::size > 1)
      return noexcept(storage_t<tail_t>::template recurse<R>(std::declval<Action>(), 0, std::declval<BaseT>().tail,
                                                             std::declval<OthersT>().tail...));
    else
      return true;
  }

  template <class R, class Action, class BaseT, class... OthersT>
  constexpr static R recurse(Action &&act, size_t ind, BaseT &&self, OthersT &&... others)
      noexcept(std::is_nothrow_invocable_v<Action, decltype((std::forward<BaseT>(self).head)),
                                           decltype((std::forward<OthersT>(others).head))...> &&
               is_recurse_noexcept<R, Action, BaseT, OthersT...>()) {
    if constexpr (TList::size > 1) {
      if (ind > 0)
        return storage_t<tail_t>::template recurse<R>(
            std::forward<Action>(act), ind - 1, std::forward<BaseT>(self).tail, std::forward<OthersT>(others).tail...);
      else
        return act(std::forward<BaseT>(self).head, std::forward<OthersT>(others).head...);
    } else {
      assert(ind == 0);
      return act(std::forward<BaseT>(self).head, std::forward<OthersT>(others).head...);
    }
  }

  template <class StorageHelper, typename U = TList>
  static void init_from(base_t &self, size_t ind, bool assign, StorageHelper &&other)
      noexcept(storage_t<tail_t>::template is_nothrow_init_from<StorageHelper> &&
               std::is_nothrow_constructible_v<head_t, StorageHelper &&> &&
               std::is_nothrow_assignable_v<head_t &, decltype(std::forward<StorageHelper>(other).head)>) {
    return recurse<void>(
        [&](auto &head, auto &&other_head) {
          if (assign)
            head = std::forward<decltype(other_head)>(other_head);
          else
            new (&head) std::remove_reference_t<decltype(head)>(std::forward<decltype(other_head)>(other_head));
        },
        ind, self, std::forward<StorageHelper>(other));
  }

  template <class StorageHelper>
  static constexpr bool is_nothrow_init_from = noexcept(init_from(std::declval<base_t &>(), 0, false,
                                                                  std::declval<StorageHelper &&>()));

  template <class StorageHelper>
  void init_from(size_t ind, bool assign, StorageHelper &&other) noexcept(is_nothrow_init_from<StorageHelper>) {
    init_from(*this, ind, assign, std::forward<StorageHelper>(other));
  }

  template <class R, class BaseT, class Visitor>
  constexpr static R visit(Visitor &&vis, size_t ind, BaseT &&self) noexcept(TList::template is_nothrow<Visitor, BaseT>) {
    return recurse<R>(std::forward<Visitor>(vis), ind, std::forward<BaseT>(self));
  }

  template <size_t I, class... Args> argument_pack_at_t<I, TList> &emplace(Args &&... args) {
    auto &res = this->template get<I>();
    new (const_cast<std::decay_t<decltype(res)> *>(&res))
        variant_helper::argument_pack_at_t<I, TList>(std::forward<Args>(args)...);
    return res;
  }
};

template <> struct storage_t<type_list<>> : storage_helper<type_list<>> {
  using base_t = storage_helper<type_list<>>;
  using base_t::storage_helper;

  template <class StorageHelper> static constexpr bool is_nothrow_init_from = true;
};
} // namespace variant_helper
