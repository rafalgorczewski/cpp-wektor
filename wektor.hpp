#ifndef HQRP_WEKTOR_HPP
#define HQRP_WEKTOR_HPP

#include <algorithm>
#include <cmath>
#include <cstddef>
#include <exception>
#include <initializer_list>
#include <iterator>
#include <memory>
#include <new>
#include <type_traits>
#include <utility>
#include <vector>

namespace hqrp {
  template <typename T, typename Alloc = std::allocator<T>>
  class wektor {
   private:
    static constexpr auto DEFAULT_CAPACITY = 1;
    static constexpr auto CAPACITY_FACTOR = 2;

    template <typename U>
    class random_access_iterator {
     public:
      using iterator_category = std::random_access_iterator_tag;
      using value_type = U;
      using difference_type = std::ptrdiff_t;
      using reference = U&;
      using pointer = U*;

      random_access_iterator() : random_access_iterator(nullptr) {
      }
      random_access_iterator(pointer ptr) : m_ptr_pos(ptr) {
      }

      random_access_iterator& operator+=(difference_type n) {
        m_ptr_pos += n;
        return *this;
      }
      random_access_iterator operator+(difference_type n) const {
        auto added_it = *this;
        added_it += n;
        return added_it;
      }
      random_access_iterator& operator-=(difference_type n) {
        m_ptr_pos -= n;
        return *this;
      }
      random_access_iterator operator-(difference_type n) const {
        auto diff_it = *this;
        diff_it -= n;
        return diff_it;
      }
      reference operator[](difference_type n) {
        return *(m_ptr_pos + n);
      }
      bool operator<(random_access_iterator rhs) const {
        if (std::distance(m_ptr_pos, rhs.m_ptr_pos) < 0) {
          return true;
        }
        return false;
      }
      bool operator>(random_access_iterator rhs) const {
        if (std::distance(m_ptr_pos, rhs.m_ptr_pos) > 0) {
          return true;
        }
        return false;
      }
      bool operator<=(random_access_iterator rhs) const {
        if (std::distance(m_ptr_pos, rhs.m_ptr_pos) <= 0) {
          return true;
        }
        return false;
      }
      bool operator>=(random_access_iterator rhs) const {
        if (std::distance(m_ptr_pos, rhs.m_ptr_pos) >= 0) {
          return true;
        }
        return false;
      }
      random_access_iterator& operator++() {
        m_ptr_pos += 1;
        return *this;
      }
      random_access_iterator operator++(int) {
        auto it_before_increment = *this;
        m_ptr_pos += 1;
        return it_before_increment;
      }
      random_access_iterator& operator--() {
        m_ptr_pos -= 1;
        return *this;
      }
      random_access_iterator operator--(int) {
        auto it_before_decrement = *this;
        m_ptr_pos -= 1;
        return it_before_decrement;
      }
      bool operator==(random_access_iterator rhs) const {
        return m_ptr_pos == rhs.m_ptr_pos;
      }
      bool operator!=(random_access_iterator rhs) const {
        return m_ptr_pos != rhs.m_ptr_pos;
      }
      reference operator*() const {
        return *m_ptr_pos;
      }
      pointer operator->() const {
        return m_ptr_pos;
      }

      friend random_access_iterator operator+(difference_type n,
                                              random_access_iterator rhs) {
        return rhs + n;
      }

      friend difference_type operator-(random_access_iterator lhs,
                                       random_access_iterator rhs) {
        return std::distance(rhs.m_ptr_pos, lhs.m_ptr_pos);
      }

     private:
      value_type* m_ptr_pos;
    };

   public:
    using value_type = T;
    using allocator_type = Alloc;
    using size_type = std::size_t;
    using difference_type = std::ptrdiff_t;
    using reference = T&;
    using const_reference = const T&;
    using r_value_reference = T&&;
    using pointer = typename std::allocator_traits<Alloc>::pointer;
    using const_pointer = typename std::allocator_traits<Alloc>::const_pointer;
    using iterator = random_access_iterator<T>;
    using const_iterator = random_access_iterator<const T>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;

    wektor(std::initializer_list<value_type> initializer_list)
        : m_allocator(allocator_type()),
          m_size(0),
          m_capacity(
            get_proposed_capacity(initializer_list.size(), CAPACITY_FACTOR)),
          m_buffer(m_allocator.allocate(m_capacity)) {
      for (const auto& element : initializer_list) {
        push_back(element);
      }
    }
    explicit wektor(size_type size,
                    const value_type& default_element = value_type(),
                    const allocator_type& allocator = allocator_type())
        : m_allocator(allocator),
          m_size(0),
          m_capacity(get_proposed_capacity(size, CAPACITY_FACTOR)),
          m_buffer(m_allocator.allocate(m_capacity)) {
      for (size_type i = 0; i < size; ++i) {
        push_back(default_element);
      }
    }

    wektor() : wektor(0) {
    }
    explicit wektor(const allocator_type& allocator)
        : wektor(0, value_type(), allocator) {
    }
    wektor(size_type size, const allocator_type& allocator)
        : wektor(size, value_type(), allocator) {
    }

    ~wektor() {
      for (size_type i = 0; i < m_size; ++i) {
        m_allocator.destroy(m_buffer + i);
      }
      m_allocator.deallocate(m_buffer, 0);
    }

    wektor(const wektor& rhs) {
      m_allocator = rhs.m_allocator;
      m_size = rhs.m_size;
      m_capacity = rhs.capacity;
      m_buffer = m_allocator.allocate(m_capacity);
      std::copy(rhs.begin(), rhs.end(), m_buffer);
    }
    wektor& operator=(const wektor& rhs) {
      if (this != &rhs) {
        m_allocator = rhs.m_allocator;
        m_size = rhs.m_size;
        m_capacity = rhs.capacity;
        m_buffer = m_allocator.allocate(m_capacity);
        std::copy(rhs.begin(), rhs.end(), m_buffer);
      }
      return *this;
    }
    wektor(wektor&& rhs) {
      m_allocator = rhs.m_allocator;
      m_size = rhs.m_size;
      m_capacity = rhs.capacity;
      m_buffer = rhs.m_buffer;
      rhs.m_buffer = nullptr;
    }
    wektor& operator=(wektor&& rhs) noexcept(
      std::allocator_traits<
        allocator_type>::propagate_on_container_move_assignment::value ||
      std::allocator_traits<allocator_type>::is_always_equal::value) {
      if (this != &rhs) {
        m_allocator = rhs.m_allocator;
        m_size = rhs.m_size;
        m_capacity = rhs.capacity;
        m_buffer = rhs.m_buffer;
        rhs.m_buffer = nullptr;
      }
      return *this;
    }
    wektor& operator=(std::initializer_list<value_type> initializer_list) {
      for (size_type i = 0; i < m_size; ++i) {
        m_allocator.destroy(m_buffer++);
      }
      m_allocator.deallocate(m_buffer, 0);

      m_size = initializer_list.size();
      m_capacity = get_proposed_capacity(m_size, CAPACITY_FACTOR);
      m_buffer = m_allocator.allocate(m_capacity);
      for (const auto& element : initializer_list) {
        push_back(element);
      }
      return *this;
    }

    reference at(size_type pos) {
      if (pos > m_size) {
        throw std::out_of_range("Index out of range.");
      }
      return *(m_buffer + pos);
    }
    const_reference at(size_type pos) const {
      if (pos > m_size) {
        throw std::out_of_range("Index out of range.");
      }
      return *(m_buffer + pos);
    }

    allocator_type get_allocator() const {
      return m_allocator;
    }

    reference operator[](size_type pos) {
      return at(pos);
    }
    const_reference operator[](size_type pos) const {
      return at(pos);
    }

    reference front() {
      return *(m_buffer + m_size - 1);
    }
    const_reference front() const {
      return *(m_buffer + m_size - 1);
    }

    reference back() {
      return *m_buffer;
    }
    const_reference back() const {
      return *m_buffer;
    }

    pointer data() noexcept {
      return m_buffer;
    }
    const_pointer data() const noexcept {
      return m_buffer;
    }

    iterator begin() noexcept {
      return { m_buffer };
    }
    const_iterator begin() const noexcept {
      return { m_buffer };
    }
    const_iterator cbegin() const noexcept {
      return { m_buffer };
    }
    reverse_iterator rbegin() noexcept {
      return { m_buffer };
    }
    const_reverse_iterator rbegin() const noexcept {
      return { m_buffer };
    }
    const_reverse_iterator crbegin() const noexcept {
      return { m_buffer };
    }

    iterator end() noexcept {
      return { m_buffer + m_size };
    }
    const_iterator end() const noexcept {
      return { m_buffer + m_size };
    }
    const_iterator cend() const noexcept {
      return { m_buffer + m_size };
    }
    reverse_iterator rend() noexcept {
      return { m_buffer + m_size };
    }
    const_reverse_iterator rend() const noexcept {
      return { m_buffer + m_size };
    }
    const_reverse_iterator crend() const noexcept {
      return { m_buffer + m_size };
    }

    bool empty() const noexcept {
      return m_size == 0;
    }

    size_type size() const noexcept {
      return m_size;
    }

    size_type capacity() const noexcept {
      return m_capacity;
    }

    void shrink_to_fit() {
      pointer new_buffer = m_allocator.allocate(m_size);
      // noexcept(value_type(std::declval<value_type>()))
      if constexpr (std::is_nothrow_move_constructible<value_type>::value) {
        std::move(begin(),
                  end(),
                  std::raw_storage_iterator<pointer, value_type>(new_buffer));
      } else {
        std::copy(begin(),
                  end(),
                  std::raw_storage_iterator<pointer, value_type>(new_buffer));
      }
      for (size_type i = 0; i < m_size; ++i) {
        m_allocator.destroy(m_buffer + i);
      }
      m_allocator.deallocate(m_buffer, 0);
      m_buffer = new_buffer;
    }

    void clear() noexcept {
      for (size_type i = 0; i < m_size; ++i) {
        m_allocator.destroy(m_buffer + i);
      }
    }

    void push_back(const_reference value) {
      if (m_size + 1 > m_capacity) {
        m_capacity = get_proposed_capacity(m_size + 1, CAPACITY_FACTOR);
        pointer new_buffer = m_allocator.allocate(m_capacity);
        if constexpr (std::is_nothrow_move_constructible<value_type>::value) {
          std::move(begin(),
                    end(),
                    std::raw_storage_iterator<pointer, value_type>(new_buffer));
        } else {
          std::copy(begin(),
                    end(),
                    std::raw_storage_iterator<pointer, value_type>(new_buffer));
        }
        for (size_type i = 0; i < m_size; ++i) {
          m_allocator.destroy(m_buffer + i);
        }
        m_allocator.deallocate(m_buffer, 0);
        m_buffer = new_buffer;
      }
      m_allocator.construct(m_buffer + m_size, value);
      ++m_size;
    }
    void push_back(r_value_reference value) {
      if (m_size + 1 > m_capacity) {
        m_capacity = get_proposed_capacity(m_size + 1, CAPACITY_FACTOR);
        pointer new_buffer = m_allocator.allocate(m_capacity);
        if constexpr (std::is_nothrow_move_constructible<value_type>::value) {
          std::move(begin(),
                    end(),
                    std::raw_storage_iterator<pointer, value_type>(new_buffer));
        } else {
          std::copy(begin(),
                    end(),
                    std::raw_storage_iterator<pointer, value_type>(new_buffer));
        }
        for (size_type i = 0; i < m_size; ++i) {
          m_allocator.destroy(m_buffer + i);
        }
        m_allocator.deallocate(m_buffer, 0);
        m_buffer = new_buffer;
      }
      m_allocator.construct(m_buffer + m_size, std::move(value));
      ++m_size;
    }

    template <typename... Args>
    reference emplace_back(Args&&... args) {
      if (m_size + 1 > m_capacity) {
        m_capacity = get_proposed_capacity(m_size + 1, CAPACITY_FACTOR);
        pointer new_buffer = m_allocator.allocate(m_capacity);
        if constexpr (std::is_nothrow_move_constructible<value_type>::value) {
          std::move(begin(),
                    end(),
                    std::raw_storage_iterator<pointer, value_type>(new_buffer));
        } else {
          std::copy(begin(),
                    end(),
                    std::raw_storage_iterator<pointer, value_type>(new_buffer));
        }
        for (size_type i = 0; i < m_size; ++i) {
          m_allocator.destroy(m_buffer + i);
        }
        m_allocator.deallocate(m_buffer, 0);
        m_buffer = new_buffer;
      }
      m_allocator.construct(m_buffer + m_size, std::forward<Args>(args)...);
      ++m_size;
      return *(m_buffer + m_size);
    }

    void pop_back() {
      m_allocator.destroy(m_buffer + m_size);
      --m_size;
    }

    void resize([[maybe_unused]] size_type count) {
      // TODO
    }
    void resize([[maybe_unused]] size_type count,
                [[maybe_unused]] const_reference value) {
      // TODO
    }

    void swap([[maybe_unused]] wektor<value_type, Alloc>& other) noexcept(
      std::allocator_traits<
        allocator_type>::propagate_on_container_swap::value ||
      std::allocator_traits<allocator_type>::is_always_equal::value) {
      // TODO
    }

   private:
    allocator_type m_allocator{ allocator_type() };
    size_type m_size;
    size_type m_capacity;
    value_type* m_buffer{};

    size_type get_proposed_capacity(size_type size, int factor) {
      size_type capacity = m_capacity;
      for (size_type i = 0; i < size; ++i) {
        if (i > capacity) {
          capacity *= factor;
        }
      }
      return capacity + 1;
    }
  };
}  // namespace hqrp

#endif
