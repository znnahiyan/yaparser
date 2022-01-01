#ifndef VECTOR_HPP_INCLUDED
#define VECTOR_HPP_INCLUDED

#include <iostream>
#include <memory>
#include <algorithm>
#include <utility>
#include <iterator>
#include <exception>
#include <initializer_list>
#include <compare>
#include <concepts>
#include <xutility>

// @note: We use std::allocator to allow memory allocation and construction to be done in
//        separate steps. This is necessary for methods like .emplace(...) or .reserve(...).

// @note: Much inspiration has been taken from std::vector, but still kept rather simple,
//        with minimal template meta-programming.
//
// There are differences, including:
//      - Use of [[no_unique_address]] instead of template-based compressed pair to perform EBO
//      - Use of standard library functions from <memory> and <utility>

// @note:
//  We try to:
//      - minimize calculations during basic operations
//      - mimic the public interface of std::vector
//      - avoid invalidating iterators
//      - maintain class invariants and RAII property
//  However, we:
//      - WILL NOT perform index bounds-checking
//      - WILL NOT maintain thread-safe or atomic operations
//      - WILL NOT test full compatibility with <algorithm> or <ranges>
//      - WILL NOT orphan any iterators if they are invalidated, e.g. by resize()

// @todo:
//      - perform iterator compatibility checking for comparisons (do they point to the same containers?)
//      - _construct_*() should only construct elements, and not perform any allocations
//      - erase(), clear() should skip destruction if T is a trivially-destructible type

template <typename TVector> class VectorConstIterator;
template <typename TVector> class VectorIterator;

template <typename T, typename Allocator = std::allocator<T>>
class Vector
{
public:
    using value_type = T;
    using pointer = T*;
    using const_pointer = const T*;
    using reference = T&;
    using const_reference = const T&;
    using allocator_type = Allocator;
    using allocator_traits = std::allocator_traits<Allocator>;
    using size_type = typename allocator_traits::size_type;
    using difference_type = typename allocator_traits::difference_type;
    
    using iterator = VectorIterator<Vector>;
    using const_iterator = VectorConstIterator<Vector>;
    using reverse_iterator = std::reverse_iterator<iterator>;
    using const_reverse_iterator = std::reverse_iterator<const_iterator>;
    
private:
    pointer m_first{}; // same as begin()
    pointer m_last{};  // same as end()
    pointer m_end{};   // end of reserve memory

    // @note: [2021-12-22] workaround for LLVM not supporting [[no_unique_address]] for the MS ABI.
    #if defined(_MSC_VER)
        #pragma warning(disable:4848)
        #define _NO_UNIQUE_ADDRESS [[msvc::no_unique_address]]
    #else
        #define _NO_UNIQUE_ADDRESS [[no_unique_address]]
    #endif

    // @note: [[no_unique_address]] enables empty base optimization for stateless allocators.
    _NO_UNIQUE_ADDRESS Allocator allocator; // allocator instance

public:
    // Default constructors
    constexpr Vector() = default;
    constexpr Vector(const Allocator &alloc) : allocator(alloc) {}
    
    // Fill constructors
    constexpr Vector(const size_type count, const Allocator &alloc = Allocator()) : allocator(alloc)
    {
        _reallocate(count);
        _construct_n(count, T{});
    }
    constexpr Vector(const size_type count, const T &value, const Allocator &alloc = Allocator()) : allocator(alloc)
    {
        _reallocate(count);
        _construct_n(count, value);
    }
    
    // Iterator-based constructors
    constexpr Vector(std::initializer_list<T> ilist, const Allocator &alloc = Allocator()) : allocator(alloc)
    {
        _copy(ilist.begin(), ilist.end());
    }
    
    template <std::input_iterator InputIt>
    constexpr Vector(InputIt first, InputIt last, const Allocator &alloc = Allocator()) : allocator(alloc)
    {
        _copy(first, last);
    }
    
    // Copy constructors & copy assignment operator
    constexpr Vector(const Vector &rhs) : allocator(rhs.allocator)
    {
        _copy(rhs.begin(), rhs.end());
    }
    constexpr Vector(const Vector &rhs, const Allocator &alloc) : allocator(alloc)
    {
        _copy(rhs.begin(), rhs.end());
    }
    
    constexpr Vector& operator=(const Vector &rhs) {
        allocator = rhs.allocator;
        _copy(rhs.begin(), rhs.end());
        return *this;
    }
    
    // Move constructors & move assignment operator
    constexpr Vector(Vector &&rhs)
        : allocator(std::move(rhs.allocator)),
          m_first  (std::move(rhs.m_first)),
          m_last   (std::move(rhs.m_last)),
          m_end    (std::move(rhs.m_end))
    {}
    constexpr Vector(Vector &&rhs, const Allocator &alloc)
        : allocator(alloc),
          m_first  (std::move(rhs.m_first)),
          m_last   (std::move(rhs.m_last)),
          m_end    (std::move(rhs.m_end))
    {}

    constexpr Vector& operator=(Vector &&rhs) {
        allocator = std::move(rhs.allocator);
        m_first   = std::exchange(rhs.m_first, {});
        m_last    = std::exchange(rhs.m_last,  {});
        m_end     = std::exchange(rhs.m_end,   {});
        return *this;
    }
    
    // Destructor
    constexpr ~Vector() {
        clear();
        _deallocate();
    }

    // Allocation
    constexpr size_type size() const
    {
        if (m_first && m_last && m_end)
            return static_cast<size_type>(m_last - m_first);
        return 0;
    }
    constexpr size_type capacity() const
    {
        if (m_first && m_last && m_end)
            return static_cast<size_type>(m_end - m_first);
        return 0;
    }
    constexpr void reserve(const size_type new_capacity)
    {
        if (new_capacity > capacity())
            _reallocate_n(new_capacity);
    }
    constexpr void resize(const size_type new_size)
    {
        _resize(new_size);
    }
    
    // @note: clear is not supposed to reduce the capacity of the vector.
    constexpr void clear() {
        if (m_first) {
            auto old_size = size();
            std::destroy_n(m_first, old_size);
            m_last = m_first;
        }
    }
    
    // Iterators
    constexpr iterator               begin()         { return iterator(m_first);               }
    constexpr iterator               end()           { return iterator(m_last);                }
    constexpr const_iterator         begin()   const { return const_iterator(m_first);         }
    constexpr const_iterator         end()     const { return const_iterator(m_last);          }
    constexpr const_iterator         cbegin()  const { return const_iterator(begin());         }
    constexpr const_iterator         cend()    const { return const_iterator(end());           }
    constexpr reverse_iterator       rbegin()        { return reverse_iterator(end());         }
    constexpr reverse_iterator       rend()          { return reverse_iterator(begin());       }
    constexpr const_reverse_iterator rbegin()  const { return const_reverse_iterator(end());   }
    constexpr const_reverse_iterator rend()    const { return const_reverse_iterator(begin()); }
    constexpr const_reverse_iterator crbegin() const { return const_reverse_iterator(end());   }
    constexpr const_reverse_iterator crend()   const { return const_reverse_iterator(begin()); }
    
    // Operators
    constexpr reference operator[](const size_type index)
    {
        return m_first[index];
    }
    constexpr const_reference operator[] (const size_type index) const
    {
        return m_first[index];
    }
    constexpr reference at(const size_type index)
    {
        return m_first[index];
    }
    constexpr const_reference at(const size_type index) const
    {
        return m_first[index];
    }
    
    // Comparison operators with lexicographical ordering
    template <typename U> requires std::equality_comparable_with<T, U>
    friend constexpr bool operator==(const Vector<T> &lhs, const Vector<U> &rhs) {
        if (lhs.size() != rhs.size())
            return false;

        auto lhs_it = lhs.cbegin();
        auto rhs_it = rhs.cbegin();
        for (; lhs_it != lhs.cend(); ++lhs_it, ++rhs_it)
            if (*lhs_it != *rhs_it)
                return false;
        return true;
    }
    
    template <typename U> requires std::totally_ordered_with<T, U>
    friend constexpr std::strong_ordering operator<=>(const Vector<T> &lhs, const Vector<U> &rhs) {
        auto ordering = lhs.size() <=> rhs.size();
        if (ordering != std::strong_ordering::equivalent)
            return ordering;

        auto lhs_it = lhs.cbegin();
        auto rhs_it = rhs.cbegin();
        for (; lhs_it != lhs.cend(); ++lhs_it, ++rhs_it) {
            ordering = *lhs_it <=> *rhs_it;
            if (ordering != std::strong_ordering::equivalent)
                return ordering;
        }
        return ordering;
    }
    
    // Manipulation
    template <class... TValue>
    constexpr decltype(auto) emplace_back(TValue&&... value) {
        if (m_last == m_end)
            _reallocate_atleast(size()+1);
        
        try {
            allocator_traits::construct(allocator, m_last, std::forward<TValue>(value)...);
        }
        catch (const std::exception &ex) {
            std::cerr << ex.what() << '\n';
            throw ex;
        }
        
        auto& result = *m_last;
        ++m_last;
        return result;
    }
    
    constexpr decltype(auto) push_back(const_reference value) {
        if (m_last == m_end)
            _reallocate_atleast(size()+1);
        
        try {
            *m_last = value;
        }
        catch (const std::exception &ex) {
            std::cerr << ex.what() << '\n';
            throw ex;
        }
        
        auto& result = *m_last;
        ++m_last;
        return result;
    }
    
    constexpr decltype(auto) push_back(T &&value) {
        return emplace_back(value);
    }
    
    constexpr iterator insert(const_iterator position, const_reference value) {
        const auto index = static_cast<size_type>(position - cbegin());
        if (m_last == m_end)
            _reallocate_atleast(size() + 1);
        
        const_iterator input_it{cbegin() + index};
        iterator insert_it{begin() + index};
        
        if (input_it != cend()) {
            // @note: the last element is moved with a move constructor into an uninitialized element in reserve memory.
            // auto move_it = cend() - 1;
            auto dest_ptr = m_last + 1;
            // allocator_traits::construct(allocator, dest_ptr, std::move(*move_it));
            // @note: the previous elements are moved with move assignments into already-initialized elements.
            std::move_backward(input_it, cend(), dest_ptr);
        }
        
        ++m_last;
        *insert_it = value;
        return insert_it;
    }
    
    template <std::input_iterator InputIt>
    constexpr iterator insert(const_iterator position, InputIt first, InputIt last) {
        const auto index = static_cast<size_type>(position - cbegin());
        
        if (first == last)
            return iterator{begin() + index};

        const auto count = static_cast<size_type>(std::distance(first, last));
        if (m_end - m_last < count)
            _reallocate_atleast(size() + count);
        
        const_iterator input_it{cbegin() + index};
        iterator insert_it{begin() + index};
        
        if (input_it != cend()) {
            // @note: the last element is moved with a move constructor into uninitialized elements in reserve memory.
            // [0] [1] [2] [3] [4] [5] [6] [7]
            // XXX XXX XXX XXX  ^
            //                 end
            
            // v[end]   = T(v[end-2])
            // v[end+1] = T(v[end-1])
            
            auto move_it = cend() - count;
            auto dest_ptr = m_last;
            
            for (; move_it != cend(); ++move_it, ++dest_ptr)
                allocator_traits::construct(allocator, dest_ptr, std::move(*move_it));
            
            // v[3] = std::move(v[1])
            // v[2] = std::move(v[0])
            
            // @note: the previous elements are moved with move assignments into already-initialized elements.
            std::move_backward(cbegin(), cend() - count, m_first + count);
        }
        
        m_last += count;
        return std::copy(first, last, insert_it);
    }
    
    constexpr iterator erase(iterator it) {
        if (m_first) {
            std::destroy_at(it.ptr);
            auto result = std::move(it+1, end(), it);
            --m_last;
            return result;
        }
        return {};
    }
    
    constexpr iterator erase(iterator first, iterator last)
    {
        if (m_first) {
            const auto count = static_cast<size_type>(last - first);
            if (count != 0) {
                std::destroy_n(first, count);
                auto result = std::move(first + count, end(), first);
                m_last -= count;
                return result;
            }
        }
        return {};
    }
    
private:
    constexpr size_type _calculate_growth(const size_type new_size)
    {
        size_type x = new_size;
        
        // Reset all 1 bits except for the MSB.
        while (x & (x-1))
            x &= x-1;
        
        // Shift the MSB to the left.
        x <<= 1;
        
        return x;
    }
    
    constexpr void _reallocate(const size_type new_capacity)
    {
        // @todo: implement logic to destroy elements that are outside of the new capacity
        if (new_capacity < capacity())
            return;
        
        const auto old_size = this->size();
        
        try {
            const pointer new_m_first = allocator.allocate(new_capacity);
            
            if (m_first) {
                try {
                    for (size_type i{}; i != old_size; ++i)
                        allocator_traits::construct(allocator, new_m_first + i, std::move(m_first[i]));
                }
                catch (const std::exception &ex) {
                    allocator.deallocate(new_m_first, new_capacity);
                    std::cerr << ex.what() << '\n';
                    throw ex;
                }
            }
            
            _change_array(new_m_first, old_size, new_capacity);
        }
        catch(const std::bad_array_new_length &ex) { std::cerr << ex.what() << '\n'; throw ex; }
        catch(const std::bad_alloc &ex)            { std::cerr << ex.what() << '\n'; throw ex; }
    }
    
    constexpr void _reallocate_atleast(const size_type new_capacity)
    {
        _reallocate(_calculate_growth(new_capacity));
    }
    
    constexpr void _deallocate() {
        auto old_size = size();
        allocator.deallocate(m_first, old_size);
        _change_array({}, {}, {});
    }
    
    constexpr void _change_array(const pointer new_m_first, const size_type new_size, const size_type new_capacity)
    {
        m_first = new_m_first;
        m_last  = new_m_first + new_size;
        m_end   = new_m_first + new_capacity;
    }
    
    constexpr void _construct_n(const size_type n, const T &value)
    {
        for (size_type i{}; i != n; ++i)
            allocator_traits::construct(allocator, m_first + i, value);
        m_last += n;
    }
    
    template <std::input_iterator InputIt>
    constexpr void _construct_copy(InputIt first, InputIt last, size_type position)
    {
        size_t i = position;
        for (; first != last; ++i, ++first)
            allocator_traits::construct(allocator, m_first + i, *first);
        m_last += (i - position);
    }
    
    constexpr void _resize(const size_type new_size, const T &value)
    {
        const auto size = this->size();
        
        // Trim
        if (new_size < size) {
            const pointer new_m_last = m_first + new_size;
            std::destroy_n(new_m_last, size - new_size);
            m_last = new_m_last;
            return;
        }
        
        // Append
        if (new_size > size) {
            if (new_size > capacity())
                _reallocate_atleast(new_size);
            const pointer new_m_last = m_first + new_size;
            for (pointer it = m_last; it != new_m_last; ++it)
                allocator_traits::construct(allocator, it, value);
            m_last = new_m_last;
        }
        
        // @note: Avoid reallocation if new_size == size.
    }
    
    template <std::input_iterator InputIt>
    constexpr void _copy(InputIt first, InputIt last) {
        if constexpr (std::_Is_random_iter_v<InputIt>)
        {
            if (first != last) {
                const auto new_size = static_cast<size_type>(std::distance(first, last));
                _reallocate(new_size);
                _construct_copy(first, last, {});
            }
        }
        else
        {
            auto current_size = size();
            for (; first != last; ++first)
                _resize(++current_size, *first);
        }
    }
};

template <typename TVector>
class VectorConstIterator
{
public:
    using iterator_concept  = std::contiguous_iterator_tag;
    using iterator_category = std::random_access_iterator_tag;
    using value_type        = typename TVector::value_type;
    using difference_type   = typename TVector::difference_type;
    using pointer           = typename TVector::const_pointer;
    using reference         = const value_type&;
    
    using TPtr = typename TVector::pointer;
    
public:
    TPtr ptr{};

    constexpr VectorConstIterator() = default;
    
    constexpr VectorConstIterator(TPtr _ptr) : ptr(_ptr) {}

    constexpr VectorConstIterator& operator=(const VectorConstIterator &rhs) = default;

    constexpr reference operator*() const {
        return *ptr;
    }
    
    constexpr pointer operator->() const {
        return ptr;
    }
    
    constexpr VectorConstIterator& operator++() {
        ++ptr;
        return *this;
    }
    
    constexpr VectorConstIterator& operator++(int) {
        VectorConstIterator temp = *this;
        ++*this;
        return temp;
    }
    
    constexpr VectorConstIterator& operator--() {
        --ptr;
        return *this;
    }
    
    constexpr VectorConstIterator& operator--(int) {
        VectorConstIterator temp = *this;
        --*this;
        return temp;
    }
    
    constexpr VectorConstIterator& operator+=(const difference_type offset) {
        ptr += offset;
        return *this;
    }
    
    constexpr VectorConstIterator operator+(const difference_type offset) const {
        VectorConstIterator temp = *this;
        temp += offset;
        return temp;
    }
    
    friend constexpr VectorConstIterator operator+(const difference_type offset, VectorConstIterator rhs) {
        return rhs += offset;
    }
    
    constexpr VectorConstIterator& operator-=(const difference_type offset) {
        return *this += -offset;
    }
    
    constexpr VectorConstIterator operator-(const difference_type offset) const {
        VectorConstIterator temp = *this;
        temp -= offset;
        return temp;
    }

    constexpr difference_type operator-(const VectorConstIterator &rhs) const {
        return ptr - rhs.ptr;
    }

    constexpr reference operator[](const difference_type offset) const {
        return *(*this + offset);
    }
    
    constexpr bool operator==(const VectorConstIterator &rhs) const {
        return ptr == rhs.ptr;
    }
    
    constexpr std::strong_ordering operator<=>(const VectorConstIterator &rhs) const {
        return ptr <=> rhs.ptr;
    }
};

template <typename TVector>
class VectorIterator : public VectorConstIterator<TVector>
{
private:
    using _Base = VectorConstIterator<TVector>;
    
public:
    using iterator_concept  = std::contiguous_iterator_tag;
    using iterator_category = std::random_access_iterator_tag;
    using value_type        = typename TVector::value_type;
    using difference_type   = typename TVector::difference_type;
    using pointer           = typename TVector::pointer;
    using reference         = value_type&;
    
    using _Base::_Base;
    
    constexpr VectorIterator& operator=(const VectorIterator&) = default;
    
    constexpr reference operator*() const {
        return const_cast<reference>(_Base::operator*());
    }
    
    constexpr pointer operator->() const {
        return this->ptr;
    }
    
    constexpr VectorIterator& operator++() {
        _Base::operator++();
        return *this;
    }
    
    constexpr VectorIterator& operator++(int) {
        VectorIterator temp = *this;
        _Base::operator++();
        return temp;
    }
    
    constexpr VectorIterator& operator--() {
        _Base::operator--();
        return *this;
    }
    
    constexpr VectorIterator& operator--(int) {
        VectorIterator temp = *this;
        _Base::operator--();
        return temp;
    }
    
    constexpr VectorIterator& operator+=(const difference_type offset) {
        _Base::operator+=(offset);
        return *this;
    }
    
    constexpr VectorIterator operator+(const difference_type offset) const {
        VectorIterator temp = *this;
        temp += offset;
        return temp;
    }
    
    friend constexpr VectorIterator operator+(const difference_type offset, VectorIterator rhs) {
        return rhs += offset;
    }
    
    constexpr VectorIterator& operator-=(const difference_type offset) {
        _Base::operator-=(offset);
        return *this;
    }
    
    using _Base::operator-;
    
    constexpr VectorIterator operator-(const difference_type offset) const {
        VectorIterator temp = *this;
        temp -= offset;
        return temp;
    }
    
    constexpr reference operator[](const difference_type offset) const {
        return _Base::operator[](offset);
        // return const_cast<reference>(_Base::operator[](offset));
    }
};

template <typename TVector>
struct std::pointer_traits<VectorConstIterator<TVector>> {
    using pointer         = VectorConstIterator<TVector>;
    using element_type    = const typename pointer::value_type;
    using difference_type = typename pointer::difference_type;
    
    static constexpr element_type* to_address(const pointer it) {
        return std::to_address(it.ptr);
    }
};

template <typename TVector>
struct std::pointer_traits<VectorIterator<TVector>> {
    using pointer         = VectorIterator<TVector>;
    using element_type    = typename pointer::value_type;
    using difference_type = typename pointer::difference_type;
    
    static constexpr element_type* to_address(const pointer it) {
        return std::to_address(it.ptr);
    }
};

#endif // VECTOR_HPP_INCLUDED