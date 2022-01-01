#ifndef TEST_HPP_INCLUDED
#define TEST_HPP_INCLUDED

#include <initializer_list>
#include <stdexcept>

#include "container/vector.hpp"

class test_error : public std::logic_error { using std::logic_error::logic_error; };

template <typename T, typename U>
constexpr bool assert_array_equal(const T &x, const U &y) {
    auto x_it = x.begin();
    auto y_it = y.begin();
    for (; x_it != x.end() && y_it != y.end(); ++x_it, ++y_it)
        if (*x_it != *y_it)
            return false;
    if (x_it == x.end() && y_it != y.end()) return false; else
    if (x_it != x.end() && y_it == y.end()) return false; else
        return true;
};

constexpr bool test_vector() {
    Vector<int> v;
    
    if (!assert_array_equal(v, std::initializer_list<int>{}))
        throw test_error("Vector::Vector() does not produce an empty vector");
    
    v.emplace_back(10);
    
    if (!assert_array_equal(v, std::initializer_list<int>{10}))
        throw test_error("Vector::emplace_back() produces unexpected result");
    
    // v.insert(v.cbegin(), 20);
    
    // if (!assert_array_equal(v, std::initializer_list<int>{20, 10}))
    //     throw test_error("Vector::insert() produces unexpected result");
    
    // v.insert(v.cend(), 0);
    
    // if (!assert_array_equal(v, std::initializer_list<int>{20, 10, 0}))
    //     throw test_error("Vector::insert() produces unexpected result");
    
    return true;
}

constexpr bool test_full() {
    static_assert(test_vector(), "Vector test failed");
    return true;
}

#endif