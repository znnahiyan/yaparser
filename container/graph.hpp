#ifndef GRAPH_HPP_INCLUDED
#define GRAPH_HPP_INCLUDED

#include <vector>

// #include "vector.hpp"
#include "../exception.hpp"

template <typename... Targs> using Vector = std::vector<Targs...>;

// Graph implementation.
struct BasicVertex
{
};

template <typename TVert>
struct BasicEdge
{
    typename Vector<TVert>::size_type from;
    typename Vector<TVert>::size_type to;

    bool operator== (const BasicEdge &) const = default;
    auto operator<=>(const BasicEdge &) const = default;
};

template <typename TVert = BasicVertex, typename TEdge = BasicEdge<TVert>>
requires std::derived_from<TVert, BasicVertex> && std::derived_from<TEdge, BasicEdge<TVert>>
class Graph
{
private:
    Vector<TVert> m_verts;
    Vector<TEdge> m_edges;
    Vector<size_t> m_verts_unique;
    Vector<size_t> m_edges_unique;

public:
    Graph() = default;
    Graph(decltype(m_verts) verts, decltype(m_edges) edges) : m_verts(verts), m_edges(edges) {}

    using vert_value_type = TVert;
    using edge_value_type = TEdge;
    using vert_iterator = typename decltype(m_verts)::iterator;
    using edge_iterator = typename decltype(m_edges)::iterator;
    using vert_reverse_iterator = typename decltype(m_verts)::reverse_iterator;
    using edge_reverse_iterator = typename decltype(m_edges)::reverse_iterator;
    using vert_const_iterator = typename decltype(m_verts)::const_iterator;
    using edge_const_iterator = typename decltype(m_edges)::const_iterator;
    using vert_const_reverse_iterator = typename decltype(m_verts)::const_reverse_iterator;
    using edge_const_reverse_iterator = typename decltype(m_edges)::const_reverse_iterator;
    using vert_size_type = typename decltype(m_verts)::size_type;
    using edge_size_type = typename decltype(m_edges)::size_type;

    constexpr auto insert_vert(const TVert &vert)
    {
        auto insert_unique_it = std::lower_bound(
            m_verts_unique.begin(),
            m_verts_unique.end(),
            vert,
            [&](size_t a, const TVert &b) { return m_verts[a] < b; }
        );

        if (insert_unique_it != m_verts_unique.end() && m_verts[*insert_unique_it] == vert)
        {
            auto insert_idx = *insert_unique_it;
            return std::pair(insert_idx, false);
        }

        m_verts.push_back(vert);
        size_t insert_idx = m_verts.size() - 1;
        insert_unique_it = m_verts_unique.insert(insert_unique_it, insert_idx);

        for (auto &edge : m_edges)
        {
            if (edge.from >= insert_idx)
                ++edge.from;
            if (edge.to >= insert_idx)
                ++edge.to;
        }

        return std::pair(insert_idx, true);
    }
    constexpr auto insert_edge(const TEdge &edge)
    {
        auto insert_unique_it = std::lower_bound(
            m_edges_unique.begin(),
            m_edges_unique.end(),
            edge,
            [&](size_t a, const TEdge &b) { return m_edges[a] < b; }
        );

        if (insert_unique_it != m_edges_unique.end() && m_edges[*insert_unique_it] == edge)
        {
            auto insert_idx = *insert_unique_it;
            return std::pair(insert_idx, false);
        }

        m_edges.push_back(edge);
        size_t insert_idx = m_edges.size() - 1;
        insert_unique_it = m_edges_unique.insert(insert_unique_it, insert_idx);

        return std::pair(insert_idx, true);
    }

    constexpr const auto &verts() const { return m_verts; }
    constexpr const auto &edges() const { return m_edges; }

    constexpr auto erase_vert(vert_iterator it)
    {
        // @fixme: The method needs to be updated to use m_verts_unique.
        throw NotImplementedException();
        
        // Delete adjacent edges, and correct vertex indices stored in edges.
        for (auto edge_it = m_edges.begin(); edge_it != m_edges.end(); ++edge_it)
        {
            while (edge_it->from == it || edge_it->to == it)
                edge_it = m_edges.erase(edge_it);
            if (edge_it->from > it)
                --edge_it->from;
            if (edge_it->to > it)
                --edge_it->to;
        }

        // Remove the vertex element.
        return m_verts.erase(it);
    }

    constexpr auto erase_edge(edge_iterator it)
    {
        // @fixme: The method needs to be updated to consider m_edges_unique.
        throw NotImplementedException();

        // Remove the edge element.
        return m_edges.erase(it);
    }

    auto insert_neighbor(size_t from, const TVert &to, TEdge edge = TEdge())
    {
        struct insert_return_type
        {
            vert_size_type vert_position;
            edge_size_type edge_position;
            bool vert_inserted;
            bool edge_inserted;
        } result;

        // Create new vertex and edge elements.
        std::tie(result.vert_position, result.vert_inserted) = insert_vert(to);
        edge.from = from;
        edge.to = result.vert_position;
        std::tie(result.edge_position, result.edge_inserted) = insert_edge(edge);

        // Return iterators to the newly created vertex and edge.
        return result;
    }
};

#endif // GRAPH_HPP_INCLUDED