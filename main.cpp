// Zulker Nayeen Nahiyan (ID: 1910063)

// Note: This answer requires C++20. It is the earliest standard in which
//       semaphores have been natively defined in C++.
//       Supported by GCC 11, Clang 11, MSVC 19.28.

#ifndef __cplusplus
    #error C++ is required
#elif __cplusplus < 202002L
    #error C++20 is required
#endif

#include <iostream>
#include <string>
#include <string_view>
#include <sstream>
#include <iomanip>
#include <initializer_list>
#include <exception>
#include <algorithm>
#include <cctype>
#include <compare>
#include <concepts>
#include <optional>
#include <variant>
#include <functional>
#include <numeric>

#include <vector>
#include <stack>
#include <set>
#include <map>

template <typename... Targs> using Vector = std::vector<Targs...>;
template <typename... Targs> using Stack = std::stack<Targs...>;
template <typename... Targs> using Set = std::set<Targs...>;
template <typename... Targs> using Map = std::map<Targs...>;
template <typename T>        using Reference = std::reference_wrapper<T>;

// @note: While I made a very comprehensive class for Vector, VectorIterator, and VectorConstIterator,
//        unfortunately, it does not work properly with elements of non-trivial types. It, however,
//        does work perfectly with trivial types such as int, so I don't know what the issue is.
// #include "container/vector.hpp"
#include "exception.hpp"
#include "container/graph.hpp"

class Symbol
{
public:
    const std::string repr;

    explicit Symbol()                         : repr("")    { /* std::cout << "Symbol()" << std::endl; */ }
    explicit Symbol(const std::string &repr_) : repr(repr_) { /* std::cout << "Symbol(\"" << repr_ << "\")" << std::endl; */ }

    enum class Type
    {
        Variable,
        Terminal
    };

    virtual Type type() const = 0;

    bool operator== (const Symbol &other) const { return repr ==  other.repr; };
    auto operator<=>(const Symbol &other) const { return repr <=> other.repr; };

    friend std::ostream &operator<<(std::ostream &os, const Symbol &symbol)
    {
        return os << symbol.repr;
    }
};

// @note: Necessary to forward relational operators for std::reference_wrapper<...> to their underlying class.
//        Otherwise, the comparator template argument would have to be set for every ordered container class:
//        e.g. Set<Reference<Symbol>, std::less<Symbol &>> mySymbolSet;
//                                    ^^^^^^^^^^^^^^^^^^^
constexpr bool operator== (Reference<Symbol> lhs, Reference<Symbol> rhs) { return lhs.get() ==  rhs.get(); }
constexpr auto operator<=>(Reference<Symbol> lhs, Reference<Symbol> rhs) { return lhs.get() <=> rhs.get(); }

class Terminal : public Symbol
{
public:
    using Symbol::Symbol;
    Type type() const override { return Type::Terminal; }
};

Terminal epsilon = Terminal("^");
Terminal terminator = Terminal("$");

class Production;

class Variable : public Symbol
{
public:
    Vector<Reference<Production>> productions;
    Set<Reference<Terminal>>      first;
    Set<Reference<Terminal>>      follow;

    using Symbol::Symbol;
    Type type() const override { return Type::Variable; }
};

class Production
{
public:
    Vector<Reference<Symbol>> str;

    Production() = default;
    Production(const Production &) = default;
    Production(const Vector<Reference<Symbol>> &str_) : str(str_) {}

    enum class Limit
    {
        All,
        LeftMost,
        RightMost
    };

    // Find all derivations of a given production. (More useful for top-down parsing (TDP) than bottom-up parsing (BUP).)
    Vector<Production> derivations(Limit limit = Limit::All)
    {
        Vector<Production> results;
        size_t idx;
        Vector<Reference<Symbol>>::iterator begin_it, end_it;

        switch (limit)
        {
        case Limit::All:
        case Limit::LeftMost:
            begin_it = str.begin();
            end_it = str.end();
            idx = 0;
            break;
        case Limit::RightMost:
            begin_it = str.end(); --begin_it;
            end_it = str.begin(); --end_it;
            idx = str.size() - 1;
            break;
        }

        for (auto it = begin_it; it != end_it;)
        {
            Symbol &symbol_ref = *it;
            if (symbol_ref.type() == Symbol::Type::Variable)
            {
                auto &variable = dynamic_cast<Variable &>(symbol_ref);

                for (Production &production : variable.productions)
                {
                    auto &copy = results.emplace_back(*this);
                    auto copy_it = copy.str.begin();
                    std::advance(copy_it, idx);
                    copy_it = copy.str.erase(copy_it);
                    copy.str.insert(copy_it, production.str.begin(), production.str.end());
                }

                if (limit != Limit::All)
                    break;
            }

            switch (limit)
            {
            case Limit::All:
            case Limit::LeftMost:
                ++it;
                ++idx;
                break;
            case Limit::RightMost:
                --it;
                --idx;
                break;
            }
        }

        return results;
    };

    bool operator==(const Production &other) const
    {
        auto it = str.cbegin(), other_it = other.str.cbegin();

        if (str.size() != other.str.size())
            return false;

        while (it != str.cend() && other_it != other.str.cend())
        {
            if (it->get() != other_it->get())
                return false;
            ++it, ++other_it;
        }

        return true;
    }

    auto operator<=>(const Production &other) const
    {
        auto it = str.cbegin(), other_it = other.str.cbegin();

        while (it != str.cend() && other_it != other.str.cend())
        {
            auto ordering = it->get() <=> other_it->get();
            if (ordering != std::strong_ordering::equivalent)
                return ordering;
            ++it, ++other_it;
        }

        if (it == str.cend() && other_it != other.str.cend())
            return std::strong_ordering::less;
        else if (it != str.cend() && other_it == other.str.cend())
            return std::strong_ordering::greater;
        else
            return std::strong_ordering::equivalent;
    }

    friend std::ostream &operator<<(std::ostream &os, const Production &prod)
    {
        std::ostringstream buffer;
        for (auto it = prod.str.begin(); it != prod.str.end(); ++it)
        {
            if (it != prod.str.begin())
                buffer << ' ';
            buffer << it->get().repr;
        }
        return os << buffer.str();
    }
};

class Rule;

class DividedProduction : public Production
{
public:
    Vector<Reference<Symbol>>::iterator divider_it;

    DividedProduction()
        : Production(), divider_it(str.begin()) {}
    DividedProduction(const Production &prod)
        : Production(prod), divider_it(str.begin()) {}
    DividedProduction(const Vector<Reference<Symbol>> &str_)
        : Production(str_), divider_it(str.begin()) {}
    DividedProduction(const DividedProduction &prod)
        : Production(prod), divider_it(str.begin() + (prod.divider_it - prod.str.begin())) {}

    bool reduce(const Rule &);

    Symbol &shift()
    {
        if (divider_it == str.end())
            return epsilon;

        auto &consumed = divider_it->get();
        ++divider_it;
        return consumed;
    }
    
    bool operator==(const DividedProduction &other) const
    {
        auto divider_idx = divider_it - str.begin();
        auto other_divider_idx = other.divider_it - other.str.begin();
        if (divider_idx != other_divider_idx)
            return false;
        return static_cast<const Production &>(*this) == static_cast<const Production &>(other);
    }

    auto operator<=>(const DividedProduction &other) const
    {
        auto divider_idx = divider_it - str.begin();
        auto other_divider_idx = other.divider_it - other.str.begin();
        auto ordering = divider_idx <=> other_divider_idx;
        if (ordering != std::strong_ordering::equivalent)
            return ordering;
        return static_cast<const Production &>(*this) <=> static_cast<const Production &>(other);
    }

    friend std::ostream &operator<<(std::ostream &os, const DividedProduction &prod)
    {
        std::ostringstream buffer;
        for (auto it = prod.str.begin(); it != prod.str.end(); ++it)
        {
            if (it != prod.str.begin())
                buffer << ' ';
            if (it == prod.divider_it)
                buffer << ". ";
            buffer << it->get().repr;
        }
        if (prod.divider_it == prod.str.end())
        {
            if (prod.divider_it != prod.str.begin())
                buffer << ' ';
            buffer << '.';
        }
        return os << buffer.str();
    }
};

class Rule
{
public:
    Production lhs;
    Production rhs;

    Rule() = default;
    Rule(const Production &lhs_, const Production &rhs_) : lhs(lhs_), rhs(rhs_) {}

    Variable &head() const
    {
        const auto head_size = lhs.str.size();
        if (head_size != 1) {
            std::cerr << "ERROR:" << std::endl;
            std::cerr << "lhs=\"" << lhs << "\"" << std::endl;
            std::cerr << "rhs=\"" << rhs << "\"" << std::endl;
            throw GrammarError("Cannot get the head of a rule which is not context-free");
        }

        Symbol &symbol = lhs.str.begin()->get();

        if (symbol.type() != Symbol::Type::Variable)
            throw GrammarError("A valid rule cannot have a terminal as the head");

        return dynamic_cast<Variable &>(symbol);
    }

    bool operator==(const Rule &other) const
    {
        return lhs == other.lhs && rhs == other.rhs;
    }
    auto operator<=>(const Rule &other) const
    {
        auto lhs_ordering = lhs <=> other.lhs;
        if (lhs_ordering != std::strong_ordering::equivalent)
            return lhs_ordering;
        else
            return rhs <=> other.rhs;
    }

    friend std::ostream &operator<<(std::ostream &os, const Rule &rule)
    {
        std::ostringstream buffer;
        buffer << rule.lhs << " -> " << rule.rhs;
        return os << buffer.str();
    }
};

bool DividedProduction::reduce(const Rule &rule)
{
    auto body_it_rev = rule.rhs.str.rbegin();
    auto prod_it_rev = std::make_reverse_iterator(divider_it);
    for (; body_it_rev != rule.rhs.str.rend(); ++body_it_rev, ++prod_it_rev)
    {
        if (prod_it_rev == str.rend())
            return false;
        if (body_it_rev->get() != prod_it_rev->get())
            return false;
    }
    auto prod_it_fwd = str.erase(prod_it_rev.base(), divider_it);
    divider_it = str.insert(prod_it_fwd, rule.head());
    ++divider_it;
    return true;
}

class DividedRule : public Rule
{
public:
    DividedProduction rhs;

    using Rule::Rule;
    DividedRule(const Rule &rule) : rhs(rule.rhs) {
        lhs = rule.lhs;
    }

    Rule& base() {
        // @note: The base class's member `rhs` is shadowed by the derived class's meme `rhs`.
        //        But when we cast back to the base class, the old `rhs` is used again, which was empty.
        //        Thus, we have to copy the member from DerivedRule::rhs to Rule::rhs.
        Rule &rule = static_cast<Rule&>(*this);
        rule.rhs = rhs;
        return rule;
    }

    bool is_final_item() const {
        // @note: Move code into DividedProduction?
        return rhs.divider_it == rhs.str.end();
    }

    bool operator==(const Rule &other) const = delete;
    std::strong_ordering operator<=>(const Rule &other) const = delete;

    bool operator==(const DividedRule &other) const
    {
        return lhs == other.lhs && rhs == other.rhs;
    }
    auto operator<=>(const DividedRule &other) const
    {
        auto lhs_ordering = lhs <=> other.lhs;
        if (lhs_ordering != std::strong_ordering::equivalent)
            return lhs_ordering;
        else
            return rhs <=> other.rhs;
    }

    friend std::ostream &operator<<(std::ostream &os, const DividedRule &rule)
    {
        std::ostringstream buffer;
        buffer << rule.lhs << " -> " << rule.rhs;
        return os << buffer.str();
    }
};

class Grammar
{
public:
    Map<std::string, Variable> variables;
    Map<std::string, Terminal> terminals;
    Vector<Rule> rules;

public:
    Grammar(std::initializer_list<std::string> rules_)
    {
        // Convert a rule string "S -> A a B b" into instances of Rule, Production, Variable, and Terminal.
        for (auto str : rules_)
        {
            // Split by whitespace.
            std::istringstream buffer(str);
            Vector<std::string> tokens{std::istream_iterator<std::string>(buffer), std::istream_iterator<std::string>{}};

            // Split by arrow symbol.
            const std::string delimiter = "->";
            const auto delimiter_it = std::find(tokens.cbegin(), tokens.cend(), delimiter);
            const Vector<std::string> lhs_tokens(tokens.cbegin(), delimiter_it);
            const Vector<std::string> rhs_tokens(delimiter_it + 1, tokens.cend());

            // Instantiate a Rule.
            auto &rule = rules.emplace_back();

            // Parse both left-hand side and right-hand sides of the rule.
            for (auto [side_tokens, side_prod] : {
                     std::make_pair(std::ref(lhs_tokens), std::ref(rule.lhs)),
                     std::make_pair(std::ref(rhs_tokens), std::ref(rule.rhs))})
            {
                for (auto token : side_tokens)
                {
                    // Capital letters are considered to be variables, while every other symbol is considered a terminal.
                    if (std::isalpha(token[0]) && std::isupper(token[0]))
                    {
                        auto last_it = this->variables.emplace(token, token).first;
                        side_prod.str.emplace_back(last_it->second);
                    }
                    else
                    {
                        auto last_it = this->terminals.emplace(token, token).first;
                        side_prod.str.emplace_back(last_it->second);
                    }
                }
            }
        }
    }

    friend std::ostream &operator<<(std::ostream &os, const Grammar &g)
    {
        os << "Rules:" << std::endl;
        size_t i = 0;
        for (auto &r : g.rules)
            os << '[' << i++ << "] = " << r << std::endl;

        os << "\nVariables:";
        for (auto &v : g.variables)
            os << ' ' << v.second;
        os << std::endl;

        os << "Terminals:";
        for (auto &t : g.terminals)
            os << ' ' << t.second;
        os << std::endl;

        return os;
    }
};

class ContextFreeGrammar : public Grammar
{
public:
    ContextFreeGrammar(std::initializer_list<std::string> rules_) : Grammar(rules_)
    {
        for (auto &r : rules)
        {
            Variable &v = r.head();
            v.productions.emplace_back(r.rhs);
        }
    }

    void augment_grammar()
    {
        Variable &augmented_start = this->variables.emplace("S'", Variable("S'")).first->second;
        Variable &original_start = this->rules[0].head();
        this->rules.insert(rules.begin(), Rule(Production({augmented_start}), Production({original_start})));
    }

    void find_first_follow()
    {
        bool changed = true;

        for (auto &[k, v] : variables)
        {
            v.first.clear();
            v.follow.clear();
        }

        augment_grammar();
        rules[0].head().follow.insert(terminator);

        while (changed)
        {
            changed = false;

            for (Rule &rule : rules)
            {
                Variable &v = rule.head();

                // The algorithm pseudocode for both First[·] and Follow[·] are available here:
                // [1] Ullman et al., Compilers (2nd ed.), pp. 220, §4.4.2
                // [2] Stephen Watt, "Parsing Algorithms 2: LR Parsing," Module 6, CS 4447, University of Western Ontario
                //     URL: https://www.csd.uwo.ca/~watt/home/courses/2008-09/cs4447a/notes/
                // [3] Aiken et al., "TDP and Introduction to BUP," Lecture 7, CS143, Stanford University
                //     URL: https://web.stanford.edu/class/cs143/lectures/

                // Find First[v].
                auto is_nullable = [&](Symbol &symbol)
                {
                    if (symbol.type() == Symbol::Type::Variable)
                    {
                        Variable &variable = dynamic_cast<Variable &>(symbol);
                        return variable.first.find(epsilon) != variable.first.end();
                    }
                    return false;
                };

                if (std::all_of(rule.rhs.str.begin(), rule.rhs.str.end(), is_nullable))
                {
                    changed |= v.first.emplace(epsilon).second;
                }

                for (Symbol &symbol : rule.rhs.str)
                {
                    if (symbol.type() == Symbol::Type::Terminal)
                    {
                        auto &terminal = dynamic_cast<Terminal &>(symbol);
                        changed |= v.first.insert(terminal).second;
                        break;
                    }
                    else
                    {
                        auto &variable = dynamic_cast<Variable &>(symbol);
                        for (Terminal &terminal : variable.first)
                        {
                            if (terminal != epsilon)
                                changed |= v.first.insert(terminal).second;
                        }
                    }
                    if (!is_nullable(symbol))
                        break;
                }

                // Find Follow[v].
                Vector<Reference<decltype(v.follow)>> follows = {v.follow};
                decltype(v.follow) terminal_follow; // for terminals, which doesn't have a follow set.

                for (auto it = rule.rhs.str.rbegin(); it != rule.rhs.str.rend(); ++it)
                {
                    Symbol &symbol = *it;

                    if (symbol.type() == Symbol::Type::Variable)
                    {
                        auto &variable = dynamic_cast<Variable &>(symbol);
                        for (auto &follow : follows)
                            for (Terminal &terminal : follow.get())
                                if (terminal != epsilon)
                                    changed |= variable.follow.insert(terminal).second;
                    }

                    if (is_nullable(symbol))
                    {
                        // Guaranteed to be a variable, since terminals aren't nullable.
                        auto &variable = dynamic_cast<Variable &>(symbol);
                        follows.push_back(variable.first);
                    }
                    else
                    {
                        // Terminal or non-nullable variable encountered, so we replace all follow sets.
                        follows.clear();
                        if (symbol.type() == Symbol::Type::Variable)
                            follows.push_back(dynamic_cast<Variable &>(symbol).first);
                        else
                        {
                            terminal_follow.clear();
                            terminal_follow.insert(dynamic_cast<Terminal &>(symbol));
                            follows.push_back(terminal_follow);
                        }
                    }
                }
            }
        }
    }

    void print_first()
    {
        std::cout << std::endl;
        for (auto& [_, variable] : variables)
        {
            std::cout << "First(" << variable << ") =";
            for (Terminal &t : variable.first)
                std::cout << ' ' << t;
            std::cout << std::endl;
        }
    }

    void print_follow()
    {
        std::cout << std::endl;
        for (auto& [_, variable] : variables)
        {
            std::cout << "Follow(" << variable << ") =";
            for (Terminal &t : variable.follow)
                std::cout << ' ' << t;
            std::cout << std::endl;
        }
    }
};

class Token
{
public:
    enum class Name
    {
        unknown = 0,
        whitespace, // whitespace
        num,        // number
        op,         // arithmetic operator
        paren,      // parentheses
        eof = -1    // end of file
    };

    using Attribute = std::variant<std::monostate, int>;

    friend std::ostream &operator<<(std::ostream &os, Token::Name name)
    {
        switch (name) {
            case Token::Name::unknown:    return os << "unknown";
            case Token::Name::whitespace: return os << "whitespace";
            case Token::Name::num:        return os << "num";
            case Token::Name::op:         return os << "op";
            case Token::Name::paren:      return os << "paren";
            case Token::Name::eof:        return os << "eof";
        }
    }

    Reference<Terminal> terminal;
    Name name;
    std::string lexeme;
    std::string::size_type lexeme_offset;
    Attribute attribute{}; // @todo: Generalize token attributes for more types.

    friend std::ostream &operator<<(std::ostream &os, const Token &token)
    {
        std::ostringstream buffer;
        buffer << '<' << token.name << ", " << token.lexeme << '>';
        return os << buffer.str();
    }
};

std::string string_pinpoint(const std::string &str, std::string::size_type offset, const std::string &message = {}) {
    using namespace std::string_literals;
    
    const long long context_length = 30;

    long long line_begin_idx = str.find_last_of('\n', offset);
    long long line_end_idx   = str.find_first_of('\n', offset);
    if (line_begin_idx == std::string::npos)
        line_begin_idx = 0ll;
    if (line_end_idx == std::string::npos)
        line_end_idx = str.length();

    auto begin_idx = std::max(static_cast<long long>(offset) - context_length, line_begin_idx);
    auto end_idx   = std::min(static_cast<long long>(offset) + context_length, line_end_idx);
    auto portion = std::string{
        str.begin() + begin_idx,
        str.begin() + end_idx
    };

    auto byline = message;
    byline += (begin_idx != line_begin_idx) ? "..."s : ""s;
    auto highlight = std::string(offset - begin_idx + byline.length(), ' ') + "^";
    byline += portion;
    byline += (end_idx != line_end_idx) ? "..."s : ""s;
    return byline + "\n" + highlight;
}

class LexicalAnalyzer
{
private:
    static bool match_any_char(std::string s, std::string m)
    {
        return s.length() == 1 && std::find(m.begin(), m.end(), s[0]) != m.end();
    }
    
    static std::string_view match_any_char(std::string_view s, std::string m) {
        if (std::find(m.begin(), m.end(), s[0]) != m.end())
            return std::string_view{s.begin(), std::next(s.begin())};
        else
            return std::string_view{s.begin(), s.begin()}; // or can we return std::string_view{}?
    }

public:
    using MatcherFunction = std::function<std::string_view(std::string_view)>;

    Map<Token::Name, Vector<Reference<Terminal>>> terminal_map;

    static const auto &matchers() {
        // @note: Alternatively, we could use the standard library's <regex> header.
        static Map<Token::Name, MatcherFunction> map = {
            {
                // Numbers: [0-9]+
                Token::Name::num,
                [](std::string_view v) -> std::string_view {
                    for (auto it = v.begin(); it != v.end(); ++it)
                        // @note: Assuming ASCII or ISO/IEC 8859 character encoding.
                        if (!::isdigit(*it))
                            return std::string_view{v.begin(), it};
                    return v;
                }
            },
            {
                // Operators: [+-*/]
                Token::Name::op,
                [](std::string_view v) -> std::string_view {
                    return match_any_char(v, "+-*/");
                }
            },
            {
                // Parentheses: [\[{()}\]]
                Token::Name::paren,
                [](std::string_view v) -> std::string_view {
                    return match_any_char(v, "[{()}]");
                }
            },
            {
                // Whitespace: [ ]+
                Token::Name::whitespace,
                [](std::string_view v) -> std::string_view {
                    auto it = v.begin() + v.find_first_not_of(' ');
                    return std::string_view{v.begin(), it};
                }
            }
        };
        return map;
    }

    LexicalAnalyzer(ContextFreeGrammar &grammar)
    {
        // @warning: auto& is necessary to prevent dangling references!
        for (auto& [_, terminal] : grammar.terminals)
        {
            auto name = terminal_match(terminal);
            
            if (terminal_map.contains(name))
                terminal_map[name].emplace_back(Reference<Terminal>{terminal});
            else
                terminal_map[name] = Vector<Reference<Terminal>>{Reference<Terminal>{terminal}};
        }
    }

    static Token::Name terminal_match(const Terminal &terminal)
    {
        // @todo: Generalize this logic, or include a Token::Name member field for Terminal.
        auto r = terminal.repr;
        if (r == "int" || r == "num")
            return Token::Name::num;
        if (match_any_char(r, "+-*/"))
            return Token::Name::op;
        if (match_any_char(r, "[{()}]"))
            return Token::Name::paren;
        return Token::Name::unknown;
    }

    using Match = std::pair<Token::Name, size_t>;

    static Match lexeme_match(const std::string_view &view)
    {
        // Implements greedy/longest matching technique.
        return std::accumulate(
            matchers().begin(),
            matchers().end(),
            Match{Token::Name::unknown, size_t{0}},
            [&view](auto result, auto matcher) -> Match {
                size_t match_length = matcher.second(view).length();
                if (match_length > result.second)
                    return std::pair{matcher.first, match_length};
                else
                    return result;
            }
        );
    }

    Vector<Token> get_tokens(const std::string &str)
    {
        std::string_view view = str;
        std::string_view::size_type offset = 0;

        // Match lexemes.
        Vector<Token> results;
        while (!view.empty())
        {
            // Find the lexeme.
            auto [name, length] = lexeme_match(view);
            auto lexeme = std::string{view.substr(0, length)};
            view.remove_prefix(length);
            offset += length;

            if (name == Token::Name::unknown) {
                auto pinpoint = string_pinpoint(str, offset, "    note: error occurred at: ");
                throw LexicalError("Unexpected token encountered\n" + pinpoint);
            }

            if (name == Token::Name::whitespace)
                continue;
            
            if (!terminal_map.contains(name)) {
                auto pinpoint = string_pinpoint(str, offset, "    note: error occurred at: ");
                throw LexicalError("Unable to find a symbol for the token: " + lexeme + "\n" + pinpoint);
            }

            // Find the related symbol instance, and then add it to the results.
            bool found = false;
            for (auto terminal : terminal_map[name])
                if (name == Token::Name::num || terminal.get().repr == lexeme)
                {
                    // Construct token based on token type.
                    if (name == Token::Name::num) {
                        int value = std::stoi(lexeme);
                        results.push_back(Token{terminal, name, std::move(lexeme), offset, value});
                    }
                    if (name == Token::Name::op)
                        results.push_back(Token{terminal, name, std::move(lexeme), offset, std::monostate{}});
                    if (name == Token::Name::paren)
                        results.push_back(Token{terminal, name, std::move(lexeme), offset, std::monostate{}});
                    found = true;
                }

            if (!found) {
                auto pinpoint = string_pinpoint(str, offset, "    note: error occurred at: ");
                throw LexicalError("Unable to find a symbol for the token: " + lexeme + "\n" + pinpoint);
            }
        }

        return results;
    }
};

class ParseTree
{
};

class Parser
{
};

class BottomUpParser : public Parser
{
protected:
    ContextFreeGrammar grammar;

public:
    struct Item : public DividedRule, public BasicVertex
    {
    };
    struct State : public Vector<DividedRule>, public BasicVertex
    {
        using Vector<DividedRule>::Vector;

        std::optional<DividedRule> get_final_item() const {
            for (auto& item : *this)
                if (item.is_final_item())
                    return item;
            return std::nullopt;
        }

        friend std::ostream &operator<<(std::ostream &os, const State &item_collection)
        {
            const auto prefix = " - [", separator = "\n        ", postfix = "]";
            std::ostringstream buffer;
            bool first = true;
            for (auto &item : item_collection)
            {
                if (first)
                {
                    buffer << prefix;
                    first = false;
                }
                else
                    buffer << separator;
                buffer << item;
            }
            buffer << postfix;
            return os << buffer.str();
        }
    };

    template <typename TVert>
    struct Transition : public BasicEdge<TVert>
    {
        Reference<Symbol> symbol;
    };

    using NFAGraph = Graph<Item, Transition<Item>>;
    using DFAGraph = Graph<State, Transition<State>>;

    explicit BottomUpParser(const ContextFreeGrammar &grammar_) : grammar(grammar_) {}

    auto closure(const Vector<DividedRule> &initial_rules, bool include_initial = false, bool recurse = true)
    {
        // Pseudocode: From [1], pp. 245
        //     Closure(I):
        //         repeat
        //         for any item A → α . Xβ
        //             for any X → γ
        //                 I += X → . γ
        //         until I does not change

        Set<DividedRule> results;
        Stack<Reference<Variable>> todo;

        if (include_initial)
            results.insert(initial_rules.begin(), initial_rules.end());

        for (const DividedRule &rule : initial_rules)
        {
            if (!rule.is_final_item())
            {
                Symbol &symbol = *rule.rhs.divider_it;
                if (symbol.type() == Symbol::Type::Variable)
                    todo.push(dynamic_cast<Variable &>(symbol));
            }
        }

        while (!todo.empty())
        {
            Variable &head = todo.top();
            todo.pop();

            for (const Rule &rule : grammar.rules)
                if (head == rule.head())
                {
                    auto [_, inserted] = results.emplace(rule);
                    if (!inserted)
                        continue;

                    auto symbol_it = rule.rhs.str.begin();
                    if (symbol_it != rule.rhs.str.end())
                        if (symbol_it->get().type() == Symbol::Type::Variable)
                            todo.push(dynamic_cast<Variable &>(symbol_it->get()));
                }

            if (!recurse)
                break;
        }

        return State(results.begin(), results.end());
    }

    auto closure(const DividedRule &initial_rule, bool include_initial = true, bool recurse = true)
    {
        return closure(Vector<DividedRule>{initial_rule}, include_initial, recurse);
    }

    NFAGraph construct_nfa()
    {
        // State Diagram construction
        //     T = Closure({ S' → . S$ }); // states
        //     E = {} // edges (gotos and shifts)
        //     repeat until no change in E or T
        //         for each state I in T
        //             for each item A → α . Xβ in I
        //                 J = Goto(I,X);
        //                 T += J;
        //                 E += (X: (I,J)) // the edge (I,J) labeled X

        NFAGraph graph;
        Stack<size_t> todo;

        Item first_item = {grammar.rules[0]};
        size_t first_item_idx = graph.insert_vert(first_item).first;

        todo.push(first_item_idx);

        while (!todo.empty())
        {
            size_t item_idx = todo.top();
            todo.pop();

            auto closure_set = closure(graph.verts()[item_idx], false, false);
            for (auto &prod : closure_set)
            {
                auto edge = typename NFAGraph::edge_value_type{.symbol = epsilon};
                auto result = graph.insert_neighbor(item_idx, Item{prod}, edge);

                if (result.vert_inserted)
                    todo.emplace(result.vert_position);
            }

            auto item_copy = graph.verts()[item_idx];
            auto &symbol = item_copy.rhs.shift();
            if (symbol.type() != Symbol::Type::Terminal || symbol != epsilon)
            {
                NFAGraph::edge_value_type edge = {.symbol = symbol};
                auto result = graph.insert_neighbor(item_idx, item_copy, edge);

                if (result.vert_inserted)
                    todo.emplace(result.vert_position);
            }
        }

        return graph;
    }

    DFAGraph construct_dfa()
    {
        DFAGraph graph;
        Stack<size_t> todo;
        Map<Reference<Symbol>, Vector<DividedRule>> next;

        DividedRule first_item = grammar.rules[0];
        State first_item_collection = closure(first_item, true, true);
        size_t first_item_collection_idx = graph.insert_vert(first_item_collection).first;

        todo.push(first_item_collection_idx);

        while (!todo.empty())
        {
            auto item_collection_idx = todo.top();
            todo.pop();
            auto &item_collection = graph.verts()[item_collection_idx];

            next.clear();

            for (DividedRule item_copy : item_collection)
            {
                auto &symbol = item_copy.rhs.shift();
                if (symbol.type() != Symbol::Type::Terminal || symbol != epsilon)
                {
                    if (next.contains(symbol))
                        next[symbol].push_back(item_copy);
                    else
                        next[symbol] = Vector<DividedRule>{item_copy};
                }
            }

            for (auto [symbol, rules] : next)
            {
                auto closure_set = closure(rules, true, true);
                DFAGraph::edge_value_type edge = {.symbol = symbol};
                auto result = graph.insert_neighbor(item_collection_idx, closure_set, edge);

                if (result.vert_inserted)
                    todo.emplace(result.vert_position);
            }
        }

        return graph;
    }
};

std::ostream &operator<<(std::ostream &os, const BottomUpParser::NFAGraph &graph)
{
    os << "\nVertices:" << std::endl;
    for (auto &item : graph.verts())
        os << item << std::endl;

    const std::streamsize col_width = 24;
    os << "\nEdges:" << std::endl;
    os << std::left
       << std::setw(col_width) << "FROM"
       << std::setw(col_width) << "TO"
       << std::setw(col_width) << "TRANSITION" << std::endl
       << std::string(col_width * 3, '-') << std::endl;
    for (auto &edge : graph.edges())
    {
        os << std::left
           << std::setw(col_width) << graph.verts()[edge.from]
           << std::setw(col_width) << graph.verts()[edge.to]
           << edge.symbol
           << std::endl;
    }
    return os;
}

std::ostream &operator<<(std::ostream &os, BottomUpParser::DFAGraph graph)
{
    os << "\nVertices:" << std::endl;
    const std::streamsize index_width = 2;
    std::cout << std::left;
    size_t count = 0;
    for (auto &item : graph.verts())
        os << "I_" << std::setw(index_width) << count++ << item << std::endl;

    const std::streamsize col_width = 10;
    os << "\nEdges:" << std::endl;
    os << std::left
       << std::setw(col_width) << "FROM"
       << std::setw(col_width) << "TO"
       << std::setw(col_width) << "TRANSITION" << std::endl
       << std::string(col_width * 3, '-') << std::endl;
    for (auto &edge : graph.edges())
    {
        os << std::left
           << "I_" << std::setw(col_width - 2) << edge.from
           << "I_" << std::setw(col_width - 2) << edge.to
           << edge.symbol
           << std::endl;
    }
    return os;
}

struct SemanticTranslator {
    std::function<Token::Attribute(size_t, Vector<Token::Attribute>)> apply;
};

class SLR1Parser : public BottomUpParser
{
public:
    struct Action
    {
        enum class Type
        {
            Error,
            Accept,
            Shift,
            Reduce
        };

        Type type = Type::Error;
        size_t index = std::numeric_limits<size_t>::max();
        
        friend std::ostream& operator<<(std::ostream& os, Action action) {
            std::ostringstream buffer;
            switch (action.type) {
                case Action::Type::Error:  break;
                case Action::Type::Accept: buffer << "Acc"; break;
                case Action::Type::Reduce: buffer << "R" << action.index; break;
                case Action::Type::Shift:  buffer << "S" << action.index; break;
            }
            return os << buffer.str();
        }
    };

    struct Goto
    {
        size_t index = std::numeric_limits<size_t>::max();
        
        friend std::ostream& operator<<(std::ostream& os, Goto goto_) {
            return os << goto_.index;
        }
    };

    using ActionKey = std::pair<size_t, Reference<Terminal>>;
    using GotoKey = std::pair<size_t, Reference<Variable>>;

    DFAGraph graph;
    Map<ActionKey, Action> actions;
    Map<GotoKey, Goto> gotos;
    
    using BottomUpParser::BottomUpParser;

    // Populate the action and goto tables.
    SLR1Parser &construct_table()
    {
        graph = construct_dfa();
        
        for (auto it = graph.verts().begin(); it != graph.verts().end(); ++it) {
            auto &item_collection = *it;
            auto  item_collection_idx = it - graph.verts().begin();

            auto final_item = item_collection.get_final_item();
            
            if (!final_item)
                continue;
            
            if (final_item->base() == grammar.rules[0]) {
                // Match the accept action.
                auto key = ActionKey{item_collection_idx, terminator};
                actions[key] = Action{Action::Type::Accept};
            }
            else
            {
                // Match the reduce action.
                auto rule_it = std::find(grammar.rules.begin(), grammar.rules.end(), final_item->base());
                if (rule_it == grammar.rules.end())
                    throw SyntaxError("Cannot find the rule instance for a divided rule instance of the grammar");
                size_t reduce_idx = std::distance(grammar.rules.begin(), rule_it);
                
                for (Terminal &t : final_item->head().follow) {
                    auto key = ActionKey{item_collection_idx, t};
                
                    if (actions.contains(key) && actions[key].type == Action::Type::Reduce)
                        throw GrammarError("Reduce-reduce conflict detected in grammar");
    
                    actions[key] = Action{Action::Type::Reduce, reduce_idx};
                }
            }
        }
        
        for (auto &edge : graph.edges())
        {
            Symbol &symbol = edge.symbol;
            
            // Populate the action table.
            if (symbol.type() == Symbol::Type::Terminal)
            {
                auto &terminal = dynamic_cast<Terminal &>(symbol);
                auto key = ActionKey{edge.from, terminal};
                
                if (actions.contains(key) && actions[key].type == Action::Type::Reduce)
                    throw GrammarError("Shift-reduce conflict detected in grammar");

                actions[key] = Action{Action::Type::Shift, edge.to};
            }
            // Populate the goto table.
            else
            {
                // Make a goto.
                auto &variable = dynamic_cast<Variable &>(symbol);
                auto key = GotoKey{edge.from, variable};
                gotos[key] = Goto{edge.to};
            }
        }
        
        return *this;
    }
    
    void parse(Vector<Token> tokens, std::optional<SemanticTranslator> translator, const std::string &str)
    {
        using StateSymbol = std::pair<size_t, Reference<Symbol>>;
        
        Stack<StateSymbol> stack;
        Terminal dummy("_");
        
        Stack<Token::Attribute> sdt_stack; // syntax-directed translation
        
        stack.push(StateSymbol(0, dummy));
        tokens.push_back(Token{terminator, Token::Name::eof, terminator.repr});
        auto input_it = tokens.begin();
        
        // @debug
        std::streamsize token_width = 0;
        const std::streamsize column_width = 10;
        for (auto &token : tokens) {
            std::ostringstream buffer;
            buffer << token;
            size_t length = buffer.str().length();
            if (token_width < length)
                token_width = length;
        }
        auto print_stack = [&dummy](Stack<StateSymbol> stack)
        {
            Stack<StateSymbol> temp;
            for (; !stack.empty(); stack.pop())
                temp.push(stack.top());
            for (; !temp.empty(); temp.pop()) {
                auto &top = temp.top();
                if (top.second.get() != dummy)
                    std::cout << "<"
                              << std::left  << std::setw(4) << (std::to_string(top.first) + ", ")
                              << std::right << std::setw(3) << top.second.get() << "> ";
                else
                    std::cout << "<" << top.first << "> ";
            }
            std::cout << std::endl;
        };
        auto print_sdt_stack = [](Stack<Token::Attribute> stack)
        {
            decltype(stack) temp;
            for (; !stack.empty(); stack.pop())
                temp.push(stack.top());
            for (; !temp.empty(); temp.pop()) {
                auto &top = temp.top();
                if (std::holds_alternative<int>(top))
                    std::cout << std::get<int>(top) << " ";
                else
                    std::cout << "_ ";
            }
            std::cout << std::endl;
        };
        
        std::cout << std::endl
                  << std::setw( token_width) << "Token"  << " | "
                  << std::setw(column_width) << "Action" << " | "
                  << "Stack\n";
        std::cout << std::string( token_width, '-') << "-+-"
                  << std::string(column_width, '-') << "-+-"
                  << std::string(80, '-') << std::endl;
        
        while (!stack.empty())
        {
            auto key = ActionKey{stack.top().first, input_it->terminal};
            Action action;
            
            if (actions.contains(key))
                action = actions[key];
            
            // @debug
            std::cout << std::right << std::setw( token_width) << *input_it << " | "
                      << std::right << std::setw(column_width) << action << " | ";
            
            if (translator.has_value())
                print_sdt_stack(sdt_stack);
            else
                print_stack(stack);
            
            switch(action.type)
            {
                case Action::Type::Shift:
                    stack.push(StateSymbol{action.index, input_it->terminal});
                    sdt_stack.push(input_it->attribute);
                    ++input_it;
                    break;
                case Action::Type::Reduce: {
                    const auto &rule = grammar.rules[action.index];
                    size_t pop_count = rule.rhs.str.size();
                    Vector<Token::Attribute> popped{};
                    while (pop_count--) {
                        stack.pop();
                        if (translator.has_value()) {
                            popped.insert(popped.begin(), sdt_stack.top());
                            sdt_stack.pop();
                        }
                    }
                    
                    auto &head = rule.head();
                    auto key = GotoKey{stack.top().first, head};
                    stack.push(StateSymbol{gotos[key].index, head});
                    
                    if (translator.has_value()) {
                        auto result = translator->apply(action.index, popped);
                        sdt_stack.push(result);
                    }
                    
                    break;
                }
                case Action::Type::Accept:
                    std::cout << "\nResult: " << std::get<int>(sdt_stack.top()) << std::endl;
                    return;
                case Action::Type::Error:
                    auto pinpoint = string_pinpoint(str, input_it->lexeme_offset, "    note: error occurred at: ");
                    throw SyntaxError("Encountered an error action in the parsing table\n" + pinpoint);
            }
        }
    }
    
    friend std::ostream &operator<<(std::ostream &os, SLR1Parser &parser)
    {
        os << "\nSLR(1) Parsing Table:\n\n";
        
        Vector<Reference<Terminal>> terminals;
        Vector<Reference<Variable>> variables;
        
        for (auto& [_, terminal] : parser.grammar.terminals)
            terminals.emplace_back(terminal);
        for (auto& [_, variable] : parser.grammar.variables)
            if (variable != parser.grammar.rules[0].head())
                variables.emplace_back(variable);

        terminals.emplace_back(terminator);
        
        const std::streamsize column_width = 8;
        const std::streamsize  index_width = 2 + std::to_string(parser.graph.verts().size()-1).length();
        const std::streamsize action_width = column_width * terminals.size();
        const std::streamsize   goto_width = column_width * variables.size();
        
        // Print heading.
        os << std::setw( index_width) << ""
           << "   "
           << std::setw(action_width) << "Action"
           << "   "
           << std::setw(  goto_width) << "Goto" << '\n';
        
        // Print symbols.
        os << std::setw(index_width) << "";
        os << "   ";
        for (Terminal &terminal : terminals)
            os << std::setw(column_width) << terminal;
        os << "   ";
        for (Variable &variable : variables)
            os << std::setw(column_width) << variable;
        os << '\n';
        
        // Print horizontal separator.
        auto print_separator = [&] {
            os << std::string( index_width, ' ')
               << " +-"
               << std::string(action_width, '-')
               << "-+-"
               << std::string(  goto_width, '-')
               << "-+ \n";
        };
        
        print_separator();
        
        // Print actions & gotos.
        for (size_t i = 0; i < parser.graph.verts().size(); ++i) {
            os << std::right;
            os << std::setw(index_width) << ("I_" + std::to_string(i));
            
            os << std::left;
            os << " | ";
            
            for (Terminal &terminal : terminals) {
                os << std::setw(column_width);
                auto key = ActionKey{i, terminal};
                
                if (parser.actions.contains(key))
                    os << parser.actions.at(key);
                else
                    os << "";
            }
            
            os << " | ";
            
            for (Variable &variable : variables) {
                os << std::setw(column_width);
                auto key = GotoKey{i, variable};
                
                if (parser.gotos.contains(key))
                    os << parser.gotos.at(key);
                else
                    os << "";
            }
            
            os << " | \n";
        }
        
        // Print horizontal separator.
        print_separator();
        
        return os;
    }
};
    
int main()
{
    // ContextFreeGrammar grammar({
    //     "E -> T + E",
    //     "E -> T",
    //     "T -> int * T",
    //     "T -> int",
    //     "T -> ( E )"
    // });
    
    // ContextFreeGrammar grammar({
    //     "E -> E + T",
    //     "E -> T",
    //     "T -> T * F",
    //     "T -> F",
    //     "F -> id"
    // });
    
    ContextFreeGrammar grammar({
        "E -> E + T",
        "E -> T",
        "T -> T * F",
        "T -> F",
        "F -> ( E )",
        "F -> int"
    });
    
    // ADJUST THIS TO ADD MORE RULES    
    SemanticTranslator translator{
        std::function([](size_t index, Vector<Token::Attribute> popped) -> Token::Attribute {
            switch (index) {
                case 1: return std::get<int>(popped[0]) + std::get<int>(popped[2]);
                case 2: return popped[0];
                case 3: return std::get<int>(popped[0]) * std::get<int>(popped[2]);
                case 4: return popped[0];
                case 5: return popped[1];
                case 6: return popped[0];
                default:
                    throw SemanticError("No semantic rule exists for the given grammar index: " + std::to_string(index));
            }
        })
    };

    // ContextFreeGrammar grammar({
    //     "X -> Y",
    //     "X -> a",
    //     "Y ->",
    //     "Y -> c",
    //     "Z -> d",
    //     "Z -> X Y Z"
    // });

    // ContextFreeGrammar grammar({
    //     "E -> T X",
    //     "T -> ( E )",
    //     "T -> int Y",
    //     "X -> + E",
    //     "X ->",
    //     "Y -> * T",
    //     "Y ->"
    // });
    
    // ContextFreeGrammar grammar({
    //     "S -> if C then S S'",
    //     "S -> if C then S",
    //     "S -> a",
    //     "S' -> else S",
    //     "S' -> ",
    //     "C -> b"
    // });
    
    grammar.find_first_follow();
    std::cout << std::boolalpha << grammar;
    grammar.print_first();
    grammar.print_follow();

    try
    {
        SLR1Parser parser(grammar);
        
        // std::cout << '\n' << "Non-deterministic Finite Automaton (NFA):"
        //           << '\n' << "========================================="
        //           << parser.construct_nfa() << std::endl;
                  
        std::cout << '\n' << "Deterministic Finite Automaton (DFA):"
                  << '\n' << "====================================="
                  << parser.construct_dfa() << std::endl;
        
        std::cout << '\n' << "Syntax Analysis:"
                  << '\n' << "================"
                  << parser.construct_table() << std::endl;

        bool first = true;
        
        std::cout << '\n' << "Lexical Analysis:"
                  << '\n' << "=================";
        
        while (true)
        {
            try
            {
                LexicalAnalyzer lexer(grammar);
                std::string input = "1 * ( 2 + 3 )";
                
                auto process = [&]{
                    std::cout << "\nParsing the input string: \"" << input << '"' << std::endl;
                    auto tokens = lexer.get_tokens(input);
                    for (auto token : tokens)
                        std::cout << token << std::endl;
                    try
                    {
                        parser.parse(tokens, translator, input);
                    }
                    catch (const SyntaxError &ex)
                    {
                        std::cerr << ex.what() << std::endl;
                    }
                };
                
                if (first)
                    first = false, process();
                
                while (std::cout << "\n>>> ", std::getline(std::cin, input) && !input.empty())
                    process();
            }
            catch (const LexicalError &ex)
            {
                std::cerr << ex.what() << std::endl;
                continue;
            }
            break;
        }
    }
    catch (const GrammarError &ex)
    {
        std::cerr << ex.what() << std::endl;
        return 1;
    }

    std::cout << "\nDone!" << std::endl;
    return 0;
}