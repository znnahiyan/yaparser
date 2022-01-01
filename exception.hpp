#ifndef EXCEPTION_HPP_INCLUDED
#define EXCEPTION_HPP_INCLUDED

#include <stdexcept>

class NotImplementedException : public std::logic_error {
public:
    NotImplementedException() : std::logic_error{"Function not yet implemented."} {}
};

class CompilerError : public std::logic_error { using std::logic_error::logic_error; };

class LexicalError  : public CompilerError { public: explicit LexicalError (const std::string& msg) : CompilerError("\nLexicalError: "  + msg) {} };
class GrammarError  : public CompilerError { public: explicit GrammarError (const std::string& msg) : CompilerError("\nGrammarError: "  + msg) {} };
class SyntaxError   : public CompilerError { public: explicit SyntaxError  (const std::string& msg) : CompilerError("\nSyntaxError: "   + msg) {} };
class SemanticError : public CompilerError { public: explicit SemanticError(const std::string& msg) : CompilerError("\nSemanticError: " + msg) {} };

#endif // EXCEPTION_HPP_INCLUDED