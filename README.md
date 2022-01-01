# yaparser
*Yet Another Parser — A flexible C++20-based SDT-enabled parser*

## Usage
An example grammar and a set of syntax-directed rules have been defined in `main()` with only the following lines of code:

```c++
ContextFreeGrammar grammar({
    "E -> E + T",
    "E -> T",
    "T -> T * F",
    "T -> F",
    "F -> ( E )",
    "F -> int"
});

SemanticTranslator translator{
    std::function([](size_t index, Vector<Token::Attribute> popped) -> Token::Attribute {
        switch (index) {
            case 1: return std::get<int>(popped[0]) + std::get<int>(popped[2]);   // E -> E + T
            case 2: return popped[0];                                             // E -> T
            case 3: return std::get<int>(popped[0]) * std::get<int>(popped[2]);   // T -> T * F
            case 4: return popped[0];                                             // T -> F
            case 5: return popped[1];                                             // F -> ( E )
            case 6: return popped[0];                                             // F -> int
            default:
                throw SemanticError("No semantic rule exists for the given grammar index: " + std::to_string(index));
        }
    })
};
```

This is all that you need to make a working arithmetic calculator!

## Demonstration
![](docs/images/demo.png?raw=true "Parse of \"1 * (2 + 3)\"")

## Compilation
- GCC/Clang:
  - Unix/Bash: `mkdir -p ./bin && g++ -std=c++20 ./main.cpp -o ./bin/parser`
  - Windows/PowerShell: `New-Item -ItemType Directory -Force -Path .\bin; g++ -std=c++20 ./main.cpp -o ./bin/parser`

## Walkthrough
This is a flexible parser program that:
1. Takes a context-free grammar given as a list of strings.
2. Parses the grammar to make an SLR(1) parsing table.
3. Takes in an input string.
4. Uses a longest-matching lexical analyzer to split up and label the string into tokens.
5. Takes in syntax rules defined as lambda functions to make a semantic translator.
5. Uses the table and token list to parse a given string, using the translator to apply logic.

## Current issues
- [ ] Need to organize code structure into separate files
- [ ] Need to optimize parser and lexical analyzer
- [ ] Custom vector class has memory leak issues with non-trivial datatypes
