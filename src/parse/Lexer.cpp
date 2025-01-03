/*
    This file implements the Lexical Analyzer for chocopy.
*/

#include "parse/Lexer.h"

#include <print>

namespace chocopyc::Parse {
    auto Lexer::lex_next_tok() -> void {
        // Do something significant to not get optimized out.
        std::println("lexer routine");
    }
} // namespace chocopyc::Parse