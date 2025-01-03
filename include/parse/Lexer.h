/*
    This file defines the Lexical Analyzer for chocopy.
*/

#ifndef CHOCOPYC_PARSE_LEXER_H
#define CHOCOPYC_PARSE_LEXER_H

#include "source/SourceFile.h"

#include <stack>

namespace chocopyc::Parse {
// This object is the lexical analyzer that will scan the chocopy source file
// for tokens.
class Lexer {
    // This field represents the instance of the source file information object.
    std::shared_ptr<Source::SourceFile> src_file;

    // This field represents the current pointer within the buffer.
    char *ptr;

    // This field represents the stack that will be used to track whitespace
    // values for indenting.
    std::stack<int> whitespace_stack;

  public:
    Lexer(std::shared_ptr<Source::SourceFile> src_file)
        : src_file{src_file}, ptr{src_file->get_start_ptr()} {}

    // This method will serve as the main lexer routine.
    auto lex_next_tok() -> void;
};
} // namespace chocopyc::Parse

#endif